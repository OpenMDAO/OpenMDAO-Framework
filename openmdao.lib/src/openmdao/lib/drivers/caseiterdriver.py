__all__ = ('CaseIteratorDriver',)
__version__ = '0.1'

import Queue
import threading
import time

from openmdao.main import Driver, Bool
from openmdao.main.component import RUN_OK, RUN_FAILED, RUN_STOPPED
from openmdao.main.interfaces import ICaseIterator
from openmdao.main.variable import INPUT

SERVER_EMPTY    = 1
SERVER_READY    = 2
SERVER_COMPLETE = 3
SERVER_ERROR    = 4

class ServerError(Exception):
    pass


class CaseIteratorDriver(Driver):
    """
    Run a set of cases provided by an ICaseIterator in a manner similar
    to the ROSE framework.

    TODO: support concurrent evaluation.
    """

    def __init__(self, name, parent=None, doc=None):
        super(CaseIteratorDriver, self).__init__(name, parent, doc)

        Bool('sequential', self, INPUT, default=True,
             doc='Evaluate cases sequentially.')

        self.add_socket('iterator', ICaseIterator, 'Cases to evaluate.')
        self.add_socket('outerator', None, 'Something to append() to.')

        self._operator = Operator(self)
        self._conductor = Conductor(self)
        self._conductor.set_operator(self._operator)
        self._operator.set_conductor(self._conductor)

    def execute(self):
        """ Run each case in iterator and record results in outerator. """
        if not self.check_socket('iterator'):
            self.error('No iterator plugin')
            return RUN_FAILED

        if not self.check_socket('outerator'):
            self.error('No outerator plugin')
            return RUN_FAILED

        self._iterator = self.iterator.__iter__()
        return self._operator.start()


class Operator(object):
    """ ROSE operator which uses multiple local server processes. """

    def __init__(self, driver, n_servers=0):
        self.driver = driver
        self.assembly = driver.parent
        self.n_servers = n_servers
        self.conductor = None
        self.model_file = None
        self.reply_queue = None
        self.server_lock = None
        self.servers = {}  # Server information, keyed by name.
        self.queues = {}   # Request queues, keyed by server name.
        self.in_use = {}   # In-use flags, keyed by server name.

    def set_conductor(self, conductor):
        """ Set our conductor. """
        self.conductor = conductor

    def start(self):
        """ Start model evaluations. Continue until conductor is done. """
        if self.driver.sequential or self.n_servers < 1:
            self.driver.info('Operator.start(), sequential.')
            while self.conductor.server_ready(None):
                pass
        else:
            self.driver.info('Operator.start(), n_servers %d', self.n_servers)
            self.driver.raise_exception('Concurrent evaluation is not supported yet.',
                                        NotImplementedError)

            # Replicate model and save to file.
            # Must do this before creating any locks or queues.
            replicant = self.assembly.replicate()
            self.model_file = 'replicant.dam'
            replicant.save(self.model_file)
            del replicant

            # Start servers.
            self.server_lock = threading.Lock()
            self.reply_queue = Queue.Queue()
            for i in range(self.n_servers):
                name = 'cid_%d' % (i+1)
                server_thread = threading.Thread(target=self._service_loop,
                                                 args=(name,))
                server_thread.setDaemon(True)
                server_thread.start()
                time.sleep(0.1)  # Pacing for GX at least.

            for i in range(self.n_servers):
                name, status = self.reply_queue.get()
            if len(self.servers) > 0:
                if len(self.servers) < self.n_servers:
                    self.driver.warning('Only %d servers created',
                                        len(self.servers))
            else:
                self.driver.raise_exception('No servers created!', RuntimeError)

            # Kick-off initial state.
            for name in self.servers.keys():
                self.in_use[name] = self.conductor.server_ready(name)

            # Continue until no servers are busy.
            while self._busy():
                name, result = self.reply_queue.get()
                self.in_use[name] = self.conductor.server_ready(name)

            # Shut-down servers.
            for name in self.servers.keys():
                self.queues[name].put(None)
            for i in range(len(self.servers)):
                name, status = self.reply_queue.get()

            # Clean up.
            self.reply_queue = None
            self.server_lock = None
            self.servers = {}
            self.queues = {}
            self.in_use = {}

        if self.driver._stop:
            return RUN_STOPPED
        else:
            return RUN_OK

    def _service_loop(self, name):
        """ Each server has an associated thread executing this. """
#        ram = da.Simulation.get_simulation().ram
        ram = None
        server, server_info = ram.allocate({}, transient=True)
        if server is None:
            self.driver.error('Server allocation for %s failed :-(', name)
            self.reply_queue.put((name, False))
            return

# TODO: external files should be part of saved state.
        ram.walk(self.assembly, server)

        request_queue = Queue.Queue()

        self.server_lock.acquire()
        self.servers[name] = server
        self.queues[name] = request_queue
        self.in_use[name] = False
        self.server_lock.release()

        self.reply_queue.put((name, True))

        while True:
            request = request_queue.get()
            if request is None:
                break
            result = request[0](request[1])
            self.reply_queue.put((name, result))

        ram.release(server)
        self.reply_queue.put((name, True))

    def _busy(self):
        """ Return True while at least one server is in use. """
        for name in self.servers.keys():
            if self.in_use[name]:
                return True
        return False

    def load_model(self, server):
        """ Load a model into a server. """
        if server is not None:
            self.queues[server].put((self._load_model, server))
        return True

    def _load_model(self, server):
        """ Load model into server. """
# TODO: use filexfer() utility.
        inp = open(self.model_file, 'rb')
        out = self.servers[server].open(self.model_file, 'wb')
        chunk = 1 << 17    # 128KB
        data = inp.read(chunk)
        while data:
            out.write(data)
            data = inp.read(chunk)
        inp.close()
        out.close()
        if not self.servers[server].load_model(self.model_file):
            self.driver.error('server.load_model failed :-(')
            return False
        return True

    def model_set(self, server, name, index, value):
        """ Set value in server's model. """
        self.assembly.driver.debug("model_set '%s' %s", name, value)
        comp_name, attr = name.split('.', 1)
        if server is None:
            comp = getattr(self.assembly, comp_name)
        else:
            comp = getattr(self.servers[server].tla, comp_name)
        comp.set(attr, value, index)
        return True

    def model_get(self, server, name, index):
        """ Get value from server's model. """
        self.assembly.driver.debug("model_get '%s'", name)
        comp_name, attr = name.split('.', 1)
        if server is None:
            comp = getattr(self.assembly, comp_name)
        else:
            comp = getattr(self.servers[server].tla, comp_name)
        return comp.get(attr, index)

    def model_execute(self, server):
        """ Execute model in server. """
        self.assembly.driver.debug('model_execute')
        if server is None:
            self.assembly.workflow.run()
        else:
            self.queues[server].put((self._model_execute, server))

    def _model_execute(self, server):
        """ Execute model. """
        self.servers[server].tla.run()

    def model_status(self, server):
        """ Return execute status from model. """
        if server is None:
            return self.assembly.workflow.execute_status
        else:
            return self.servers[server].tla.execute_status

    def model_cleanup(self, server):
        """ Clean up model resources. """
        return True


class Conductor(object):
    """
    Responds to asynchronous callbacks from Operator to run cases
    retrieved from Iterator.  Results are processed by Outerator.
    """

    def __init__(self, driver):
        self.driver = driver
        self.server_states = {}
        self.server_cases = {}
        self.rerun = []

        self.operator = None

        self.reload_model = True    

    def set_operator(self, operator):
        self.operator = operator

    def server_ready(self, server):
        """
        Invoked by Operator.start().
        Returns True is there are more cases to run.
        """
# TODO: improve response to a stop request.
        server_state = self.server_states.get(server, SERVER_EMPTY)
        if server_state == SERVER_EMPTY:
            try:
                self.operator.load_model(server)
                self.server_states[server] = SERVER_READY
                return True
            except ServerError:
                self.server_states[server] = SERVER_ERROR
                return True

        elif server_state == SERVER_READY:
            # Test for stop request.
            if self.driver._stop:
                return False
            # Check if there are cases that need to be rerun.
            if self.rerun:
                self._run_case(self.rerun.pop[0], server)
                return True
            else:
                # Try to get a new case.
                try:
                    case = self.driver._iterator.next()
                except StopIteration:
                    return False
                else:
                    self._run_case(case, server)
                    return True
        
        elif server_state == SERVER_COMPLETE:
            try:
                case = self.server_cases[server]
                # Grab the data from the model.
                case.status = self.operator.model_status(server)
                for i, niv in enumerate(case.outputs):
                    case.outputs[i] = (niv[0], niv[1],
                        self.operator.model_get(server, niv[0], niv[1]))
                # Record the data.
                self.driver.outerator.append(case)

                if case.status == RUN_OK:
                    if self.reload_model:
                        self.operator.model_cleanup(server)
                        self.operator.load_model(server)
                else:
                    self.operator.load_model(server)
                self.server_states[server] = SERVER_READY
                return True
            except ServerError:
                # Handle server error separately.
                return True
        
        elif server_state == SERVER_ERROR:
            try:
                self.operator.load_model(server)
                self.server_states[server] = SERVER_READY
                return True
            except ServerError:
                return True

    def _run_case(self, case, server):
        """ Setup and run case. """
        try:
            self.server_cases[server] = case
            for name, index, value in case.inputs:
                try:
                    self.operator.model_set(server, name, index, value)
                except Exception, exc:
                    msg = "Exception setting '%s': %s" % (name, str(exc))
                    self.driver.error(msg)
            self.operator.model_execute(server)
            self.server_states[server] = SERVER_COMPLETE
        except ServerError:
            self.server_states[server] = SERVER_ERROR
            self.rerun.append(case)

