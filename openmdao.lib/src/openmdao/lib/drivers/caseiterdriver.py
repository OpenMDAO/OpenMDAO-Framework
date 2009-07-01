__all__ = ('CaseIteratorDriver','ServerError')
__version__ = '0.1'

import Queue
import threading
import time

from enthought.traits.api import Range, Bool, Instance, Any

from openmdao.main.api import Driver
from openmdao.main.exceptions import RunStopped
from openmdao.main.interfaces import ICaseIterator, IComponent
from openmdao.main.util import filexfer


SERVER_EMPTY    = 1
SERVER_READY    = 2
SERVER_COMPLETE = 3
SERVER_ERROR    = 4

class ServerError(Exception):
    """ Raised when a server therad has problems. """
    pass


class CaseIteratorDriver(Driver):
    """
    Run a set of cases provided by an ICaseIterator in a manner similar
    to the ROSE framework.

    .. parsed-literal::

        TODO: define interface for 'outerator'.
        TODO: support concurrent evaluation.
        TODO: improve response to a stop request.

    """

    iterator = Instance(ICaseIterator, desc='Cases to evaluate.', required=True)
    outerator = Instance(object, desc='Something to append() to.', required=True)
    model = Instance(IComponent, desc='Model to be executed.', required=True)
    
    sequential = Bool(True, iostatus='in',
                      desc='Evaluate cases sequentially.')

    reload_model = Bool(True, iostatus='in',
                        desc='Reload model between executions.')

    max_retries = Range(value=1, low=0, iostatus='in',
                        desc='Number of times to retry a case.')

    def __init__(self, *args, **kwargs):
        super(CaseIteratorDriver, self).__init__(*args, **kwargs)

        self._iter = None
        self._n_servers = 0
        self._model_file = None
        self._reply_queue = None
        self._server_lock = None
        self._servers = {}  # Server information, keyed by name.
        self._queues = {}   # Request queues, keyed by server name.
        self._in_use = {}   # In-use flags, keyed by server name.

        self._server_states = {}
        self._server_cases = {}
        self._exceptions = {}
        self._rerun = []

    def execute(self):
        """ Run each case in iterator and record results in outerator. """
        self._rerun = []
        self._iter = self.iterator.__iter__()

        if self.sequential or self._n_servers < 1:
            self.info('Start sequential evaluation.')
            while self._server_ready(None):
                pass
        else:
            self.info('Start concurrent evaluation, n_servers %d',
                      self._n_servers)
            self.raise_exception('Concurrent evaluation is not supported yet.',
                                 NotImplementedError)

            # Replicate model and save to file.
            # Must do this before creating any locks or queues.
            replicant = self.parent.replicate()
            self._model_file = 'replicant.dam'
            replicant.save(self._model_file)
            del replicant

            # Start servers.
            self._server_lock = threading.Lock()
            self._reply_queue = Queue.Queue()
            for i in range(self._n_servers):
                name = 'cid_%d' % (i+1)
                server_thread = threading.Thread(target=self._service_loop,
                                                 args=(name,))
                server_thread.setDaemon(True)
                server_thread.start()
                time.sleep(0.1)  # Pacing.

            for i in range(self._n_servers):
                name, status = self._reply_queue.get()
            if len(self._servers) > 0:
                if len(self._servers) < self._n_servers:
                    self.warning('Only %d servers created', len(self._servers))
            else:
                self.raise_exception('No servers created!', RuntimeError)

            # Kick-off initial state.
            for name in self._servers.keys():
                self._in_use[name] = self._server_ready(name)

            # Continue until no servers are busy.
            while self._busy():
                name, result = self._reply_queue.get()
                self._in_use[name] = self._server_ready(name)

            # Shut-down servers.
            for name in self._servers.keys():
                self._queues[name].put(None)
            for i in range(len(self._servers)):
                name, status = self._reply_queue.get()

            # Clean up.
            self._reply_queue = None
            self._server_lock = None
            self._servers = {}
            self._queues = {}
            self._in_use = {}

        if self._stop:
            self.raise_exception('Stop requested', RunStopped)

    def _server_ready(self, server):
        """
        Responds to asynchronous callbacks during execute() to run cases
        retrieved from Iterator.  Results are processed by Outerator.
        Returns True if there are more cases to run.
        """
        server_state = self._server_states.get(server, SERVER_EMPTY)
        if server_state == SERVER_EMPTY:
            try:
                self._load_model(server)
                self._server_states[server] = SERVER_READY
                return True
            except ServerError:
                self._server_states[server] = SERVER_ERROR
                return True

        elif server_state == SERVER_READY:
            # Test for stop request.
            if self._stop:
                return False
            # Check if there are cases that need to be rerun.
            if self._rerun:
                self._run_case(self._rerun.pop(0), server)
                return True
            else:
                # Try to get a new case.
                try:
                    case = self._iter.next()
                except StopIteration:
                    return False
                else:
                    if not case.max_retries:
                        case.max_retries = self.max_retries
                    case.retries = 0
                    case.msg = None
                    self._run_case(case, server)
                    return True
        
        elif server_state == SERVER_COMPLETE:
            try:
                case = self._server_cases[server]
                exc = self._model_status(server)
                if exc is None:
                    for i, niv in enumerate(case.outputs):
                        try:
                            case.outputs[i] = (niv[0], niv[1],
                                self._model_get(server, niv[0], niv[1]))
                        except Exception, exc:
                            msg = "Exception getting '%s': %s" % (niv[0], exc)
                            case.msg = '%s: %s' % (self.get_pathname(), msg)
                else:
                    case.msg = str(exc)
                self.outerator.append(case)

                if not case.msg:
                    if self.reload_model:
                        self._model_cleanup(server)
                        self._load_model(server)
                else:
                    self._load_model(server)
                self._server_states[server] = SERVER_READY
                return True
            except ServerError:
                # Handle server error separately.
                return True
        
        elif server_state == SERVER_ERROR:
            try:
                self._load_model(server)
            except ServerError:
                return True
            else:
                self._server_states[server] = SERVER_READY
                return True

    def _run_case(self, case, server):
        """ Setup and run a case. """
        try:
            self._server_cases[server] = case
            for name, index, value in case.inputs:
                try:
                    self._model_set(server, name, index, value)
                except Exception, exc:
                    msg = "Exception setting '%s': %s" % (name, exc)
                    self.raise_exception(msg, ServerError)
            self._model_execute(server)
            self._server_states[server] = SERVER_COMPLETE
        except ServerError, exc:
            self._server_states[server] = SERVER_ERROR
            if case.retries < case.max_retries:
                case.retries += 1
                self._rerun.append(case)
            else:
                case.msg = str(exc)
                self.outerator.append(case)

    def _service_loop(self, name):
        """ Each server has an associated thread executing this. """
#        ram = da.Simulation.get_simulation().ram
        ram = None
        server, server_info = ram.allocate({}, transient=True)
        if server is None:
            self.error('Server allocation for %s failed :-(', name)
            self._reply_queue.put((name, False))
            return

# TODO: external files should be part of saved state.
        ram.walk(self.parent, server)

        request_queue = Queue.Queue()

        self._server_lock.acquire()
        self._servers[name] = server
        self._queues[name] = request_queue
        self._in_use[name] = False
        self._server_lock.release()

        self._reply_queue.put((name, True))

        while True:
            request = request_queue.get()
            if request is None:
                break
            result = request[0](request[1])
            self._reply_queue.put((name, result))

        ram.release(server)
        self._reply_queue.put((name, True))

    def _busy(self):
        """ Return True while at least one server is in use. """
        for name in self._servers.keys():
            if self._in_use[name]:
                return True
        return False

    def _load_model(self, server):
        """ Load a model into a server. """
        if server is not None:
            self._queues[server].put((self._remote_load_model, server))
        return True

    def _remote_load_model(self, server):
        """ Load model into server. """
        filexfer(None, self._model_file,
                 self._servers[server], self._model_file, 'b')
        if not self._servers[server]._load_model(self._model_file):
            self.error('server._load_model failed :-(')
            return False
        return True

    def _model_set(self, server, name, index, value):
        """ Set value in server's model. """
        if server is None:
            self.parent.set(name, value, index)
        else:
            comp_name, attr = name.split('.', 1)
            comp = getattr(self._servers[server].tla, comp_name)
            comp.set(attr, value, index)
        return True

    def _model_get(self, server, name, index):
        """ Get value from server's model. """
        if server is None:
            return self.parent.get(name, index)
        else:
            comp_name, attr = name.split('.', 1)
            comp = getattr(self._servers[server].tla, comp_name)
            return comp.get(attr, index)

    def _model_execute(self, server):
        """ Execute model in server. """
        self._exceptions[server] = None
        if server is None:
            try:
                self.model.run()
            except Exception, exc:
                self._exceptions[server] = exc
        else:
            self._queues[server].put((self._remote_model_execute, server))

    def _remote_model_execute(self, server):
        """ Execute model. """
        try:
            self._servers[server].tla.run()
        except Exception, exc:
            self._exceptions[server] = exc

    def _model_status(self, server):
        """ Return execute status from model. """
        return self._exceptions[server]

    def _model_cleanup(self, server):
        """ Clean up model resources. """
        return True

