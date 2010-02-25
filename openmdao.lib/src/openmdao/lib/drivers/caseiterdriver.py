import os.path
import Queue
import sys
import threading
import time

from enthought.traits.api import Range, Bool, Instance

from openmdao.main.api import Component, Driver
from openmdao.main.exceptions import RunStopped
from openmdao.main.interfaces import ICaseIterator
from openmdao.main.util import filexfer
from openmdao.main.resource import ResourceAllocationManager as RAM

__all__ = ('CaseIteratorDriver', 'ServerError')

SERVER_EMPTY    = 'empty'
SERVER_READY    = 'ready'
SERVER_COMPLETE = 'complete'
SERVER_ERROR    = 'error'


class ServerError(Exception):
    """ Raised when a server thread has problems. """
    pass


class CaseIteratorDriver(Driver):
    """
    Run a set of cases provided by an :class:`ICaseIterator` in a manner similar
    to the ROSE framework. Concurrent evaluation is supported, with the various
    evaluations executed across servers obtained from the
    :class:`ResourceAllocationManager`.

    - The `iterator` socket provides the cases to be evaluated.
    - The `model` socket provides the model to be executed.
    - The `recorder` socket is used to record results.
    - If `sequential` is True, then the cases are evaluated sequentially.
    - If `reload_model` is True, the model is reloaded between executions.
    - `max_retries` sets the number of times to retry a failed case.

    .. parsed-literal::

        TODO: define interface for 'recorder'.
        TODO: support stepping and resuming execution.
        TODO: improve response to a stop request.

    """

    iterator = Instance(ICaseIterator, desc='Cases to evaluate.', required=True)
    recorder = Instance(object, desc='Something to append() to.', required=True)
    model = Instance(Component, desc='Model to be executed.', required=True)
    
    sequential = Bool(True, iostatus='in',
                      desc='Evaluate cases sequentially.')

    reload_model = Bool(True, iostatus='in',
                        desc='Reload model between executions.')

    max_retries = Range(value=1, low=0, iostatus='in',
                        desc='Number of times to retry a case.')

    def __init__(self, *args, **kwargs):
        super(CaseIteratorDriver, self).__init__(*args, **kwargs)

        self._iter = None
        self._replicants = 0

        self._egg_file = None
        self._egg_required_distributions = None
        self._egg_orphan_modules = None

        self._reply_queue = None
        self._server_lock = None
        self._servers = {}      # Server objects, keyed by name.
        self._top_levels = {}   # Server top-level objects, keyed by name.
        self._server_info = {}  # Server information, keyed by name.
        self._queues = {}       # Request queues, keyed by server name.
        self._in_use = {}       # In-use flags, keyed by server name.

        self._server_states = {}
        self._server_cases = {}
        self._exceptions = {}
        self._orphans = []      # Cases assigned to servers that wouldn't start.
        self._rerun = []        # Cases that failed and should be retried.

    def execute(self):
        """ Run each case in iterator and record results in recorder. """
        self._start()
        self._cleanup()
        if self._stop:
            self.raise_exception('Stop requested', RunStopped)

    def _start(self, replicate=True):
        """
        Start evaluating cases. If `replicate`, then replicate the model
        and save to an egg file first.
        """
        self._orphans = []
        self._rerun = []
        self._iter = self.iterator.__iter__()

        if self.sequential:
            self.info('Start sequential evaluation.')
            try:
                case = self._iter.next()
            except StopIteration:
                pass
            else:
                self._server_cases[None] = case
                self._server_states[None] = SERVER_EMPTY
                while self._server_ready(None):
                    pass
        else:
            self.info('Start concurrent evaluation')
            if replicate or self._egg_file is None:
                # Save model to egg.
                # Must do this before creating any locks or queues.
                self._replicants += 1
                version = 'replicant.%d' % (self._replicants)
                egg_info = self.model.save_to_egg(self.model.name, version)
                self._egg_file = egg_info[0]
                self._egg_required_distributions = egg_info[1]
                self._egg_orphan_modules = [name for name, path in egg_info[2]]

            # Determine max number of servers available.
            resources = {
                'required_distributions':self._egg_required_distributions,
                'orphan_modules':self._egg_orphan_modules,
                'python_version':sys.version[:3]}
            max_servers = RAM.max_servers(resources)
            self.debug('max_servers %d', max_servers)
            if max_servers <= 0:
                msg = 'No servers supporting required resources %s' % resources
                self.raise_exception(msg, RuntimeError)

            # Kick off initial wave of cases.
            self._server_lock = threading.Lock()
            self._reply_queue = Queue.Queue()
            n_servers = 0
            while n_servers < max_servers:
                # Grab next case.
                try:
                    case = self._iter.next()
                except StopIteration:
                    if self._orphans:
                        case = self._orphans.pop(0)
                    elif self._rerun:
                        case = self._rerun.pop(0)
                    else:
                        break

                # Start server worker thread.
                n_servers += 1
                name = '%s_%d' % (self.name, n_servers)
                self.debug('starting worker for %s', name)
                self._servers[name] = None
                self._in_use[name] = True
                self._server_cases[name] = case
                self._server_states[name] = SERVER_EMPTY
                server_thread = threading.Thread(target=self._service_loop,
                                                 args=(name, resources))
                server_thread.daemon = True
                server_thread.start()

                # Process any pending events.
                while self._busy():
                    try:
                        name, result = self._reply_queue.get(True, 0.1)
                    except Queue.Empty:
                        break
                    else:
                        if self._servers[name] is None:
                            self.debug('server startup failed for %s', name)
                            self._in_use[name] = False
                            self._orphans.append(self._server_cases[name])
                            self._server_cases[name] = None
                        else:
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
            self._top_levels = {}
            self._queues = {}
            self._in_use = {}

        return True

    def _cleanup(self):
        """ Cleanup egg file if necessary. """
        if self._egg_file and os.path.exists(self._egg_file):
            os.remove(self._egg_file)
            self._egg_file = None

    def _server_ready(self, server):
        """
        Responds to asynchronous callbacks during execute() to run cases
        retrieved from Iterator.  Results are processed by Outerator.
        Returns True if there are more cases to run.
        """
        state = self._server_states[server]
        self.debug('server %s state %s', server, state)

        if state == SERVER_EMPTY:
            try:
                self.debug('    load_model')
                self._load_model(server)
                self._server_states[server] = SERVER_READY
                return True
            except ServerError:
                self._server_states[server] = SERVER_ERROR
                return True

        elif state == SERVER_READY:
            # Test for stop request.
            if self._stop:
                self.debug('    stop requested')
                return False

            # Select case to run.
            if self._server_cases[server] is not None:
                self.debug('    run initial case')
                case = self._server_cases[server]
                if not case.max_retries:
                    case.max_retries = self.max_retries
                case.retries = 0
                case.msg = None
                self._run_case(case, server)
                return True
            elif self._rerun:
                self.debug('    rerun case')
                self._run_case(self._rerun.pop(0), server)
                return True
            else:
                try:
                    case = self._iter.next()
                except StopIteration:
                    self.debug('    no more cases')
                    return False
                else:
                    self.debug('    run next case')
                    if not case.max_retries:
                        case.max_retries = self.max_retries
                    case.retries = 0
                    case.msg = None
                    self._run_case(case, server)
                    return True
        
        elif state == SERVER_COMPLETE:
            case = self._server_cases[server]
            self._server_cases[server] = None
            try:
                exc = self._model_status(server)
                if exc is None:
                    # Grab the data from the model.
                    for i, niv in enumerate(case.outputs):
                        try:
                            case.outputs[i] = (niv[0], niv[1],
                                self._model_get(server, niv[0], niv[1]))
                        except Exception, exc:
                            msg = "Exception getting '%s': %s" % (niv[0], exc)
                            case.msg = '%s: %s' % (self.get_pathname(), msg)
                else:
                    self.debug('    exception %s', exc)
                    case.msg = str(exc)
                # Record the data.
                self.recorder.append(case)

                if not case.msg:
                    if self.reload_model:
                        self.debug('    reload')
                        self._model_cleanup(server)
                        self._load_model(server)
                else:
                    self.debug('    load')
                    self._load_model(server)
                self._server_states[server] = SERVER_READY
                return True
            except ServerError:
                # Handle server error separately.
                self.debug('    server error')
                return True

        elif state == SERVER_ERROR:
            self._server_cases[server] = None
            try:
                self._load_model(server)
            except ServerError:
                return True
            else:
                self._server_states[server] = SERVER_READY
                return True

        else:
            self.error('unexpected state %s for server %s', state, server)

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
                self.recorder.append(case)

    def _service_loop(self, name, resource_desc=None):
        """ Each server has an associated thread executing this. """
        resource_desc = resource_desc or {}
        server, server_info = RAM.allocate(resource_desc)
        if server is None:
            self.error('Server allocation for %s failed :-(', name)
            self._reply_queue.put((name, False))
            return

        request_queue = Queue.Queue()

        with self._server_lock:
            self._servers[name] = server
            self._server_info[name] = server_info
            self._queues[name] = request_queue

        self._reply_queue.put((name, True))  # ACK startup.

        while True:
            request = request_queue.get()
            if request is None:
                break
            result = request[0](request[1])
            self._reply_queue.put((name, result))

        RAM.release(server)
        self._reply_queue.put((name, True))  # ACK shutdown.

    def _busy(self):
        """ Return True while at least one server is in use. """
        for name in self._in_use.keys():
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
        filexfer(None, self._egg_file,
                 self._servers[server], self._egg_file, 'b')
        tlo = self._servers[server].load_model(self._egg_file)
        if not tlo:
            self.error("server.load_model of '%s' failed :-(",
                       self._egg_file)
            return False
        self._top_levels[server] = tlo
        return True

    def _model_set(self, server, name, index, value):
        """ Set value in server's model. """
        if server is None:
            self.model.set(name, value, index)
        else:
            self._top_levels[server].set(name, value, index)
        return True

    def _model_get(self, server, name, index):
        """ Get value from server's model. """
        if server is None:
            return self.model.get(name, index)
        else:
            return self._top_levels[server].get(name, index)

    def _model_execute(self, server):
        """ Execute model in server. """
        self._exceptions[server] = None
        if server is None:
            try:
                self.model.run()
            except Exception, exc:
                self._exceptions[server] = exc
                self.exception('Caught exception: %s' % exc)
        else:
            self._queues[server].put((self._remote_model_execute, server))

    def _remote_model_execute(self, server):
        """ Execute model. """
        try:
            self._top_levels[server].run()
        except Exception, exc:
            self._exceptions[server] = exc
            self.error('Caught exception from server %s, PID %d on %s: %s',
                       self._server_info[server]['name'],
                       self._server_info[server]['pid'],
                       self._server_info[server]['host'], exc)

    def _model_status(self, server):
        """ Return execute status from model. """
        return self._exceptions[server]

    def _model_cleanup(self, server):
        """ Clean up model resources. """
        return True

