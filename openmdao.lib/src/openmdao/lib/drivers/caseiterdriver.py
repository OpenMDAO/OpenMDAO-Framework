import os.path
import Queue
import sys
import threading

from openmdao.main.api import Component, Driver
from openmdao.main.exceptions import RunStopped
from openmdao.main.interfaces import ICaseIterator, ICaseRecorder
from openmdao.main.resource import ResourceAllocationManager as RAM
from openmdao.lib.api import Bool, Instance, Int
from openmdao.util.filexfer import filexfer

_EMPTY    = 'empty'
_READY    = 'ready'
_COMPLETE = 'complete'
_ERROR    = 'error'

class ServerError(Exception):
    """ Raised when a server thread has problems. """
    pass


class CaseIterDriverBase(Driver):
    """
    A base class for Drivers that run sets of cases in a manner similar
    to the ROSE framework. Concurrent evaluation is supported, with the various
    evaluations executed across servers obtained from the
    :class:`ResourceAllocationManager`.

    - The `model` to be executed is found in the workflow.
    - The `recorder` socket is used to record results.
    - If `sequential` is True, then the cases are evaluated sequentially.
    - If `reload_model` is True, the model is reloaded between executions.
    - `max_retries` sets the number of times to retry a failed case.
    
    """

    recorder = Instance(object, desc='Something to write Cases to.', required=True)
    
    sequential = Bool(True, iotype='in',
                      desc='Evaluate cases sequentially.')

    reload_model = Bool(True, iotype='in',
                        desc='Reload model between executions.')

    max_retries = Int(1, low=0, iotype='in',
                      desc='Number of times to retry a case.')

    def __init__(self, *args, **kwargs):
        super(CaseIterDriverBase, self).__init__(*args, **kwargs)

        self._iter = None  # Set to None when iterator is empty.
        self._replicants = 0

        self._egg_file = None
        self._egg_required_distributions = None
        self._egg_orphan_modules = None

        self._reply_queue = None  # Replies from server threads.
        self._server_lock = None  # Lock for server data.

        # Various per-server data keyed by server name.
        self._servers = {}
        self._top_levels = {}
        self._server_info = {}
        self._queues = {}
        self._in_use = {}
        self._server_states = {}
        self._server_cases = {}
        self._exceptions = {}

        self._todo = []   # Cases grabbed during server startup.
        self._rerun = []  # Cases that failed and should be retried.

    def execute(self):
        """ Runs all cases and records results in `recorder`. """
        self.setup()
        self.resume()

    def resume(self, remove_egg=True):
        """
        Resume execution. If `remove_egg` is True, then the egg file created
        for concurrent evaluation is removed at the end of the run.
        """
        self._stop = False
        if self._iter is None:
            self.raise_exception('Run already complete', RuntimeError)

        try:
            if self.sequential:
                self._logger.info('Start sequential evaluation.')
                while self._iter is not None:
                    if self._stop:
                        break
                    try:
                        self.step()
                    except StopIteration:
                        break
            else:
                self._logger.info('Start concurrent evaluation.')
                self._start()
        finally:
            self._cleanup(remove_egg)

        if self._stop:
            self.raise_exception('Run stopped', RunStopped)

    def step(self):
        """ Evaluate the next case. """
        self._stop = False
        if self._iter is None:
            self.setup()

        try:
            self._todo.append(self._iter.next())
        except StopIteration:
            if not self._rerun:
                self._iter = None
                raise

        self._server_cases[None] = None
        self._server_states[None] = _EMPTY
        while self._server_ready(None, stepping=True):
            pass

    def stop(self):
        """ Avoid default driver handling of stop signal. """
        self._stop = True

    def setup(self, replicate=True):
        """
        Setup to begin new run. If `replicate`, then replicate the model
        and save to an egg file first (for concurent evaluation).
        """
        self._cleanup(remove_egg=replicate)

        if not self.sequential:
            if replicate or self._egg_file is None:
                # Save model to egg.
                # Must do this before creating any locks or queues.
                self._replicants += 1
                version = 'replicant.%d' % (self._replicants)
                driver = self.parent.driver
                self.parent.add('driver', Driver()) # this driver will execute the workflow once
                self.parent.driver.workflow = self.workflow
                try:
                    #egg_info = self.model.save_to_egg(self.model.name, version)
                    # FIXME: what name should we give to the egg?
                    egg_info = self.parent.save_to_egg(self.name, version)
                finally:
                    self.parent.driver = driver
                self._egg_file = egg_info[0]
                self._egg_required_distributions = egg_info[1]
                self._egg_orphan_modules = [name for name, path in egg_info[2]]

        self._iter = self.get_case_iterator()
        
    def get_case_iterator(self):
        raise NotImplemented('get_case_iterator')

    def _start(self):
        """ Start evaluating cases concurrently. """

        # Determine maximum number of servers available.
        resources = {
            'required_distributions':self._egg_required_distributions,
            'orphan_modules':self._egg_orphan_modules,
            'python_version':sys.version[:3]}
        max_servers = RAM.max_servers(resources)
        self._logger.debug('max_servers %d', max_servers)
        if max_servers <= 0:
            msg = 'No servers supporting required resources %s' % resources
            self.raise_exception(msg, RuntimeError)

        # Kick off initial wave of cases.
        self._server_lock = threading.Lock()
        self._reply_queue = Queue.Queue()
        n_servers = 0
        while n_servers < max_servers:
            if self._stop:
                break

            if self._iter is None:
                break

            # Get next case. Limits servers started if max_servers > cases.
            try:
                self._todo.append(self._iter.next())
            except StopIteration:
                if not self._rerun:
                    self._iter = None
                    break

            # Start server worker thread.
            n_servers += 1
            name = '%s_%d' % (self.name, n_servers)
            self._logger.debug('starting worker for %s', name)
            self._servers[name] = None
            self._in_use[name] = True
            self._server_cases[name] = None
            self._server_states[name] = _EMPTY
            server_thread = threading.Thread(target=self._service_loop,
                                             args=(name, resources))
            server_thread.daemon = True
            server_thread.start()

            if sys.platform != 'win32':
                # Process any pending events.
                while self._busy():
                    try:
                        name, result = self._reply_queue.get(True, 0.1)
                    except Queue.Empty:
                        break  # Timeout.
                    else:
                        if self._servers[name] is None:
                            self._logger.debug('server startup failed for %s', name)
                            self._in_use[name] = False
                        else:
                            self._in_use[name] = self._server_ready(name)

        if sys.platform == 'win32':
            # Don't start server processing until all servers are started,
            # otherwise we have egg removal issues.
            for name in self._in_use.keys():
                name, result = self._reply_queue.get()
                if self._servers[name] is None:
                    self._logger.debug('server startup failed for %s', name)
                    self._in_use[name] = False

            # Kick-off started servers.
            for name in self._in_use.keys():
                if self._in_use[name]:
                    self._in_use[name] = self._server_ready(name)

        # Continue until no servers are busy.
        while self._busy():
            name, result = self._reply_queue.get()
            self._in_use[name] = self._server_ready(name)

        # Shut-down (started) servers.
        for queue in self._queues.values():
            queue.put(None)
        for i in range(len(self._queues)):
            try:
                name, status = self._reply_queue.get(True, 1)
            except Queue.Empty:
                pass
            else:
                del self._queues[name]
        for name in self._queues.keys():
            self._logger.warning('Timeout waiting for %s to shut-down.', name)

    def _busy(self):
        """ Return True while at least one server is in use. """
        return any(self._in_use.values())

    def _cleanup(self, remove_egg=True):
        """ Cleanup egg file if necessary. """
        self._reply_queue = None
        self._server_lock = None

        self._servers = {}
        self._top_levels = {}
        self._server_info = {}
        self._queues = {}
        self._in_use = {}
        self._server_states = {}
        self._server_cases = {}
        self._exceptions = {}

        self._todo = []
        self._rerun = []

        if self._egg_file and os.path.exists(self._egg_file):
            os.remove(self._egg_file)
            self._egg_file = None

    def _server_ready(self, server, stepping=False):
        """
        Responds to asynchronous callbacks during :meth:`execute` to run cases
        retrieved from `self._iter`.  Results are processed by `recorder`.
        If `stepping`, then we don't grab any new cases.
        Returns True if this server is still in use.
        """
        state = self._server_states[server]
        self._logger.debug('server %s state %s', server, state)
        in_use = True

        if state == _EMPTY:
            try:
                self._logger.debug('    load_model')
                self._load_model(server)
                self._server_states[server] = _READY
            except ServerError:
                self._server_states[server] = _ERROR

        elif state == _READY:
            # Test for stop request.
            if self._stop:
                self._logger.debug('    stop requested')
                in_use = False

            # Select case to run.
            elif self._todo:
                self._logger.debug('    run startup case')
                self._run_case(self._todo.pop(0), server)
            elif self._rerun:
                self._logger.debug('    rerun case')
                self._run_case(self._rerun.pop(0), server, rerun=True)
            elif self._iter is None:
                self._logger.debug('    no more cases')
                in_use = False
            elif stepping:
                in_use = False
            else:
                try:
                    case = self._iter.next()
                except StopIteration:
                    self._logger.debug('    no more cases')
                    in_use = False
                    self._iter = None
                else:
                    self._logger.debug('    run next case')
                    self._run_case(case, server)
        
        elif state == _COMPLETE:
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
                        except Exception as exc:
                            msg = "Exception getting '%s': %s" % (niv[0], exc)
                            case.msg = '%s: %s' % (self.get_pathname(), msg)
                else:
                    self._logger.debug('    exception %s', exc)
                    case.msg = str(exc)
                # Record the data.
                self.recorder.append(case)

                if not case.msg:
                    if self.reload_model:
                        self._logger.debug('    reload')
                        self._load_model(server)
                else:
                    self._logger.debug('    load')
                    self._load_model(server)
                self._server_states[server] = _READY
            except ServerError:
                # Handle server error separately.
                self._logger.debug('    server error')

        elif state == _ERROR:
            self._server_cases[server] = None
            try:
                self._load_model(server)
            except ServerError:
                pass  # Needs work!
            else:
                self._server_states[server] = _READY

        else:
            self._logger.error('unexpected state %s for server %s', state, server)
            in_use = False

        return in_use

    def _run_case(self, case, server, rerun=False):
        """ Setup and run a case. """
        if not rerun:
            if not case.max_retries:
                case.max_retries = self.max_retries
            case.retries = 0

        case.msg = None
        self._server_cases[server] = case

        try:
            for name, index, value in case.inputs:
                try:
                    self._model_set(server, name, index, value)
                except Exception as exc:
                    msg = "Exception setting '%s': %s" % (name, exc)
                    self.raise_exception(msg, ServerError)
            self._model_execute(server)
            self._server_states[server] = _COMPLETE
        except ServerError as exc:
            self._server_states[server] = _ERROR
            if case.retries < case.max_retries:
                case.retries += 1
                self._rerun.append(case)
            else:
                case.msg = str(exc)
                self.recorder.append(case)

    def _service_loop(self, name, resource_desc):
        """ Each server has an associated thread executing this. """
        server, server_info = RAM.allocate(resource_desc)
        if server is None:
            self._logger.error('Server allocation for %s failed :-(', name)
            self._reply_queue.put((name, False))
            return
        else:
            server_info['egg_file'] = None

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

    def _load_model(self, server):
        """ Load a model into a server. """
        if server is not None:
            self._queues[server].put((self._remote_load_model, server))
        return True

    def _remote_load_model(self, server):
        """ Load model into remote server. """
        egg_file = self._server_info[server].get('egg_file', None)
        if egg_file is not self._egg_file:
            # Only transfer if changed.
            filexfer(None, self._egg_file,
                     self._servers[server], self._egg_file, 'b')
            self._server_info[server]['egg_file'] = self._egg_file
        tlo = self._servers[server].load_model(self._egg_file)
        if not tlo:
            self._logger.error("server.load_model of '%s' failed :-(",
                       self._egg_file)
            return False
        self._top_levels[server] = tlo
        return True

    def _model_set(self, server, name, index, value):
        """ Set value in server's model. """
        if server is None:
            self.parent.set(name, value, index)
        else:
            self._top_levels[server].set(name, value, index)
        return True

    def _model_get(self, server, name, index):
        """ Get value from server's model. """
        if server is None:
            return self.parent.get(name, index)
        else:
            return self._top_levels[server].get(name, index)

    def _model_execute(self, server):
        """ Execute model in server. """
        self._exceptions[server] = None
        if server is None:
            try:
                self.workflow.run()
            except Exception as exc:
                self._exceptions[server] = exc
                self._logger.critical('Caught exception: %s' % exc)
        else:
            self._queues[server].put((self._remote_model_execute, server))

    def _remote_model_execute(self, server):
        """ Execute model in remote server. """
        try:
            self._top_levels[server].run()
        except Exception as exc:
            self._exceptions[server] = exc
            self._logger.error('Caught exception from server %s, PID %d on %s: %s',
                       self._server_info[server]['name'],
                       self._server_info[server]['pid'],
                       self._server_info[server]['host'], exc)

    def _model_status(self, server):
        """ Return execute status from model. """
        return self._exceptions[server]


class CaseIteratorDriver(CaseIterDriverBase):
    """
    Run a set of cases provided by an :class:`ICaseIterator` in a manner similar
    to the ROSE framework. Concurrent evaluation is supported, with the various
    evaluations executed across servers obtained from the
    :class:`ResourceAllocationManager`.

    - The `iterator` input provides the cases to be evaluated.
    """

    iterator = Instance(ICaseIterator, iotype='in',
                        desc='Iterator supplying Cases to evaluate.', required=True)
    
    def get_case_iterator(self):
        """Returns a new iterator over the Case set"""
        return self.iterator.__iter__()
