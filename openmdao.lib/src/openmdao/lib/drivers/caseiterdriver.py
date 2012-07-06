"""
.. _`caseiterdriver.py`:

"""

import logging
import os.path
import Queue
import sys
import thread
import threading
import traceback

from openmdao.main.datatypes.api import Bool, Dict, Enum, Int, Slot

from openmdao.main.api import Driver
from openmdao.main.exceptions import RunStopped, TracedError, traceback_str
from openmdao.main.expreval import ExprEvaluator
from openmdao.main.interfaces import ICaseIterator, ICaseRecorder, ICaseFilter
from openmdao.main.rbac import get_credentials, set_credentials
from openmdao.main.resource import ResourceAllocationManager as RAM
from openmdao.main.resource import LocalAllocator
from openmdao.util.filexfer import filexfer

from openmdao.util.decorators import add_delegate
from openmdao.main.hasparameters import HasParameters

from openmdao.lib.casehandlers.api import ListCaseRecorder

_EMPTY     = 'empty'
_LOADING   = 'loading'
_EXECUTING = 'executing'

class _ServerError(Exception):
    """ Raised when a server thread has problems. """
    pass


class CaseIterDriverBase(Driver):
    """
    A base class for Drivers that run sets of cases in a manner similar
    to the ROSE framework. Concurrent evaluation is supported, with the various
    evaluations executed across servers obtained from the
    :class:`ResourceAllocationManager`.
    """

    sequential = Bool(True, iotype='in',
                      desc='If True, evaluate cases sequentially.')

    reload_model = Bool(True, iotype='in',
                        desc='If True, reload the model between executions.')

    error_policy = Enum(values=('ABORT', 'RETRY'), iotype='in',
                        desc='If ABORT, any error stops the evaluation of the'
                             ' whole set of cases.')

    max_retries = Int(1, low=0, iotype='in',
                      desc='Maximum number of times to retry a failed case.')

    extra_resources = Dict(iotype='in',
                           desc='Extra resource requirements (unusual).')

    ignore_egg_requirements = Bool(False, iotype='in',
                                   desc='If True, no distribution or orphan'
                                        ' requirements will be included in the'
                                        ' generated egg.')

    def __init__(self, *args, **kwargs):
        super(CaseIterDriverBase, self).__init__(*args, **kwargs)
        self._iter = None  # Set to None when iterator is empty.
        self._seqno = 0    # Used to set itername for case.
        self._replicants = 0
        self._abort_exc = None  # Set if error_policy == ABORT.

        self._egg_file = None
        self._egg_required_distributions = None
        self._egg_orphan_modules = None

        self._reply_q = None  # Replies from server threads.
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
        self._load_failures = {}
 
        self._todo = []   # Cases grabbed during server startup.
        self._rerun = []  # Cases that failed and should be retried.
        self._generation = 0  # Used to keep worker names unique.

    def execute(self):
        """
        Runs all cases and records results in `recorder`.
        Uses :meth:`setup` and :meth:`resume` with default arguments.
        """
        self.setup()
        self.resume()

    def resume(self, remove_egg=True):
        """
        Resume execution.

        remove_egg: bool
            If True, then the egg file created for concurrent evaluation is
            removed at the end of the run.  Re-using the egg file can
            eliminate a lot of startup overhead.
        """
        self._stop = False
        self._abort_exc = None
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
            if self._abort_exc is None:
                self.raise_exception('Run stopped', RunStopped)
            else:
                self.raise_exception('Run aborted: %s' % traceback_str(self._abort_exc),
                                     RuntimeError)

    def step(self):
        """ Evaluate the next case. """
        self._stop = False
        self._abort_exc = None
        if self._iter is None:
            self.setup()

        try:
            case = self._iter.next()
        except StopIteration:
            if not self._rerun:
                self._iter = None
                self._seqno = 0
                raise

        self._seqno += 1
        self._todo.append((case, self._seqno))
        self._server_cases[None] = None
        self._server_states[None] = _EMPTY
        while self._server_ready(None, stepping=True):
            pass

    def stop(self):
        """ Stop evaluating cases. """
        # Necessary to avoid default driver handling of stop signal.
        self._stop = True

    def setup(self, replicate=True):
        """
        Setup to begin new run.

        replicate: bool
             If True, then replicate the model and save to an egg file
             first (for concurrent evaluation).
        """
        self._cleanup(remove_egg=replicate)

        if not self.sequential:
            if replicate or self._egg_file is None:
                # Save model to egg.
                # Must do this before creating any locks or queues.
                self._replicants += 1
                version = 'replicant.%d' % (self._replicants)

                # If only local host will be used, we can skip determining
                # distributions required by the egg.
                allocators = RAM.list_allocators()
                need_reqs = False
                if not self.ignore_egg_requirements:
                    for allocator in allocators:
                        if not isinstance(allocator, LocalAllocator):
                            need_reqs = True
                            break

                driver = self.parent.driver
                self.parent.add('driver', Driver()) # this driver will execute the workflow once
                self.parent.driver.workflow = self.workflow
                try:
                    #egg_info = self.model.save_to_egg(self.model.name, version)
                    # FIXME: what name should we give to the egg?
                    egg_info = self.parent.save_to_egg(self.name, version,
                                                    need_requirements=need_reqs)
                finally:
                    self.parent.driver = driver

                self._egg_file = egg_info[0]
                self._egg_required_distributions = egg_info[1]
                self._egg_orphan_modules = [name for name, path in egg_info[2]]

        self._iter = self.get_case_iterator()
        self._seqno = 0
        
    def get_case_iterator(self):
        """Returns a new iterator over the Case set."""
        raise NotImplementedError('get_case_iterator')

    def _start(self):
        """ Start evaluating cases concurrently. """
        # Need credentials in case we're using a PublicKey server.
        credentials = get_credentials()

        # Determine maximum number of servers available.
        resources = {
            'required_distributions':self._egg_required_distributions,
            'orphan_modules':self._egg_orphan_modules,
            'python_version':sys.version[:3]}
        if self.extra_resources:
            resources.update(self.extra_resources)
        max_servers = RAM.max_servers(resources)
        self._logger.debug('max_servers %d', max_servers)
        if max_servers <= 0:
            msg = 'No servers supporting required resources %s' % resources
            self.raise_exception(msg, RuntimeError)

        # Kick off initial wave of cases.
        self._server_lock = threading.Lock()
        self._reply_q = Queue.Queue()
        self._generation += 1
        n_servers = 0
        while n_servers < max_servers:
            if not self._more_to_go():
                break

            # Get next case. Limits servers started if max_servers > cases.
            try:
                case = self._iter.next()
            except StopIteration:
                if not self._rerun:
                    self._iter = None
                    self._seqno = 0
                    break

            self._seqno += 1
            self._todo.append((case, self._seqno))

            # Start server worker thread.
            n_servers += 1
            name = '%s_%d_%d' % (self.name, self._generation, n_servers)
            self._logger.debug('starting worker for %r', name)
            self._servers[name] = None
            self._in_use[name] = True
            self._server_cases[name] = None
            self._server_states[name] = _EMPTY
            self._load_failures[name] = 0
            server_thread = threading.Thread(target=self._service_loop,
                                             args=(name, resources,
                                                   credentials, self._reply_q))
            server_thread.daemon = True
            try:
                server_thread.start()
            except thread.error:
                self._logger.warning('worker thread startup failed for %r',
                                     name)
                self._in_use[name] = False
                break

            if sys.platform != 'win32':
                # Process any pending events.
                while self._busy():
                    try:
                        name, result, exc = self._reply_q.get(True, 0.01)
                    except Queue.Empty:
                        break  # Timeout.
                    else:
                        # Difficult to force startup failure.
                        if self._servers[name] is None:  #pragma nocover
                            self._logger.debug('server startup failed for %r',
                                               name)
                            self._in_use[name] = False
                        else:
                            self._in_use[name] = self._server_ready(name)

        if sys.platform == 'win32':  #pragma no cover
            # Don't start server processing until all servers are started,
            # otherwise we have egg removal issues.
            for name in self._in_use.keys():
                name, result, exc = self._reply_q.get()
                if self._servers[name] is None:
                    self._logger.debug('server startup failed for %r', name)
                    self._in_use[name] = False

            # Kick-off started servers.
            for name in self._in_use.keys():
                if self._in_use[name]:
                    self._in_use[name] = self._server_ready(name)

        # Continue until no servers are busy.
        while self._busy():
            if self._more_to_go():
                timeout = None
            else:
                # Don't wait indefinitely for a server we don't need.
                # This has happened with a server that got 'lost'
                # in RAM.allocate()
                timeout = 60
            try:
                name, result, exc = self._reply_q.get(timeout=timeout)
            # Hard to force worker to hang, which is handled here.
            except Queue.Empty:  #pragma no cover
                msgs = []
                for name, in_use in self._in_use.items():
                    if in_use:
                        try:
                            server = self._servers[name]
                            info = self._server_info[name]
                        except KeyError:
                            msgs.append('%r: no startup reply' % name)
                            self._in_use[name] = False
                        else:
                            state = self._server_states[name]
                            if state not in (_LOADING, _EXECUTING):
                                msgs.append('%r: %r %s %s'
                                            % (name, self._servers[name],
                                               state, self._server_info[name]))
                                self._in_use[name] = False
                if msgs:
                    self._logger.error('Timeout waiting with nothing left to do:')
                    for msg in msgs:
                        self._logger.error('    %s', msg)
            else:
                self._in_use[name] = self._server_ready(name)

        # Shut-down (started) servers.
        self._logger.debug('Shut-down (started) servers')
        for queue in self._queues.values():
            queue.put(None)
        for i in range(len(self._queues)):
            try:
                name, status, exc = self._reply_q.get(True, 60)
            # Hard to force worker to hang, which is handled here.
            except Queue.Empty:  #pragma no cover
                pass
            else:
                if name in self._queues:  # 'Stale' worker can reply *late*.
                    del self._queues[name]
        # Hard to force worker to hang, which is handled here.
        for name in self._queues.keys():  #pragma no cover
            self._logger.warning('Timeout waiting for %r to shut-down.', name)

    def _busy(self):
        """ Return True while at least one server is in use. """
        return any(self._in_use.values())

    def _cleanup(self, remove_egg=True):
        """
        Cleanup internal state, and egg file if necessary.
        Note: this happens unconditionally, so it will cause issues
              for workers which haven't shut down by now.
        """
        self._reply_q = None
        self._server_lock = None

        self._servers = {}
        self._top_levels = {}
        self._server_info = {}
        self._queues = {}
        self._in_use = {}
        self._server_states = {}
        self._server_cases = {}
        self._exceptions = {}
        self._load_failures = {}

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
        self._logger.debug('server %r state %s', server, state)
        in_use = True

        if state == _EMPTY:
            if server is None or server in self._queues:
                if self._more_to_go(stepping):
                    self._logger.debug('    load_model')
                    self._load_model(server)
                    self._server_states[server] = _LOADING
                else:
                    self._logger.debug('    no more cases')
                    in_use = False
            # Difficult to force startup failure.
            else:  #pragma nocover
                in_use = False  # Never started.

        elif state == _LOADING:
            exc = self._model_status(server)
            if exc is None:
                in_use = self._start_next_case(server, stepping)
            else:
                self._logger.debug('    exception while loading: %r', exc)
                if self.error_policy == 'ABORT':
                    if self._abort_exc is None:
                        self._abort_exc = exc
                    self._stop = True
                    self._server_states[server] = _EMPTY
                    in_use = False
                else:
                    self._load_failures[server] += 1
                    if self._load_failures[server] < 3:
                        in_use = self._start_processing(server, stepping)
                    else:
                        self._logger.debug('    too many load failures')
                        self._server_states[server] = _EMPTY
                        in_use = False

        elif state == _EXECUTING:
            case, seqno = self._server_cases[server]
            self._server_cases[server] = None
            exc = self._model_status(server)
            if exc is None:
                # Grab the data from the model.
                scope = self.parent if server is None else self._top_levels[server]
                try:
                    case.update_outputs(scope)
                except Exception as exc:
                    msg = 'Exception getting case outputs: %s' % exc
                    self._logger.debug('    %s', msg)
                    case.msg = '%s: %s' % (self.get_pathname(), msg)
            else:
                self._logger.debug('    exception while executing: %r', exc)
                case.msg = str(exc)

            if case.msg is not None and self.error_policy == 'ABORT':
                if self._abort_exc is None:
                    self._abort_exc = exc
                self._stop = True

            # Record the data.
            self._record_case(case, seqno)

            # Set up for next case.
            in_use = self._start_processing(server, stepping, reload=True)

        # Just being defensive, should never happen.
        else:  #pragma no cover
            msg = 'unexpected state %r for server %r' % (state, server)
            self._logger.error(msg)
            if self.error_policy == 'ABORT':
                if self._abort_exc is None:
                    self._abort_exc = RuntimeError(msg)
                self._stop = True
            in_use = False

        return in_use

    def _more_to_go(self, stepping=False):
        """ Return True if there's more work to do. """
        if self._stop:
            return False
        if self._todo or self._rerun:
            return True
        if not stepping and self._iter is not None:
            return True
        return False

    def _start_processing(self, server, stepping, reload=False):
        """
        If there's something to do, start processing by either loading
        the model, or going straight to running it.
        """
        if self._more_to_go(stepping):
            if reload:
                if self.reload_model:
                    self._logger.debug('    reload')
                    self._load_model(server)
                    self._server_states[server] = _LOADING
                    in_use = True
                else:
                    in_use = self._start_next_case(server)
            else:
                self._logger.debug('    load')
                self._load_model(server)
                self._server_states[server] = _LOADING
                in_use = True
        else:
            self._logger.debug('    no more cases')
            self._server_states[server] = _EMPTY
            in_use = False
        return in_use

    def _start_next_case(self, server, stepping=False):
        """ Look for the next case and start it. """
        if self._todo:
            self._logger.debug('    run startup case')
            case, seqno = self._todo.pop(0)
            in_use = self._run_case(case, seqno, server)
        elif self._rerun:
            self._logger.debug('    rerun case')
            case, seqno = self._rerun.pop(0)
            in_use = self._run_case(case, seqno, server, rerun=True)
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
                self._iter = None
                self._seqno = 0
                in_use = False
            else:
                self._logger.debug('    run next case')
                self._seqno += 1
                in_use = self._run_case(case, self._seqno, server)
                
        return in_use

    def _run_case(self, case, seqno, server, rerun=False):
        """ Setup and start a case. Returns True if started. """
        if not rerun:
            if not case.max_retries:
                case.max_retries = self.max_retries
            case.retries = 0
        case.msg = None
        case.parent_uuid = self._case_id

        # Additional user-requested variables
        # These must be added here so that the outputs are in the cases
        # before they are in the server list.
        for printvar in self.printvars:

            if  '*' in printvar:
                printvars = self._get_all_varpaths(printvar)
            else:
                printvars = [printvar]

            for var in printvars:
                val = ExprEvaluator(var, scope=self.parent).evaluate()
                case.add_output(var, val)

        try:
            for event in self.get_events(): 
                try: 
                    self._model_set(server, event, None, True)
                except Exception as exc:
                    msg = 'Exception setting %r: %s' % (event, exc)
                    self._logger.debug('    %s', msg)
                    self.raise_exception(msg, _ServerError)
            try:
                scope = self.parent if server is None else self._top_levels[server]
                case.apply_inputs(scope)
            except Exception as exc:
                msg = 'Exception setting case inputs: %s' % exc
                self._logger.debug('    %s', msg)
                self.raise_exception(msg, _ServerError)
            self._server_cases[server] = (case, seqno)
            self._model_execute(server)
            self._server_states[server] = _EXECUTING
        except _ServerError as exc:
            case.msg = str(exc)
            self._record_case(case, seqno)
            return self._start_processing(server, stepping=False)
        else:
            return True

    def _record_case(self, case, seqno):
        """ If successful, record the case. Otherwise possibly retry. """
        if case.msg and case.retries < case.max_retries:
            case.msg = None
            case.retries += 1
            self._rerun.append((case, seqno))
        else:
            
            for recorder in self.recorders:
                recorder.record(case)

    def _service_loop(self, name, resource_desc, credentials, reply_q):
        """ Each server has an associated thread executing this. """
        set_credentials(credentials)

        server, server_info = RAM.allocate(resource_desc)
        # Just being defensive, this should never happen.
        if server is None:  #pragma no cover
            self._logger.error('Server allocation for %r failed :-(', name)
            reply_q.put((name, False, None))
            return
        else:
            # Clear egg re-use indicator.
            server_info['egg_file'] = None
            self._logger.debug('%r using %r', name, server_info['name'])
            if self._logger.level == logging.NOTSET:
                # By default avoid lots of protocol messages.
                server.set_log_level(logging.DEBUG)
            else:
                server.set_log_level(self._logger.level)

        request_q = Queue.Queue()

        try:
            with self._server_lock:
                self._servers[name] = server
                self._server_info[name] = server_info
                self._queues[name] = request_q

            reply_q.put((name, True, None))  # ACK startup.

            while True:
                request = request_q.get()
                if request is None:
                    break
                try:
                    result = request[0](request[1])
                except Exception as req_exc:
                    self._logger.error('%r: %s caused %r', name,
                                       request[0], req_exc)
                    result = None
                else:
                    req_exc = None
                reply_q.put((name, result, req_exc))
        except Exception as exc:  # pragma no cover
            # This can easily happen if we take a long time to allocate and
            # we get 'cleaned-up' before we get started.
            if self._server_lock is not None:
                self._logger.error('%r: %r', name, exc)
        finally:
            self._logger.debug('%r releasing server', name)
            RAM.release(server)
            reply_q.put((name, True, None))  # ACK shutdown.

    def _load_model(self, server):
        """ Load a model into a server. """
        self._exceptions[server] = None
        if server is not None:
            self._queues[server].put((self._remote_load_model, server))

    def _remote_load_model(self, server):
        """ Load model into remote server. """
        egg_file = self._server_info[server].get('egg_file', None)
        if egg_file is None or egg_file is not self._egg_file:
            # Only transfer if changed.
            try:
                filexfer(None, self._egg_file,
                         self._servers[server], self._egg_file, 'b')
            # Difficult to force model file transfer error.
            except Exception as exc:  #pragma nocover
                self._logger.error('server %r filexfer of %r failed: %r',
                                   server, self._egg_file, exc)
                self._top_levels[server] = None
                self._exceptions[server] = TracedError(exc, traceback.format_exc())
                return
            else:
                self._server_info[server]['egg_file'] = self._egg_file
        try:
            tlo = self._servers[server].load_model(self._egg_file)
        # Difficult to force load error.
        except Exception as exc:  #pragma nocover
            self._logger.error('server.load_model of %r failed: %r',
                               self._egg_file, exc)
            self._top_levels[server] = None
            self._exceptions[server] = TracedError(exc, traceback.format_exc())
        else:
            self._top_levels[server] = tlo

    def _model_set(self, server, name, index, value):
        """ Set value in server's model. """
        if server is None:
            self.parent.set(name, value, index)
        else:
            self._top_levels[server].set(name, value, index)

    def _model_execute(self, server):
        """ Execute model in server. """
        self._exceptions[server] = None
        if server is None:
            try:
                self.workflow.run(case_id=self._server_cases[server][0].uuid)
            except Exception as exc:
                self._exceptions[server] = TracedError(exc, traceback.format_exc())
                self._logger.critical('Caught exception: %r' % exc)
        else:
            self._queues[server].put((self._remote_model_execute, server))

    def _remote_model_execute(self, server):
        """ Execute model in remote server. """
        case, seqno = self._server_cases[server]
        try:
            self._top_levels[server].set_itername(self.get_itername(), seqno)
            self._top_levels[server].run(case_id=case.uuid)
        except Exception as exc:
            self._exceptions[server] = TracedError(exc, traceback.format_exc())
            self._logger.error('Caught exception from server %r, PID %d on %s: %r',
                               self._server_info[server]['name'],
                               self._server_info[server]['pid'],
                               self._server_info[server]['host'], exc)

    def _model_status(self, server):
        """ Return execute status from model. """
        return self._exceptions[server]


class CaseIteratorDriver(CaseIterDriverBase):
    """
    Run a set of cases provided by an :class:`ICaseIterator`. Concurrent
    evaluation is supported, with the various evaluations executed across
    servers obtained from the :class:`ResourceAllocationManager`.
    """

    iterator = Slot(ICaseIterator, iotype='in',
                      desc='Iterator supplying Cases to evaluate.')
    
    evaluated = Slot(ICaseIterator, iotype='out',
                      desc='Iterator supplying evaluated Cases.')
    
    filter = Slot(ICaseFilter, iotype='in',
                  desc='Filter used to select cases to evaluate.')

    def get_case_iterator(self):
        """Returns a new iterator over the Case set."""
        if self.iterator is not None:
            if self.filter is None:
                return iter(self.iterator)
            else:
                return self._select_cases()
        else:
            self.raise_exception("iterator has not been set", ValueError)

    def _select_cases(self):
        """ Select cases to be evaluated. """
        for i, case in enumerate(iter(self.iterator)):
            if self.filter.select(i, case):
                yield case

    def execute(self):
        """ Evaluate cases from `iterator` and place in `evaluated`. """
        self.evaluated = None
        self.recorders.append(ListCaseRecorder())
        try:
            super(CaseIteratorDriver, self).execute()
        finally:
            self.evaluated = self.recorders.pop().get_iterator()

