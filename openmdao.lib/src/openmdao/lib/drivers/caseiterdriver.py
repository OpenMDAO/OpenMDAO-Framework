"""
.. _`caseiterdriver.py`:

"""

from cStringIO import StringIO
import gc
import logging
import os.path
import Queue
import sys
import thread
import threading
from uuid import uuid1, getnode

from numpy import array

from openmdao.main.api import Driver, VariableTree
from openmdao.main.datatypes.api import Bool, Dict, Enum, Int
from openmdao.main.exceptions import traceback_str, exception_str
from openmdao.main.expreval import ExprEvaluator
from openmdao.main.hasparameters import HasVarTreeParameters
from openmdao.main.hasresponses import HasVarTreeResponses
from openmdao.main.interfaces import IHasParameters, IHasResponses, implements
from openmdao.main.rbac import get_credentials, set_credentials
from openmdao.main.resource import ResourceAllocationManager as RAM
from openmdao.main.resource import LocalAllocator
from openmdao.main.variable import is_legal_name, make_legal_path
from openmdao.main.array_helpers import flattened_value

from openmdao.util.decorators import add_delegate
from openmdao.util.filexfer import filexfer


_EMPTY     = 'empty'
_LOADING   = 'loading'
_EXECUTING = 'executing'


class _Case(object):
    """ Input data and required outputs for a particular simulation run. """

    _uuid_node = getnode()
    _uuid_seq = 0

    @staticmethod
    def next_uuid():
        """ Generate a unique identifier. """
        _Case._uuid_seq += 1
        return str(uuid1(node=_Case._uuid_node, clock_seq=_Case._uuid_seq))

    def __init__(self, index, inputs, outputs, extra_outputs,
                 case_uuid=None, parent_uuid=''):
        self.index = index  # Index of input and output values.
        self.retries = 0    # Retry counter.
        self.exc = None     # a sys.exec_info() tuple
        self._exprs = None  # Dictionary of ExprEvaluators.

        self._inputs = {}
        for name, value in inputs:
            if isinstance(name, tuple):
                for _name in name:
                    self._register_expr(_name)
                    self._inputs[_name] = value
            else:
                self._register_expr(name)
                self._inputs[name] = value

        self._outputs = outputs or []
        for name in self._outputs:
            self._register_expr(name)

        self._extra_outputs = extra_outputs or []
        for name in self._extra_outputs:
            self._register_expr(name)

        if case_uuid:
            self.uuid = str(case_uuid)
        else:
            self.uuid = _Case.next_uuid()
        self.parent_uuid = str(parent_uuid)  # identifier of parent case, if any

    def _register_expr(self, name):
        """
        If the given string contains an expression, create an ExprEvaluator
        and store it in self._exprs.
        """
        if not is_legal_name(name):
            expr = ExprEvaluator(name)
            if self._exprs is None:
                self._exprs = {}
            self._exprs[name] = expr

    def apply_inputs(self, scope, parent):
        """
        Take the values of all of the inputs in this case and apply them
        to the specified scope.
        """
        for name, value in self._inputs.items():
            if self._exprs is None:
                expr = None
            else:
                expr = self._exprs.get(name)
            if expr:
                expr.set(value, scope) #, tovector=True)
            else:
                scope.set(name, value)

        parent._system.vec.get('u').set_from_scope(scope)


    def fetch_outputs(self, scope, extra=False, itername=''):
        """
        Return list of ``(name, value)`` of outputs from `scope`.
        If `extra` is True, then fetch extra case recording outputs,
        but skip `itername`, it should be handled by the caller.
        """
        outputs = self._extra_outputs if extra else self._outputs
        data = []
        exc = None

        # Fetch all outputs we can, retaining only the last exception.
        exprs = self._exprs
        if exprs:
            for name in outputs:
                if extra and name == itername:
                    continue
                expr = exprs.get(name)
                try:
                    if expr:
                        value = expr.evaluate(scope)
                    else:
                        value = scope.get(name)
                    data.append((name, value))
                except Exception:
                    exc = sys.exc_info()
        else:
            for name in outputs:
                if extra and name == itername:
                    continue
                try:
                    value = scope.get(name)
                    data.append((name, value))
                except Exception:
                    exc = sys.exc_info()

        return (data, exc)

    def __str__(self):
        stream = StringIO()
        write = stream.write
        write("Case: %s\n" % self.index)
        write("   uuid: %s\n" % self.uuid)
        write("   parent_uuid: %s\n" % self.parent_uuid)
        write("   inputs:\n")
        for name, val in sorted(self._inputs.items()):
            write("      %s: %s\n" % (name, val))
        write("   outputs:\n")
        for name in sorted(self._outputs):
            write("      %s\n" % name)
        write("   extra outputs:\n")
        for name in sorted(self._extra_outputs):
            write("      %s\n" % name)
        write("   retries: %s\n" % self.retries)
        write("   exc: %s\n" % exception_str(self.exc))
        if self.exc is not None:
            write("        %s\n" % traceback_str(self.exc))
        return stream.getvalue()


class _ServerData(object):
    """ Holds data related to a server and it's current state. """

    def __init__(self, name):
        self.name = name        # Name for log output (None => local).
        self.top = None         # Top level object in server.
        self.state = _EMPTY     # See states above.
        self.case = None        # Current case being evaluated.
        self.exception = None   # sys.exc_info() from last operation.

        self.server = None      # Remote server proxy.
        self.info = None        # Identifying information (host, pid, etc.)
        self.queue = None       # Queue to put requests.
        self.in_use = False     # True if being used.
        self.load_failures = 0  # Load failure count.



@add_delegate(HasVarTreeParameters, HasVarTreeResponses)
class CaseIteratorDriver(Driver):
    """
    A base class for Drivers that run sets of cases. Concurrent evaluation is
    supported, with the various evaluations executed across servers obtained
    from the :class:`ResourceAllocationManager`.
    """

    implements(IHasParameters, IHasResponses)

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
        super(CaseIteratorDriver, self).__init__(*args, **kwargs)
        self._iter = None  # Set to None when iterator is empty.
        self._replicants = 0
        self._abort_exc = None  # Set if error_policy == ABORT.

        self._egg_file = None
        self._egg_required_distributions = None
        self._egg_orphan_modules = None

        self._reply_q = None  # Replies from server threads.
        self._server_lock = None  # Lock for server data.

        # Various per-server data keyed by server name.
        self._servers = {}
        self._seq_server = _ServerData(None)

        self._todo = []   # Cases grabbed during server startup.
        self._rerun = []  # Cases that failed and should be retried.
        self._generation = 0  # Used to keep worker names unique.

        # var wasn't showing up in parent depgraph without this
        self.error_policy = 'ABORT'

    def set_inputs(self, generator):
        """ Set case inputs from generator values. """
        inputs = array([vals for vals in generator])
        start = 0
        for path, param in self.get_parameters().items():
            size = param.size
            if size == 1:
                values = inputs[:, start]
            else:
                end = start + size
                values = inputs[:, start:end].reshape((-1,)+param.shape)
            start += size

            if isinstance(path, tuple):
                path = path[0]  # Use first target of ParameterGroup.
            path = make_legal_path(path)
            self.set('case_inputs.'+path, list(values))

    def execute(self):
        """
        Runs all cases and records results in `recorder`.
        Uses :meth:`setup` and :meth:`resume` with default arguments.
        """
        self._setup()

        try:
            if self.sequential:
                self._logger.info('Start sequential evaluation.')
                server = self._servers[None] = self._seq_server
                server.top = self.parent
                while self._iter is not None:
                    try:
                        case = self._iter.next()
                        self._todo.append(case)
                        server.exception = None
                        server.case = None
                        server.state = _LOADING  # 'server' already loaded.
                        while self._server_ready(server):
                            pass
                    except StopIteration:
                        if not self._rerun:
                            self._iter = None
                        break
            else:
                self._logger.info('Start concurrent evaluation.')
                self._start()
        finally:
            self._cleanup()

        if self._abort_exc is not None:
            msg = "%s: Run aborted: %s" % (self.get_pathname(), traceback_str(self._abort_exc))
            newexc = self._abort_exc[0](msg)
            raise self._abort_exc[0], newexc, self._abort_exc[2]

    def _setup(self):
        """ Setup to begin new run. """
        # if params have changed we need to setup systems again
        if self.workflow._system is None:
            obj = self
            while obj.parent is not None:
                obj = obj.parent
            obj._setup()

        if not self.sequential:
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

            # Replicate and mutate model to run our workflow once.
            # Originally this was done in-place, but that 'invalidated'
            # various workflow quantities.
            replicant = self.parent.copy()
            workflow = replicant.get(self.name+'.workflow')
            driver = replicant.add('driver', Driver())
            workflow.parent = driver
            workflow.scope = None
            replicant.driver.workflow = workflow
            egg_info = replicant.save_to_egg(self.name, version,
                                             need_requirements=need_reqs)
            replicant = workflow = driver = None  # Release objects.
            gc.collect()  # Collect/compact before possible fork.

            self._egg_file = egg_info[0]
            self._egg_required_distributions = egg_info[1]
            self._egg_orphan_modules = [name for name, path in egg_info[2]]

        inp_paths = []
        inp_values = []
        for path, param in self.get_parameters().items():
            if isinstance(path, tuple):
                path = path[0]  # Use first target of ParameterGroup.
            path = make_legal_path(path)
            value = self.get('case_inputs.'+path)
            for target in param.targets:
                inp_paths.append(target)
                inp_values.append(value)

        outputs = self.get_responses().keys()
        extra_outputs = self.workflow._rec_outputs

        length = len(inp_values[0]) if inp_values else 0
        cases = []
        for i in range(length):
            inputs = []
            for j in range(len(inp_paths)):
                inputs.append((inp_paths[j], inp_values[j][i]))
            cases.append(_Case(i, inputs, outputs, extra_outputs,
                               parent_uuid=self._case_uuid))
        self.init_responses(length)

        self._iter = iter(cases)
        self._abort_exc = None

    def _start(self):
        """ Start evaluating cases concurrently. """
        # Need credentials in case we're using a PublicKey server.
        credentials = get_credentials()

        # Determine maximum number of servers available.
        resources = {
            'required_distributions': self._egg_required_distributions,
            'orphan_modules': self._egg_orphan_modules,
            'python_version': sys.version[:3]
        }
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
                    break

            self._todo.append(case)

            # Start server worker thread.
            n_servers += 1
            name = '%s_%d_%d' % (self.name, self._generation, n_servers)
            self._logger.debug('starting worker for %r', name)
            server = self._servers[name] = _ServerData(name)
            server.in_use = True
            server_thread = threading.Thread(target=self._service_loop,
                                             args=(name, resources,
                                                   credentials, self._reply_q))
            server_thread.daemon = True
            try:
                server_thread.start()
            except thread.error:
                self._logger.warning('worker thread startup failed for %r',
                                     name)
                server.in_use = False
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
                        if server.server is None:  # pragma nocover
                            self._logger.debug('server startup failed for %r',
                                               name)
                            server.in_use = False
                        else:
                            server.in_use = self._server_ready(server)

        if sys.platform == 'win32':  # pragma no cover
            # Don't start server processing until all servers are started,
            # otherwise we have egg removal issues.
            for i in self._servers:
                name, result, exc = self._reply_q.get()
                server = self._servers[name]
                if server.server is None:
                    self._logger.debug('server startup failed for %r', name)
                    server.in_use = False

            # Kick-off started servers.
            for server in self._servers.values():
                if server.in_use:
                    server.in_use = self._server_ready(server)

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
            except Queue.Empty:  # pragma no cover
                msgs = []
                for name, server in self._servers.items():
                    if server.in_use:
                        if server.server is None or server.info is None:
                            msgs.append('%r: no startup reply' % name)
                            server.in_use = False
                        else:
                            state = server.state
                            if state not in (_LOADING, _EXECUTING):
                                msgs.append('%r: %r %s %s'
                                            % (name, server.server,
                                               state, server.info))
                                server.in_use = False
                if msgs:
                    self._logger.error('Timeout waiting with nothing left to do:')
                    for msg in msgs:
                        self._logger.error('    %s', msg)
            else:
                server = self._servers[name]
                server.in_use = self._server_ready(server)

        # Shut-down (started) servers.
        self._logger.debug('Shut-down (started) servers')
        n_queues = 0
        for server in self._servers.values():
            if server.queue is not None:
                server.queue.put(None)
                n_queues += 1
        for i in range(n_queues):
            try:
                name, status, exc = self._reply_q.get(True, 60)
            # Hard to force worker to hang, which is handled here.
            except Queue.Empty:  # pragma no cover
                pass
            else:
                self._servers[name].queue = None
        # Hard to force worker to hang, which is handled here.
        for server in self._servers.values():  # pragma no cover
            if server.queue is not None:
                self._logger.warning('Timeout waiting for %r to shut-down.',
                                     server.name)

    def _busy(self):
        """ Return True while at least one server is in use. """
        for server in self._servers.values():
            if server.in_use:
                return True
        return False

    def _cleanup(self):
        """
        Cleanup internal state, and egg file if necessary.
        Note: this happens unconditionally, so it will cause issues
              for workers which haven't shut down by now.
        """
        self._iter = None
        self._reply_q = None
        self._server_lock = None
        self._servers = {}
        self._seq_server.top = None  # Avoid leak.
        self._todo = []
        self._rerun = []

        if self._egg_file and os.path.exists(self._egg_file):
            os.remove(self._egg_file)
            self._egg_file = None

    def _server_ready(self, server):
        """
        Responds to asynchronous callbacks during :meth:`execute` to run cases
        retrieved from `self._iter`.  Results are processed by `recorder`.
        Returns True if this server is still in use.
        """
        state = server.state
        self._logger.debug('server %r state %s', server.name, state)
        in_use = True

        if state == _LOADING:
            if server.exception is None:
                in_use = self._start_next_case(server)
            else:
                typ, exc, tback = server.exception
                self._logger.debug('    exception while loading: %r', exc)
                if self.error_policy == 'ABORT':
                    if self._abort_exc is None:
                        self._abort_exc = server.exception
                    self._stop = True
                    server.state = _EMPTY
                    in_use = False
                else:
                    server.load_failures += 1
                    if server.load_failures < 3:
                        in_use = self._start_processing(server)
                    else:
                        self._logger.debug('    too many load failures')
                        server.state = _EMPTY
                        in_use = False

        elif state == _EXECUTING:
            case = server.case
            server.case = None
            if server.exception is None:
                # Grab the results from the model and record.
                try:
                    self._record_case(server.top, case)
                except Exception as exc:
                    msg = 'Exception recording case: %s' % exc
                    self._logger.debug('    %s', msg)
                    self._logger.debug('%s', case)
                    case.msg = '%s: %s' % (self.get_pathname(), msg)
            else:
                self._logger.debug('    exception while executing: %r', server.exception[1])
                case.exc = server.exception

            if case.exc is not None:
                if self.error_policy == 'ABORT':
                    if self._abort_exc is None:
                        self._abort_exc = case.exc
                    self._stop = True
                elif case.retries < self.max_retries:
                    case.exc = None
                    case.retries += 1
                    self._rerun.append(case)
                else:
                    self._logger.error('Too many retries for %s', case)

            # Set up for next case.
            in_use = self._start_processing(server, reload=True)

        elif state == _EMPTY:
            if server.name is None or server.queue is not None:
                if self._more_to_go():
                    if server.queue is not None:
                        self._logger.debug('    load_model')
                        server.load_failures = 0
                        self._load_model(server)
                    server.state = _LOADING
                else:
                    self._logger.debug('    no more cases')
                    in_use = False
            # Difficult to force startup failure.
            else:  # pragma nocover
                in_use = False  # Never started.

        # Just being defensive, should never happen.
        else:  # pragma no cover
            msg = 'unexpected state %r for server %r' % (state, server.name)
            self._logger.error(msg)
            if self.error_policy == 'ABORT':
                if self._abort_exc is None:
                    try:
                        raise RuntimeError(msg)
                    except RuntimeError:
                        self._abort_exc = sys.exc_info()
                self._stop = True
            in_use = False

        return in_use

    def _more_to_go(self):
        """ Return True if there's more work to do. """
        if self._stop:
            return False
        if self._todo or self._rerun:
            return True
        if self._iter is not None:
            return True
        return False

    def _start_processing(self, server, reload=False):
        """
        If there's something to do, start processing by either loading
        the model, or going straight to running it.
        """
        if self._more_to_go():
            if server.name is None:
                in_use = self._start_next_case(server)
            elif reload:
                if self.reload_model:
                    self._logger.debug('    reload')
                    self._load_model(server)
                    server.state = _LOADING
                    in_use = True
                else:
                    in_use = self._start_next_case(server)
            else:
                self._logger.debug('    load')
                self._load_model(server)
                server.state = _LOADING
                in_use = True
        else:
            self._logger.debug('    no more cases')
            server.state = _EMPTY
            in_use = False
        return in_use

    def _start_next_case(self, server):
        """ Look for the next case and start it. """

        if self._todo:
            self._logger.debug('    run startup case')
            case = self._todo.pop(0)
            in_use = self._run_case(case, server)
        elif self._rerun:
            self._logger.debug('    rerun case')
            case = self._rerun.pop(0)
            in_use = self._run_case(case, server)
        elif self._iter is None:
            self._logger.debug('    no more cases')
            in_use = False
        else:
            try:
                case = self._iter.next()
            except StopIteration:
                self._logger.debug('    no more cases')
                self._iter = None
                in_use = False
            else:
                self._logger.debug('    run next case')
                in_use = self._run_case(case, server)

        return in_use

    def _run_case(self, case, server):
        """ Setup and start a case. Returns True if started. """
        case.exc = None

        # We record the case and are responsible for unique case ids.
        case.uuid = _Case.next_uuid()
        case.parent_uuid = self._case_uuid

        try:
            case.apply_inputs(server.top, self)
        except Exception:
            case.exc = sys.exc_info()
            msg = 'Exception setting case inputs: %s' % case.exc[1]
            self._logger.error('    %s', msg)
            if case.retries < self.max_retries:
                case.retries += 1
                self._rerun.append(case)
            return self._start_processing(server)

        server.case = case
        self._model_execute(server)
        server.state = _EXECUTING
        return True

    def _record_case(self, scope, case):
        """
        Record case data from `scope` in ``case_outputs``.
        Also sends case data to recorders.
        """
        case_outputs, exc = case.fetch_outputs(scope)
        if exc is None and case.exc is None:
            index = case.index
            for path, value in case_outputs:
                path = make_legal_path(path)
                if self.sequential and isinstance(value, VariableTree):
                    value = value.copy()
                self.set('case_outputs.%s[%d]'%(path,index), value)

        # Record workflow data in recorders.
        workflow = self.workflow
        if workflow._rec_required:
            inputs = []
            recording = workflow._rec_parameters
            for name, param in self.get_parameters().items():
                if param in recording:
                    if isinstance(name, tuple):  # Use first target.
                        name = name[0]
                    inputs.append(case._inputs[name])

            outputs = []
            recording = set(workflow._rec_responses)
            for path, value in case_outputs:
                if path in recording:
                    if self.sequential and isinstance(value, VariableTree):
                        value = value.copy()
                    outputs.append(value)

            itername = '%s.workflow.itername' % self.name
            extra, extra_exc = case.fetch_outputs(scope, extra=True,
                                                  itername=itername)
            for path, value in extra:
                if self.sequential and isinstance(value, VariableTree):
                    value = value.copy()
                outputs.append(value)

            if itername in workflow._rec_outputs:
                if self.itername:
                    outputs.append('%s.%s' % (self.itername, case.index+1))
                else:
                    outputs.append('%s' % (case.index+1))

            top = scope
            while top.parent:
                top = top.parent
            for recorder in top.recorders:
                recorder.record(self, inputs, outputs,
                                case.exc or exc or extra_exc,
                                case.uuid, self._case_uuid)

    def _service_loop(self, name, resource_desc, credentials, reply_q):
        """ Each server has an associated thread executing this. """
        set_credentials(credentials)

        server, server_info = RAM.allocate(resource_desc)
        # Just being defensive, this should never happen.
        if server is None:  # pragma no cover
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
                sdata = self._servers[name]
                sdata.server = server
                sdata.info = server_info
                sdata.queue = request_q

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
        server.exception = None
        if server.queue is not None:
            server.queue.put((self._remote_load_model, server))

    def _remote_load_model(self, server):
        """ Load model into remote server. """
        egg_file = server.info.get('egg_file', None)
        if egg_file is None or egg_file is not self._egg_file:
            # Only transfer if changed.
            try:
                filexfer(None, self._egg_file,
                         server.server, self._egg_file, 'b')
            # Difficult to force model file transfer error.
            except Exception as exc:  # pragma nocover
                self._logger.error('server %r filexfer of %r failed: %r',
                                   server.name, self._egg_file, exc)
                server.top = None
                server.exception = sys.exc_info()
                return
            else:
                server.info['egg_file'] = self._egg_file
        try:
            tlo = server.server.load_model(self._egg_file)
        # Difficult to force load error.
        except Exception as exc:  # pragma nocover
            self._logger.error('server.load_model of %r failed: %r',
                               self._egg_file, exc)
            server.top = None
            server.exception = sys.exc_info()
        else:
            server.top = tlo

    def _model_execute(self, server):
        """ Execute model in server. """
        server.exception = None
        if server.queue is None:
            try:
                self.run_iteration(server.case.uuid)
            except Exception as exc:
                server.exception = sys.exc_info()
                self._logger.critical('Caught exception: %r' % exc)
        else:
            server.queue.put((self._remote_model_execute, server))

    def _remote_model_execute(self, server):
        """ Execute model in remote server. """
        case = server.case
        try:
            server.top.set_itername(self.get_itername(), case.index+1)
            server.top.run(case_uuid=case.uuid)
        except Exception as exc:
            server.exception = sys.exc_info()
            self._logger.error('Caught exception from server %r,'
                               ' PID %d on %s: %r',
                               server.info['name'], server.info['pid'],
                               server.info['host'], exc)
