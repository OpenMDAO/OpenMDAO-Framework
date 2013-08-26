import cmd
try:
    import simplejson as json
except ImportError:
    import json
import logging
import os.path
import sys
import traceback

from setuptools.command import easy_install
from zope.interface import implementedBy

from openmdao.main.api import Assembly, Component, Driver, logger, \
                              set_as_top, get_available_types
from openmdao.main.variable import json_default

from openmdao.main.project import Project, ProjFinder, \
                                  _clear_insts, _match_insts
from openmdao.main.publisher import publish, Publisher
from openmdao.main.mp_support import has_interface, is_instance
from openmdao.main.interfaces import IContainer, IComponent, IAssembly
from openmdao.main.factorymanager import register_class_factory, \
                                         remove_class_factory, get_signature
from openmdao.main.repo import get_repo, find_vcs

from openmdao.main.releaseinfo import __version__, __date__

from openmdao.util.nameutil import isidentifier
from openmdao.util.fileutil import file_md5
from openmdao.util.dep import plugin_groups

from openmdao.gui.util import packagedict
from openmdao.gui.filemanager import FileManager
from openmdao.gui.projdirfactory import ProjDirFactory


def modifies_model(target):
    ''' Decorator for methods that may have modified the model;
        performs maintenance on root level containers/assemblies and
        publishes the potentially updated components.
    '''

    def wrapper(self, *args, **kwargs):
        result = target(self, *args, **kwargs)
        self._update_roots()
        self._update_workflows()
        if self.publish_updates:
            self.publish_components()
        return result
    return wrapper


class ConsoleServer(cmd.Cmd):
    ''' Object which knows how to load a model and provides a command line
        interface and various methods to access and modify that model.
    '''

    def __init__(self, name='', host='', publish_updates=True):
        cmd.Cmd.__init__(self)

        self.intro = 'OpenMDAO ' + __version__ + ' (' + __date__ + ')'
        self.prompt = 'OpenMDAO>> '

        self._hist = []

        self.host = host
        self._projname = ''
        self.proj = None
        self.exc_info = None
        self.publish_updates = publish_updates
        self._publish_comps = {}

        self._log_directory = os.getcwd()
        self._log_handler = None
        self._log_subscribers = 0

        self._partial_cmd = None  # for multi-line commands

        self.projdirfactory = None
        self.files = None

        # make sure we have a ProjFinder in sys.path_hooks
        if not ProjFinder in sys.path_hooks:
            sys.path_hooks = [ProjFinder] + sys.path_hooks

    def set_current_project(self, path):
        """ Set current project name. """
        # Called by ProjectHandler, since load_project() is too late to
        # affect the rendering of the template.
        self._projname = os.path.basename(path)

    def get_current_project(self):
        """ Get current project name. """
        return self._projname

    def _update_roots(self):
        ''' Ensure that all root containers in the project dictionary know
            their own name and that all root assemblies are set as top.
        '''
        for k, v in self.proj.items():
            if has_interface(v, IContainer):
                if v.name != k:
                    v.name = k
            if is_instance(v, Assembly) and v._call_cpath_updated:
                set_as_top(v)

    def _update_workflows(self):
        ''' Call :meth:`_update_workflow` on drivers to capture any workflow
            updates now rather than waiting until they are run.
        '''
        for k, v in self.proj.items():
            if has_interface(v, IContainer):
                for driver in [obj for name, obj in v.items(recurse=True)
                               if is_instance(obj, Driver)]:
                    driver._update_workflow()

    def publish_components(self):
        ''' Publish the current component tree and subscribed components.
        '''
        try:
            publish('components', self.get_components())
            publish('', {'Dataflow': self.get_dataflow('')})
            publish('', {'Workflow': self.get_workflow('')})
        except Exception as err:
            self._error(err, sys.exc_info())
        else:
            comps = self._publish_comps.keys()
            for pathname in comps:
                comp, root = self.get_object(pathname, report=False)
                if comp is None:
                    del self._publish_comps[pathname]
                    publish(pathname, {})
                else:
                    publish(pathname, comp.get_attributes(io_only=False))

    def send_pub_msg(self, msg, topic):
        ''' Publish the given message with the given topic.
        '''
        publish(topic, msg)

    def _error(self, err, exc_info):
        ''' Publish error message and save stack trace if case it's requested.
        '''
        self._partial_cmd = None
        self.exc_info = exc_info
        msg = '%s: %s' % (err.__class__.__name__, err)
        logger.error(msg)
        self._print_error(msg)

    def _print_error(self, msg):
        ''' Publish error message.
        '''
        try:
            publish('console_errors', msg)
        except:
            logger.error('publishing of message failed')

    def do_trace(self, arg):
        ''' Print remembered trace from last exception.
        '''
        if self.exc_info:
            exc_type, exc_value, exc_traceback = self.exc_info
            traceback.print_exception(exc_type, exc_value, exc_traceback)
        else:
            print "No trace available."

    def precmd(self, line):
        ''' This method is called after the line has been input but before
            it has been interpreted. If you want to modify the input line
            before execution (for example, variable substitution), do it here.
        '''
        #self._hist += [line.strip()]
        return line

    @modifies_model
    def onecmd(self, line):
        self._hist.append(line)
        try:
            cmd.Cmd.onecmd(self, line)
        except Exception as err:
            self._error(err, sys.exc_info())

    def parseline(self, line):
        """Have to override this because base class version strips the lines,
        making multi-line Python commands impossible.
        """
        #line = line.strip()
        if not line:
            return None, None, line
        elif line[0] == '?':
            line = 'help ' + line[1:]
        elif line[0] == '!':
            if hasattr(self, 'do_shell'):
                line = 'shell ' + line[1:]
            else:
                return None, None, line
        i, n = 0, len(line)
        while i < n and line[i] in self.identchars:
            i = i + 1
        cmd, arg = line[:i], line[i:].strip()
        return cmd, arg, line

    def emptyline(self):
        # Default for empty line is to repeat last command - yuck
        if self._partial_cmd:
            self.default('')

    def default(self, line):
        ''' Called on an input line when the command prefix is not recognized.
            In this case we execute the line as Python code.
        '''
        line = line.rstrip()
        if self._partial_cmd is None:
            if line.endswith(':'):
                self._partial_cmd = line
                return
        else:
            if line:
                self._partial_cmd = self._partial_cmd + '\n' + line
            if line.startswith(' ') or line.startswith('\t'):
                return
            else:
                line = self._partial_cmd
                self._partial_cmd = None
        try:
            result = self.proj.command(line)
            if result is not None:
                print result
        except Exception as err:
            self._error(err, sys.exc_info())

    @modifies_model
    def run(self, pathname, *args, **kwargs):
        ''' Run `pathname` or the model (i.e., the top assembly).
        '''
        pathname = pathname or 'top'
        if pathname in self.proj:
            print "Executing..."
            try:
                comp = self.proj.get(pathname)
                comp.run(*args, **kwargs)
                print "Execution complete."
            except Exception as err:
                self._error(err, sys.exc_info())
        else:
            self._print_error("Execution failed: No %r component was found." %
                              pathname)

    @modifies_model
    def execfile(self, filename):
        ''' Execfile in server's globals.
        '''
        try:
            self.proj.command("execfile('%s', '%s')" %
                              (filename, file_md5(filename)))
        except Exception as err:
            self._error(err, sys.exc_info())

    def get_pid(self):
        ''' Return this server's :attr:`pid`.
        '''
        return os.getpid()

    def get_project(self):
        ''' Return the current model as a project archive.
        '''
        return self.proj

    def get_history(self):
        ''' Return this server's :attr:`_hist`.
        '''
        return self._hist

    def get_recorded_cmds(self):
        ''' Return this server's :attr:`_recorded_cmds`.
        '''
        return self._recorded_cmds[:]

    def get_object(self, pathname, report=True):
        ''' Get the container with the specified pathname.
            Returns the container and the name of the root object.
        '''
        cont = None
        parts = pathname.split('.', 1)
        root = parts[0]
        if self.proj and root in self.proj:
            if root == pathname:
                cont = self.proj.get(root)
            else:
                try:
                    root_obj = self.proj.get(root)
                except Exception as err:
                    self._error(err, sys.exc_info())
                else:
                    try:
                        cont = root_obj.get(parts[1])
                    except AttributeError as err:
                        # When publishing, don't report remove as an error.
                        if report:
                            self._error(err, sys.exc_info())
                    except Exception as err:
                        self._error(err, sys.exc_info())
        return cont, root

    def _get_components(self, cont, pathname=None):
        ''' Get a heierarchical list of all the components in the given
            container or dictionary.  The name of the root container, if
            specified, is prepended to all pathnames.
        '''
        comps = []
        for k, v in cont.items():
            if is_instance(v, Component):
                comp = {}
                if cont is self.proj._model_globals:
                    comp['pathname'] = k
                    children = self._get_components(v, k)
                else:
                    comp['pathname'] = '.'.join([pathname, k]) if pathname else k
                    children = self._get_components(v, comp['pathname'])
                if len(children) > 0:
                    comp['children'] = children
                comp['type'] = str(v.__class__.__name__)
                inames = []
                for klass in list(implementedBy(v.__class__)):
                    inames.append(klass.__name__)
                comp['interfaces'] = inames
                comps.append(comp)
        return comps

    def get_components(self):
        ''' Get hierarchical dictionary of openmdao objects.
        '''
        return json.dumps(self._get_components(self.proj._model_globals),
                          default=json_default)

    def get_connections(self, pathname, src_name, dst_name):
        ''' For the assembly with the given pathname, get a list of the outputs
            from the component *src_name* (sources), the inputs to the component
            *dst_name* (destinations), and the connections between them.
        '''
        conns = {}
        asm, root = self.get_object(pathname)
        if asm:
            try:
                conns = asm.get_connections(src_name, dst_name)
            except Exception as err:
                self._error(err, sys.exc_info())
        return json.dumps(conns, default=json_default)

    def get_dataflow(self, pathname):
        ''' Get the structure of the specified assembly or of the global
            namespace if no pathname is specified; consists of the list of
            components and the connections between them (i.e., the dataflow).
        '''
        dataflow = {}
        if pathname and len(pathname) > 0:
            try:
                asm, root = self.get_object(pathname)
                if has_interface(asm, IAssembly):
                    dataflow = asm.get_dataflow()
            except Exception as err:
                self._error(err, sys.exc_info())
        else:
            components = []
            for k, v in self.proj.items():
                if is_instance(v, Component):
                    inames = [cls.__name__
                              for cls in list(implementedBy(v.__class__))]
                    components.append({
                        'name': k,
                        'pathname': k,
                        'type': type(v).__name__,
                        'valid': v.is_valid(),
                        'interfaces': inames,
                        'python_id': id(v)
                    })
            dataflow['components']  = components
            dataflow['connections'] = []
            dataflow['parameters']  = []
            dataflow['constraints'] = []
            dataflow['objectives']  = []
        return json.dumps(dataflow, default=json_default)

    def get_available_events(self, pathname):
        ''' Serve a list of events that are available to a driver.
        '''
        events = []
        if pathname:
            drvr, root = self.get_object(pathname)
            events = drvr.list_available_events()

        return json.dumps(events, default=json_default)

    def get_workflow(self, pathname):
        ''' Get the workflow for the specified driver or assembly.
            If no driver or assembly is specified, get the workflows for
            all of the top-level assemblies.
        '''
        flows = []
        if pathname:
            drvr, root = self.get_object(pathname)
            # allow for request on the parent assembly
            if is_instance(drvr, Assembly):
                drvr = drvr.get('driver')
                pathname = pathname + '.driver'
            if drvr:
                try:
                    flow = drvr.get_workflow()
                except Exception as err:
                    self._error(err, sys.exc_info())
                flows.append(flow)
        else:
            for k, v in self.proj.items():
                if is_instance(v, Assembly):
                    v = v.get('driver')
                if is_instance(v, Driver):
                    flow = v.get_workflow()
                    flows.append(flow)
        return json.dumps(flows, default=json_default)

    def get_attributes(self, pathname):
        ''' Get the attributes of the specified object.
        '''
        attr = {}
        comp, root = self.get_object(pathname)
        try:
            if comp:
                attr = comp.get_attributes(io_only=False)
            return json.dumps(attr, default=json_default)
        except Exception as err:
            self._error(err, sys.exc_info())

    def get_passthroughs(self, pathname):
        ''' Get the inputs and outputs of the assembly's child components
            and indicate for each whether or not it is a passthrough variable.
        '''
        asm, root = self.get_object(pathname)
        passthroughs = asm.get_passthroughs()
        return json.dumps(passthroughs, default=json_default)

    def get_value(self, pathname):
        ''' Get the value of the object with the given pathname.
        '''
        try:
            val, root = self.get_object(pathname)
            return val
        except Exception as err:
            self._print_error("error getting value: %s" % err)

    def get_types(self):
        ''' Get a dictionary of types available for creation.
        '''
        #Don't want to get variable types showing up, so we exclude
        #'openmdao.variable' from this list.
        keyset = set(plugin_groups.keys())
        exclset = set(['openmdao.variable'])
        groups = list(keyset - exclset)
        return packagedict(get_available_types(groups))

    @modifies_model
    def load_project(self, projdir):
        ''' Activate the project in the specified directory;
            instantiate a file manager and projdirfactory.
        '''
        _clear_insts()
        self.cleanup()

        try:
            # Start a new log file.
            logging.getLogger().handlers[0].doRollover()

            self.files = FileManager('files', path=projdir,
                                     publish_updates=self.publish_updates)

            self.projdirfactory = ProjDirFactory(projdir,
                                                 observer=self.files.observer)
            register_class_factory(self.projdirfactory)

            self.proj = Project(projdir)
            repo = get_repo(projdir)
            if repo is None:
                find_vcs()[0](projdir).init_repo()
            self.proj.activate()
        except Exception as err:
            self._error(err, sys.exc_info())

    def commit_project(self, comment=''):
        ''' Save the current project macro and commit to the project repo.
        '''
        if self.proj:
            try:
                repo = get_repo(self.proj.path)
                repo.commit(comment)
                print 'Committed project in directory ', self.proj.path
            except Exception as err:
                self._error(err, sys.exc_info())
        else:
            self._print_error('No Project to commit')

    @modifies_model
    def revert_project(self, commit_id=None):
        ''' Revert to the most recent commit of the project.
        '''
        if self.proj:
            try:
                repo = get_repo(self.proj.path)
                repo.revert(commit_id)
                if commit_id is None:
                    commit_id = 'latest'
                print "Reverted project %s to commit '%s'" % (self.proj.name, commit_id)
            except Exception as err:
                self._error(err, sys.exc_info())
                return err  # give the caller an indication that something went wrong so he can
                            # give the proper error response to the http call if desired. Raising
                            # an exception here doesn't work
        else:
            msg = 'No Project to revert'
            self._print_error(msg)
            return Exception(msg)

    def get_signature(self, classname):
        ''' Get constructor argument signature for `classname`.
        '''
        try:
            return get_signature(str(classname))
        except Exception as err:
            self._error(err, sys.exc_info())

    def put_object(self, pathname, classname, args=None):
        ''' Create or replace object with the given pathname with a new object
            of the specified type.
        '''
        obj, root = self.get_object(pathname, report=False)
        if obj:
            self.replace_object(pathname, classname, args)
        else:
            self.add_object(pathname, classname, args)

    @modifies_model
    def add_object(self, pathname, classname, args):
        ''' Add a new object of the given type to the specified parent.
        '''
        parentname, dot, name = pathname.rpartition('.')
        if isidentifier(name):
            name = name.encode('utf8')
            if args is None:
                args = ''
            cmd = 'create("%s"%s)' % (classname, args)
            if parentname:
                cmd = '%s.add("%s", %s)' % (parentname, name, cmd)
            else:
                cmd = '%s = set_as_top(%s)' % (name, cmd)
            try:
                self.proj.command(cmd)
            except Exception as err:
                self._error(err, sys.exc_info())
        else:
            self._print_error('Error adding object:'
                              ' "%s" is not a valid identifier' % name)

    @modifies_model
    def replace_object(self, pathname, classname, args=None):
        ''' Replace existing object with object of the given type.
        '''
        pathname = pathname.encode('utf8')
        parentname, dot, name = pathname.rpartition('.')
        if parentname:
            try:
                self.proj.command('%s.replace("%s", create("%s"))'
                                  % (parentname, name, classname))
            except Exception as err:
                self._error(err, sys.exc_info())
        else:
            self._print_error('Error replacing component, no parent: "%s"'
                              % pathname)

    def cleanup(self):
        ''' Cleanup various resources.
        '''
        if self.proj:
            self.proj.deactivate()
        if self.projdirfactory:
            self.projdirfactory.cleanup()
            remove_class_factory(self.projdirfactory)
        if self.files:
            self.files.cleanup()

    def get_files(self):
        ''' Get a nested dictionary of files.
        '''
        try:
            return self.files.get_files(root=self.proj.path)
        except AttributeError:
            return {}

    def get_file(self, filename):
        ''' Get contents of a file.
            Returns a tuple of (file contents, mimetype, encoding).
            Tuple values will be None if file was not found.
        '''
        return self.files.get_file(filename)

    def ensure_dir(self, dirname):
        ''' Create directory
            (does nothing if directory already exists).
        '''
        return self.files.ensure_dir(dirname)

    def write_file(self, filename, contents):
        ''' Write contents to file.
        '''
        ret = self.files.write_file(filename, contents)
        if not ret is True:
            return ret

    def add_file(self, filename, contents):
        ''' Add file.
        '''
        return self.files.add_file(filename, contents)

    def delete_file(self, filename):
        ''' Delete file from project.
            Returns False if file was not found; otherwise returns True.
        '''
        return self.files.delete_file(filename)

    def rename_file(self, oldpath, newname):
        ''' Rename file.
        '''
        return self.files.rename_file(oldpath, newname)

    def install_addon(self, url, distribution):
        print "Installing", distribution, "from", url
        easy_install.main(["-U", "-f", url, distribution])

    def add_subscriber(self, pathname, publish):
        ''' Publish the specified topic.
        '''
        if pathname in ['', 'components', 'files', 'types',
                        'console_errors', 'file_errors']:
            # these topics are published automatically
            return
        elif pathname == 'log_msgs':
            if publish:
                self._start_log_msgs(pathname)
            else:
                self._stop_log_msgs()
        elif pathname.startswith('/'):  # treat it as a filename
            if publish:
                Publisher.register(pathname, pathname[1:])
            else:
                Publisher.unregister(pathname)
        else:
            parts = pathname.split('.', 1)
            if len(parts) > 1:
                root = self.proj.get(parts[0])
                if root:
                    rest = parts[1]
                    root.register_published_vars(rest, publish)

            cont, root = self.get_object(pathname)
            if has_interface(cont, IComponent):
                if publish:
                    if pathname in self._publish_comps:
                        self._publish_comps[pathname] += 1
                    else:
                        self._publish_comps[pathname] = 1
                else:
                    if pathname in self._publish_comps:
                        self._publish_comps[pathname] -= 1
                        if self._publish_comps[pathname] < 1:
                            del self._publish_comps[pathname]

    def _start_log_msgs(self, topic):
        """ Start sending log messages. """
        # Need to lock access while we capture state.
        logging._acquireLock()
        try:
            # Flush output.
            for handler in logging.getLogger().handlers:
                handler.flush()

            # Grab previously logged messages.
            log_path = os.path.join(self._log_directory, 'openmdao_log.txt')
            with open(log_path, 'r') as inp:
                line = True  # Just to get things started.
                while line:
                    lines = []
                    for i in range(100):  # Process in chunks.
                        line = inp.readline()
                        if line:
                            lines.append(line)
                        else:
                            break
                    if lines:
                        publish('log_msgs',
                                dict(active=False, text=''.join(lines)))
            # End of historical messages.
            publish('log_msgs', dict(active=False, text=''))

            # Add handler to get any new messages.
            if self._log_handler is None:
                self._log_handler = _LogHandler()
                logging.getLogger().addHandler(self._log_handler)
        except Exception:
            print "Can't initiate logging:"
            traceback.print_exc()
        finally:
            logging._releaseLock()
        self._log_subscribers += 1

    def _stop_log_msgs(self):
        """ Stop sending log messages. """
        self._log_subscribers -= 1
        if self._log_subscribers <= 0:
            if self._log_handler is not None:
                logging.getLogger().removeHandler(self._log_handler)
                self._log_handler = None
            self._log_subscribers = 0

    def is_macro(self, filename):
        return filename.lstrip('/') == os.path.join(os.path.basename(self.proj.macrodir),
                                                    self.proj.macro)

    def file_forces_reload(self, filename):
        """Returns True if the given file (assumed to be a file in the
        project) has classes that have been instantiated in the current
        process or if the file is a macro file. Note that this doesn't keep
        track of removes/deletions, so if an instance was created earlier and
        then deleted, it will still be reported.
        """
        pdf = self.projdirfactory
        if pdf:
            if self.is_macro(filename):
                return True
            if filename.endswith('.py'):
                filename = filename.lstrip('/')
                filename = os.path.join(self.proj.path, filename)
                info = pdf._files.get(filename)
                if info and _match_insts(info.classes.keys()):
                    return True
        return False


class _LogHandler(logging.StreamHandler):
    """ Logging handler that publishes messages. """

    def __init__(self):
        # Python < 2.7 doesn't like super() here.
        logging.StreamHandler.__init__(self, _LogStream())
        # Formatting set to match format of file.
        msg_fmt = '%(asctime)s %(levelname)s %(name)s: %(message)s'
        date_fmt = '%b %d %H:%M:%S'
        self.setFormatter(logging.Formatter(msg_fmt, date_fmt))


class _LogStream(object):
    """ Provides stream interface to publisher. """

    def write(self, msg):
        publish('log_msgs', dict(active=True, text=msg))

    def flush(self):
        pass
