import os
import os.path
import sys
import traceback
import cmd
import jsonpickle

from setuptools.command import easy_install
from zope.interface import implementedBy

from openmdao.main.api import Assembly, Component, Driver, logger, \
                              set_as_top, get_available_types
from openmdao.main.project import project_from_archive, Project, parse_archive_name, \
                                  ProjFinder, _clear_insts, _match_insts, add_proj_to_path
from openmdao.main.publisher import publish
from openmdao.main.mp_support import has_interface, is_instance
from openmdao.main.interfaces import IContainer, IComponent, IAssembly
from openmdao.main.factorymanager import register_class_factory, remove_class_factory

from openmdao.lib.releaseinfo import __version__, __date__

from openmdao.util.nameutil import isidentifier
from openmdao.util.fileutil import file_md5

from openmdao.gui.util import packagedict, ensure_dir
from openmdao.gui.filemanager import FileManager
from openmdao.gui.projdirfactory import ProjDirFactory


def modifies_model(target):
    ''' decorator for methods that may have modified the model
        performs maintenance on root level containers/assemblies and
        publishes the potentially updated components
    '''

    def wrapper(self, *args, **kwargs):
        result = target(self, *args, **kwargs)
        self._update_roots()
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
        self.projfile = ''
        self.proj = None
        self.exc_info = None
        self.publish_updates = publish_updates
        self._publish_comps = {}

        self._partial_cmd = None  # for multi-line commands

        self.projdirfactory = None

        try:
            self.files = FileManager('files', publish_updates=self.publish_updates)
        except Exception as err:
            self._error(err, sys.exc_info())

    def _update_roots(self):
        ''' Ensure that all root containers in the project dictionary know
            their own name and that all root assemblies are set as top
        '''
        for k, v in self.proj.items():
            if has_interface(v, IContainer):
                if v.name != k:
                    v.name = k
            if is_instance(v, Assembly) and v._call_cpath_updated:
                set_as_top(v)

    def publish_components(self):
        ''' publish the current component tree and subscribed components
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
                comp, root = self.get_container(pathname, report=False)
                if comp is None:
                    del self._publish_comps[pathname]
                    publish(pathname, {})
                else:
                    publish(pathname, comp.get_attributes(io_only=False))

    def send_pub_msg(self, msg, topic):
        ''' publish the given message with the given topic
        '''
        publish(topic, msg)

    def _error(self, err, exc_info):
        ''' print error message and save stack trace in case it's requested
        '''
        self._partial_cmd = None
        self.exc_info = exc_info
        msg = '%s: %s' % (err.__class__.__name__, err)
        logger.error(msg)
        self._print_error(msg)

    def _print_error(self, msg):
        ''' print & publish error message
        '''
        print msg
        try:
            publish('console_errors', msg)
        except:
            logger.error('publishing of message failed')

    def do_trace(self, arg):
        ''' print remembered trace from last exception
        '''
        if self.exc_info:
            exc_type, exc_value, exc_traceback = self.exc_info
            traceback.print_exception(exc_type, exc_value, exc_traceback)
        else:
            print "No trace available."

    def precmd(self, line):
        ''' This method is called after the line has been input but before
            it has been interpreted. If you want to modify the input line
            before execution (for example, variable substitution) do it here.
        '''
        #self._hist += [line.strip()]
        return line

    @modifies_model
    def onecmd(self, line):
        self._hist.append(line)
        try:
            cmd.Cmd.onecmd(self, line)
        except Exception, err:
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
            In that case we execute the line as Python code.
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
        except Exception, err:
            self._error(err, sys.exc_info())

    @modifies_model
    def run(self, *args, **kwargs):
        ''' run the model (i.e. the top assembly)
        '''

        if 'top' in self.proj:
            print "Executing..."
            try:
                top = self.proj.get('top')
                top.run(*args, **kwargs)
                print "Execution complete."
            except Exception, err:
                self._error(err, sys.exc_info())
        else:
            self._print_error("Execution failed: No 'top' assembly was found.")

    @modifies_model
    def execfile(self, filename):
        ''' execfile in server's globals.
        '''
        try:
            self.proj.command("execfile('%s', '%s')" %
                                 (filename, file_md5(filename)))
        except Exception, err:
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

    def get_JSON(self):
        ''' return current state as JSON
        '''
        return jsonpickle.encode(self.proj._model_globals)

    def get_container(self, pathname, report=True):
        ''' get the container with the specified pathname
            returns the container and the name of the root object
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
        ''' get a heierarchical list of all the components in the given
            container or dictionary.  the name of the root container, if
            specified, is prepended to all pathnames
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
        ''' get hierarchical dictionary of openmdao objects
        '''
        return jsonpickle.encode(self._get_components(self.proj._model_globals))

    def get_connections(self, pathname, src_name, dst_name):
        ''' get list of source variables, destination variables and the
            connections between them
        '''
        conns = {}
        asm, root = self.get_container(pathname)
        if asm:
            try:
                # outputs
                sources = []
                if src_name:
                    src = asm.get(src_name)
                else:
                    src = asm
                connected = src.list_outputs(connected=True)
                for name in src.list_outputs():
                    units = ''
                    meta = src.get_metadata(name)
                    if meta and 'units' in meta:
                        units = meta['units']
                    sources.append({'name': name,
                                    'type': type(src.get(name)).__name__,
                                    'valid': src.get_valid([name])[0],
                                    'units': units,
                                    'connected': (name in connected)
                                   })
                # connections to assembly can be passthrough (input to input)
                if src is asm:
                    connected = src.list_inputs(connected=True)
                    for name in src.list_inputs():
                        units = ''
                        meta = src.get_metadata(name)
                        if meta and 'units' in meta:
                            units = meta['units']
                        sources.append({'name': name,
                                        'type': type(src.get(name)).__name__,
                                        'valid': src.get_valid([name])[0],
                                        'units': units,
                                        'connected': (name in connected)
                                       })
                conns['sources'] = sorted(sources, key=lambda d: d['name'])

                # inputs
                dests = []
                if dst_name:
                    dst = asm.get(dst_name)
                else:
                    dst = asm
                connected = dst.list_inputs(connected=True)
                for name in dst.list_inputs():
                    units = ''
                    meta = dst.get_metadata(name)
                    if meta and 'units' in meta:
                        units = meta['units']
                    dests.append({'name': name,
                                  'type': type(dst.get(name)).__name__,
                                  'valid': dst.get_valid([name])[0],
                                  'units': units,
                                  'connected': (name in connected)
                                })
                # connections to assembly can be passthrough (output to output)
                if dst == asm:
                    connected = dst.list_outputs(connected=True)
                    for name in dst.list_outputs():
                        units = ''
                        meta = dst.get_metadata(name)
                        if meta and 'units' in meta:
                            units = meta['units']
                        dests.append({'name': name,
                                      'type': type(dst.get(name)).__name__,
                                      'valid': dst.get_valid([name])[0],
                                      'units': units,
                                      'connected': (name in connected)
                                     })
                conns['destinations'] = sorted(dests, key=lambda d: d['name'])

                # connections
                connections = []
                conntuples = asm.list_connections(show_passthrough=True)
                for src, dst in conntuples:
                    if (src_name and src.startswith(src_name + ".") \
                       or (not src_name and src.find('.') < 0)) and \
                       (dst_name and dst.startswith(dst_name + ".") \
                       or (not dst_name and dst.find('.') < 0)):
                        connections.append([src, dst])
                conns['connections'] = connections
            except Exception, err:
                self._error(err, sys.exc_info())
        return jsonpickle.encode(conns)

    def get_dataflow(self, pathname):
        ''' get the structure of the specified assembly, or of the global
            namespace if no pathname is specified, consisting of the list of
            components and the connections between them (i.e. the dataflow)
        '''
        dataflow = {}
        if pathname and len(pathname) > 0:
            try:
                asm, root = self.get_container(pathname)
                if has_interface(asm, IAssembly):
                    dataflow = asm.get_dataflow()
            except Exception, err:
                self._error(err, sys.exc_info())
        else:
            components = []
            for k, v in self.proj.items():
                if is_instance(v, Component):
                    inames = [cls.__name__
                              for cls in list(implementedBy(v.__class__))]
                    components.append({'name': k,
                                       'pathname': k,
                                       'type': type(v).__name__,
                                       'valid': v.is_valid(),
                                       'interfaces': inames,
                                       'python_id': id(v)
                                      })
            dataflow['components'] = components
            dataflow['connections'] = []
            dataflow['parameters'] = []
            dataflow['constraints'] = []
            dataflow['objectives'] = []
        return jsonpickle.encode(dataflow)

    def get_workflow(self, pathname):
        flows = []
        if pathname:
            drvr, root = self.get_container(pathname)
            # allow for request on the parent assembly
            if is_instance(drvr, Assembly):
                drvr = drvr.get('driver')
                pathname = pathname + '.driver'
            if drvr:
                try:
                    flow = drvr.get_workflow()
                except Exception, err:
                    self._error(err, sys.exc_info())
                flows.append(flow)
        else:
            for k, v in self.proj.items():
                if is_instance(v, Assembly):
                    v = v.get('driver')
                if is_instance(v, Driver):
                    flow = {}
                    flow['pathname'] = v.get_pathname()
                    flow['type'] = type(v).__module__ + '.' + type(v).__name__
                    flow['workflow'] = []
                    flow['valid'] = v.is_valid()
                    for comp in v.workflow:
                        pathname = comp.get_pathname()
                        if is_instance(comp, Assembly) and comp.driver:
                            flow['workflow'].append({
                                'pathname': pathname,
                                'type':     type(comp).__module__ + '.' + type(comp).__name__,
                                'driver':   comp.driver.get_workflow(),
                                'valid':    comp.is_valid()
                              })
                        elif is_instance(comp, Driver):
                            flow['workflow'].append(comp.get_workflow())
                        else:
                            flow['workflow'].append({
                                'pathname': pathname,
                                'type':     type(comp).__module__ + '.' + type(comp).__name__,
                                'valid':    comp.is_valid()
                              })
                    flows.append(flow)
        return jsonpickle.encode(flows)

    def get_attributes(self, pathname):
        attr = {}
        comp, root = self.get_container(pathname)
        if comp:
            try:
                attr = comp.get_attributes(io_only=False)
            except Exception, err:
                self._error(err, sys.exc_info())
        return jsonpickle.encode(attr)

    def get_value(self, pathname):
        ''' get the value of the object with the given pathname
        '''
        try:
            val, root = self.get_container(pathname)
            return val
        except Exception, err:
            self._print_error("error getting value: %s" % err)

    def get_types(self):
        return packagedict(get_available_types())

    @modifies_model
    def load_project(self, filename):
        _clear_insts()
        self.projfile = filename
        try:
            if self.proj:
                self.proj.deactivate()
            if self.projdirfactory:
                self.projdirfactory.cleanup()
                remove_class_factory(self.projdirfactory)

            # make sure we have a ProjFinder in sys.path_hooks
            for hook in sys.path_hooks:
                if hook is ProjFinder:
                    break
            else:
                sys.path_hooks = [ProjFinder] + sys.path_hooks

            # have to do things in a specific order here. First, create the files,
            # then point the ProjDirFactory at the files, then finally create the
            # Project. Executing the project macro (which happens in the Project __init__)
            # requires that the ProjDirFactory is already in place.
            project_from_archive(filename, dest_dir=self.files.getcwd(), create=False)
            projdir = os.path.join(self.files.getcwd(), parse_archive_name(filename))
            
            add_proj_to_path(projdir)
            
            self.projdirfactory = ProjDirFactory(projdir,
                                                 observer=self.files.observer)
            register_class_factory(self.projdirfactory)
            self.proj = Project(projdir)
        except Exception, err:
            self._error(err, sys.exc_info())

    def save_project(self):
        ''' save the current project state & export it whence it came
        '''
        if self.proj:
            try:
                self.proj.save()
                print 'Project state saved.'
                if len(self.projfile) > 0:
                    dir = os.path.dirname(self.projfile)
                    ensure_dir(dir)
                    self.proj.export(destdir=dir)
                    print 'Exported to ', dir + '/' + self.proj.name
                else:
                    self._print_error('Export failed, directory not known')
            except Exception, err:
                self._error(err, sys.exc_info())
        else:
            self._print_error('No Project to save')

    @modifies_model
    def add_component(self, name, classname, parentname):
        ''' add a new component of the given type to the specified parent.
        '''
        if isidentifier(name):
            name = name.encode('utf8')
            if parentname:
                cmd = '%s.add("%s",create("%s"))' % (parentname, name, classname)
            else:
                cmd = '%s = create("%s")' % (name, classname)
            try:
                self.proj.command(cmd)
            except Exception, err:
                self._error(err, sys.exc_info())
        else:
            self._print_error('Error adding component: "%s" is not a valid identifier' % name)

    @modifies_model
    def replace_component(self, pathname, classname):
        ''' replace existing component with component of the given type.
        '''
        pathname = pathname.encode('utf8')
        parentname, dot, name = pathname.rpartition('.')
        if parentname:
            try:
                self.proj.command('%s.replace("%s", create("%s"))' % (parentname,
                                                                      name, classname))
            except Exception, err:
                self._error(err, sys.exc_info())
        else:
            self._print_error('Error replacing component, no parent: "%s"'
                              % pathname)

    def cleanup(self):
        ''' Cleanup various resources.
        '''
        if self.projdirfactory:
            self.projdirfactory.cleanup()
            remove_class_factory(self.projdirfactory)
        self.files.cleanup()

    def get_files(self):
        ''' get a nested dictionary of files
        '''
        try:
            return self.files.get_files(root=self.proj.path)
        except AttributeError:
            return {}

    def get_file(self, filename):
        ''' get contents of a file
            returns None if file was not found
        '''
        return self.files.get_file(filename)

    def ensure_dir(self, dirname):
        ''' create directory
            (does nothing if directory already exists)
        '''
        return self.files.ensure_dir(dirname)

    def write_file(self, filename, contents):
        ''' write contents to file
        '''
        return self.files.write_file(filename, contents)

    def add_file(self, filename, contents):
        ''' add file
        '''
        return self.files.add_file(filename, contents)

    def delete_file(self, filename):
        ''' delete file from project
            returns False if file was not found, otherwise returns True
        '''
        return self.files.delete_file(filename)

    def install_addon(self, url, distribution):
        print "Installing", distribution, "from", url
        easy_install.main(["-U", "-f", url, distribution])

    def add_subscriber(self, pathname, publish):
        ''' publish the specified topic
        '''
        if pathname in ['', 'components', 'files', 'types',
                        'console_errors', 'file_errors']:
            # these topics are published automatically
            return

        parts = pathname.split('.')
        if len(parts) > 1:
            root = self.proj.get(parts[0])
            if root:
                rest = '.'.join(parts[1:])
                root.register_published_vars(rest, publish)

        cont, root = self.get_container(pathname)
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

    def file_has_instances(self, filename):
        """Returns True if the given file (assumed to be a file in the project)
        has classes that have been instantiated in the current process. Note that
        this doesn't keep track of removes/deletions, so if an instance was created
        earlier and then deleted, it will still be reported.
        """
        pdf = self.projdirfactory
        if pdf:
            filename = filename.lstrip('/')
            filename = os.path.join(self.proj.path, filename)
            info = pdf._files.get(filename)
            if info and _match_insts(info.classes.keys()):
                return True
        return False
