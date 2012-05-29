import os
import os.path
import sys
import traceback
import cmd
import jsonpickle

from setuptools.command import easy_install

from enthought.traits.api import HasTraits
from openmdao.main.factorymanager import create, get_available_types
from openmdao.main.component import Component
from openmdao.main.assembly import Assembly, set_as_top

from openmdao.lib.releaseinfo import __version__, __date__

from openmdao.main.project import project_from_archive

from openmdao.main.publisher import Publisher

from openmdao.main.mp_support import has_interface, is_instance
from openmdao.main.interfaces import IContainer, IComponent, IAssembly
from zope.interface import implementedBy

from openmdao.gui.util import packagedict, ensure_dir
from openmdao.gui.filemanager import FileManager


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
        self.known_types = []

        self.host = host
        self.projfile = ''
        self.proj = None
        self.exc_info = None
        self.publish_updates = publish_updates
        self.publisher = None
        self._publish_comps = {}

        self.files = FileManager('files', publish_updates=publish_updates)

    def _update_roots(self):
        ''' Ensure that all root containers in the project dictionary know
            their own name and that all root assemblies are set as top
        '''
        g = self.proj.__dict__.items()
        for k, v in g:
            if has_interface(v, IContainer):
                if v.name != k:
                    v.name = k
            if is_instance(v, Assembly):
                set_as_top(v)

    def publish_components(self):
        ''' publish the current component tree and subscribed components
        '''
        if not self.publisher:
            try:
                self.publisher = Publisher.get_instance()
            except Exception, err:
                print 'Error getting publisher:', err
                self.publisher = None

        if self.publisher:
            self.publisher.publish('components', self.get_components())
            self.publisher.publish('', {'Dataflow': self.get_dataflow('')})
            for pathname in self._publish_comps:
                comp, root = self.get_container(pathname)
                self.publisher.publish(pathname, comp.get_attributes(ioOnly=False))

            # this will go away with bret's change
            self.publisher.publish('types', self.get_types())

    def _error(self, err, exc_info):
        ''' print error message and save stack trace in case it's requested
        '''
        self.exc_info = exc_info
        print str(err.__class__.__name__), ":", err

    def do_trace(self, arg):
        ''' print remembered trace from last exception
        '''
        if self.exc_info:
            exc_type, exc_value, exc_traceback = self.exc_info
            traceback.print_exception(exc_type, exc_value, exc_traceback)
            traceback.print_tb(exc_traceback, limit=30)
        else:
            print "No trace available."

    def precmd(self, line):
        ''' This method is called after the line has been input but before
            it has been interpreted. If you want to modifdy the input line
            before execution (for example, variable substitution) do it here.
        '''
        self._hist += [line.strip()]
        return line

    def onecmd(self, line):
        self._hist += [line.strip()]
        # Override the onecmd() method so we can trap error returns
        try:
            cmd.Cmd.onecmd(self, line)
        except Exception, err:
            self._error(err, sys.exc_info())

    def emptyline(self):
        # Default for empty line is to repeat last command - yuck
        pass

    @modifies_model
    def default(self, line):
        ''' Called on an input line when the command prefix is not recognized.
            In that case we execute the line as Python code.
        '''
        isStatement = False
        try:
            compile(line, '<string>', 'eval')
        except SyntaxError:
            isStatement = True

        if isStatement:
            try:
                exec(line) in self.proj.__dict__
            except Exception, err:
                self._error(err, sys.exc_info())
        else:
            try:
                result = eval(line, self.proj.__dict__)
                if result is not None:
                    print result
            except Exception, err:
                self._error(err, sys.exc_info())

    @modifies_model
    def run(self, *args, **kwargs):
        ''' run the model (i.e. the top assembly)
        '''

        if 'top' in self.proj.__dict__:
            print "Executing..."
            try:
                top = self.proj.__dict__['top']
                top.run(*args, **kwargs)
                print "Execution complete."
            except Exception, err:
                self._error(err, sys.exc_info())
        else:
            print "Execution failed: No 'top' assembly was found."

    def execfile(self, filename):
        ''' execfile in server's globals.
        '''

        try:
            # first import all definitions
            basename = os.path.splitext(filename)[0]
            cmd = 'from ' + basename + ' import *'
            self.default(cmd)
            # then execute anything after "if __name__ == __main__:"
            # setting __name__ to __main__ won't work... fuggedaboutit
            with open(filename) as file:
                contents = file.read()
            main_str = 'if __name__ == "__main__":'
            contents.replace("if __name__ == '__main__':'", main_str)
            idx = contents.find(main_str)
            if idx >= 0:
                idx = idx + len(main_str)
                contents = 'if True:\n' + contents[idx:]
                self.default(contents)
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

    def get_JSON(self):
        ''' return current state as JSON
        '''
        return jsonpickle.encode(self.proj.__dict__)

    def get_container(self, pathname):
        ''' get the container with the specified pathname
            returns the container and the name of the root object
        '''
        cont = None
        root = pathname.split('.')[0]
        if self.proj and root in self.proj.__dict__:
            if root == pathname:
                cont = self.proj.__dict__[root]
            else:
                rest = pathname[len(root) + 1:]
                try:
                    cont = self.proj.__dict__[root].get(rest)
                except Exception, err:
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
                if cont == self.proj.__dict__:
                    comp['pathname'] = k
                    children = self._get_components(v, k)
                else:
                    comp['pathname'] = pathname + '.' + k if pathname else k
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
        comps = self._get_components(self.proj.__dict__)
        return jsonpickle.encode(comps)

    def get_connections(self, pathname, src_name, dst_name):
        ''' get list of src outputs, dst inputs and connections between them
        '''
        conns = {}
        asm, root = self.get_container(pathname)
        if asm:
            try:
                # outputs
                outputs = []
                src = asm.get(src_name)
                connected = src.list_outputs(connected=True)
                for name in src.list_outputs():
                    units = ''
                    meta = src.get_metadata(name)
                    if meta and 'units' in meta:
                        units = meta['units']
                    outputs.append({'name': name,
                                    'type': type(src.get(name)).__name__,
                                    'valid': src.get_valid([name])[0],
                                    'units': units,
                                    'connected': (name in connected)
                                   })
                conns['outputs'] = outputs

                # inputs
                inputs = []
                dst = asm.get(dst_name)
                connected = dst.list_inputs(connected=True)
                for name in dst.list_inputs():
                    units = ''
                    meta = dst.get_metadata(name)
                    if meta and 'units' in meta:
                        units = meta['units']
                    inputs.append({'name': name,
                                   'type': type(dst.get(name)).__name__,
                                   'valid': dst.get_valid([name])[0],
                                   'units': units,
                                   'connected': (name in connected)
                                 })
                conns['inputs'] = inputs

                # connections
                connections = []
                conntuples = asm.list_connections(show_passthrough=False)
                for src, dst in conntuples:
                    if src.startswith(src_name + ".") and \
                       dst.startswith(dst_name + "."):
                        connections.append([src, dst])
                conns['connections'] = connections
            except Exception, err:
                self._error(err, sys.exc_info())
        return jsonpickle.encode(conns)

    def set_connections(self, pathname, src_name, dst_name, connections):
        ''' set connections between src and dst components in the given
            assembly
        '''
        asm, root = self.get_container(pathname)
        if asm:
            try:
                conntuples = asm.list_connections(show_passthrough=False)
                # disconnect any connections that are not in the new set
                for src, dst in conntuples:
                    if src.startswith(src_name + ".") and \
                       dst.startswith(dst_name + "."):
                        if [src, dst] not in connections:
                            print "disconnecting", src, dst
                            asm.disconnect(src, dst)
                # connect stuff in new set that is not already connected
                for src, dst in connections:
                    if (src, dst) not in conntuples:
                        print "connecting", src, dst
                        asm.connect(src, dst)
            except Exception, err:
                self._error(err, sys.exc_info())

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
            g = self.proj.__dict__.items()
            for k, v in g:
                if is_instance(v, Component):
                    components.append({'name': k,
                                       'pathname': k,
                                       'type': type(v).__name__,
                                       'valid': v.is_valid()
                                      })
            dataflow['components'] = components
            dataflow['connections'] = []
        return jsonpickle.encode(dataflow)

    def get_workflow(self, pathname):
        flow = {}
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
        return jsonpickle.encode(flow)

    def get_attributes(self, pathname):
        attr = {}
        comp, root = self.get_container(pathname)
        if comp:
            try:
                attr = comp.get_attributes(ioOnly=False)
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
            print "error getting value:", err

    def get_types(self):
        types = packagedict(get_available_types())
        try:
            types['working'] = self.get_workingtypes()
        except Exception, err:
            print "Error adding working types:", str(err)
        return types

    def get_workingtypes(self):
        ''' Return this server's user defined types.
        '''
        g = self.proj.__dict__.items()
        for k, v in g:
            if not k in self.known_types and \
               ((type(v).__name__ == 'classobj') or \
                str(v).startswith('<class')):
                try:
                    obj = self.proj.__dict__[k]()
                    if is_instance(obj, HasTraits):
                        self.known_types.append((k, 'n/a'))
                except Exception:
                    # print 'Class', k, 'not included in working types'
                    pass
        return packagedict(self.known_types)

    @modifies_model
    def load_project(self, filename):
        print 'loading project from:', filename
        self.projfile = filename
        try:
            self.proj = project_from_archive(filename,
                                             dest_dir=self.files.getcwd())
            self.proj.activate()
        except Exception, err:
            self._error(err, sys.exc_info())

    def save_project(self):
        ''' save the cuurent project state & export it whence it came
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
                    print 'Export failed, directory not known'
            except Exception, err:
                self._error(err, sys.exc_info())
        else:
            print 'No Project to save'

    @modifies_model
    def add_component(self, name, classname, parentname):
        ''' add a new component of the given type to the specified parent.
        '''
        name = name.encode('utf8')
        if (parentname and len(parentname) > 0):
            parent, root = self.get_container(parentname)
            if parent:
                try:
                    if classname in self.proj.__dict__:
                        parent.add(name, self.proj.__dict__[classname]())
                    else:
                        parent.add(name, create(classname))
                except Exception, err:
                    self._error(err, sys.exc_info())
            else:
                print "Error adding component, parent not found:", parentname
        else:
            self.create(classname, name)

    @modifies_model
    def create(self, typname, name):
        ''' create a new object of the given type.
        '''
        try:
            if (typname.find('.') < 0):
                self.default(name + '=' + typname + '()')
            else:
                self.proj.__dict__[name] = create(typname)
        except Exception, err:
            self._error(err, sys.exc_info())

        return self.proj.__dict__

    def cleanup(self):
        ''' Cleanup this server's files.
        '''
        self.files.cleanup()

    def get_files(self):
        ''' get a nested dictionary of files
        '''
        return self.files.get_files()

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

    def publish(self, pathname, publish):
        ''' publish the specified topic
        '''
        if pathname in ['', 'components', 'files', 'types']:
            # these topics are published automatically
            return

        if not self.publisher:
            try:
                self.publisher = Publisher.get_instance()
            except Exception, err:
                print 'Error getting publisher:', err
                self.publisher = None

        if self.publisher:
            parts = pathname.split('.')
            if len(parts) > 1:
                root = self.proj.__dict__[parts[0]]
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

