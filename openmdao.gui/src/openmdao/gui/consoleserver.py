import os, os.path, sys, traceback
import shutil
import cmd
import jsonpickle
import tempfile
import zipfile

from setuptools.command import easy_install

from enthought.traits.api import HasTraits
from openmdao.main.variable import Variable
from openmdao.main.factorymanager import create, get_available_types
from openmdao.main.component import Component
from openmdao.main.assembly import Assembly
from openmdao.main.driver import Driver
from openmdao.main.datatypes.slot import Slot
from openmdao.main.publisher import Publisher

from openmdao.lib.releaseinfo import __version__, __date__

from openmdao.main.project import *

from openmdao.main.mp_support import has_interface, is_instance
from openmdao.main.interfaces import *
from zope.interface import implementedBy

import networkx as nx

from openmdao.util.network import get_unused_ip_port
from openmdao.gui.util import *

def modifies_model(target):
    ''' decorator for methods that modify the model
        performs maintenance on root level containers/assemblies
    '''
    def wrapper(self, *args, **kwargs):
        result = target(self, *args, **kwargs)
        self._update_roots()
        return result
    return wrapper


class ConsoleServer(cmd.Cmd):
    ''' Object which knows how to load a model and provides a command line interface
        and various methods to access and modify that model.
    '''

    def __init__(self, name='', host=''):
        cmd.Cmd.__init__(self)

        print '<<<'+str(os.getpid())+'>>> ConsoleServer ..............'
        
        self.intro  = 'OpenMDAO '+__version__+' ('+__date__+')'
        self.prompt = 'OpenMDAO>> '
        
        self._hist    = []      ## No history yet
        self.known_types = []

        self.host = host
        self.pid = os.getpid()
        self.name = name or ('-cserver-%d' % self.pid)
        self.orig_dir = os.getcwd()
        self.root_dir = tempfile.mkdtemp(self.name)
        if os.path.exists(self.root_dir):
            shutil.rmtree(self.root_dir)
        os.mkdir(self.root_dir)
        os.chdir(self.root_dir)
        
        print 'root_dir=',self.root_dir
        
        self.projfile = ''
        self.proj = None
        self.exc_info = None

    def _update_roots(self):
        ''' Ensure that all root containers in the project dictionary know
            their own name and that all root assemblies are set as top
        '''
        g = self.proj.__dict__.items()
        for k,v in g:
            if has_interface(v,IContainer):
                if v.name != k:
                    v.name = k
            if is_instance(v,Assembly):
                set_as_top(v)
                    
    def _error(self,err,exc_info):
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
        
    def getcwd(self):
        return os.getcwd()

    def chdir(self, dirname):
        if not os.path.isdir(dirname):
            os.mkdir(dirname)
        os.chdir(dirname)
        sys.path[0] = dirname

    def precmd(self, line):
        ''' This method is called after the line has been input but before
            it has been interpreted. If you want to modifdy the input line
            before execution (for example, variable substitution) do it here.
        '''
        self._hist += [ line.strip() ]
        return line

    @modifies_model
    def onecmd(self, line):
        self._hist += [ line.strip() ]
        # Override the onecmd() method so we can trap error returns
        try:
            cmd.Cmd.onecmd(self, line)
        except Exception, err:
            self._error(err,sys.exc_info())

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
            code = compile(line, '<string>', 'eval')
        except SyntaxError:
            isStatement = True

        if isStatement:
            try:
                exec(line) in self.proj.__dict__
            except Exception, err:
                self._error(err,sys.exc_info())
        else:
            try:
                result = eval(line, self.proj.__dict__)
                if result is not None:
                    print result
            except Exception, err:
                self._error(err,sys.exc_info())
            
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
                self._error(err,sys.exc_info())
        else:
            print "Execution failed: No 'top' assembly was found."

    def execfile(self, filename):
        ''' execfile in server's globals. 
        '''
        try:
            # first import all definitions
            basename = os.path.splitext(filename)[0]
            cmd = 'from '+basename+' import *'
            self.default(cmd)
            # then execute anything after "if __name__ == __main__:"
            with open(filename) as file:
                contents = file.read()
            main_str = 'if __name__ == "__main__":'
            contents.replace("if __name__ == '__main__':'",main_str)
            idx = contents.find(main_str)
            if idx >= 0:
                idx = idx + len(main_str)
                contents = 'if True:\n' + contents[idx:]
                self.default(contents)
        except Exception, err:
            self._error(err,sys.exc_info())

    def get_pid(self):
        ''' Return this server's :attr:`pid`. 
        '''
        return self.pid
        
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

    def get_container(self,pathname):
        ''' get the container with the specified pathname
            returns the container and the name of the root object
        '''
        cont = None
        root = pathname.split('.')[0]
        if self.proj and root in self.proj.__dict__:
            if root == pathname:
                cont = self.proj.__dict__[root]
            else:
                rest = pathname[len(root)+1:]
                try:
                    cont = self.proj.__dict__[root].get(rest)
                except Exception, err:
                    self._error(err,sys.exc_info())
        return cont, root
        
    def _get_components(self,cont,pathname=None):
        ''' get a heierarchical list of all the components in the given
            container or dictionary.  the name of the root container, if
            specified, is prepended to all pathnames
        '''
        
        comps = []
        for k,v in cont.items():                        
            if is_instance(v,Component):
                comp = {}
                if cont == self.proj.__dict__:
                    comp['pathname'] = k
                    children = self._get_components(v,k)
                else:
                    comp['pathname'] = pathname+'.'+ k if pathname else k
                    children = self._get_components(v,comp['pathname'])
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

    def get_connections(self,pathname,src_name,dst_name):
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
                    meta = src.get_metadata(name);
                    if meta and 'units' in meta:
                        units = meta['units']
                    outputs.append({'name': name,
                                    'type': type(src.get(name)).__name__ ,
                                    'valid': src.get_valid([name])[0],
                                    'units': units,
                                    'connected': (name in connected)
                                   })                                    
                conns['outputs'] = outputs;
                    
                # inputs
                inputs = []
                dst = asm.get(dst_name)
                connected = dst.list_inputs(connected=True)
                for name in dst.list_inputs():
                    units = ''
                    meta = dst.get_metadata(name);
                    if meta and 'units' in meta:
                        units = meta['units']
                    inputs.append({'name': name,
                                   'type': type(dst.get(name)).__name__ ,
                                   'valid': dst.get_valid([name])[0],
                                   'units': units,
                                   'connected': (name in connected)
                                 })
                conns['inputs']  = inputs;
                
                # connections
                connections = [];
                conntuples = asm.list_connections(show_passthrough=False)
                for src,dst in conntuples:
                    if src.startswith(src_name+".") and dst.startswith(dst_name+"."):
                        connections.append([src, dst])
                conns['connections'] = connections;
            except Exception, err:
                self._error(err,sys.exc_info())
        return jsonpickle.encode(conns)

    def set_connections(self,pathname,src_name,dst_name,connections):
        ''' set connections between src and dst components in the given assembly
        '''
        asm, root = self.get_container(pathname)
        if asm:
            try:
                conntuples = asm.list_connections(show_passthrough=False)
                # disconnect any connections that are not in the new set
                for src,dst in conntuples:
                    if src.startswith(src_name+".") and dst.startswith(dst_name+"."):
                        if [src,dst] not in connections:
                            print "disconnecting",src,dst
                            asm.disconnect(src,dst)
                # connect stuff in new set that is not already connected
                for src,dst in connections:
                    if (src,dst) not in conntuples:
                        print "connecting",src,dst
                        asm.connect(src,dst)
                conns['connections'] = connections;
            except Exception, err:
                self._error(err,sys.exc_info())

    def _get_structure(self,asm,pathname):
        ''' get the list of components and connections between them
            that make up the data flow for the given assembly 
        '''
        components = []
        connections = []
        if is_instance(asm,Assembly):
            # list of components (name & type) in the assembly
            g = asm._depgraph._depgraph._graph
            for name in nx.algorithms.dag.topological_sort(g):
                if not name.startswith('@'):
                    comp = asm.get(name)
                    if is_instance(comp,Component):
                        components.append({ 'name': comp.name,
                                            'pathname': pathname + '.' + name,
                                            'type': type(comp).__name__ })
            # list of connections (convert tuples to lists)
            conntuples = asm.list_connections(show_passthrough=False)
            for connection in conntuples:
                connections.append(list(connection))
        return { 'components': components, 'connections': connections }

    def get_structure(self,pathname):
        ''' get the structure of the specified assembly, or of the global 
            namespace if no pathname is specified, consisting of the list
            of components and the connections between them
        '''
        structure = {}
        if pathname and len(pathname) > 0:
            try:
                asm, root = self.get_container(pathname)
                structure = self._get_structure(asm, pathname)
            except Exception, err:
                self._error(err,sys.exc_info())
        else:
            components = []
            g = self.proj.__dict__.items()
            for k,v in g:
                if is_instance(v,Component):
                    components.append({ 'name': k,
                                        'pathname': k+'.'+v.get_pathname(),
                                        'type': type(v).__name__ })
            structure['components'] = components
            structure['connections'] = []
        return jsonpickle.encode(structure)

    def _get_workflow(self,drvr,pathname,root):
        ''' get the driver info and the list of components that make up the
            driver's workflow, recurse on nested drivers
        '''
        ret = {}
        ret['pathname'] = pathname
        ret['type'] = type(drvr).__module__+'.'+type(drvr).__name__ 
        ret['workflow'] = []
        for comp in drvr.workflow:
            pathname = comp.get_pathname()
            if is_instance(comp,Assembly) and comp.driver:
                ret['workflow'].append({ 
                    'pathname': pathname,
                    'type':     type(comp).__module__+'.'+type(comp).__name__,
                    'driver':   self._get_workflow(comp.driver,pathname+'.driver',root)
                })
            elif is_instance(comp,Driver):
                ret['workflow'].append(self._get_workflow(comp,pathname,root))            
            else:
                ret['workflow'].append({ 
                    'pathname': pathname,
                    'type':     type(comp).__module__+'.'+type(comp).__name__,
                })
        return ret

    def get_workflow(self,pathname):
        flow = {}
        drvr, root = self.get_container(pathname)
        # allow for request on the parent assembly
        if is_instance(drvr,Assembly):
            drvr = drvr.get('driver')
            pathname = pathname + '.driver'
        if drvr:
            try:
                flow = self._get_workflow(drvr,pathname,root)
            except Exception, err:
                self._error(err,sys.exc_info())
        return jsonpickle.encode(flow)

    def _get_attributes(self,comp,pathname,root):
        ''' get attributes of object 
        '''
        attrs = {}
        
        if has_interface(comp,IComponent):
            inputs = []
            for vname in comp.list_inputs():
                v = comp.get(vname)
                attr = {}
                if not is_instance(v,Component):
                    attr['name'] = vname
                    attr['type'] = type(v).__name__
                    attr['value'] = str(v)
                    attr['valid'] = comp.get_valid([vname])[0]
                    meta = comp.get_metadata(vname);
                    if meta:
                        for field in ['units','high','low','desc']:
                            if field in meta:
                                attr[field] = meta[field]
                            else:
                                attr[field] = ''
                inputs.append(attr)
            attrs['Inputs'] = inputs
                
            outputs = []
            for vname in comp.list_outputs():
                v = comp.get(vname)
                attr = {}
                if not is_instance(v,Component):
                    attr['name'] = vname
                    attr['type'] = type(v).__name__
                    attr['value'] = str(v)
                    attr['valid'] = comp.get_valid([vname])[0]
                    meta = comp.get_metadata(vname);
                    if meta:
                        for field in ['units','high','low','desc']:
                            if field in meta:
                                attr[field] = meta[field]
                            else:
                                attr[field] = ''
                outputs.append(attr)
            attrs['Outputs'] = outputs

        if is_instance(comp,Assembly):
            attrs['Structure'] = self._get_structure(comp,pathname)
        
        if has_interface(comp,IDriver):
            attrs['Workflow'] = self._get_workflow(comp,pathname,root)
        
        if has_interface(comp,IHasCouplingVars):
            couples = []
            objs = comp.list_coupling_vars()
            for indep,dep in objs:
                attr = {}
                attr['independent'] = indep
                attr['dependent'] = dep
                couples.append(attr)
            attrs['CouplingVars'] = couples
            
        if has_interface(comp,IHasObjectives):
            objectives = []
            objs = comp.get_objectives()
            for key in objs.keys():
                attr = {}
                attr['name'] = str(key)
                attr['expr'] = objs[key].text
                attr['scope'] = objs[key].scope.name
                objectives.append(attr)
            attrs['Objectives'] = objectives
            
        if has_interface(comp,IHasParameters):
            parameters = []
            parms = comp.get_parameters()
            for key,parm in parms.iteritems():
                attr = {}
                attr['name']    = str(key)
                attr['target']  = parm.target
                attr['low']     = parm.low
                attr['high']    = parm.high
                attr['scaler']  = parm.scaler
                attr['adder']   = parm.adder
                attr['fd_step'] = parm.fd_step
                #attr['scope']   = parm.scope.name
                parameters.append(attr)
            attrs['Parameters'] = parameters
        
        if has_interface(comp,IHasConstraints) or has_interface(comp,IHasEqConstraints):
            constraints = []
            cons = comp.get_eq_constraints()
            for key,con in cons.iteritems():
                attr = {}
                attr['name']    = str(key)
                attr['expr']    = str(con)
                attr['scaler']  = con.scaler
                attr['adder']   = con.adder
                constraints.append(attr)
            attrs['EqConstraints'] = constraints
            
        if has_interface(comp,IHasConstraints) or has_interface(comp,IHasIneqConstraints):
            constraints = []
            cons = comp.get_ineq_constraints()
            for key,con in cons.iteritems():
                attr = {}
                attr['name']    = str(key)
                attr['expr']    = str(con)
                attr['scaler']  = con.scaler
                attr['adder']   = con.adder
                constraints.append(attr)
            attrs['IneqConstraints'] = constraints
            
        slots = []
        for name, value in comp.traits().items():
            if value.is_trait_type(Slot):
                attr = {}
                attr['name'] = name
                attr['klass'] = value.trait_type.klass.__name__
                inames = []
                for klass in list(implementedBy(attr['klass'])):
                    inames.append(klass.__name__)
                attr['interfaces'] = inames
                if getattr(comp, name) is None:
                    attr['filled'] = False
                else:
                    attr['filled'] = True
                meta = comp.get_metadata(name);
                if meta:
                    for field in [ 'desc' ]:    # just desc?
                        if field in meta:
                            attr[field] = meta[field]
                        else:
                            attr[field] = ''
                    attr['type'] = meta['vartypename']
                slots.append(attr)            
        attrs['Slots'] = slots

        return attrs
        
    def get_attributes(self,pathname):
        attr = {}
        comp, root = self.get_container(pathname)
        if comp:
            try:
                attr = self._get_attributes(comp,pathname,root)
                attr['type'] = type(comp).__name__
            except Exception, err:
                self._error(err,sys.exc_info())
        return jsonpickle.encode(attr)
    
    def get_available_types(self):
        return packagedict(get_available_types())
        
    def get_workingtypes(self):
        ''' Return this server's user defined types. 
        '''
        g = self.proj.__dict__.items()
        for k,v in g:
            if not k in self.known_types and \
               ((type(v).__name__ == 'classobj') or str(v).startswith('<class')):
                try:
                    obj = self.proj.__dict__[k]()
                    if is_instance(obj, HasTraits):
                        self.known_types.append( ( k , 'n/a') )
                except Exception, err:
                    # print 'Class',k,'not included in working types'
                    # self._error(err,sys.exc_info())
                    pass
        return packagedict(self.known_types)

    @modifies_model
    def load_project(self,filename):
        print 'loading project from:',filename
        self.projfile = filename
        self.proj = project_from_archive(filename,dest_dir=self.getcwd())
        self.proj.activate()
        Publisher.get_instance()
        
    def save_project(self):
        ''' save the cuurent project state & export it whence it came
        '''
        if self.proj:
            try:
                self.proj.save()
                print 'Project state saved.'
                if len(self.projfile)>0:
                    dir = os.path.dirname(self.projfile)
                    ensure_dir(dir)
                    self.proj.export(destdir=dir)
                    print 'Exported to ',dir+'/'+self.proj.name
                else:
                    print 'Export failed, directory not known'
            except Exception, err:
                self._error(err,sys.exc_info())
        else:
            print 'No Project to save'

    @modifies_model
    def add_component(self,name,classname,parentname):
        ''' add a new component of the given type to the specified parent. 
        '''
        if (parentname and len(parentname)>0):
            parent, root = self.get_container(parentname)
            if parent:
                try:
                    if classname in self.proj.__dict__:
                        parent.add(name,self.proj.__dict__[classname]())
                    else:
                        parent.add(name,create(classname))
                except Exception, err:
                    self._error(err,sys.exc_info())
            else:
                print "Error adding component: parent",parentname,"not found."
        else:
            self.create(classname,name)

    @modifies_model
    def create(self,typname,name):
        ''' create a new object of the given type. 
        '''
        try:
            if (typname.find('.') < 0):
                self.default(name+'='+typname+'()')
            else:
                self.proj.__dict__[name]=create(typname)
        except Exception, err:
            self._error(err,sys.exc_info())

        return self.proj.__dict__            

    def cleanup(self):
        ''' Cleanup this server's directory. 
        '''
        self.stdout = self.sysout
        self.stderr = self.syserr
        os.chdir(self.orig_dir)
        if os.path.exists(self.root_dir):
            try:
                print "trying to rmtree ",self.root_dir
                shutil.rmtree(self.root_dir)
            except Exception, err:
                self._error(err,sys.exc_info())
        
    def get_files(self):
        ''' get a nested dictionary of files in the working directory
        '''
        cwd = os.getcwd()
        return filedict(cwd,root=cwd)

    def get_file(self,filename):
        ''' get contents of file in working directory
            returns None if file was not found
        '''
        filepath = os.getcwd()+'/'+str(filename)
        if os.path.exists(filepath):
            contents=open(filepath, 'rb').read()
            return contents
        else:
            return None

    def ensure_dir(self,dirname):
        ''' create directory in working directory
            (does nothing if directory already exists)
        '''
        dirpath = os.getcwd()+'/'+str(dirname)
        if not os.path.isdir(dirpath):
            os.makedirs(dirpath)

    def write_file(self,filename,contents):
        ''' write contents to file in working directory
        '''
        try:
            filepath = os.getcwd()+'/'+str(filename)
            fout = open(filepath,'wb')
            fout.write(contents)
            fout.close()
            return True
        except Exception, err:
            return err

    def add_file(self,filename,contents):
        ''' add file to working directory
            if it's a zip file, unzip it
        '''
        self.write_file(filename, contents)
        if zipfile.is_zipfile(filename):
            userdir = os.getcwd()
            zfile = zipfile.ZipFile( filename, "r" )
            zfile.printdir()
            for fname in zfile.namelist():
                if fname.endswith('/'):
                    dirname = userdir+'/'+fname
                    if not os.path.exists(dirname):
                        os.makedirs(dirname)
            for fname in zfile.namelist():
                if not fname.endswith('/'):
                    data = zfile.read(fname)
                    fname = userdir+'/'+fname
                    fname = fname.replace('\\','/')
                    fout = open(fname, "wb")
                    fout.write(data)
                    fout.close()
            zfile.close()
            os.remove(filename)

    def delete_file(self,filename):
        ''' delete file in working directory
            returns False if file was not found, otherwise returns True
        '''
        filepath = os.getcwd()+'/'+str(filename)
        if os.path.exists(filepath):
            if os.path.isdir(filepath):
                os.rmdir(filepath)
            else:
                os.remove(filepath)
            return True
        else:
            return False
                        
    def install_addon(self,url,distribution):
        print "Installing",distribution,"from",url
        easy_install.main( ["-U","-f",url,distribution] )
            
    def get_value(self,pathname):
        ''' get the value of the object with the given pathname
        '''
        try:
            val, root = self.get_container(pathname)            
            return val
        except Exception, err:
            print "error getting value:",err
