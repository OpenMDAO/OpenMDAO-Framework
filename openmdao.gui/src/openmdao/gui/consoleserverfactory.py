import logging
import os, os.path, sys, traceback
import platform
import shutil
import cmd
import jsonpickle
import tempfile
import zipfile

from setuptools.command import easy_install

from cStringIO import StringIO

from enthought.traits.api import HasTraits
from openmdao.main.variable import Variable

from multiprocessing.managers import BaseManager
from openmdao.main.factory import Factory
from openmdao.main.factorymanager import create
from openmdao.main.component import Component
from openmdao.main.driver import Driver
from openmdao.main.factorymanager import get_available_types
from openmdao.main.datatypes.slot import Slot

from openmdao.lib.releaseinfo import __version__, __date__

from openmdao.main.project import *

from openmdao.main.mp_support import has_interface, is_instance
from openmdao.main.interfaces import *
from zope.interface import implementedBy

import networkx as nx

from mdao_util import *

class ConsoleServerFactory(Factory):
    ''' creates and keeps track of :class:`ConsoleServer`
    '''

    def __init__(self):
        super(ConsoleServerFactory, self).__init__()
        self.cserver_dict = {}
        self.temp_files = {}

    def __del__(self):
        ''' make sure we clean up on exit
        '''
        #self.cleanup()

    def create(self, name, **ctor_args):
        ''' Create a :class:`ConsoleServer` and return a proxy for it. 
        '''
        manager = BaseManager()
        manager.register('ConsoleServer', ConsoleServer)
        manager.start()
        return manager.ConsoleServer()
        
    def console_server(self,server_id):
        ''' create a new :class:`ConsoleServer` associated with an id
        '''
        if not self.cserver_dict.has_key(server_id):
            cserver = self.create('mdao-'+server_id)
            self.cserver_dict[server_id] = cserver;
        else:
            cserver = self.cserver_dict[server_id]
        return cserver
        
    def delete_server(self,server_id):
        ''' delete the :class:`ConsoleServer` associated with an id
        '''
        if self.cserver_dict.has_key(server_id):
            cserver = self.cserver_dict[server_id]
            del self.cserver_dict[server_id]
            cserver.cleanup()
            del cserver
        
    def get_tempdir(self,name):
        ''' create a temporary directory prefixed with the given name
        '''
        if not name in self.temp_files:
            self.temp_files[name] = tempfile.mkdtemp(prefix='mdao-'+name+'.')
        return self.temp_files[name]
        
    def cleanup(self):        
        ''' clean up temporary files, etc
        '''
        for server_id, cserver in self.cserver_dict.items():
            del self.cserver_dict[server_id]
            cserver.cleanup()
            del cserver            
        for name in self.temp_files:
            f = self.temp_files[name]
            if os.path.exists(f):
                rmtree(f)
                
class ConsoleServer(cmd.Cmd):
    ''' Object which knows how to load a model.
    Executes in a subdirectory of the startup directory.
    All remote file accesses must be within the tree rooted there.
    '''

    def __init__(self, name='', host=''):
        cmd.Cmd.__init__(self)

        print '<<<'+str(os.getpid())+'>>> ConsoleServer ..............'
        
        #intercept stdout & stderr
        self.sysout = sys.stdout
        self.syserr = sys.stderr
        self.cout = StringIO()
        sys.stdout = self.cout
        sys.stderr = self.cout

        self.intro  = 'OpenMDAO '+__version__+' ('+__date__+')'
        self.prompt = 'OpenMDAO>> '
        
        self._hist    = []      ## No history yet
        self._globals = {}

        self.host = host
        self.pid = os.getpid()
        self.name = name or ('-cserver-%d' % self.pid)
        self.orig_dir = os.getcwd()
        self.root_dir = tempfile.mkdtemp(self.name)
        if os.path.exists(self.root_dir):
            logging.warning('%s: Removing existing directory %s',
                            self.name, self.root_dir)
            shutil.rmtree(self.root_dir)
        os.mkdir(self.root_dir)
        os.chdir(self.root_dir)
        
        print 'root_dir=',self.root_dir
        
        self.projfile = ''
        self.proj = None
        self.exc_info = None

    def error(self,err,exc_info):
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

    def onecmd(self, line):
        self._hist += [ line.strip() ]
        # Override the onecmd() method so we can trap error returns
        try:
            cmd.Cmd.onecmd(self, line)
        except Exception, err:
            self.error(err,sys.exc_info())

    def emptyline(self):
        # Default for empty line is to repeat last command - yuck
        pass

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
                exec(line) in self._globals
            except Exception, err:
                self.error(err,sys.exc_info())
        else:
            try:
                result = eval(line, self._globals)
                if result is not None:
                    print result
            except Exception, err:
                self.error(err,sys.exc_info())

    def set_top(self,pathname):
        print 'setting top to:',pathname
        cont = self.get_container(pathname)
        if cont:
            self._globals['top'] = cont
            cont.name = 'top'
            set_as_top(cont)
        else:
            print pathname,'not found.'

    def run(self, *args, **kwargs):
        ''' run the model (i.e. the top assembly)
        '''
        top = self._globals['top']
        if top:
            print "Executing..."
            try:
                top.run(*args, **kwargs)
                print "Execution complete."
            except Exception, err:
                self.error(err,sys.exc_info())
        else:
            print "Execution failed: No top level assembly was found."
        
    def execfile(self, filename):
        ''' execfile in server's globals. 
        '''
        # set name so any "if __name__ == __main__" code will be executed
        self._globals['__name__'] = '__main__'
        print "Executing",filename,"..."
        try:
            with open(filename) as file:
                exec(compile(file.read(), filename, 'exec'), self._globals)
            print "Execution complete."
        except Exception, err:
            self.error(err,sys.exc_info())

    def get_output(self):
        ''' get any pending output and clear the outputput buffer
        '''
        output = self.cout.getvalue()     
        self.cout.truncate(0)
        return output
        
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
        return jsonpickle.encode(self._globals)

    def get_container(self,pathname):
        ''' get the conatainer with the specified pathname
        '''
        cont = None
        # for backwards compatibility
        if len(pathname) == 0:
            pathname = 'top'
        root = pathname.split('.')[0]
        if root in self._globals:
            if len(root) == len(pathname):
                cont = self._globals[root]
            else:
                rest = pathname[len(root)+1:]
                cont = self._globals[root].get(rest)
        return cont
        
    def _get_components(self,cont):
        comps = []
        for n,v in cont.items():
            if is_instance(v,Component):
                comp = {}
                if cont == self._globals:
                    comp['pathname'] = n
                else:
                    comp['pathname'] = v.get_pathname()
                comp['type'] = str(v.__class__.__name__)
                inames = []
                for klass in list(implementedBy(v.__class__)):
                    inames.append(klass.__name__)
                comp['interfaces'] = inames
                children = self._get_components(v)
                if len(children) > 0:
                    comp['children'] = children
                comps.append(comp)
        return comps
        
    def get_components(self):
        ''' get hierarchical dictionary of openmdao objects 
        '''
        comps = self._get_components(self._globals)
        return jsonpickle.encode(comps)

    def get_connections(self,pathname,src_name,dst_name):
        ''' get list of src outputs, dst inputs and connections between them
        '''
        conns = {}
        asm = self.get_container(pathname)
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
                self.error(err,sys.exc_info())
        return jsonpickle.encode(conns)

    def set_connections(self,pathname,src_name,dst_name,connections):
        ''' set connections between src and dst components in the given assembly
        '''
        asm = self.get_container(pathname)
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
                self.error(err,sys.exc_info())

    def _get_structure(self,asm):
        ''' get the list of components and connections between them
            that make up the data flow for the given assembly 
        '''
        components = []
        connections = []
        if is_instance(asm,Assembly):
            # list of components (name & type) in the assembly
            g = asm._depgraph._graph
            for name in nx.algorithms.dag.topological_sort(g):
                if not name.startswith('@'):
                    comp = asm.get(name)
                    if is_instance(comp,Component):
                        components.append({ 'name': comp.name,
                                            'pathname': comp.get_pathname(),
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
        if len(pathname) > 0:
            try:
                asm = self.get_container(pathname)
                structure = self._get_structure(asm)
            except Exception, err:
                self.error(err,sys.exc_info())
        else:
            components = []
            g = self._globals.items()
            for k,v in g:
                if is_instance(v,Component):
                    components.append({ 'name': k,
                                        'pathname': v.get_pathname(),
                                        'type': type(v).__name__ })
            structure['components'] = components
            structure['connections'] = []
        return jsonpickle.encode(structure)

    def _get_workflow(self,drvr):
        ''' get the driver info and the list of components that make up the
            driver's workflow, recurse on nested drivers
        '''
        ret = {}
        ret['pathname'] = drvr.get_pathname()
        ret['type'] = type(drvr).__module__+'.'+type(drvr).__name__ 
        ret['workflow'] = []
        for comp in drvr.workflow:
            if is_instance(comp,Assembly) and comp.driver:
                ret['workflow'].append({ 
                    'pathname': comp.get_pathname(),
                    'type':     type(comp).__module__+'.'+type(comp).__name__,
                    'driver':   self._get_workflow(comp.driver)
                })
            elif is_instance(comp,Driver):
                ret['workflow'].append(self._get_workflow(comp))            
            else:
                ret['workflow'].append({ 
                    'pathname': comp.get_pathname(),
                    'type':     type(comp).__module__+'.'+type(comp).__name__,
                })
        return ret

    def get_workflow(self,pathname):
        flow = {}
        drvr = self.get_container(pathname)
        # allow for request on the parent assembly
        if is_instance(drvr,Assembly):
            drvr = drvr.get('driver')
        if drvr:
            try:
                flow = self._get_workflow(drvr)
            except Exception, err:
                self.error(err,sys.exc_info())
        return jsonpickle.encode(flow)

    def _get_attributes(self,comp):
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
                    attr['value'] = v
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
                    attr['value'] = v
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
            attrs['Structure'] = self._get_structure(comp)
        
        if has_interface(comp,IDriver):
            attrs['Workflow'] = self._get_workflow(comp)
        
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
        comp = self.get_container(pathname)
        if comp:
            try:
                attr = self._get_attributes(comp)
                attr['type'] = type(comp).__name__
            except Exception, err:
                self.error(err,sys.exc_info())
        return jsonpickle.encode(attr)
    
    def get_available_types(self):
        return packagedict(get_available_types())
        
    def get_workingtypes(self):
        ''' Return this server's user defined types. 
        '''
        types = []
        try:
            g = self._globals.items()
            for k,v in g:
                if (type(v).__name__ == 'classobj') or str(v).startswith('<class'):
                    obj = self._globals[k]()
                    if is_instance(obj, HasTraits):
                        types.append( ( k , 'n/a') )
        except Exception, err:
            self.error(err,sys.exc_info())
        return packagedict(types)

    def load_project(self,filename):
        print 'loading project from:',filename
        self.projfile = filename
        self.proj = project_from_archive(filename,dest_dir=self.getcwd())
        self.proj.activate()        
        self.proj.top.name = 'top'
        set_as_top(self.proj.top)
        self._globals['top'] = self.proj.top
        
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
                self.error(err,sys.exc_info())
        else:
            print 'No Project to save'

    def add_component(self,name,classname,parentname):
        ''' add a new component of the given type to the specified parent. 
        '''
        if (parentname and len(parentname)>0):
            parent = self.get_container(parentname)
            if parent:
                try:
                    if classname in self._globals:
                        parent.add(name,self._globals[classname]())
                    else:
                        parent.add(name,create(classname))
                except Exception, err:
                    self.error(err,sys.exc_info())
            else:
                print "Error adding component: parent",parentname,"not found."
        else:
            self.create(classname,name)
            
    def create(self,typname,name):
        ''' create a new object of the given type. 
        '''
        try:
            if (typname.find('.') < 0):
                self.default(name+'='+typname+'()')
            else:
                self._globals[name]=create(typname)
        except Exception, err:
            self.error(err,sys.exc_info())
            
        return self._globals

    def cleanup(self):
        ''' Cleanup this server's directory. 
        '''
        self.stdout = self.sysout
        self.stderr = self.syserr
        logging.shutdown()
        os.chdir(self.orig_dir)
        if os.path.exists(self.root_dir):
            try:
                print "trying to rmtree ",self.root_dir
                shutil.rmtree(self.root_dir)
            except Exception, err:
                self.error(err,sys.exc_info())
        
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
            contents=open(filepath, 'r').read()
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
        filepath = os.getcwd()+'/'+str(filename)
        fout = open(filepath,'wb')
        fout.write(contents)
        fout.close()

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
            
