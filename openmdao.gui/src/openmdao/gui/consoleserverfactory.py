import logging
import os, os.path, sys
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

from openmdao.lib.releaseinfo import __version__, __date__

from openmdao.main.project import *

from openmdao.main.mp_support import has_interface, is_instance
from openmdao.main.interfaces import *

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
        self._locals  = {}      ## Initialize execution namespace for user
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
        self.top = None
        
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
            print 'The command returned an error: %s\n' % str(err)

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
                exec(line) in self._locals, self._globals
            except Exception, e:
                print str(e.__class__.__name__), ":", e
        else:
            try:
                result = eval(line, self._globals, self._locals)
                if result is not None:
                    print result
            except Exception, e:
                print str(e.__class__.__name__), ":", e

    def run(self):
        ''' run the model (i.e. the top assembly)
        '''
        if self.top:
            print "Executing..."
            try:
                self.top.run()
                print "Execution complete."
            except Exception, err:
                print "** Execution failed:"
                print str(err)                
        else:
            print "Execution failed: No top level assembly was found."
        
    def execfile(self, file):
        ''' execfile in server's globals. 
        '''
        # set name so any "if __name__ == __main__" code will be executed
        self._globals['__name__'] = '__main__'
        execfile(file,self._globals)

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
        
    def _get_components(self,cont):
        comps = {}
        for n,v in cont.items():
            if isinstance(v, HasTraits):
                if isinstance(v,Component):
                    comps[n] = self._get_components(v)
                else:
                    comps[n] = v
        return comps
        
    def get_components(self):
        ''' get hierarchical dictionary of openmdao objects 
        '''
        comps = {}
        if self.top:
            comps = self._get_components(self.top)
        return jsonpickle.encode(comps)

    def _get_dataflow(self,asm):
        ''' get the list of components and connections between them
            that make up the workflow for the top level driver 
        '''
        components = []
        connections = []
        if isinstance(asm,Assembly):
            # list of components (name & type) in the workflow
            # TODO:  want to get the order using iter()
            for comp in asm.driver.iteration_set():
                if isinstance(comp,Assembly):
                    components.append({ 'name': comp.name,
                                        'pathname': comp.get_pathname(),
                                        'type': type(comp).__name__,
                                        'workflow': self._get_dataflow(comp) })
                else:
                    components.append({ 'name': comp.name,
                                        'pathname': comp.get_pathname(),
                                        'type': type(comp).__name__ })
            # list of connections (convert tuples to lists)
            conntuples = asm.list_connections()
            for connection in conntuples:
                connections.append(list(connection))
        return { 'components': components, 'connections': connections }

    def get_dataflow(self):
        if self.top:
            return self._get_workflow(self.top)
        else:
            return {}
            
    def _get_workflow(self,drvr):
        ''' get the driver info and the list of components that make up the
            driver's workflow, recurse on nested drivers
        '''
        ret = {}
        ret['pathname'] = drvr.get_pathname()
        ret['type'] = type(drvr).__module__+'.'+type(drvr).__name__ 
        ret['workflow'] = []
        for comp in drvr.iteration_set(recurse=False):
            if isinstance(comp,Assembly) and comp.driver:
                ret['workflow'].append({ 
                    'pathname': comp.get_pathname(),
                    'type':     type(comp).__module__+'.'+type(comp).__name__,
                    'driver':   self._get_workflow(comp.driver)
                })
            elif isinstance(comp,Driver):
                ret['workflow'].append(self._get_workflow(comp))            
            else:
                ret['workflow'].append({ 
                    'pathname': comp.get_pathname(),
                    'type':     type(comp).__module__+'.'+type(comp).__name__,
                })
        return ret

    def get_workflow(self):
        if self.top:
            try:
                return jsonpickle.encode(self._get_workflow(self.top.driver))
            except Exception, err:
                print "Error getting workflow:", str(err)
        else:
            return {}

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
                outputs.append(attr)
            attrs['Outputs'] = outputs

        if has_interface(comp,IDriver):
            attrs['Workflow'] = comp.workflow.get_names()
        
        if has_interface(comp,IHasCouplingVars):
            couples = []
            objs = comp.list_coupling_vars()
            for indep,dep in objs:
                attr = {}
                print " ===> indep",indep,"dep:",dep
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
                attr['expr']    = con.__str__
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
                attr['expr']    = con.__str__
                attr['scaler']  = con.scaler
                attr['adder']   = con.adder
                constraints.append(attr)
            attrs['IneqConstraints'] = constraints
            
        return attrs
        
    def get_attributes(self,name):
        try:
            attr = {}
            comp = self.top.get(name)
            if self.top and comp:
                attr = self._get_attributes(comp)
                attr['type'] = type(comp).__name__
            return jsonpickle.encode(attr)
        except Exception, err:
            print "Error getting attributes of",name,":", str(err)
    
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
                    if isinstance(obj, HasTraits):
                        types.append( ( k , 'n/a') )
        except Exception, err:
            print "Error getting working types:", str(err)
        return packagedict(types)

    def load_project(self,filename):
        print 'loading project from:',filename
        self.projfile = filename
        self.proj = project_from_archive(filename,dest_dir=self.getcwd())
        self.proj.activate()
        self.top = self.proj.top
        self._globals['top'] = self.top
        
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
                print "Save failed:", str(err)                
        else:
            print 'No Project to save'

    def set_top(self,name):
        print 'setting top to:',name
        self._globals['top'] = self.top.get(name)
        self.top = self._globals['top']
        self.proj.top = self.top
        set_as_top(self.top)

    def add_component(self,name,classname):
        ''' add a new component of the given type to the top assembly. 
        '''
        try:
            if classname in self._globals:
                self.top.add(name,self._globals[classname]())
            else:
                self.top.add(name,create(classname))
        except Exception, err:
            print "Add component failed:", str(err)
            
    def create(self,typname,name):
        ''' create a new object of the given type. 
        '''
        try:
            if (typname.find('.') < 0):
                self.default(name+'='+typname+'()')
            else:
                self._globals[name]=create(typname)
        except Exception, err:
            print "create failed:", str(err)
            
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
            except Exception, e:
                print "failed to rmtree ",self.root_dir
                print e
        
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
            
