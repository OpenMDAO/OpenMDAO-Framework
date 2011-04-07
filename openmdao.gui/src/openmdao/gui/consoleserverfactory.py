import logging
import os.path
import sys
import platform
import shutil
import cmd
import jsonpickle

from cStringIO import StringIO

from openmdao.main.factory import Factory
from multiprocessing.managers import BaseManager

from openmdao.main.factorymanager import create

class ConsoleServerFactory(Factory):
    """
    An :class:`ConsoleServerFactory` creates :class:`ConsoleServers` 
    """

    def __init__(self):
        super(ConsoleServerFactory, self).__init__()

    def create(self, name, **ctor_args):
        """ Create an :class:`ConsoleServer` and return a proxy for it. """
        manager = BaseManager()
        manager.register('ConsoleServer', ConsoleServer)
        manager.start()
        return manager.ConsoleServer()
    
class ConsoleServer(cmd.Cmd):
    """
    Object which knows how to load a model.
    Executes in a subdirectory of the startup directory.
    All remote file accesses must be within the tree rooted there.
    """

    def __init__(self, name='', host=''):
        cmd.Cmd.__init__(self)

        #intercept stdout & stderr
        self.cout = StringIO()
        sys.stdout = self.cout
        sys.stderr = self.cout

        self.prompt = "OpenMDAO>> "
        self.intro  = "Welcome to OpenMDAO!"
        
        self._hist    = []      ## No history yet
        self._locals  = {}      ## Initialize execution namespace for user
        self._globals = {}

        self.host = host
        self.pid = os.getpid()
        self.name = name or ('console-%d' % self.pid)
        self.orig_dir = os.getcwd()
        self.root_dir = os.path.join(self.orig_dir, self.name)
        if os.path.exists(self.root_dir):
            logging.warning('%s: Removing existing directory %s',
                            self.name, self.root_dir)
            shutil.rmtree(self.root_dir)
        os.mkdir(self.root_dir)
        os.chdir(self.root_dir)

    def getcwd(self):
        return os.getcwd()

    def chdir(self, dirname):
        if not os.path.isdir(dirname):
            os.mkdir(dirname)
        os.chdir(dirname)
        sys.path[0] = dirname

    def precmd(self, line):
        """ This method is called after the line has been input but before
            it has been interpreted. If you want to modifdy the input line
            before execution (for example, variable substitution) do it here.
        """
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
        """Called on an input line when the command prefix is not recognized.
           In that case we execute the line as Python code.
        """
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

    def execfile(self, file):       
        """ execfile in server's globals. """        
        # set name so any "if __name__ == __main__" code will be executed
        self._globals['__name__'] = '__main__'
        execfile(file,self._globals)

    def get_output(self):
        output = self.cout.getvalue()     
        self.cout.truncate(0)
        return output
        
    def get_pid(self):
        """ Return this server's :attr:`pid`. """
        return self.pid

    def get_history(self):
        """ Return this server's :attr:`_hist`. """
        return self._hist

    def get_JSON(self):
        return jsonpickle.encode(self._globals)
        
    def get_objects(self):  # TODO:
        """ get hierarchy of openmdao objects (per get_avaialable_types())
            and their I/O traits (list_inputs() & list_outputs()). """
        return jsonpickle.encode(self._globals)
        
    def get_workingtypes(self):
        """ Return this server's user defined types. """
        types = []
        g = self._globals.items()
        for k,v in g:
            if str(v).startswith('<class'):
                types.append(k)
        return types

    def create(self,typname,name):
        """ create a new object of the given type. """
        try:
            if (typname.find('.') < 0):
                self.default(name+'='+typname+'()')
            else:
                self._globals[name]=create(typname)
        except Exception, err:
            print "create failed:", str(err)
            
        return self._globals

    def cleanup(self):
        """ Cleanup this server's directory. """
        logging.shutdown()
        os.chdir(self.orig_dir)
        if os.path.exists(self.root_dir):
            try:
                print "trying to rmtree ",self.root_dir
                shutil.rmtree(self.root_dir)
            except e:
                print "failed to rmtree ",self.root_dir
                print e
        
