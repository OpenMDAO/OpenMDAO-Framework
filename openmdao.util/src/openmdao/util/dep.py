"""
Routines analyzing dependencies (class and module) in Python source.
"""

import os
from os import makedirs
import sys
import shutil
import fnmatch
from os.path import islink, isdir, join
from os.path import normpath, dirname, exists, isfile, abspath
from token import NAME, OP
import ast
import imp

import networkx as nx

from openmdao.util.fileutil import find_files, get_module_path

# This is a dict containing all of the entry point groups that OpenMDAO uses to
# identify plugins, and their corresponding Interface.
plugin_groups = { 'openmdao.container': 'IContainer',
                  'openmdao.component': 'IComponent', 
                  'openmdao.driver': 'IDriver', 
                  'openmdao.variable': 'IVariable', 
                  'openmdao.surrogatemodel': 'ISurrogate',
                  'openmdao.doegenerator': 'IDOEgenerator', 
                  'openmdao.caseiterator': 'ICaseIterator', 
                  'openmdao.caserecorder': 'ICaseRecorder', 
                  'openmdao.architecture': 'IArchitecture', 
                  'openmdao.optproblem': 'IOptProblem', 
                  'openmdao.differentiator': 'IDifferentiator',
                  }

class StrVisitor(ast.NodeVisitor):
    def __init__(self):
        ast.NodeVisitor.__init__(self)
        self.parts = []

    def visit_Name(self, node):
        self.parts.append(node.id)
        
    def visit_Str(self, node):
        self.parts.append(node.s)
        
    def get_value(self):
        return '.'.join(self.parts)

def _to_str(arg):
    """Take groups of Name nodes or a Str node and convert to a string."""
    myvisitor = StrVisitor()
    for node in ast.walk(arg):
        myvisitor.visit(node)
    return myvisitor.get_value()


def _real_name(name, finfo):
    """Given the name of an object, return the full name (including package)
    from where it is actually defined.
    """
    while True:
        parts = name.rsplit('.', 1)
        if len(parts) > 1 and parts[0] in finfo:
            trans = finfo[parts[0]].localnames.get(parts[1], parts[1])
            if trans == name:
                return trans
            else:
                name = trans
                continue
        return name

class ClassInfo(object):
    def __init__(self, name, bases, decorators,  meta):
        self.name = name
        self.bases = bases
        self.decorators = decorators
        self.entry_points = []
        self.meta = meta
        
        for dec in decorators:
            if dec.func.id == 'entry_point':
                args = [_to_str(a) for a in dec.args]
                if len(args) == 1:
                    args.append(name)
                self.entry_points.append((args[0],args[1],name))
        
    def resolve_true_basenames(self, finfo):
        """Take module pathnames of base classes that may be an indirect names and
        convert them to their true absolute module pathname.  For example,
        "from openmdao.main.api import Component" implies the module
        path of Component is ``openmdao.main.api.Component``, but inside
        of ``openmdao.main.api`` is the import statement 
        "from openmdao.main.component import Component," so the true
        module pathname of Component is ``openmdao.main.component.Component``.
        
        finfo: list
        """
        self.bases = [_real_name(b, finfo) for b in self.bases]
        
            
class _ClassBodyVisitor(ast.NodeVisitor):
    def __init__(self):
        self.metadata = {}
        ast.NodeVisitor.__init__(self)
        
    def visit_ClassDef(self, node):
        for bnode in node.body:
            self.visit(bnode)
            
    def visit_Call(self, node):
        if isinstance(node.func, ast.Name) and node.func.id == 'implements':
            for arg in node.args:
                if isinstance(arg, ast.Name):
                    self.metadata.setdefault('ifaces',[]).append(arg.id)
    
    def visit_Assign(self, node):
        if len(self.metadata)==0 and len(node.targets) == 1:
            lhs = node.targets[0]
            if isinstance(lhs, ast.Name) and lhs.id=='__openmdao_meta__':
                self.metadata.update(ast.literal_eval(node.value))

class PythonSourceFileAnalyser(ast.NodeVisitor):
    """Collects info about imports and class inheritance from a 
    Python file.
    """
    def __init__(self, fname):
        ast.NodeVisitor.__init__(self)
        self.fname = os.path.abspath(fname)
        self.modpath = get_module_path(fname)
        self.classes = {}
        self.localnames = {}  # map of local names to package names
        
        # in order to get this to work with the 'ast' lib, I have
        # to read using universal newlines and append a newline
        # to the string I read for some files.  The 'compiler' lib
        # didn't have this problem. :(
        f = open(fname, 'Ur')
        try:
            for node in ast.walk(ast.parse(f.read()+'\n', fname)):
                myvisitor.visit(node)
        finally:
            f.close()
        
    def translate(self, finfo):
        """Take module pathnames of classes that may be indirect names and
        convert them to their true absolute module pathname.  For example,
        "from openmdao.main.api import Component" implies the module
        path of Component is ``openmdao.main.api.Component``, but inside
        of ``openmdao.main.api`` is the import statement 
        "from openmdao.main.component import Component," so the true
        module pathname of Component is ``openmdao.main.component.Component``.
        """
        for classinfo in self.classes.values():
            classinfo.resolve_true_basenames(finfo)
                            
    def visit_ClassDef(self, node):
        """This executes every time a class definition is parsed."""
        fullname = '.'.join([self.modpath, node.name])
        self.localnames[node.name] = fullname
        bases = [_to_str(b) for b in node.bases]

        bvisitor = _ClassBodyVisitor()
        bvisitor.visit(node)

        self.classes[fullname] = ClassInfo(fullname, 
                                           [self.localnames.get(b,b) for b in bases], 
                                           node.decorator_list, 
                                           bvisitor.metadata)
        
    def visit_Import(self, node):
        """This executes every time an "import foo" style import statement 
        is parsed.
        """
        for al in node.names:
            if al.asname is None:
                self.localnames[al.name] = al.name
            else:
                self.localnames[al.asname] = al.name

    def visit_ImportFrom(self, node):
        """This executes every time a "from foo import bar" style import
        statement is parsed.
        """
        for al in node.names:
            if al.asname is None:
                self.localnames[al.name] = '.'.join([node.module, al.name])
            else:
                self.localnames[al.asname] = '.'.join([node.module, al.name])
                

class PythonSourceTreeAnalyser(object):
    def __init__(self, startdir=None, exclude=None, startfiles=None):
        # inheritance graph. It's a directed graph with base classes
        # pointing to the classes that inherit from them
        self.graph = nx.DiGraph()
        
        if isinstance(startdir, basestring):
            self.startdirs = [startdir]
        elif startdir is None:
            self.startdirs = []
        else:
            self.startdirs = startdir
            
        if startfiles is None:
            startfiles = []
            
        self.startdirs = [os.path.expandvars(os.path.expanduser(d)) for d in self.startdirs]
            
        self.exclude = exclude
        self.ifaces = set()
        self.fileinfo = {}
            
        self._analyze(startfiles)
            
    def analyze_file(self, pyfile, first_pass=False):
        myvisitor = PythonSourceFileAnalyser(pyfile)
        self.fileinfo[get_module_path(pyfile)] = myvisitor
        
        if not first_pass:
            myvisitor.translate(self.fileinfo)
            self._update_graph(myvisitor)
            #myvisitor.update_impls(self.graph)
            
        return myvisitor
        
    def _analyze(self, startfiles):
        """Gather import and class inheritance information from
        the source trees under the specified set of starting 
        directories.
        """
        # if any startfiles were specified, parse them first
        for pyfile in startfiles:
            self.analyze_file(pyfile, first_pass=True)

        # gather python files from the specified starting directories
        # and parse them, extracting class and import information
        for pyfile in find_files(self.startdirs, "*.py", self.exclude):
            self.analyze_file(pyfile, first_pass=True)
        # now translate any indirect imports into absolute module pathnames
        # NOTE: only indirect imports within the set of specified source
        #       trees will be fully resolved, i.e., if a file in your set
        #       of source trees imports
        #       openmdao.main.api but you don't include openmdao.main in your
        #       list of source trees, any imports within openmdao.main.api
        #       won't be included in the translation.  This means that
        #       openmdao.main.api.Component will not be translated to
        #       openmdao.main.component.Component like it should.
        for visitor in self.fileinfo.values():
            visitor.translate(self.fileinfo)
    
        # build the inheritance/interface graph
        for visitor in self.fileinfo.values():
            self._update_graph(visitor)
            
        # now, update ifaces metadata for all of the classinfo objects in the graph
        ifaces = set(plugin_groups.values())
        for iface in ifaces:
            for inheritor in self.find_inheritors(iface):
                if inheritor not in ifaces:
                    self.graph.node[inheritor]['classinfo'].meta.setdefault('ifaces',[]).append(iface)
        
    def _update_graph(self, visitor):
        for classname, classinfo in visitor.classes.items():
            self.graph.add_node(classname, classinfo=classinfo)
            for base in classinfo.bases:
                self.graph.add_edge(base, classname)
            for impl in classinfo.impls:
                self.ifaces.add(impl)
                self.graph.add_edge(impl, classname)
    
    def remove_module(self, modname):
        mod = self.fileinfo[modname]
        
    def find_inheritors(self, base):
        """Returns a list of names of classes that inherit from the given base class."""
        try:
            paths = nx.shortest_path(self.graph, source=base, target=None)
        except KeyError:
            return []
        del paths[base] # don't want the base itself in the list
        return paths.keys()

def py_gen(dname):
    """generate a list of python files in the given directory and its subdirs, but only 
    if the subdirs are python package directories.
    """
    for path, dirlist, filelist in os.walk(dname):
        for name in filelist:
            if name.endswith('.py'):
                yield join(path, name)
        # when using os.walk, dirlist can be modified in place to prune the tree, so
        # use that to remove any subdirs that are not package dirs (i.e. that don't contain an __init__.py file)
        to_remove = []
        for d in dirlist:
            if not os.path.isfile(os.path.join(path,d,'__init__.py')):
                to_remove.append(d)
        for d in to_remove[::-1]:
            dirlist.remove(d)
    
def mod_gen(pathlist=None):
    """Generate tuples of the form (filepath, modpath) for all python modules under the given
    list of directories.  Files in subdirectories will be ignored if they are not contained in
    python package directories, i.e., if their directory doesn't contain an __init__.py file.
    """
    if pathlist is None:
        pathlist = sys.path

    for path in pathlist:
        for name in py_gen(path):
            dname, fname = os.path.split(name)
            if fname == '__init__.py':
                mpath = '.'.join(dname[len(path)+1:].split(os.sep))
                if mpath:
                    yield (name, mpath)
            else:
                yield (name, '.'.join(name[:-3][len(path)+1:].split(os.sep)))

