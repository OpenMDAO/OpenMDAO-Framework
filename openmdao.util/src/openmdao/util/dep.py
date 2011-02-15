"""
Routines analyzing dependencies (class and module) in python source
"""

import os
from os import makedirs
import sys
import shutil
import fnmatch
from os.path import islink, isdir, join
from os.path import normpath, dirname, exists, isfile, abspath
from token import NAME, OP
from tokenize import generate_tokens
import compiler

import networkx as nx

from openmdao.util.fileutil import exclude_files, get_module_path
from openmdao.main.api import Component as mycomp

def _to_str(arg):
    """Take arg of the form Getattr(Getattr(Name('foo'),'bar'),'blah')
    and convert it to 'foo.bar.blah'.
    """
    if isinstance(arg, compiler.ast.Getattr):
        return '.'.join([_to_str(arg.expr), arg.attrname])
    elif isinstance(arg, compiler.ast.Name):
        return arg.name
    elif isinstance(arg, compiler.ast.Const):
        return arg.value
    else:
        return arg
            
def _real_name(name, finfo):
    while True:
        parts = name.rsplit('.', 1)
        if len(parts) > 1:
            if parts[0] in finfo:
                trans = finfo[parts[0]].localnames.get(parts[1], parts[1])
                if trans == name:
                    return trans
                else:
                    name = trans
                    continue
        return name

class ClassInfo(object):
    def __init__(self, name, bases, decorators):
        self.bases = bases
        self.decorators = decorators
        
    def resolve_true_basenames(self, finfo):
        """Take module pathnames of base classes that may be an indirect names and
        convert them to their true absolute module pathname.  For example,
        'from openmdao.main.api import Component' implies the module
        path of Component is openmdao.main.api.Component, but inside
        of openmdao.main.api is the import statement 
        'from openmdao.main.component import Component', so the true
        module pathname of Component is openmdao.main.component.Component.
        
        finfo: list
        """
        self.bases = [_real_name(b, finfo) for b in self.bases]
        

class PythonSourceFileAnalyser(compiler.visitor.ASTVisitor):
    """Collects info about imports and class inheritance from a 
    python file.
    """
    def __init__(self, fname):
        compiler.visitor.ASTVisitor.__init__(self)
        self.fname = os.path.abspath(fname)
        self.modpath = get_module_path(fname)
        self.classes = {}
        self.localnames = {}  # map of local names to package names
        
    def translate(self, finfo):
        """Take module pathnames of classes that may be indirect names and
        convert them to their true absolute module pathname.  For example,
        'from openmdao.main.api import Component' implies the module
        path of Component is openmdao.main.api.Component, but inside
        of openmdao.main.api is the import statement 
        'from openmdao.main.component import Component', so the true
        module pathname of Component is openmdao.main.component.Component.
        """
        for classinfo in self.classes.values():
            classinfo.resolve_true_basenames(finfo)
                
    def visitClass(self, node):
        """This executes every time a class definition is parsed."""
        fullname = '.'.join([self.modpath, node.name])
        self.localnames[node.name] = fullname
        self.classes[fullname] = ClassInfo(fullname, node.bases, node.decorators)
        
        bases = [_to_str(b) for b in node.bases]
        self.classes[fullname] = ClassInfo(bases=[self.localnames.get(b, b) for b in bases])
        if node.decorators:
            for dec in node.decorators.nodes:
                if dec.node.name == 'entry_point':
                    args = [_to_str(a) for a in dec.args]
                    if len(args) == 1:
                        args.append(fullname)
                    self.classes[fullname].entry_points.append((args[0],args[1],args[1]))
        
    def visitImport(self, node):
        """This executes every time an 'import foo' style import statement 
        is parsed.
        """
        for name, alias in node.names:
            if alias is None:
                self.localnames[name] = name
            else:
                self.localnames[alias] = name

    def visitFrom(self, node):
        """This executes every time a 'from foo import bar' style import
        statement is parsed.
        """
        for name, alias in node.names:
            if alias is None:
                self.localnames[name] = '.'.join([node.modname, name])
            else:
                self.localnames[alias] = '.'.join([node.modname, name])
                

class PythonSourceTreeAnalyser(object):
    def __init__(self, startdir=None, excludes=None):
        # inheritance graph. It's a directed graph with base classes
        # pointing to the classes that inherit from them
        self.graph = nx.DiGraph()
        
        if isinstance(startdir, basestring):
            self.startdirs = [startdir]
        elif startdir is None:
            self.startdirs = []
        else:
            self.startdirs = startdir
            
        if excludes is None:
            self.excludes = []
        else:
            self.excludes = excludes
            
        self._analyze()
            
    def _analyze(self):
        """Gather import and class inheritance information from
        the source trees under the specified set of starting 
        directories.
        """
        fileinfo = {}
        
        # gather python files from the specified starting directories
        # and parse them, extracting class and import information
        for pyfile in exclude_files(self.excludes, "*.py", self.startdirs):
            myvisitor = PythonSourceFileAnalyser(pyfile)
            compiler.visitor.walk(compiler.parseFile(pyfile), myvisitor)
            fileinfo[get_module_path(pyfile)] = myvisitor
            
        # now translate any indirect imports into absolute module pathnames
        # NOTE: only indirect imports within the set of specified source
        #       trees will be fully resolved, i.e., if a file in your set
        #       of source trees imports
        #       openmdao.main.api but you don't include openmdao.main in your
        #       list of source trees, any imports within openmdao.main.api
        #       won't be included in the translation.  This means that
        #       openmdao.main.api.Component will not be translated to
        #       openmdao.main.component.Component like it should.
        for visitor in fileinfo.values():
            visitor.translate(fileinfo)
    
        # build the inheritance graph
        for visitor in fileinfo.values():
            for classname, classinfo in visitor.classes.items():
                for base in classinfo.bases:
                    self.graph.add_edge(classname, base)
    
        # flip orientation of inheritance graph so we can find all classes
        # that inherit from a particular base more easily
        self.graph = self.graph.reverse(copy=False)
        
    def find_inheritors(self, base):
        paths = nx.shortest_path(self.graph, source=base, target=None)
        del paths[base] # don't want the base itself in the list
        return paths.keys()
