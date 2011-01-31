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

from openmdao.util.fileutil import exclude_files, get_module_path, find_module

class PythonSourceFileAnalyser(compiler.visitor.ASTVisitor):
    """Collects info about imports and class inheritance from a 
    python file.
    """
    def __init__(self, fname):
        compiler.visitor.ASTVisitor.__init__(self)
        self.fname = fname
        self.modpath = get_module_path(fname)
        self.classes = {}
        self.impnames = {}  # map of local names to package names
        self.imported_files = set()
        
    def _translate(self, finfo):
        """Take module pathnames of classes that may be indirect names and
        convert them to their true absolute module pathname.  For example,
        'from openmdao.main.api import Component' implies the module
        path of Component is openmdao.main.api.Component, but inside
        of openmdao.main.api is the import statement 
        'from openmdao.main.component import Component', so the true
        module pathname of Component is openmdao.main.component.Component.
        """
        for klass, bases in self.classes.items():
            self.classes[klass] = [self._real_name(b, finfo) for b in bases]
                
    def visitClass(self, node):
        fullname = '.'.join([self.modpath, node.name])
        bases = [self._to_str(b) for b in node.bases]
        self.classes[fullname] = [self.impnames.get(b, b) for b in bases]
        self.impnames[node.name] = fullname
        
    def visitImport(self, node):
        for name, alias in node.names:
            pathname = find_module(name)
            if pathname:
                self.imported_files.add(pathname)
            if alias is None:
                self.impnames[name] = name
            else:
                self.impnames[alias] = name

    def visitFrom(self, node):
        for name, alias in node.names:
            modpath = '.'.join([node.modname, name])
            pathname = find_module(modpath)
            if pathname is None:
                pathname = find_module(node.modname)
            if pathname:
                self.imported_files.add(pathname)
                
            if alias is None:
                self.impnames[name] = modpath
            else:
                self.impnames[alias] = modpath
                
    def _to_str(self, arg):
        """Take arg of the form Getattr(Getattr(Name('foo'),'bar'),'blah')
        and convert it to a module path name.
        """
        if isinstance(arg, compiler.ast.Getattr):
            return '.'.join([self._to_str(arg.expr), arg.attrname])
        elif isinstance(arg, compiler.ast.Name):
            return arg.name
        else:
            return arg
                
    def _real_name(self, name, finfo):
        if not isinstance(name, basestring):
            return name
        while True:
            parts = name.rsplit('.', 1)
            if len(parts) > 1:
                if parts[0] in finfo:
                    trans = finfo[parts[0]].impnames.get(parts[1], parts[1])
                    if trans == name:
                        return trans
                    else:
                        name = trans
                        continue
            return name

class PythonSourceTreeAnalyser(object):
    def __init__(self, startdir=None, excludes=None):
        # inheritance graph. It's a directed graph with base classes
        # pointing to the classes that inherit from them
        self.graph = nx.DiGraph()
        
        self.fileinfo = {}
        
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
        fileinfo = self.fileinfo
        
        # gather python files from the specified starting directories
        # and parse them, extracting class and import information
        lst = list(exclude_files(self.excludes, "*.py", self.startdirs))
        visited = set()
        while lst:
            pyfile = lst.pop()
            if pyfile not in visited:
                visited.add(pyfile)
                myvisitor = PythonSourceFileAnalyser(pyfile)
                compiler.visitor.walk(compiler.parseFile(pyfile), myvisitor)
                fileinfo[get_module_path(pyfile)] = myvisitor
                lst.extend(myvisitor.imported_files)
            
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
            visitor._translate(fileinfo)

        # build the inheritance graph
        for visitor in fileinfo.values():
            for klass, bases in visitor.classes.items():
                for base in bases:
                    self.graph.add_edge(klass, base)
    
        # flip orientation of inheritance graph so we can find all classes
        # that inherit from a particular base more easily
        self.graph = self.graph.reverse(copy=False)
        
    def find_inheritors(self, base):
        if base not in self.graph:
            return []
        paths = nx.shortest_path(self.graph, source=base, target=None)
        del paths[base] # don't want the base itself in the list
        return paths.keys()
