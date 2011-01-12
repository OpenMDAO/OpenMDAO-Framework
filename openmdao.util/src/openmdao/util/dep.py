"""
Routines analysing dependencies (class and module) in python source
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
        
    def translate(self, finfo):
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
            if alias is None:
                self.impnames[name] = name
            else:
                self.impnames[alias] = name

    def visitFrom(self, node):
        for name, alias in node.names:
            if alias is None:
                self.impnames[name] = '.'.join([node.modname, name])
            else:
                self.impnames[alias] = '.'.join([node.modname, name])
                
    def _to_str(self, arg):
        """Take arg of the form Getattr(Getattr(Name('foo'),'bar'),'blah')
        and convert it to a module path name.
        """
        if isinstance(arg, compiler.ast.Getattr):
            return '.'.join([_to_str(arg.expr), arg.attrname])
        elif isinstance(arg, compiler.ast.Name):
            return arg.name
        else:
            return arg
                
    def _real_name(self, name, finfo):
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

if __name__ == '__main__':
    
    fileinfo = {}
    excludes = [os.path.join('*','test','*')]
    for pyfile in exclude_files(excludes, 
                                "*.py", 
           ['/home/bret/dev/OpenMDAO/plugin_detection/openmdao.lib', 
            '/home/bret/dev/OpenMDAO/plugin_detection/openmdao.main']):
        myvisitor = PythonSourceFileAnalyser(pyfile)
        compiler.visitor.walk(compiler.parseFile(pyfile), myvisitor)
        fileinfo[get_module_path(pyfile)] = myvisitor
        
    # now translate any indirect imports
    for visitor in fileinfo.values():
        visitor.translate(fileinfo)

    # build the inheritance graph
    graph = nx.DiGraph()
    for visitor in fileinfo.values():
        for klass, bases in visitor.classes.items():
            for base in bases:
                graph.add_edge(klass, base)

    # find all inheritors from openmdao.main.api.Component
    for edge in graph.in_edges('openmdao.main.component.Component'):
        print edge

