"""
Routines for doing plugin related things
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

from openmdao.util.fileutil import find_files
from openmdao.main.api import Component as mycomp

def _to_str(arg):
    if isinstance(arg, compiler.ast.Getattr):
        return '.'.join([_to_str(arg.expr), arg.attrname])
    elif isinstance(arg, compiler.ast.Name):
        return arg.name
    else:
        return arg
                
def _real_name(name, finfo):
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

class MyVisitor(compiler.visitor.ASTVisitor):
    """Collects info about imports and class inheritance from an
    AST.
    """
    def __init__(self, fname):
        compiler.visitor.ASTVisitor.__init__(self)
        self.fname = fname
        self.classes = {}
        self.impnames = {}  # map of local names to package names
        
    def translate(self, finfo):
        for klass, bases in self.classes.items():
            self.classes[klass] = [_real_name(b, finfo) for b in bases]
                
    def visitClass(self, node):
        bases = [_to_str(b) for b in node.bases]
        self.classes[node.name] = [self.impnames.get(b, b) for b in bases]
        self.impnames[node.name] = node.name
        
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
                
def get_module_path(fpath):
    """Given a module filename, return its full python name including
    enclosing packages. (based on existence of __init__.py files)
    """
    pnames = [os.path.basename(fpath)[:-3]]
    path = os.path.dirname(os.path.abspath(fpath))
    while os.path.isfile(os.path.join(path, '__init__.py')):
            path, pname = os.path.split(path)
            pnames.append(pname)
    return '.'.join(pnames[::-1])
   
def update_inheritance(graph, class_list):
    for klass, bases in class_list:
        for base in bases:
            graph.add_edge(klass, base)
            
            
def fnmatches(fname, excludes):
    for pat in excludes:
        if fnmatch.fnmatch(fname, pat):
            return True
    return False
    
if __name__ == '__main__':
    
    fileinfo = {}
    excludes = [os.path.join('*','test','*')]
    for pyfile in find_files("*.py", sys.argv[1]):
        if fnmatches(pyfile, excludes):
            continue
        myvisitor = MyVisitor(pyfile)
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
    for edge in graph.in_edges('openmdao.main.api.Component'):
        print edge

