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
                

class MyVisitor(compiler.visitor.ASTVisitor):
    def __init__(self, *args, **kwargs):
        compiler.visitor.ASTVisitor.__init__(self, *args, **kwargs)
        self.classes = []
        self.impnames = {}  # map of local names to package names
        
    def visitClass(self, node):
        name = self.impnames.get(node.name, node.name)
        bases = [self.impnames.get(b,_to_str(b)) for b in node.bases]
        self.classes.append((name, bases))
        
    def visitImport(self, node):
        for name, alias in node.names:
            if alias is None:
                self.impnames[name] = name
            else:
                self.impnames[alias] = name

    def visitFrom(self, node):
        for name,alias in node.names:
            if alias is None:
                self.impnames[name] = '.'.join([node.modname, name])
            else:
                self.impnames[alias] = '.'.join([node.modname, name])
                
            
    def dump(self):
        print 'classes = %s' % self.classes
        print 'impnames = %s' % self.impnames.values()


def _parse_class(iterator):
    """after encountering 'class' during a parse, this fwkdjunction
    parses the rest of the inheritance specification up to the ':'
    """
    tok, s, _, _, _ = iterator.next()
    classinfo = [s]   # class name
    args = []
    pname = []
    while True:
        tok, s, _, _, _ = iterator.next()
        if tok == NAME:
            pname.append(s)
        elif s == ',' or s == ')':
            args.append('.'.join(pname))
            pname = []
        elif s == ':':
            if pname:
                args.append('.'.join(pname))
            break
    return classinfo+args
    
                
def get_class_decls(pyfile):
    """Gather class declaration info from the given python file
    and return it as a list of lists, where each list entry is:
    [class_name, baseclass1_name, baseclass2_name, ... ]
    """
    classes = []
    with open(pyfile, 'r') as f:
        inclass = False
        iterator = generate_tokens(f.readline)
        while True:
            try:
                tok, s, _, _, _ = iterator.next()
                if tok == NAME and s == 'class':
                    classes.append(_parse_class(iterator))
            except StopIteration:
                break
    return classes
            
def get_package_name(fpath):
    """Given a filename, return the name of the python package that
    contains it (based on existence of __init__.py files)
    """
    pnames = []
    path = os.path.dirname(os.path.abspath(fpath))
    while os.path.isfile(os.path.join(path, '__init__.py')):
            path, pname = os.path.split(path)
            pnames.append(pname)
    return '.'.join(pnames[::-1])
   
def add_inheritance(graph, class_list):
    for entry in class_list:
        for klass in entry[1:]:
            graph.add_edge(entry[0], klass)
            
if __name__ == '__main__':
    
    #get_class_decls(__file__)
    #print 'pname = ',get_package_name(__file__)
    #graph = nx.DiGraph()
    #for pyfile in find_files("*.py", sys.argv[1]):
        #myvisitor = MyVisitor()
        #ast = compiler.parseFile(pyfile)
        #compiler.visitor.walk(ast, myvisitor)
        #add_inheritance(graph, get_class_decls(pyfile))

    myvisitor = MyVisitor()
    ast = compiler.parseFile(__file__)
    compiler.visitor.walk(ast, myvisitor)
    myvisitor.dump()

