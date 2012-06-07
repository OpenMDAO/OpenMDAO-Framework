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
import time

import networkx as nx

from openmdao.util.fileutil import find_files, get_module_path, find_module
from openmdao.util.log import logger

# This is a dict containing all of the entry point groups that OpenMDAO uses to
# identify plugins, and their corresponding Interfaces.
plugin_groups = { 'openmdao.container': ['IContainer'],
                  'openmdao.component': ['IComponent','IContainer'], 
                  'openmdao.driver': ['IDriver','IComponent','IContainer'], 
                  'openmdao.variable': ['IVariable'], 
                  'openmdao.surrogatemodel': ['ISurrogate'],
                  'openmdao.doegenerator': ['IDOEgenerator'], 
                  'openmdao.caseiterator': ['ICaseIterator'], 
                  'openmdao.caserecorder': ['ICaseRecorder'], 
                  'openmdao.architecture': ['IArchitecture'], 
                  'openmdao.optproblem': ['IOptProblem','IAssembly','IComponent','IContainer'], 
                  'openmdao.differentiator': ['IDifferentiator'],
                  }

iface_set = set()
for ifaces in plugin_groups.values():
    iface_set.update(ifaces)

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


class ClassInfo(object):
    def __init__(self, name, fname, bases, meta):
        self.name = name
        self.fname = fname
        self.bases = bases
        self.meta = meta
        
        
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
                dct = ast.literal_eval(node.value)
                dct.setdefault('ifaces', [])
                dct['ifaces'].extend(self.metadata.setdefault('ifaces',[]))
                self.metadata.update(dct)

class PythonSourceFileAnalyser(ast.NodeVisitor):
    """Collects info about imports and class inheritance from a 
    Python file.
    """
    def __init__(self, fname, tree_analyser):
        ast.NodeVisitor.__init__(self)
        self.fname = os.path.abspath(os.path.expanduser(fname))
        self.modpath = get_module_path(fname)
        self.classes = {}
        self.localnames = {}  # map of local names to package names
        self.starimports = []
        self.unresolved_classes = set()
        self.tree_analyser = tree_analyser
        
        # in order to get this to work with the 'ast' lib, I have
        # to read using universal newlines and append a newline
        # to the string I read for some files.  The 'compiler' lib
        # didn't have this problem. :(
        f = open(self.fname, 'Ur')
        try:
            contents = f.read()
            if len(contents)>0 and contents[-1] != '\n':
                contents += '\n'
            for node in ast.walk(ast.parse(contents, self.fname)):
                self.visit(node)
        finally:
            f.close()
        
        self.update_graph(self.tree_analyser.graph)
        self.update_ifaces(self.tree_analyser.graph)

        self.tree_analyser = None

    def visit_ClassDef(self, node):
        """This executes every time a class definition is parsed."""
        fullname = '.'.join([self.modpath, node.name])
        self.localnames[node.name] = fullname
        bases = [_to_str(b) for b in node.bases]
            
        bvisitor = _ClassBodyVisitor()
        bvisitor.visit(node)
        
        bases = [self.localnames.get(b,b) for b in bases]

        self.classes[fullname] = ClassInfo(fullname, self.fname, bases, bvisitor.metadata)
        self.tree_analyser.class_map[fullname] = self.classes[fullname]
        
        undef_bases = [b for b in bases if b not in self.classes and not b in __builtins__]
        while undef_bases:
            base = undef_bases.pop()
            cinfo = self.tree_analyser.find_classinfo(base)
            if cinfo is None:
                parts = base.rsplit('.', 1)
                if len(parts) == 1: # no dot, so maybe it came in with a '*' import
                    trymods = self.starimports[::-1]
                    basename = base
                else:
                    trymods = [parts[0]]
                    basename = parts[1]
                    
                for modname in trymods:
                    excluded = False
                    for m in self.tree_analyser.mod_excludes:
                        if modname.startswith(m):
                            excluded = True
                            break
                    if excluded:
                        continue
                    fpath = find_module(modname)
                    if fpath is not None:
                        fanalyzer = self.tree_analyser.analyze_file(fpath)
                        if '.' not in base:
                            trybase = '.'.join([modname, base])
                        else:
                            trybase = base
                        if trybase in fanalyzer.classes:
                            break
                        elif basename in fanalyzer.localnames:
                            newname = fanalyzer.localnames[basename]
                            self.tree_analyser.class_map[trybase] = newname
                            if newname not in self.tree_analyser.class_map:
                                undef_bases.append(newname)
                            break
                else:
                    logger.error("can't locate python source for class %s" % base)
                    self.unresolved_classes.add(base)

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
            if al.name == '*':
                self.starimports.append(node.module)
                continue
            if al.asname is None:
                self.localnames[al.name] = '.'.join([node.module, al.name])
            else:
                self.localnames[al.asname] = '.'.join([node.module, al.name])
                
    def update_graph(self, graph):
        """Update the inheritance/implements graph."""
        for classname, classinfo in self.classes.items():
            graph.add_node(classname, classinfo=classinfo)
            for base in classinfo.bases:
                cinfo = self.tree_analyser.find_classinfo(base)
                if cinfo:
                    base = cinfo.name
                graph.add_edge(base, classname)
            for iface in classinfo.meta.setdefault('ifaces', []):
                graph.add_edge(iface, classname)
    
    def update_ifaces(self, graph):
        """Update our ifaces metadata based on the contents of the
        inheritance/implements graph.
        """
        for iface in iface_set:
            try:
                paths = nx.shortest_path(graph, source=iface)
            except KeyError:
                continue
            for cname, cinfo in self.classes.items():
                if cname in paths:
                    cinfo.meta.setdefault('ifaces',[]).append(iface)

class PythonSourceTreeAnalyser(object):
    def __init__(self, startdir=None, exclude=None, mod_excludes=None):
        self.files_count = 0 # number of files analyzed
        
        # inheritance graph. It's a directed graph with base classes
        # pointing to the classes that inherit from them.  Also includes interfaces
        # pointing to classes that implement them.
        self.graph = nx.DiGraph()
        
        if isinstance(startdir, basestring):
            self.startdirs = [startdir]
        elif startdir is None:
            self.startdirs = []
        else:
            self.startdirs = startdir
            
        self.startdirs = [os.path.expandvars(os.path.expanduser(d)) for d in self.startdirs]
        
        self.exclude = exclude
        if mod_excludes is None:
            self.mod_excludes = set(['enthought.'])
        else:
            self.mod_excludes = mod_excludes
            
        self.modinfo = {}  # maps module pathnames to PythonSourceFileAnalyzers
        self.fileinfo = {} # maps filenames to (PythonSourceFileAnalyzer, modtime)
        self.class_map = {} # map of classname to ClassInfo for the class
        
        for pyfile in find_files(self.startdirs, "*.py", self.exclude):
            self.analyze_file(pyfile)

    def dump(self, out):
        """Dumps the contents of this object for debugging purposes."""
        if '-edges' in sys.argv:
            out.write("\ngraph edges:\n")
            for u,v in self.graph.edges():
                out.write("(%s, %s)\n" % (u,v))
            
        if '-classes' in sys.argv:
            out.write("\nclasses\n")
            for f, tup in self.fileinfo.items():
                out.write("%s\n" % os.path.relpath(f))
                for item,cinfo in tup[0].classes.items():
                    out.write("\t%s" % item)
                    if '-inherit' in sys.argv:
                        out.write(" --> %s\n" % cinfo.bases)
                    else:
                        out.write("\n")
                
        if '-unresolved' in sys.argv:
            out.write("\nunresolved classes\n")
            for f, tup in self.fileinfo.items():
                if tup[0].unresolved_classes:
                    out.write("%s\n" % os.path.relpath(f))
                    for item in tup[0].unresolved_classes:
                        out.write("\t%s\n" % item)
        
        if '-files' in sys.argv:
            out.write('\nfiles:\n')
            for f in self.fileinfo.keys():
                out.write("%s\n" % os.path.relpath(f))
                
        out.write("\n\nFiles examined: %d\n\n" % self.files_count)
            
    def find_classinfo(self, cname):
        cinfo = cname
        while True:
            try:
                cinfo = self.class_map[cinfo]
            except KeyError:
                return None
            if isinstance(cinfo, ClassInfo):
                return cinfo

    def analyze_file(self, pyfile):
        # don't analyze the file again if we've already done it and it hasn't changed
        if pyfile in self.fileinfo:
            if os.path.getmtime(pyfile) <= self.fileinfo[pyfile][1]:
                return self.fileinfo[pyfile][0]

        myvisitor = PythonSourceFileAnalyser(pyfile, self)
        self.modinfo[get_module_path(pyfile)] = myvisitor
        self.fileinfo[myvisitor.fname] = (myvisitor, os.path.getmtime(myvisitor.fname))
        
        self.files_count += 1
        
        return myvisitor
        
    def remove_file(self, fname):
        fvisitor = self.fileinfo[fname][0]
        del self.fileinfo[fname]
        del self.modinfo[fvisitor.modpath]
        nodes = []
        for klass, cinfo in self.class_map.items():
            if isinstance(cinfo, ClassInfo):
                if cinfo.fname == fname:
                    nodes.append(klass)
            else:
                modname = get_module_path(fname)+'.'
                if klass.startswith(modname) or cinfo.startswith(modname):
                    nodes.append(klass)
        self.graph.remove_nodes_from(nodes)
        for klass in nodes:
            del self.class_map[klass]
        
    def find_inheritors(self, base):
        """Returns a list of names of classes that inherit from the given base class."""
        try:
            paths = nx.shortest_path(self.graph, source=base, target=None)
        except KeyError:
            return []
        del paths[base] # don't want the base itself in the list
        return paths.keys()


def main():
    stime = time.time()
    psta = PythonSourceTreeAnalyser()
    for f in sys.argv[1:]:
        if f[0] != '-':
            psta.analyze_file(f)
    psta.dump(sys.stdout)
    sys.stdout.write("elapsed time: %s seconds\n\n" % (time.time() - stime))


if __name__ == '__main__':
    main()