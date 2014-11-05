"""
Routines analyzing dependencies (class and module) in Python source.
"""

import os
import sys
import ast
import time
import __builtin__
import cPickle

import networkx as nx

from openmdao.util.fileutil import find_files, get_module_path, find_module
#from openmdao.util.log import logger

# This is a dict containing all of the entry point groups that OpenMDAO uses to
# identify plugins, and their corresponding Interfaces.
plugin_groups = { 'openmdao.container': ['IContainer'],
                  'openmdao.component': ['IComponent','IContainer'],
                  'openmdao.driver': ['IDriver','IComponent','IContainer'],
                  'openmdao.variable': ['IVariable'],
                  'openmdao.surrogatemodel': ['ISurrogate'],
                  'openmdao.doegenerator': ['IDOEgenerator'],
                  'openmdao.casefilter': ['ICaseFilter'],
                  'openmdao.caseiterator': ['ICaseIterator'],
                  'openmdao.caserecorder': ['ICaseRecorder'],
                  'openmdao.architecture': ['IArchitecture'],
                  'openmdao.optproblem': ['IOptProblem','IAssembly',
                                          'IComponent','IContainer'],
                  }

iface_set = set()
for ifaces in plugin_groups.values():
    iface_set.update(ifaces)


def _to_str(node):
    """Take groups of Name nodes or a Str node and convert to a string."""
    if isinstance(node, ast.Name):
        return node.id
    if not hasattr(node, 'value'):
      return None
    val = node.value
    parts = [node.attr]
    while True:
        if isinstance(val, ast.Attribute):
            parts.append(val.attr)
            val = val.value
        elif isinstance(val, ast.Name):
            parts.append(val.id)
            break
        else:  # it's more than just a simple dotted name
            return None
    return '.'.join(parts[::-1])

# the following are interfaces that get added via the add_delegate decorator
_delegate_ifaces = {
    'HasParameters': 'IHasParameters',
    'HasConstraints': 'IHasConstraints',
    'HasEqConstraints': 'IHasEqConstraints',
    'HasInEqConstraints': 'IHasInEqConstraints',
    'HasObjective': 'IHasObjective',
    'HasObjectives': 'IHasObjectives',
    'HasStopConditions': 'IHasStopConditions',
    'HasEvents': 'IHasEvents',
    'HasCouplingVars': 'IHasCouplingVars',
    }

class ClassInfo(object):
    def __init__(self, name, fname, bases, meta, decorators):
        self.name = name
        self.fname = fname
        self.bases = bases
        self.meta = meta

        ifaces = meta.setdefault('ifaces', [])
        for dec in decorators:
            if dec.func.id == 'add_delegate':
                for arg in [_to_str(a) for a in dec.args]:
                    if arg in _delegate_ifaces:
                        ifaces.append(_delegate_ifaces[arg])

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
                    self.metadata.setdefault('ifaces', []).append(arg.id)

    def visit_Assign(self, node):
        if len(self.metadata)==0 and len(node.targets) == 1:
            lhs = node.targets[0]
            if isinstance(lhs, ast.Name) and lhs.id == '__openmdao_meta__':
                dct = ast.literal_eval(node.value)
                dct.setdefault('ifaces', [])
                dct['ifaces'].extend(self.metadata.setdefault('ifaces', []))
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
        bases = [_to_str(b) for b in node.bases if b is not None]

        bvisitor = _ClassBodyVisitor()
        bvisitor.visit(node)

        bases = [self.localnames.get(b, b) for b in bases if b is not None]

        self.classes[fullname] = ClassInfo(fullname, self.fname, bases,
                                           bvisitor.metadata,
                                           node.decorator_list)
        self.tree_analyser.class_map[fullname] = self.classes[fullname]

        undef_bases = [b for b in bases if b not in self.classes and not hasattr(__builtin__, b)]
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
                        if m == modname or modname.startswith(m+'.'):
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
                            if newname not in self.tree_analyser.class_map and \
                               newname not in self.unresolved_classes:
                                undef_bases.append(newname)
                            break
                else:
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
        # need the following to handle relative imports
        if node.level == 0:
            module = node.module
        else:
            parts = self.modpath.split('.')
            module = '.'.join(parts[0:len(parts)-node.level])
            if node.module is not None:
                module += '.'.join([module, node.module])

        for al in node.names:
            if al.name == '*':
                self.starimports.append(module)
                continue
            if al.asname is None:
                self.localnames[al.name] = '.'.join([module, al.name])
            else:
                self.localnames[al.asname] = '.'.join([module, al.name])

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
                    cinfo.meta.setdefault('ifaces', []).append(iface)
                    cinfo.meta['ifaces'] = list(set(cinfo.meta['ifaces']))

class PythonSourceTreeAnalyser(object):
    def __init__(self, startdir=None, exclude=None, mod_excludes=None,
                 direxclude=None):
        self.files_count = 0 # number of files analyzed

        # inheritance graph. It's a directed graph with base classes pointing
        # to the classes that inherit from them.  Also includes interfaces
        # pointing to classes that implement them.
        self.graph = nx.DiGraph()

        if isinstance(startdir, basestring):
            self.startdirs = [startdir]
        elif startdir is None:
            self.startdirs = []
        else:
            self.startdirs = startdir

        self.startdirs = [os.path.expandvars(os.path.expanduser(d)) for d in self.startdirs]

        if mod_excludes is None:
            self.mod_excludes = set(['traits', 'zope', 'ast'])
        else:
            self.mod_excludes = mod_excludes

        self.modinfo = {}  # maps module pathnames to PythonSourceFileAnalyzers
        self.fileinfo = {} # maps filenames to (PythonSourceFileAnalyzer, modtime)
        self.class_map = {} # map of classname to ClassInfo for the class

        for pyfile in find_files(self.startdirs, "*.py", exclude=exclude,
                                 direxclude=direxclude):
            self.analyze_file(pyfile)

    def dump(self, out, options):
        """Dumps the contents of this object for debugging purposes."""
        for f, tup in self.fileinfo.items():
            out.write("%s\n" % os.path.relpath(f))
            if options.showclasses and tup[0].classes:
                out.write("   classes:\n")
                for item, cinfo in tup[0].classes.items():
                    out.write("      %s\n" % item)
                    if options.showbases and cinfo.bases and cinfo.bases != ['object']:
                        out.write("         bases:\n")
                        for base in cinfo.bases:
                            if base in tup[0].unresolved_classes:
                                out.write("            ???(%s)\n" % base)
                            else:
                                out.write("            %s\n" % base)
                    if options.showifaces and cinfo.meta.get('ifaces'):
                        out.write("         interfaces:\n")
                        for iface in cinfo.meta['ifaces']:
                            out.write("            %s\n" % iface)

        out.write("\n\nFiles examined: %d\n\n" % self.files_count)

    def find_classinfo(self, cname):
        cinfo = cname
        prev = None
        while cinfo != prev:
            prev = cinfo
            try:
                cinfo = self.class_map[cinfo]
            except KeyError:
                return None
            if isinstance(cinfo, ClassInfo):
                return cinfo
        return None

    def analyze_file(self, pyfile, use_cache=False):
        # don't analyze the file again if we've already done it and it hasn't
        # changed.  If `use_cache` is True then lookup/record in global cache.
        mtime = os.path.getmtime(pyfile)
        if pyfile in self.fileinfo:
            if mtime <= self.fileinfo[pyfile][1]:
                return self.fileinfo[pyfile][0]

        if use_cache:
            info = _FileInfoCache.lookup(pyfile)
            if info is not None and mtime <= info[1]:
                self.fileinfo[pyfile] = info
                return info[0]

        #logger.debug("analyzing %s", pyfile)

        myvisitor = PythonSourceFileAnalyser(pyfile, self)
        self.modinfo[get_module_path(pyfile)] = myvisitor
        self.fileinfo[myvisitor.fname] = (myvisitor,
                                          os.path.getmtime(myvisitor.fname))
        self.files_count += 1

        if use_cache:
            _FileInfoCache.record(pyfile, (myvisitor,
                                           os.path.getmtime(myvisitor.fname)))

        return myvisitor

    def flush_cache(self):
        _FileInfoCache.save()

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
        """Returns a list of names of classes that inherit from the given base
        class."""
        try:
            paths = nx.shortest_path(self.graph, source=base, target=None)
        except KeyError:
            return []
        del paths[base] # don't want the base itself in the list
        return paths.keys()

    def get_interfaces(self, classname):
        ''' Returns a set of interfaces for a given class.'''
        ifaces = set()
        klass = self.find_classinfo(classname)
        if klass:
            ifaces.update(klass.meta['ifaces'])
            for base in klass.bases:
                ifaces.update(self.get_interfaces(base))
        return ifaces


class _FileInfoCache(object):
    """ Retains file analysis information. """

    _cache = None
    _dirty = False

    @staticmethod
    def lookup(path):
        """ Return analysis info for `path`. """
        if _FileInfoCache._cache is None:
            _FileInfoCache._load()
        return _FileInfoCache._cache.get(path)

    @staticmethod
    def record(path, info):
        """ Record analysis info for `path`. """
        _FileInfoCache._cache[path] = info
        _FileInfoCache._dirty = True

    @staticmethod
    def save():
        """ Save analysis info to file. """
        if _FileInfoCache._dirty:
            out = _FileInfoCache._open('wb')
            cPickle.dump(_FileInfoCache._cache, out, cPickle.HIGHEST_PROTOCOL)
            out.close()
            _FileInfoCache._dirty = False

    @staticmethod
    def _load():
        """ Load analysis info from file. """
        _FileInfoCache._cache = {}
        try:
            inp = _FileInfoCache._open('rb')
        except Exception:
            return

        # Full test with coverage removes cache at start, so we won't get here.
        try:  #pragma no cover
            _FileInfoCache._cache = cPickle.load(inp)
        except Exception:  #pragma no cover
            return
        finally:  #pragma no cover
            inp.close()

    @staticmethod
    def _open(mode):
        """ Return opened file for '~/.openmdao/fileanalyzer.dat'. """
        filename = \
            os.path.expanduser(os.path.join('~', '.openmdao', 'fileanalyzer.dat'))
        dirname = os.path.dirname(filename)
        # Full test with coverage leaves directory intact.
        if not os.path.exists(dirname):  #pragma no cover
            os.mkdir(dirname)
        return open(filename, mode)


def main():
    from argparse import ArgumentParser
    parser = ArgumentParser()
    parser.add_argument("-c", "--classes", action='store_true', dest='showclasses',
                        help="show classes found")
    parser.add_argument("-b", "--bases", action="store_true", dest="showbases",
                        help="show base classes (only works if --classes is active)")
    parser.add_argument("-i", "--interfaces", action="store_true", dest="showifaces",
                        help="show interfaces of classes (only works if --classes is active)")
    parser.add_argument("-u", "--use-cache", action='store_true', dest='use_cache',
                        help="use analysis cache")
    parser.add_argument('files', metavar='fname', type=str, nargs='+',
                        help='a file or directory to be scanned')

    options = parser.parse_args()
    print options.use_cache

    stime = time.time()
    psta = PythonSourceTreeAnalyser()
    for f in options.files:
        f = os.path.abspath(os.path.expanduser(f))
        if os.path.isdir(f):
            for pyfile in find_files(f, "*.py", exclude=lambda n: 'test' in n.split(os.sep)):
                psta.analyze_file(pyfile, use_cache=options.use_cache)
        else:
            psta.analyze_file(f, use_cache=options.use_cache)
    psta.dump(sys.stdout, options)
    sys.stdout.write("elapsed time: %s seconds\n\n" % (time.time() - stime))

    if options.use_cache:
        _FileInfoCache.save()


if __name__ == '__main__':
    main()
