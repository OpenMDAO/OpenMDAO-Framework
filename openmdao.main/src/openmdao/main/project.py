"""
Routines for handling 'Projects' in Python.
"""

import os
import sys
from inspect import isclass
import tarfile
from tokenize import generate_tokens
import token
from cStringIO import StringIO
import imp
import ast
from threading import RLock
import traceback

#from pkg_resources import get_distribution, DistributionNotFound

from openmdao.main.api import set_as_top
from openmdao.main.component import SimulationRoot
from openmdao.main.variable import namecheck_rgx
from openmdao.main.factorymanager import create as factory_create
from openmdao.main.mp_support import is_instance
from openmdao.main.publisher import publish
from openmdao.util.fileutil import get_module_path, expand_path, file_md5, find_files
from openmdao.util.fileutil import find_module as util_findmodule
from openmdao.util.log import logger

# extension for project files and directories
PROJ_FILE_EXT = '.proj'
PROJ_DIR_EXT = '.projdir'

# use this to keep track of project classes that have been instantiated
# so we can determine if we need to force a Project save & reload. This
# is the reason for the existence of the custom import hook classes ProjFinder
# and ProjLoader, as well as the CtorInstrumenter ast node transformer.
#
# FIXME: This doesn't keep track of when instances are deleted, so 
# it's possible that the _instantiated_classes set will contain names
# of classes that no longer have any active instances.
_instantiated_classes = set()
_instclass_lock = RLock()


def _clear_insts():
    global _instantiated_classes
    with _instclass_lock:
        _instantiated_classes.clear()


def _register_inst(typname):
    global _instantiated_classes
    with _instclass_lock:
        _instantiated_classes.add(typname)


def _match_insts(classes):
    global _instantiated_classes
    return _instantiated_classes.intersection(classes)


def text_to_node(text):
    """Given a python source string, return the corresponding AST node. The outer
    Module node is removed so that the node corresponding to the given text can
    be added to an existing AST.
    """
    modnode = ast.parse(text, 'exec')
    if len(modnode.body) == 1:
        return modnode.body[0]
    return modnode.body


class CtorInstrumenter(ast.NodeTransformer):
    """All __init__ calls will be replaced with a call to a wrapper function
    that records the call by calling _register_inst(typename) before creating
    the instance.
    """
    def __init__(self):
        super(CtorInstrumenter, self).__init__()

    def visit_ClassDef(self, node):
        text = None
        for stmt in node.body:
            if isinstance(stmt, ast.FunctionDef) and stmt.name == '__init__':
                stmt.name = '__%s_orig_init__' % node.name  # __init__ was found - rename it to __orig_init__
                break
        else:  # no __init__ found, make one
            text = """
def __init__(self, *args, **kwargs):
    _register_inst('.'.join([self.__class__.__module__,self.__class__.__name__]))
    super(%s, self).__init__(*args, **kwargs)
""" % node.name
        if text is None:  # class has its own __init__ (name has been changed to __orig_init__)
            text = """
def __init__(self, *args, **kwargs):
    _register_inst('.'.join([self.__class__.__module__,self.__class__.__name__]))
    self.__%s_orig_init__(*args, **kwargs)
""" % node.name
        node.body = [text_to_node(text)] + node.body
        return node


def add_init_monitors(node):
    """Take the specified AST and translate it into the instrumented version,
    which will record all instances.
    """
    node = CtorInstrumenter().visit(node)
    node.body = [
        ast.copy_location(
            text_to_node('from openmdao.main.project import _register_inst'), node)
        ] + node.body
    return node

class ProjFinder(object):
    """A finder class for custom imports from an OpenMDAO project. In order for this
    to work, an entry must be added to sys.path of the form top_dir+PROJ_DIR_EXT, where top_dir
    is the top directory of the project where python files are kept.
    """
    def __init__(self, path_entry):
        """When path_entry has the form mentioned above (top_dir+PROJ_DIR_EXT), this
        returns a ProjFinder instance that will be used to locate modules within the
        project.
        """
        if path_entry.endswith(PROJ_DIR_EXT) and os.path.isdir(os.path.splitext(path_entry)[0]):
            self.path_entry = path_entry
            self.projdir = os.path.splitext(path_entry)[0]
            if os.path.isdir(self.projdir):
                return
        raise ImportError("can't import from %s" % path_entry)

    def find_module(self, modpath, path=None):
        """This looks within the project for the specified module, returning a loader
        if the module is found, and None if it isn't.
        """
        if path is None:
            path = self.path_entry
        fpath = util_findmodule(modpath, path=[self.projdir])
        if fpath:
            return ProjLoader(path)


class ProjLoader(object):
    """This is the import loader for files within an OpenMDAO project.  We use it to instrument
    the imported files so we can keep track of what classes have been instantiated so we know
    when a project must be saved and reloaded.
    """
    def __init__(self, path_entry):
        self.path_entry = path_entry
        self.projdir = os.path.splitext(path_entry)[0]
        
    def _get_filename(self, modpath):
        parts = [self.projdir]+modpath.split('.')
        path = os.path.join(*parts)
        if os.path.isdir(path):
            return os.path.join(path, '__init__.py')
        else:
            return path + '.py'
                
    def is_package(self, modpath):
        fpath = self._get_filename(modpath)
        return os.path.basename(fpath) == '__init__.py' and os.path.isfile(fpath)
        
    def get_source(self, modpath):
        with open(self._get_filename(modpath), 'r') as f:
            return f.read()
        
    def get_code(self, modpath):
        """Opens the file, compiles it into an AST and then translates it into the instrumented
        version before compiling that into bytecode.
        """
        contents = self.get_source(modpath)
        if not contents.endswith('\n'):
            contents += '\n' # to make ast.parse happy :(
        fname = self._get_filename(modpath)
        root = ast.parse(contents, filename=fname, mode='exec')
        return compile(add_init_monitors(root), fname, 'exec')

    def load_module(self, modpath):
        """Creates a new module if one doesn't exist already, and then updates the
        dict of that module based on the contents of the instrumented module file.
        """
        if modpath in sys.modules:
            mod = sys.modules[modpath]
        else:
            mod = sys.modules.setdefault(modpath, imp.new_module(modpath))
        
        mod.__file__ = self._get_filename(modpath)
        mod.__name__ = modpath
        mod.__loader__ = self
        mod.__package__ = '.'.join(modpath.split('.')[:-1])
        
        if self.is_package(modpath):
            mod.__path__ = [ self.path_entry ]
        else:
            mod.__path__ = self.path_entry
            
        try:
            code = self.get_code(modpath)
            exec(code, mod.__dict__)
        except Exception as err:
            del sys.modules[modpath] # remove bad module
            raise type(err)("Error in file "+os.path.basename(mod.__file__)+": "+str(err))
        return mod


def parse_archive_name(pathname):
    """Return the name of the project given the pathname of a project
    archive file.
    """
    if '.' in pathname:
        return '.'.join(os.path.basename(pathname).split('.')[:-1])
    else:
        return os.path.basename(pathname)


def project_from_archive(archive_name, proj_name=None, dest_dir=None, create=True):
    """Expand the given project archive file in the specified destination
    directory and return a Project object that points to the newly
    expanded project.

    archive_name: str
        Path to the project archive to be expanded.

    proj_name: str (optional)
        Name of the new project. Defaults to the name of the project contained
        in the name of the archive.

    dest_dir: str (optional)
        Directory where the project directory for the expanded archive will
        reside. Defaults to the directory where the archive is located.

    create: bool (optional)
        If True, create and return a Project object. Otherwise just unpack the
        project directory.
    """
    archive_name = expand_path(archive_name)

    if dest_dir is None:
        dest_dir = os.path.dirname(archive_name)
    else:
        dest_dir = expand_path(dest_dir)

    if proj_name is None:
        proj_name = parse_archive_name(archive_name)

    projpath = os.path.join(dest_dir, proj_name)

    if os.path.exists(projpath):
        raise RuntimeError("Directory '%s' already exists" % projpath)

    os.mkdir(projpath)
    if os.path.getsize(archive_name) > 0:
        try:
            f = open(archive_name, 'rb')
            tf = tarfile.open(fileobj=f, mode='r')
            tf.extractall(projpath)
        except Exception, err:
            logger.error(str(err))
            print "Error expanding project archive:", err
        finally:
            tf.close()

    if create:
        return Project(projpath)


#def find_distrib_for_obj(obj):
    #"""Return the name of the distribution containing the module that
    #contains the given object, or None if it's not part of a distribution.
    #"""
    #try:
        #fname = getfile(obj)
    #except TypeError:
        #return None

    #modpath = get_module_path(fname)
    #parts = modpath.split('.')
    #l = len(parts)
    #for i in range(l):
        #try:
            #dist = get_distribution('.'.join(parts[:l-i]))
        #except DistributionNotFound:
            #continue
        #return dist
    #return None


_excluded_calls = set(['run', 'execute'])


def _check_hierarchy(pathname, objs):
    # any operation we apply to a given object will be cancelled
    # out if that object or any of its parents are overwritten
    # later. Returns True if the command applying to the object
    # indicated by the given pathname should be filtered out.
    if pathname in objs:
        return True
    for name in objs:
        if pathname.startswith(name + '.'):
            return True
    return False


def filter_macro(lines):
    """Removes commands from a macro that are overridden by later commands."""
    # FIXME: this needs a lot of work. Things get a little messy when you have
    # rename and move calls mixed in and I didn't have time to sort out those issues yet,
    # so right now I'm just filtering out multiple execfile() calls and all calls to
    # run() and execute().
    filt_lines = []
    #assigns = set()
    execs = set()
    #objs = set()
    for line in lines[::-1]:
        stripped = line.strip()
        if stripped.startswith('execfile'):
            lst = list(generate_tokens(StringIO(stripped).readline))
            if lst[0][1] == 'execfile':
                if lst[2][0] == token.STRING:
                    fname = lst[2][1].strip("'").strip('"')
                    if fname in execs:
                        continue
                    else:
                        execs.add(fname)
        else:
            match = namecheck_rgx.match(stripped)
            if match:
                full = match.group()
                #rest = stripped[len(full):].strip()
                parts = full.rsplit('.', 1)
                if len(parts) > 1:
                    # remove calls to run, execute, ...
                    if parts[1] in _excluded_calls:
                        continue
                    #elif parts[1] in ['add', 'remove']:
                        #lst = list(generate_tokens(StringIO(rest).readline))
                        #if lst[1][0] == token.STRING:
                            #pathname = '.'.join([parts[0],
                                                 #lst[1][1].strip("'").strip('"')])
                            #if _check_hierarchy(pathname, objs):
                                #continue
                            #objs.add(pathname)
                            #if parts[1] == 'remove': # don't include the remove command
                                #continue             # since there won't be anything to remove

                ## only keep the most recent assignment to any variable, and throw away
                ## assigns to variables in objects that have been overridden by newer ones with
                ## the same name.
                #if rest.startswith('='):
                    #if full in assigns or _check_hierarchy(full, objs):
                        #continue
                    #else:
                        #assigns.add(full)

        filt_lines.append(line)

    return filt_lines[::-1]  # reverse the result


class _ProjDict(dict):
    """Use this dict as globals when exec'ing files. It substitutes classes
    from the imported version of the file for the __main__ version.
    """
    def __init__(self):
        super(_ProjDict, self).__init__()
        self._modname = None

    def __getitem__(self, name):
        if self._modname:
            val = getattr(sys.modules[self._modname], name, None)
            if isclass(val):
                return val
        return super(_ProjDict, self).__getitem__(name)

def add_proj_to_path(path):
    """Puts this project's directory on sys.path."""
    modeldir = path+PROJ_DIR_EXT
    if modeldir not in sys.path:
        sys.path = [modeldir]+sys.path

class Project(object):
    def __init__(self, projpath):
        """Initializes a Project containing the project found in the
        specified directory or creates a new project if one doesn't exist.

        projpath: str
            Path to the project's directory.
        """
        self._recorded_cmds = []
        self.path = expand_path(projpath)
        self._model_globals = _ProjDict()
        self._init_globals()
        macro_file = os.path.join(self.path, '_project_macro')

        if os.path.isdir(projpath):
            self.activate()
            if os.path.isfile(macro_file):
                logger.info('Reconstructing project using macro')
                self.load_macro(macro_file, execute=True)
            else:
                self._initialize()
                self.write_macro()
                        
        else:  # new project
            os.makedirs(projpath)
            self.activate()
            self._initialize()
            self.save()

    def _initialize(self):
        self.command("# Auto-generated file - DO NOT MODIFY")
        self.command("top = set_as_top(create('openmdao.main.assembly.Assembly'))")

    def _init_globals(self):
        self._model_globals['create'] = self.create   # add create funct here so macros can call it
        self._model_globals['__name__'] = '__main__'  # set name to __main__ to allow execfile to work the way we want
        self._model_globals['execfile'] = self.execfile
        self._model_globals['set_as_top'] = set_as_top

    def create(self, typname, version=None, server=None, res_desc=None, **ctor_args):
        if server is None and res_desc is None and typname in self._model_globals:
            return getattr(self._model_globals, typname)(**ctor_args)
        return factory_create(typname, version, server, res_desc, **ctor_args)

    @property
    def name(self):
        return os.path.basename(self.path)

    def __contains__(self, name):
        return name in self._model_globals

    def items(self):
        return self._model_globals.items()

    def execfile(self, fname, digest=None):
        # first, make sure file has been imported
        __import__(get_module_path(fname))
        newdigest = file_md5(fname)
        if digest and digest != newdigest:
            logger.warning("file '%s' has been modified since the last time it was exec'd" % fname)
        with open(fname) as f:
            contents = f.read()
        if contents[-1] != '\n':
            contents += '\n'
        node = add_init_monitors(ast.parse(contents, filename=fname, mode='exec'))
        exec compile(node, fname, 'exec') in self._model_globals

        # make the recorded execfile command use the current md5 hash
        self._recorded_cmds.append("execfile('%s', '%s')" % (fname, newdigest))

    def get(self, pathname):
        parts = pathname.split('.')
        try:
            obj = self._model_globals[parts[0]]
            for name in parts[1:]:
                obj = getattr(obj, name)
        except (KeyError, AttributeError) as err:
            raise AttributeError("'%s' not found: %s" % (pathname, str(err)))
        return obj

    def load_macro(self, fpath, execute=True):
        with open(fpath, 'r') as f:
            errors = []
            for i, line in enumerate(filter_macro(f.readlines())):
                if execute:
                    try:
                        self.command(line.rstrip('\n'))
                    except Exception as err:
                        msg = str(err)
                        logger.error("%s" % ''.join(traceback.format_tb(sys.exc_info()[2])))
                        try:
                            publish('console_errors', msg)
                        except:
                            logger.error("publishing of error failed")
                else:
                    self._recorded_cmds.append(line.rstrip('\n'))

    def command(self, cmd):
        err = None
        result = None
        size = len(self._recorded_cmds)

        try:
            code = compile(cmd, '<string>', 'eval')
        except SyntaxError:
            try:
                exec(cmd) in self._model_globals
            except Exception as err:
                exc_info = sys.exc_info()
        else:
            try:
                result = eval(code, self._model_globals)
            except Exception as err:
                exc_info = sys.exc_info()

        if err:
            self._recorded_cmds.append('%s #ERR' % cmd)
            raise  # err  # We don't want to hide the original stack trace!!
        else:
            # certain commands (like execfile) can modify the recorded string,
            # so only record the given command if the executed command didn't
            # add its own entry to _recorded_cmds.
            if len(self._recorded_cmds) == size:
                self._recorded_cmds.append(cmd)

        return result

    def activate(self):
        """Puts this project's directory on sys.path."""
        SimulationRoot.chroot(self.path)
        add_proj_to_path(self.path)
        modeldir = self.path+PROJ_DIR_EXT
        if modeldir not in sys.path:
            sys.path = [modeldir]+sys.path
        
    def deactivate(self):
        """Removes this project's directory from sys.path."""
        modeldir = self.path
        try:
            sys.path.remove(modeldir+PROJ_DIR_EXT)
        except:
            pass

    def write_macro(self):
        logger.info("Saving macro used to create project")
        with open(os.path.join(self.path, '_project_macro'), 'w') as f:
            for cmd in self._recorded_cmds:
                f.write(cmd)
                f.write('\n')
        
    def save(self):
        """ Save the project model to its project directory.
        """
        #fname = os.path.join(self.path, '_project_state')
        #try:
            #with open(fname, 'wb') as f:
                #pickle.dump(self._model_globals, f)
        #except Exception as err:
            #logger.error("Failed to pickle the project: %s" % str(err))
        self.write_macro()

    def export(self, projname=None, destdir='.'):
        """Creates an archive of the current project for export.

        projname: str (optional)
            The name that the project in the archive will have. Defaults to
            the current project name.

        destdir: str (optional)
            The directory where the project archive will be placed. Defaults to
            the current directory.
        """

        ddir = expand_path(destdir)
        if projname is None:
            projname = self.name

        if ddir.startswith(self.path):  # the project contains the dest directory... bad
            raise RuntimeError("Destination directory for export (%s) is within project directory (%s)" %
                               (ddir, self.path))

        self.save()
        startdir = os.getcwd()
        os.chdir(self.path)
        try:
            try:
                fname = os.path.join(ddir, projname + PROJ_FILE_EXT)
                f = open(fname, 'wb')
                tf = tarfile.open(fileobj=f, mode='w:gz')
                for entry in os.listdir(self.path):
                    tf.add(entry)
            except Exception, err:
                print "Error creating project archive:", err
            finally:
                tf.close()
        finally:
            os.chdir(startdir)
