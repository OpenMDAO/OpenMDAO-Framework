"""
Routines for handling 'Projects' in Python.
"""

import os
import sys
import tarfile
from tokenize import generate_tokens
import token
from cStringIO import StringIO
import imp
import ast
from threading import RLock
import traceback
from ConfigParser import SafeConfigParser

#from pkg_resources import get_distribution, DistributionNotFound

from openmdao.main.assembly import set_as_top
from openmdao.main.component import SimulationRoot
from openmdao.main.variable import namecheck_rgx
from openmdao.main.factorymanager import create as factory_create
from openmdao.main.publisher import publish
from openmdao.util.fileutil import get_module_path, expand_path, file_md5, \
                                   find_in_path
from openmdao.util.fileutil import find_module as util_findmodule
from openmdao.util.log import logger
from openmdao.util.astutil import parse_ast, text_to_node

# extension for project files and directories
PROJ_FILE_EXT = '.proj'
PROJ_DIR_EXT = '.projdir'
PROJ_HOME = os.path.expanduser('~/.openmdao/gui/projects')

# use this to keep track of project classes that have been instantiated
# so we can determine if we need to force a Project save & reload. This
# is the reason for the existence of the custom import hook classes ProjFinder
# and ProjLoader, as well as the _CtorInstrumenter ast node transformer.
#
# FIXME: This doesn't keep track of when instances are deleted, so
# it's possible that the _instantiated_classes set will contain names
# of classes that no longer have any active instances.
_instantiated_classes = set()
_instclass_lock = RLock()

_macro_lock = RLock()


def _clear_insts():
    with _instclass_lock:
        _instantiated_classes.clear()


def _register_inst(typname):
    with _instclass_lock:
        _instantiated_classes.add(typname)


def _match_insts(classes):
    return _instantiated_classes.intersection(classes)


class _CtorInstrumenter(ast.NodeTransformer):
    """All __init__ calls will be replaced with a call to a wrapper function
    that records the call by calling _register_inst(typename) before creating
    the instance.
    """
    def __init__(self):
        super(_CtorInstrumenter, self).__init__()

    def visit_ClassDef(self, node):
        text = None
        reg = "_register_inst('.'.join([self.__class__.__module__,self.__class__.__name__]))"
        for stmt in node.body:
            if isinstance(stmt, ast.FunctionDef) and stmt.name == '__init__':
                stmt.body = [text_to_node(reg, stmt.lineno)] + stmt.body
                break
        else:  # no __init__ found, make one
            text = """
def __init__(self, *args, **kwargs):
    _register_inst('.'.join([self.__class__.__module__,self.__class__.__name__]))
    super(%s, self).__init__(*args, **kwargs)
""" % node.name
            node.body = [text_to_node(text, node.lineno)] + node.body
        return node


def _add_init_monitors(node):
    """Take the specified AST and translate it into the instrumented version,
    which will record all instances.
    """
    node = _CtorInstrumenter().visit(node)
    node.body = [
        ast.copy_location(
            text_to_node('from openmdao.main.project import _register_inst'), node)
    ] + node.body
    return node


class ProjFinder(object):
    """A finder class for custom imports from an OpenMDAO project. For
    this to work, an entry must be added to sys.path of the form
    ``top_dir+PROJ_DIR_EXT``, where `top_dir` is the top directory of the project
    where Python files are kept.
    """
    def __init__(self, path_entry):
        """When path_entry has the form mentioned above (``top_dir+PROJ_DIR_EXT``),
        this returns a ProjFinder instance that will be used to locate modules
        within the project.
        """
        if path_entry.endswith(PROJ_DIR_EXT) and \
           os.path.isdir(os.path.splitext(path_entry)[0]):
            self.path_entry = path_entry
            self.projdir = os.path.splitext(path_entry)[0]
            if os.path.isdir(self.projdir):
                return
        raise ImportError("can't import from %s" % path_entry)

    def find_module(self, modpath, path=None):
        """This looks within the project for the specified module, returning a
        loader if the module is found and None if it isn't.
        """
        if path is None:
            path = self.path_entry
        fpath = util_findmodule(modpath, path=[self.projdir])
        if fpath:
            return ProjLoader(path)


class ProjLoader(object):
    """This is the import loader for files within an OpenMDAO project.
    We use it to instrument the imported files so we can keep track of what
    classes have been instantiated so we know when a project must be reloaded.
    """
    def __init__(self, path_entry):
        self.path_entry = path_entry
        self.projdir = os.path.splitext(path_entry)[0]

    def _get_filename(self, modpath):
        parts = [self.projdir] + modpath.split('.')
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
        """Opens the file, compiles it into an AST, and then translates it into
        the instrumented version before compiling that into bytecode.
        """
        contents = self.get_source(modpath)
        fname = self._get_filename(modpath)
        root = parse_ast(contents, fname, mode='exec')
        return compile(_add_init_monitors(root), fname, 'exec')

    def load_module(self, modpath):
        """Creates a new module if one doesn't exist already and then updates
        the dict of that module based on the contents of the instrumented
        module file.
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
            mod.__path__ = [self.path_entry]
        else:
            mod.__path__ = self.path_entry

        try:
            code = self.get_code(modpath)
            exec(code, mod.__dict__)
        except Exception as err:
            del sys.modules[modpath]  # remove bad module
            raise type(err)("Error in file %s: %s"
                            % (os.path.basename(mod.__file__), err))
        return mod


def parse_archive_name(pathname):
    """Return the name of the project given the pathname of a project
    archive file.
    """
    return os.path.splitext(os.path.basename(pathname))[0]


def project_from_archive(archive_name, proj_name=None, dest_dir=None,
                         create=True, overwrite=False):
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
        If True, create and return a Project object. Otherwise, just unpack the
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

    if not overwrite and os.path.exists(projpath):
        raise RuntimeError("Directory '%s' already exists" % projpath)

    if not os.path.exists(projpath):
        os.mkdir(projpath)
    if os.path.getsize(archive_name) > 0:
        try:
            f = open(archive_name, 'rb')
            tf = tarfile.open(fileobj=f, mode='r')
            tf.extractall(projpath)
        except Exception as err:
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


# def _check_hierarchy(pathname, objs):
#     # any operation we apply to a given object will be cancelled
#     # out if that object or any of its parents are overwritten
#     # later. Returns True if the command applying to the object
#     # indicated by the given pathname should be filtered out.
#     if pathname in objs:
#         return True
#     for name in objs:
#         if pathname.startswith(name + '.'):
#             return True
#     return False


def _filter_macro(lines):
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


class Project(object):
    def __init__(self, projpath, gui=True, globals_dict=None):
        """Initializes a Project containing the project found in the
        specified directory or creates a new project if 'create' is True
        and one doesn't exist.

        projpath: str
            Path to the project's directory.

        gui: bool (optional) (default=True)
            GUI is running. This determines how the project is loaded.

        globals_dict: dict (optional)
            If this is not None, it will be populated with the objects
            from the project model. Otherwise the Project will use its
            own internal dict.
        """
        self._recorded_cmds = []
        self._cmds_to_save = []
        self.path = expand_path(projpath)
        self._project_globals = globals_dict if globals_dict is not None else {}
        self._gui = gui

        self.macrodir = os.path.join(self.path, '_macros')
        self.macro = 'default'

        if gui and not os.path.isdir(self.macrodir):
            os.makedirs(self.macrodir)

        settings = os.path.join(self.path, '_settings.cfg')
        if gui and not os.path.isfile(settings):
            self._create_config()

        self.config = SafeConfigParser()
        self.config.optionxform = str  # Preserve case.
        files = self.config.read(settings)
        if not files:
            self._error("Failed to read project config file")

    def _create_config(self):
        """Create the initial _settings.cfg file for the project."""
        settings = """
[preferences]
export_repo = false

[info]
version = 0
description =
"""
        with open(os.path.join(self.path, '_settings.cfg'), 'wb') as f:
            f.write(settings)

    def get_info(self):
        """ Return settings 'info' section as a dictionary. """
        return dict(self.config.items('info'))

    def set_info(self, info):
        """ Set settings 'info' section from `info` dictionary. """
        for key, value in info.items():
            self.config.set('info', key, value)
        with open(os.path.join(self.path, '_settings.cfg'), 'wb') as f:
            self.config.write(f)

    def create(self, typname, version=None, server=None, res_desc=None, **ctor_args):
        if server is None and res_desc is None and typname in self._project_globals:
            return getattr(self._project_globals, typname)(**ctor_args)
        return factory_create(typname, version, server, res_desc, **ctor_args)

    @property
    def name(self):
        return os.path.basename(self.path)

    def __contains__(self, pathname):
        parts = pathname.split('.')
        try:
            obj = self._project_globals[parts[0]]
            for name in parts[1:]:
                obj = getattr(obj, name)
        except Exception:
            return False
        return True

    def items(self):
        return self._project_globals.items()

    def _error(self, msg, errclass=RuntimeError):
        if self._gui:
            logger.error(msg)
        else:
            raise errclass(msg)

    def execfile(self, fname, digest=None):
        # first, make sure file has been imported
        __import__(get_module_path(fname))
        newdigest = file_md5(fname)
        if digest and digest != newdigest:
            logger.warning("file '%s' has been modified since the last time"
                           " it was exec'd" % fname)
        with open(fname) as f:
            contents = f.read()
        node = _add_init_monitors(parse_ast(contents, fname, mode='exec'))
        exec compile(node, fname, 'exec') in self._project_globals

        # make the recorded execfile command use the current md5 hash
        self._cmds_to_save.append("execfile('%s', '%s')" % (fname, newdigest))

    def get(self, pathname):
        parts = pathname.split('.')
        try:
            obj = self._project_globals[parts[0]]
            for name in parts[1:]:
                obj = getattr(obj, name)
        except (KeyError, AttributeError) as err:
            raise AttributeError("'%s' not found: %s" % (pathname, str(err)))
        return obj

    def load_macro(self, macro_name):
        fpath = os.path.join(self.macrodir, macro_name)
        self._recorded_cmds = []
        with open(fpath, 'rU') as f:
            content = f.read()

        # fix missing newline at end of file to avoid issues later when
        # we append to it
        if not content.endswith('\n'):
            with open(fpath, 'a') as f:
                f.write('\n')

        lines = content.split('\n')

        for i, line in enumerate(lines):
            logger.debug(line)
            try:
                self.command(line, save=False)
            except Exception as err:
                logger.error(''.join(traceback.format_tb(sys.exc_info()[2])))
                msg = str(err)
                if self._gui:
                    try:
                        publish('console_errors', msg)
                    except:
                        logger.error("publishing of error failed")
                else:
                    raise RuntimeError(msg)

    def _save_command(self, save):
        """Save the current command(s) to the macro file."""
        self._recorded_cmds.extend(self._cmds_to_save)
        if save:
            with open(os.path.join(self.macrodir, self.macro), 'a') as f:
                for cmd in self._cmds_to_save:
                    f.write(cmd + '\n')
        self._cmds_to_save = []

    def command(self, cmd, save=True):
        err = None
        result = None
        self._cmds_to_save = []

        try:
            code = compile(cmd, '<string>', 'eval')
        except SyntaxError:
            try:
                exec(cmd) in self._project_globals
            except Exception as err:
                pass
        else:
            try:
                result = eval(code, self._project_globals)
            except Exception as err:
                pass

        if err:
            exc_type, exc_value, exc_traceback = sys.exc_info()
            lines = traceback.format_exception(exc_type, exc_value, exc_traceback)
            logger.error("command '%s' generated an exception:\n %s",
                         cmd, ''.join(lines))
            raise
        else:
            if not self._cmds_to_save:
                self._cmds_to_save.append(cmd)
            self._save_command(save)

        return result

    def _initialize(self):
        if os.path.isfile(os.path.join(self.macrodir, self.macro)):
            logger.info('Reconstructing project using %s macro' % self.macro)
            self.load_macro(self.macro)
        else:
            self.command("# Auto-generated file - MODIFY AT YOUR OWN RISK")

    def _init_globals(self):
        self._project_globals['create'] = self.create   # add create funct here so macros can call it
        self._project_globals['execfile'] = self.execfile
        self._project_globals['set_as_top'] = set_as_top
        if self._gui:
            self._project_globals['__name__'] = '__main__'  # set name to __main__ to allow execfile to work the way we want

    def activate(self):
        """Make this project active by putting its directory on sys.path and
        executing its macro.
        """
        if self._gui:
            # set SimulationRoot and put our path on sys.path
            SimulationRoot.chroot(self.path)
        self.add_to_path()

        # set up the project
        self._init_globals()
        self._initialize()

    def add_to_path(self):
        """Puts this project's directory on sys.path so that imports from it
        will be processed by our special loader.
        """
        if self._gui:
            # this weird extension is needed in order for our import hook
            # to fire
            projectdir = self.path + PROJ_DIR_EXT
            if projectdir not in sys.path:
                sys.path = [projectdir] + sys.path
        else:
            if self.path not in sys.path:
                sys.path = [self.path] + sys.path

    def deactivate(self):
        """Removes this project's directory from sys.path."""
        projectdir = self.path
        try:
            if self._gui:
                sys.path.remove(projectdir + PROJ_DIR_EXT)
            else:
                sys.path.remove(self.path)
        except:
            pass

    def export(self, projname=None, destdir='.'):
        """Creates an archive of the current project for export.

        projname: str (optional)
            The name that the project in the archive will have. Defaults to
            the current project name.

        destdir: str (optional)
            The directory where the project archive will be placed. Defaults to
            the current directory.
        """

        export_repo = self.config.getboolean("preferences", "export_repo")
        excludes = ['.git', '.bzr', '.hg', '.projrepo']
        ddir = expand_path(destdir)
        if projname is None:
            projname = self.name

        if os.path.basename(ddir) not in excludes and ddir.startswith(self.path):
            # the project contains the dest directory... bad
            raise RuntimeError("Destination directory for export (%s) is within"
                               " project directory (%s)" % (ddir, self.path))

        startdir = os.getcwd()
        os.chdir(self.path)
        try:
            try:
                fname = os.path.join(ddir, projname + PROJ_FILE_EXT)
                f = open(fname, 'wb')
                tf = tarfile.open(fileobj=f, mode='w:gz')
                for entry in os.listdir(self.path):
                    if export_repo or entry not in excludes:
                        tf.add(entry)
            except Exception as err:
                print "Error creating project archive:", err
                fname = None
            finally:
                tf.close()
        finally:
            os.chdir(startdir)
        return fname


def load_project(pname, globals_dict=None):
    """Load the model from the named project into the current
    globals dict so that the model can be used as part of a
    python script outside of the GUI.  pname can either be an
    absolute or relative path to a project directory, or just
    a project name.  If it's a project name, the project directory
    will be searched for in PATH, and if not found there will be
    searched for in $HOME/.openmdao/gui/projects.
    """

    abspname = os.path.abspath(pname)
    if os.path.isdir(abspname):
        ppath = abspname
    else:
        ppath = find_in_path(pname)
        hpath = os.path.join(PROJ_HOME, pname)
        if ppath is None and os.path.isdir(hpath):
            ppath = hpath

    if ppath is None:
        raise RuntimeError("can't locate project '%s'" % pname)

    proj = Project(ppath, gui=False, globals_dict=globals_dict)
    proj.activate()

    return proj


def list_projects():
    """Return a list of available projects."""
    return sorted(os.listdir(PROJ_HOME))
