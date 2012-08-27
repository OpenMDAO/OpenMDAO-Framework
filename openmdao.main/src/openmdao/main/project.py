"""
Routines for handling 'Projects' in Python.
"""

import os
import sys
import shutil
from inspect import isclass
import tarfile
import cPickle as pickle
from tokenize import generate_tokens
import token
from cStringIO import StringIO

from pkg_resources import get_distribution, DistributionNotFound

from openmdao.main.api import Container
from openmdao.main.assembly import Assembly, set_as_top
from openmdao.main.component import SimulationRoot
from openmdao.main.variable import namecheck_rgx
from openmdao.main.factorymanager import create as factory_create
from openmdao.main.mp_support import is_instance
from openmdao.util.fileutil import get_module_path, expand_path, file_md5
from openmdao.util.log import logger

# extension for project files
PROJ_FILE_EXT = '.proj'


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
        if pathname.startswith(name+'.'):
            return True
    return False
    
def filter_macro(lines):
    """Removes commands from a macro that are overridden by later commands."""
    # FIXME: this needs a lot of work. Things get a little messy when you have
    # rename and move calls mixed in and I didn't have time to sort out those issues yet,
    # so right now I'm just filtering out multiple execfile() calls and all calls to 
    # run() and execute().
    filt_lines = []
    assigns = set()
    execs = set()
    objs = set()
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
                rest = stripped[len(full):].strip()
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
            
    return filt_lines[::-1] # reverse the result
    
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

class Project(object):
    def __init__(self, projpath):
        """Initializes a Project containing the project found in the 
        specified directory or creates a new project if one doesn't exist.

        projpath: str
            Path to the project's directory.
        """
        macro_exec = False
        self._recorded_cmds = []
        self.path = expand_path(projpath)
        self._model_globals = _ProjDict()
        self._init_globals()

        if os.path.isdir(projpath):
            self.activate()
        
            ## locate file containing state, create it if it doesn't exist
            #statefile = os.path.join(projpath, '_project_state')
            #if os.path.exists(statefile):
                #try:
                    #with open(statefile, 'r') as f:
                        #self._model_globals = pickle.load(f)
                        ## this part is just to handle cases where a project was saved
                        ## before _model_globals was changed to a _ProjDict
                        #if not isinstance(self._model_globals, _ProjDict):
                            #m = _ProjDict()
                            #m.update(self._model_globals)
                            #self._model_globals = m
                            #self._init_globals()
                            
                #except Exception, e:
                    #logger.error('Unable to restore project state: %s' % e)
                    #macro_exec = True
            #else:
                #macro_exec = True
                #logger.error("%s doesn't exist" % statefile)
            #if macro_exec:
            self._initialize()
            macro_file = os.path.join(self.path, '_project_macro')
            if os.path.isfile(macro_file):
                logger.info('Reconstructing project using macro')
                self.load_macro(macro_file, execute=True, strict=True)
        else:  # new project
            os.makedirs(projpath)
            self.activate()
            self._initialize()
            self.save()

    def _initialize(self):
        self.command("top = set_as_top(create('openmdao.main.assembly.Assembly'))")
        
    def _init_globals(self):
        self._model_globals['create'] = self.create    # add create funct here so macros can call it
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
        exec contents in self._model_globals
        
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
    
    def load_macro(self, fpath, execute=True, strict=False):
        with open(fpath, 'r') as f:
            for i,line in enumerate(filter_macro(f.readlines())):
                if execute:
                    try:
                        self.command(line.rstrip('\n'))
                    except Exception as err:
                        logger.error('file %s line %d: %s' % (fpath, i + 1, str(err)))
                        if strict:
                            raise
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
            logger.error("command '%s' caused error: %s" % (cmd, str(err)))
            logger.error("%s" % exc_info[2])
            self._recorded_cmds.append('#ERR: <%s>' % cmd)
            raise err
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
        modeldir = self.path+'.prj'
        if modeldir not in sys.path:
            sys.path = [modeldir]+sys.path
            logger.error("added %s to sys.path" % modeldir)
        
    def deactivate(self):
        """Removes this project's directory from sys.path."""
        modeldir = self.path
        try:
            sys.path.remove(modeldir+'.prj')
        except:
            pass

    def save(self):
        """ Save the state of the project model to its project directory.
        """
        fname = os.path.join(self.path, '_project_state')
        try:
            with open(fname, 'wb') as f:
                pickle.dump(self._model_globals, f)
        except Exception as err:
            logger.error("Failed to pickle the project: %s" % str(err))

        if self._recorded_cmds:
            logger.info("Saving macro used to create project")
            with open(os.path.join(self.path, '_project_macro'), 'w') as f:
                for cmd in self._recorded_cmds:
                    f.write(cmd)
                    f.write('\n')
                    logger.info(cmd)

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
    