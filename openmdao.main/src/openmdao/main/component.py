
#public symbols
__all__ = ['Component', 'SimulationRoot']

__version__ = "0.1"

import fnmatch
import glob
import logging
import os.path
import pkg_resources
import shutil
import subprocess
import sys
import time

from zope.interface import implements

from openmdao.main.interfaces import IComponent
from openmdao.main import Container, String, Variable
from openmdao.main.variable import INPUT, OUTPUT
from openmdao.main.constants import SAVE_CPICKLE
from openmdao.main.filevar import FileVariable
from openmdao.main.log import LOG_DEBUG

# Execution states.
STATE_UNKNOWN = -1
STATE_IDLE    = 0
STATE_RUNNING = 1
STATE_WAITING = 2


class SimulationRoot (object):
    """Singleton object used to hold root directory."""

    # Execution root directory. Root of all legal file paths.
    __root = None

    @staticmethod
    def chdir (path):
        """Change to directory 'path' and set the singleton's root.
        Normally not called, but useful in special situations."""
        os.chdir(path)
        SimulationRoot.__root = os.getcwd()

    @staticmethod
    def get_root ():
        """Return this simulation's root directory path."""
        if SimulationRoot.__root is None:
            SimulationRoot.__root = os.getcwd()
        return SimulationRoot.__root

    @staticmethod
    def legal_path (path):
        """Return True if path is legal (decendant of our root)."""
        if SimulationRoot.__root is None:
            SimulationRoot.__root = os.getcwd()
        return path.startswith(SimulationRoot.__root)


class Component (Container):
    """This is the base class for all objects containing Variables that are 
       accessible to the OpenMDAO framework and are 'runnable'.
    """

    implements(IComponent)
    
    def __init__(self, name, parent=None, doc=None, directory='',
                 add_to_parent=True):
        super(Component, self).__init__(name, parent, doc, add_to_parent)
        
        self.state = STATE_IDLE
        self._stop = False
        self._need_check_config = True
        self._execute_needed = True
        
        self._dir_stack = []
        
        # List of meta-data dictionaries.
        self.external_files = []

        String('directory', self, INPUT, default=directory,
               doc='If non-null, the directory to execute in.')

# pylint: disable-msg=E1101
        dirpath = self.get_directory()
        if dirpath:
            if not SimulationRoot.legal_path(dirpath):
                self.raise_exception(
                    "Illegal execution directory '%s', not a decendant of '%s'."
                    % (dirpath, SimulationRoot.get_root()),
                    ValueError)
            if not os.path.exists(dirpath):
                try:
                    os.makedirs(dirpath)
                except OSError, exc:
                    self.raise_exception(
                        "Can't create execution directory '%s': %s"
                        % (dirpath, exc.strerror), OSError)
            else:
                if not os.path.isdir(dirpath):
                    self.raise_exception(
                        "Execution directory path '%s' is not a directory."
                        % dirpath, ValueError)
# pylint: enable-msg=E1101

    def check_config (self):
        """Verify that the configuration of this component is correct. This function is
        called once prior to the first execution of this component, and may be called
        explicitly at other times if desired.
        """
        pass         
    
    def _pre_execute (self):
        """Make preparations for execution. Overrides of this function are not
        recommended, but if unavoidable they should still call this version.
        """
        if self._need_check_config:
            self.check_config()
            self._need_check_config = False
        
        if self.parent is None: # if parent is None, we're not part of an Assembly
                                # so Variable validity doesn't apply. Just execute.
            self._execute_needed = True
        else:
            if hasattr(self.parent, 'update_inputs'):
                invalid_ins = self.get_inputs(valid=False)
                if len(invalid_ins) > 0:
                    self.parent.update_inputs(
                            ['.'.join([self.name,x.name]) for x in invalid_ins])
                    self._execute_needed = True
            if self._execute_needed is False and self.has_invalid_outputs():
                self._execute_needed = True
    
                            
    def execute (self):
        """Perform calculations or other actions, assuming that inputs 
        have already been set. This should be overridden in derived classes.
        """
        pass
    
    def _post_execute (self):
        """Update output variables and anything else needed after execution. 
        Overrides of this function are not recommended, but if unavoidable 
        they should still call this version.
        """
        # make our Variables valid again
        for var in self.get_outputs(valid=False):
            var.valid = True
        self._execute_needed = False
        
    def run (self, force=False):
        """Run this object. This should include fetching input variables,
        executing, and updating output variables. Do not override this function.
        """
        if self.directory:
            directory = self.get_directory()
            try:
                self.push_dir(directory)
            except OSError, exc:
                msg = "Could not move to execution directory '%s': %s" % \
                      (directory, exc.strerror)
                self.raise_exception(msg, RuntimeError)

        self.state = STATE_RUNNING
        self._stop = False
        try:
            self._pre_execute()
            if self._execute_needed or force:
                #if __debug__: self._logger.debug('execute %s' % self.get_pathname())
                self.execute()
            self._post_execute()
        finally:
            self.state = STATE_IDLE
            if self.directory:
                self.pop_dir()

    def get_directory (self):
        """Return absolute path of execution directory."""
        path = self.directory
        if not os.path.isabs(path):
            if self.parent is not None and IComponent.providedBy(self.parent):
                parent_dir = self.parent.get_directory()
            else:
                parent_dir = SimulationRoot.get_root()
            path = os.path.join(parent_dir, path)
        return path

    def push_dir (self, directory):
        """Change directory to dir, remembering current for later pop_dir()."""
        if not directory:
            directory = '.'
        cwd = os.getcwd()
        if not os.path.isabs(directory):
            directory = os.path.join(self.get_directory(), directory)
        if not SimulationRoot.legal_path(directory):
            self.raise_exception(
                "Illegal directory '%s', not a decendant of '%s'." % \
                (directory, SimulationRoot.get_root()), ValueError)
        os.chdir(directory)
        self._dir_stack.append(cwd)

    def pop_dir (self):
        """Return to previous directory saved by push_dir()."""
        os.chdir(self._dir_stack.pop())

    def checkpoint (self, outstream, format=SAVE_CPICKLE):
        """Save sufficient information for a restart. By default, this
        just calls save().
        """
        self.save(outstream, format)

    def restart (self, instream):
        """Restore state using a checkpoint file. The checkpoint file is
        typically a delta from a full saved state file. If checkpoint is
        overridden, this should also be overridden.
        """
        self.load(instream)

    def save_to_egg(self, name=None, version=None, py_dir=None,
                    force_relative=True, src_dir=None, src_files=None,
                    entry_pts=None, dst_dir=None, format=SAVE_CPICKLE,
                    proto=-1, use_setuptools=False):
        """Save state and other files to an egg.

        - `name` defaults to the name of the component.
        - `version` defaults to the component's module __version__.
        - `py_dir` defaults to the current directory.
        - If `force_relative` is True, all paths are relative to `src_dir`.
        - `src_dir` defaults to the component's directory.
        - `src_files` should be a set, and defaults to component's external files.
        - 'entry_pts' is a list of (obj, obj_name) tuples for additional entries.
        - `dst_dir` is the directory to write the egg in.

        The resulting egg can be unpacked on UNIX via 'sh egg-file'.
        Returns the egg's filename.
        """
        if src_dir is None:
            src_dir = self.get_directory()
        if src_dir.endswith(os.sep):
            src_dir = src_dir[:-1]
        if src_files is None:
            src_files = set()

        fixup_dirs = []  # Used to restore original component config.
        fixup_meta = []
        fixup_fvar = []

        # Process all components in bottom-up order.
        # We have to check relative paths like '../somedir' and if
        # we do that after adjusting a parent, things can go bad.
        components = [self]
        components.extend([c for c in self.values(pub=False, recurse=True)
                                if isinstance(c, Component)])
        for comp in sorted(components, reverse=True,
                           key=lambda comp: comp.get_pathname()):

            # Process execution directory.
            comp_dir = comp.get_directory()
            if force_relative:
                if comp_dir.startswith(src_dir):
                    if comp_dir == src_dir and comp.directory:
                        fixup_dirs.append((comp, comp.directory))
                        comp.directory = ''
                    elif os.path.isabs(comp.directory):
                        parent_dir = comp.parent.get_directory()
                        fixup_dirs.append((comp, comp.directory))
                        comp.directory = self._relpath(comp_dir, parent_dir)
                else:
                    self.raise_exception(
                        "Can't save, %s directory '%s' doesn't start with '%s'."
                        % (comp.get_pathname(), comp_dir, src_dir), ValueError)

            # Process external files.
            for metadata in comp.external_files:
                path = metadata['path']
                path = os.path.expanduser(path)
                path = os.path.expandvars(path)
                if not os.path.isabs(path):
                    path = os.path.join(comp_dir, path)
                paths = glob.glob(path)
                for path in paths:
                    path = os.path.normpath(path)
                    if not os.path.exists(path):
                        continue
                    if force_relative:
                        if path.startswith(src_dir):
                            save_path = self._relpath(path, src_dir)
                            if os.path.isabs(metadata['path']):
                                path = self._relpath(path, comp_dir)
                                fixup_meta.append((metadata, metadata['path']))
                                metadata['path'] = path
                        else:
                            self.raise_exception(
                                "Can't save, %s file '%s' doesn't start with '%s'."
                                % (comp.get_pathname(), path, src_dir),
                                ValueError)
                    else:
                        save_path = path
                    src_files.add(save_path)

            # Process FileVariables for this component only.
            for fvar in comp.get_file_vars():
                path = fvar.get_value()
                if not path:
                    continue
                if not os.path.isabs(path):
                    path = os.path.join(comp_dir, path)
                path = os.path.normpath(path)
                if not os.path.exists(path):
                    continue
                if force_relative:
                    if path.startswith(src_dir):
                        save_path = self._relpath(path, src_dir)
                        if os.path.isabs(fvar.get_value()):
                            path = self._relpath(path, comp_dir)
                            fixup_fvar.append((fvar, fvar.get_value()))
                            fvar.set_value(path)
                    else:
                        self.raise_exception(
                            "Can't save, %s path '%s' doesn't start with '%s'."
                            % (fvar.get_pathname(), path, src_dir), ValueError)
                else:
                    save_path = path
                src_files.add(save_path)

        # Save relative directory for any entry points. Some oddness with
        # parent weakrefs seems to prevent reconstruction in load().
        if entry_pts is not None:
            for entry_obj, entry_name in entry_pts:
                if not IComponent.providedBy(entry_obj):
                    continue
                relpath = entry_obj.directory
                obj = entry_obj.parent
                if obj is None:
                    raise RuntimeError('Entry point object has no parent!')
                while obj.parent is not None and \
                      IComponent.providedBy(obj.parent):
                    relpath = os.path.join(obj.directory, relpath)
                    obj = obj.parent
                entry_obj._rel_dir_path = relpath

        try:
            return super(Component, self).save_to_egg(
                       name, version, py_dir, src_dir, src_files,
                       entry_pts, dst_dir, format, proto, use_setuptools)
        finally:
            # If any component config has been modified, restore it.
            for comp, path in fixup_dirs:
                comp.directory = path
            for meta, path in fixup_meta:
                meta['path'] = path
            for fvar, path in fixup_fvar:
                fvar.set_value(path)

    def get_file_vars(self):
        """Return list of FileVariables owned by this component."""

        def _recurse_get_file_vars (container, file_vars, visited):
            """Scan both normal __dict__ and _pub."""
            objs = container.__dict__.values()
            objs.extend(container._pub.values())
            for obj in objs:
                if id(obj) in visited:
                    continue
                visited.add(id(obj))
                if isinstance(obj, FileVariable):
                    file_vars.add(obj)
                elif isinstance(obj, Component):
                    continue
                elif isinstance(obj, Variable):
                    continue
                elif isinstance(obj, Container):
                    _recurse_get_file_vars(obj, file_vars, visited)

        file_vars = set()
        visited = set()
        _recurse_get_file_vars(self, file_vars, visited)
        return file_vars

    def _relpath(self, path1, path2):
        """Return path for path1 relative to path2."""
        assert os.path.isabs(path1)
        assert os.path.isabs(path2)

        if path1.endswith(os.sep):
            path = path1[:-1]
        else:
            path = path1
        if path2.endswith(os.sep):
            start = path2[:-1]
        else:
            start = path2

        if path == start:
            return ''

        relpath = ''
        while start:
            if path.startswith(start):
                return os.path.join(relpath, path[len(start)+1:])
            relpath = os.path.join('..', relpath)
            start = os.path.dirname(start)

        self.raise_exception("'%s' has no common prefix with '%s'"
                             % (path1, path2), ValueError)

    def check_save_load(self, py_dir=None, test_dir='test_dir', cleanup=True,
                        format=SAVE_CPICKLE, logfile=None, python=None):
        """Convenience routine to check that saving & reloading work."""
        if sys.platform == 'win32':
            print '\ncheck_save_load() unsupported on win32 at this time.'
            return 0  # Enable once openmdao.util.testutil.find_python works.
        old_level = self.log_level
        self.log_level = LOG_DEBUG
        start = time.time()
        egg_name = self.save_to_egg(py_dir=py_dir, format=format)
        elapsed = time.time() - start
        size = os.path.getsize(egg_name)
        print '\nSaved %d bytes in %.2f seconds (%.2f bytes/sec)' % \
              (size, elapsed, size/elapsed)

        orig_dir = os.getcwd()
        if os.path.exists(test_dir):
            shutil.rmtree(test_dir)
        os.mkdir(test_dir)
        os.chdir(test_dir)
        egg_path = os.path.join('..', egg_name)
        unpacker = None
        try:
            print '\nUnpacking in subprocess...'
            env = os.environ
            env['OPENMDAO_INSTALL'] = '0'
            if logfile:
                stdout = open(logfile, 'w')
                stderr = subprocess.STDOUT
            else:
                stdout = None
                stderr = None

            if sys.platform == 'win32' or python:
                if not python:
                    python = 'python'
                print '    python:', python
                unpacker = 'unpack.py'
                out = open(unpacker, 'w')
                out.write("""\
from openmdao.main import Component
Component.load_from_eggfile('%s', install=False)
""" % egg_path)
                out.close()
                args = [python, unpacker]
            else:
                args = ['sh', egg_path]

            retcode = subprocess.call(args, env=env,
                                      stdout=stdout, stderr=stderr)
            print '    retcode', retcode
            if retcode == 0:
                print '\nRunning in subprocess...'
                os.chdir(self.name)
                if not python:
                    python = 'python'
                retcode = subprocess.call([python, self.name+'_loader.py'],
                                          stdout=stdout, stderr=stderr)
                print '    retcode', retcode
            if logfile:
                stdout.close()
        finally:
            if unpacker and os.path.exists(unpacker):
                os.remove(unpacker)
            os.chdir(orig_dir)
            self.log_level = old_level
            if cleanup:
                os.remove(egg_name)
                shutil.rmtree(test_dir)

        return retcode

    @staticmethod
    def load(instream, format=SAVE_CPICKLE, package=None, do_post_load=True,
             top_obj=True):
        """Load object(s) from instream."""
# This doesn't work:
#    AttributeError: 'super' object has no attribute 'load'
#        top = super(Component).load(instream, format, package, False)
        top = Container.load(instream, format, package, False)
        if IComponent.providedBy(top):
            # Get path relative to real top before we clobber directory attr.
            if top_obj:
                relpath = '.'
            else:
                if top.parent is not None:
                    relpath = top.directory
                    obj = top.parent
                    while obj.parent is not None and \
                          IComponent.providedBy(obj.parent):
                        relpath = os.path.join(obj.directory, relpath)
                        obj = obj.parent
                elif hasattr(top, '_rel_dir_path'):
                    top.warning('No parent, using saved relative directory')
                    relpath = top._rel_dir_path  # Set during save_to_egg().
                else:
                    top.warning('No parent, using null relative directory')
                    relpath = ''

            # Set root directory and create any subdirectories.
            top.directory = os.getcwd()
            for component in [c for c in top.values(pub=False, recurse=True)
                                    if IComponent.providedBy(c)]:
                directory = component.get_directory()
                if not os.path.exists(directory):
                    os.makedirs(directory)

            # If necessary, copy files from installed egg.
            if isinstance(instream, basestring) and \
               not os.path.exists(instream) and not os.path.isabs(instream):
                # If we got this far, then the stuff below "can't" fail.
                if not package:
                    dot = instream.rfind('.')
                    package = instream[:dot]
                top._restore_files(package, relpath)

        if do_post_load:
            top.parent = None
            top.post_load()
        return top

    def _restore_files(self, package, relpath):
        """Restore external files from installed egg."""
        if self.directory:
            self.push_dir(self.get_directory())
        try:
            if self.external_files:
                self.info('Restoring files in %s', os.getcwd())
                pkg_files = pkg_resources.resource_listdir(package, relpath)
            for metadata in self.external_files:
                pattern = metadata['path']
                is_input = metadata.get('input', False)
                const = metadata.get('constant', False)
                dirname = os.path.dirname(pattern)
                pattern = os.path.basename(pattern)
                if dirname:
                    if not os.path.exists(dirname):
                        os.makedirs(dirname)
                    path = relpath+'/'+dirname
                    package_files = pkg_resources.resource_listdir(package,
                                                                   path)
                    self._copy_files(pattern, package_files, package, path,
                                     is_input, const, dirname)
                else:
                    self._copy_files(pattern, pkg_files, package, relpath,
                                     is_input, const)
            for component in [c for c in self.values(pub=False, recurse=False)
                                    if IComponent.providedBy(c)]:
                path = relpath
                if component.directory:
                    # Must use '/' for resources.
                    path += '/'+component.directory
                component._restore_files(package, path)
        finally:
            if self.directory:
                self.pop_dir()

    def _copy_files(self, pattern, pkg_files, package, relpath, is_input,
                    const, directory=None):
        """Copy files from installed egg matching pattern."""
        symlink = const and sys.platform != 'win32'
        if directory:
            self.push_dir(directory)
        try:
            found = False
            for filename in pkg_files:
                if fnmatch.fnmatch(filename, pattern):
                    # Must use '/' for resources.
                    src_name = relpath+'/'+filename
                    self.debug("    '%s'", src_name)
                    if symlink:
                        src = pkg_resources.resource_filename(package, src_name)
                        dst = filename
                        os.symlink(src, dst)
                    else:
                        src = pkg_resources.resource_stream(package, src_name)
                        dst = open(filename, 'w')
                        dst.write(src.read())
                        dst.close()
                    found = True
            if not found and is_input:
                self.warning("No files found for '%s'", pattern)
        finally:
            if directory:
                self.pop_dir()

    def step (self):
        """For Components that run other components (e.g., Assembly or Drivers),
        this will run one Component and return. For simple components, it is
        the same as run().
        """
        self.run()

    def stop (self):
        """Stop this component."""
        self._stop = True

    def invalidate_deps(self, vars, notify_parent=False):
        """Invalidate all of our outputs."""
        outs = [x for x in self._pub.values() if isinstance(x, Variable) and 
                                                 x.iostatus==OUTPUT and x.valid==True]
        for out in outs:
            #if __debug__: self._logger.debug('(component.invalidate_deps) invalidating %s' % out.get_pathname())
            out.valid = False
            
        if notify_parent and self.parent and len(outs) > 0:
            self.parent.invalidate_deps(outs, True)
        return outs    

    def update_outputs(self, outnames):
        """Do what is necessary to make the specified output Variables valid.
        For a simple Component, this will result in a run().
        """
        self.run()
        
# TODO: uncomment require_gradients and require_hessians after they're better thought out
    
    #def require_gradients (self, varname, gradients):
        #"""Requests that the component be able to provide (after execution) a
        #list of gradients w.r.t. a list of variables. The format
        #of the gradients list is [dvar_1, dvar_2, ..., dvar_n]. The component
        #should return a list with entries of either a name, a tuple of the
        #form (name,index) or None.  None indicates that the component cannot
        #compute the specified derivative. name indicates the name of a
        #scalar variable in the component that contains the gradient value, and
        #(name,index) indicates the name of an array variable and the index of
        #the entry containing the gradient value. If the component cannot
        #compute any gradients of the requested varname, it can just return
        #None.
        #"""
        #return None

    #def require_hessians (self, varname, deriv_vars):
        #""" Requests that the component be able to provide (after execution)
        #the hessian w.r.t. a list of variables. The format of
        #deriv_vars is [dvar_1, dvar_2, ..., dvar_n]. The component should
        #return one of the following:

            #1) a name, which would indicate that the component contains
               #a 2D array variable or matrix containing the hessian

            #2) an array of the form 

               #[[dx1dx1, dx1dx2, ... dx1dxn],
               #[           ...             ],
               #[dxndx1, dxndx2, ... dxndxn]]

               #with entries of either name, (name,index), or None. name
               #indicates that a scalar variable in the component contains the
               #desired hessian matrix entry. (name,index) indicates that
               #an array variable contains the value at the specified index.
               #If index is a list with two entries, that indicates that
               #the variable containing the entry is a 2d array or matrix.

            #3) None, which means the the component cannot compute any values
               #of the hessian.

             #"""
        #return None
    

def eggsecutable():
    """Unpack egg. Not in loader to avoid 2GB problems with zipimport."""
    install = os.environ.get('OPENMDAO_INSTALL', '1')
    if install:
        install = int(install)
    debug = os.environ.get('OPENMDAO_INSTALL_DEBUG', '1')
    if debug:
        debug = int(debug)
    if debug:
        logging.getLogger().setLevel(logging.DEBUG)
    try:
        Component.load_from_eggfile(sys.path[0], install=install)
    except Exception, exc:
        print str(exc)
        sys.exit(1)

