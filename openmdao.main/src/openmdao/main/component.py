
#public symbols
__all__ = ['Component', 'SimulationRoot']

__version__ = "0.1"

import glob
import os.path
import shutil
import subprocess
import sys
import time

from enthought.traits.api import implements, on_trait_change, Str, Missing, Undefined, TraitError
from enthought.traits.trait_base import not_event

from openmdao.main.interfaces import IComponent
from openmdao.main.container import Container, _io_side_effects
from openmdao.main.filevar import FileValue
from openmdao.main.constants import SAVE_CPICKLE
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
    """This is the base class for all objects containing Traits that are 
       accessible to the OpenMDAO framework and are 'runnable'.
    """

    implements(IComponent)
    
    directory = Str('',desc='If non-blank, the directory to execute in.', iostatus='in')
    
    def __init__(self, name=None, parent=None, doc=None, directory='',
                 add_to_parent=True):
        super(Component, self).__init__(name, parent, doc, add_to_parent)
        
        self.state = STATE_IDLE
        self._stop = False
        self._need_check_config = True
        self._execute_needed = True
        self.directory = directory
        
        self._dir_stack = []
        
        # List of meta-data dictionaries.
        self.external_files = []


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
        """Make preparations for execution. Overrides of this function must
        call this version.
        """
        if self._need_check_config:
            self.check_config()
            self._need_check_config = False
        
        if self.parent is None: # if parent is None, we're not part of an Assembly
                                # so Variable validity doesn't apply. Just execute.
            self._execute_needed = True
        else:
            invalid_ins = self.list_inputs(valid=False)
            if len(invalid_ins) > 0:
                #self.debug('updating inputs %s on %s' % (invalid_ins,self.get_pathname()))
                self._execute_needed |= self.parent.update_inputs(self.name,
                                    ['.'.join([self.name, n]) for n in invalid_ins])
    
                            
    def execute (self):
        """Perform calculations or other actions, assuming that inputs 
        have already been set. This should be overridden in derived classes.
        """
        pass
    
    def _post_execute (self):
        """Update output variables and anything else needed after execution. 
        Overrides of this function must call this version.
        """
        # make our Variables valid again
        for name in self.keys(iostatus='in'): # inputs
            self.set_valid(name, True)
        for name in self.keys(iostatus='out'): # outputs
            self.set_valid(name, True)
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

    def list_inputs(self, valid=None):
        """Return a list of names of input values. If valid is not None,
        the the list will contain names of inputs with matching validity.
        """
        if valid is None:
            return self.keys(iostatus='in')
        else:
            return [n for n in self.keys(iostatus='in') 
                         if self.get_valid(n)==valid]
        
    def list_outputs(self, valid=None):
        """Return a list of names of output values. If valid is not None,
        the the list will contain names of outputs with matching validity.
        """
        if valid is None:
            return self.keys(iostatus='out')
        else:
            return [n for n in self.keys(iostatus='out') 
                         if self.get_valid(n)==valid]
        
    def get_directory (self):
        """Return absolute path of execution directory."""
        path = self.directory
        if not os.path.isabs(path):
            if self.parent is not None and isinstance(self.parent, Component):
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

    def save_to_egg(self, name=None, version=None, force_relative=True,
                    src_dir=None, src_files=None, dst_dir=None,
                    format=SAVE_CPICKLE, proto=-1, tmp_dir=None):
        """Save state and other files to an egg.

        - `name` defaults to the name of the component.
        - `version` defaults to the component's module __version__.
        - If `force_relative` is True, all paths are relative to `src_dir`.
        - `src_dir` defaults to the component's directory.
        - `src_files` should be a set, and defaults to component's external files.
        - `dst_dir` is the directory to write the egg in.
        - `tmp_dir` is the directory to use for temporary files.

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
        components.extend([c for c in self.values(recurse=True)
                                if isinstance(c, Component)])
        for comp in sorted(components, reverse=True,
                           key=lambda comp: comp.get_pathname()):
            self.debug('Saving %s', comp.get_pathname())

            # Process execution directory.
            comp_dir = comp.get_directory()
            self.debug("    directory '%s'", comp.directory)
            if force_relative:
                if comp_dir.startswith(src_dir):
                    if comp_dir == src_dir and comp.directory:
                        fixup_dirs.append((comp, comp.directory))
                        comp.directory = ''
                        self.debug("        directory now '%s'", comp.directory)
                    elif os.path.isabs(comp.directory):
                        parent_dir = comp.parent.get_directory()
                        fixup_dirs.append((comp, comp.directory))
                        comp.directory = self._relpath(comp_dir, parent_dir)
                        self.debug("        directory now '%s'", comp.directory)
                else:
                    self.raise_exception(
                        "Can't save, %s directory '%s' doesn't start with '%s'."
                        % (comp.get_pathname(), comp_dir, src_dir), ValueError)

            # Process external files.
            for metadata in comp.external_files:
                path = metadata['path']
                self.debug('    external path %s', path)
                path = os.path.expanduser(path)
                path = os.path.expandvars(path)
                if not os.path.isabs(path):
                    path = os.path.join(comp_dir, path)
                paths = glob.glob(path)
                for path in paths:
                    self.debug('    expanded path %s', path)
                    path = os.path.normpath(path)
                    if not os.path.exists(path):
                        self.debug("        '%s' does not exist" % path)
                        continue
                    if force_relative:
                        if path.startswith(src_dir):
                            save_path = self._relpath(path, src_dir)
                            if os.path.isabs(metadata['path']):
                                path = self._relpath(path, comp_dir)
                                fixup_meta.append((metadata, metadata['path']))
                                metadata['path'] = path
                                self.debug('        path now %s', path)
                        else:
                            self.raise_exception(
                                "Can't save, %s file '%s' doesn't start with '%s'."
                                % (comp.get_pathname(), path, src_dir),
                                ValueError)
                    else:
                        save_path = path
                    self.debug('        adding %s', save_path)
                    src_files.add(save_path)

            # Process FileTraits for this component only.
            for fvar, fvarvalue in comp.get_file_vars():
                self.debug('    fvar %s path %s', fvar, fvarvalue.filename)
                if not fvarvalue:
                    continue
                path = fvarvalue.filename
                if not os.path.isabs(path):
                    path = os.path.join(comp_dir, path)
                path = os.path.normpath(path)
                if not os.path.exists(path):
                    self.debug("        '%s' does not exist" % path)
                    continue
                if force_relative:
                    if path.startswith(src_dir):
                        save_path = self._relpath(path, src_dir)
                        if os.path.isabs(fvarvalue.filename):
                            path = self._relpath(path, comp_dir)
                            fixup_fvar.append((comp, fvar, fvarvalue))
                            comp.set(fvar+'.filename', path, force=True)
                            self.debug('        path now %s', path)
                    else:
                        self.raise_exception(
                            "Can't save, %s path '%s' doesn't start with '%s'."
                            % ('.'.join([comp.get_pathname(),
                                         fvar]), path, src_dir), ValueError)
                else:
                    save_path = path
                self.debug('        adding %s', save_path)
                src_files.add(save_path)
        try:
            return super(Component, self).save_to_egg(name, version,
                                                      force_relative,
                                                      src_dir, src_files,
                                                      dst_dir, format, proto,
                                                      tmp_dir)
        finally:
            # If any component config has been modified, restore it.
            for ccomp, path in fixup_dirs:
                ccomp.directory = path
            for meta, path in fixup_meta:
                meta['path'] = path
            for comp, name, fvar in fixup_fvar:
                comp.set(name+'.filename', fvar.filename, force=True)

    def get_file_vars(self):
        """Return list of (filevarname,filevarvalue) owned by this component."""
        def _recurse_get_file_vars (container, file_vars, visited, scope):
            for name, obj in container.items(type=not_event):
                #obj = getattr(container, name)
                if id(obj) in visited:
                    continue
                visited.append(id(obj))
                if isinstance(obj, FileValue):
                    if self is scope:
                        file_vars.add((name, obj))
                    else:
                        file_vars.add(('.'.join(
                                      [container.get_pathname(rel_to_scope=scope),name]), 
                                       obj))
                # TODO: find out why we're recursing into child Containers,
                # but not into child Components
                elif isinstance(obj, Container) and not isinstance(obj, Component):
                    _recurse_get_file_vars(obj, file_vars, visited, scope)

        file_vars = set()
        visited = []
        _recurse_get_file_vars(self, file_vars, visited, self)
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

    def check_save_load(self, test_dir='test_dir', cleanup=True,
                        format=SAVE_CPICKLE):
        """Convenience routine to check that saving & reloading work."""
        old_level = self.log_level
        self.log_level = LOG_DEBUG
        start = time.time()
        egg_name = self.save_to_egg(format=format)
        elapsed = time.time() - start
        size = os.path.getsize(egg_name)
        print '\nSaved %d bytes in %.2f seconds (%.2f bytes/sec)' % \
              (size, elapsed, size/elapsed)

        if sys.platform == 'win32':
            print '\nUnpacking check not supported on win32'
            return -1

        orig_dir = os.getcwd()
        if os.path.exists(test_dir):
            shutil.rmtree(test_dir)
        os.mkdir(test_dir)
        os.chdir(test_dir)
        egg_path = os.path.join('..', egg_name)
        try:
            print '\nUnpacking in subprocess...'
            env = os.environ
            env['OPENMDAO_INSTALL'] = '0'
            retcode = subprocess.call(['sh', egg_path], env=env)
            print '    retcode', retcode
            if retcode == 0:
                print '\nRunning in subprocess...'
                retcode = subprocess.call(['python', self.name+'_loader.py'])
                print '    retcode', retcode
        finally:
            os.chdir(orig_dir)
            self.log_level = old_level
            if cleanup:
                os.remove(egg_name)
                shutil.rmtree(test_dir)

        return retcode

    @staticmethod
    def load (instream, format=SAVE_CPICKLE, do_post_load=True):
        """Load object(s) from instream."""
# This doesn't work:
#    AttributeError: 'super' object has no attribute 'load'
#        top = super(Component).load(instream, format)
        top = Container.load(instream, format, False)

        _io_side_effects = False
        try:
            if isinstance(top, Component):
                top.directory = os.getcwd()
                for component in [c for c in top.values(recurse=True)
                                                    if isinstance(c, Component)]:
                    directory = component.get_directory()
                    if not os.path.exists(directory):
                        os.makedirs(directory)
    
            if do_post_load:
                top.post_load()
        finally:
            _io_side_effects = True
            
        return top

    def step (self):
        """For Components that run other components (e.g., Assembly or Drivers), this will run
        one Component and return. For simple components, it is the same as run().
        """
        self.run()

    def stop (self):
        """Stop this component."""
        self._stop = True

    def invalidate_deps(self, vars, notify_parent=False):
        """Invalidate all of our valid outputs."""
        valid_outs = self.list_outputs(valid=True)
        
        for var in vars:
            self.set_valid(var, False)
            
        if notify_parent and self.parent and len(valid_outs) > 0:
            self.parent.invalidate_deps(['.'.join([self.name,n]) for n in valid_outs], 
                                        notify_parent)
        for out in valid_outs:
            self._valid_dict[out] = False
            
        return valid_outs    

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
    
    
