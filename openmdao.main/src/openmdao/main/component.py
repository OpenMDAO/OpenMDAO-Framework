
#public symbols
__all__ = ['Component', 'SimulationRoot']

__version__ = "0.1"

import glob
import os
import os.path
import copy

from zope.interface import implements

from openmdao.main.interfaces import IContainer, IComponent, IAssembly, IVariable, IDriver
from openmdao.main import Container, String, Variable
from openmdao.main.variable import INPUT, OUTPUT
from openmdao.main.constants import SAVE_PICKLE, SAVE_CPICKLE
from openmdao.main.filevar import FileVariable
from openmdao.main.util import filexfer

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
    
    def __init__(self, name, parent=None, doc=None, directory='', add_to_parent=True):
        super(Component, self).__init__(name, parent, doc, add_to_parent=add_to_parent)
        
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
                    "Illegal execution directory '%s', not a decendant of '%s'." \
                    % (dirpath, SimulationRoot.get_root()),
                    ValueError)
            if not os.path.exists(dirpath):
                try:
                    os.makedirs(dirpath)
                except OSError, exc:
                    self.raise_exception(
                        "Can't create execution directory '%s': %s" \
                        % (dirpath, exc.strerror), OSError)
            else:
                if not os.path.isdir(dirpath):
                    self.raise_exception(
                        "Execution directory path '%s' is not a directory." \
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

    def save_to_egg (self, name=None, version=None, force_relative=True,
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
        components.extend([c for c in self.values(pub=False, recurse=True)
                                if isinstance(c, Component)])
        for comp in sorted(components, reverse=True,
                           key=lambda comp: comp.get_pathname()):
#            self.debug('Saving %s', comp.get_pathname())

            # Process execution directory.
            comp_dir = comp.get_directory()
#            self.debug("    directory '%s'", comp.directory)
            if force_relative:
                if comp_dir.startswith(src_dir):
                    if comp_dir == src_dir and comp.directory:
                        fixup_dirs.append((comp, comp.directory))
                        comp.directory = ''
#                        self.debug("        directory now '%s'", comp.directory)
                    elif os.path.isabs(comp.directory):
                        parent_dir = comp.parent.get_directory()
                        fixup_dirs.append((comp, comp.directory))
                        comp.directory = self._relpath(comp_dir, parent_dir)
#                        self.debug("        directory now '%s'", comp.directory)
                else:
                    self.raise_exception(
                        "Can't save, %s directory '%s' doesn't start with '%s'." \
                        % (comp.get_pathname(), comp_dir, src_dir), ValueError)

            # Process external files.
            for metadata in comp.external_files:
                path = metadata['path']
#                self.debug('    external path %s', path)
                path = os.path.expanduser(path)
                path = os.path.expandvars(path)
                if not os.path.isabs(path):
                    path = os.path.join(comp_dir, path)
                paths = glob.glob(path)
                for path in paths:
#                    self.debug('    expanded path %s', path)
                    path = os.path.normpath(path)
                    if not os.path.exists(path):
#                        self.debug("        '%s' does not exist" % path)
                        continue
                    if force_relative:
                        if path.startswith(src_dir):
                            save_path = self._relpath(path, src_dir)
                            if os.path.isabs(metadata['path']):
                                path = self._relpath(path, comp_dir)
                                fixup_meta.append((metadata, metadata['path']))
                                metadata['path'] = path
#                                self.debug('        path now %s', path)
                        else:
                            self.raise_exception(
                                "Can't save, %s file '%s' doesn't start with '%s'."
                                % (comp.get_pathname(), path, src_dir),
                                ValueError)
                    else:
                        save_path = path
#                    self.debug('        adding %s', save_path)
                    src_files.add(save_path)

            # Process FileVariables for this component only.
            for fvar in comp.get_file_vars():
                path = fvar.get_value()
#                self.debug('    fvar %s path %s', fvar.name, path)
                if not path:
                    continue
                if not os.path.isabs(path):
                    path = os.path.join(comp_dir, path)
                path = os.path.normpath(path)
                if not os.path.exists(path):
#                    self.debug("        '%s' does not exist" % path)
                    continue
                if force_relative:
                    if path.startswith(src_dir):
                        save_path = self._relpath(path, src_dir)
                        if os.path.isabs(fvar.get_value()):
                            path = self._relpath(path, comp_dir)
                            fixup_fvar.append((fvar, fvar.get_value()))
                            fvar.set_value(path)
#                            self.debug('        path now %s', path)
                    else:
                        self.raise_exception(
                            "Can't save, %s path '%s' doesn't start with '%s'." \
                            % (fvar.get_pathname(), path, src_dir), ValueError)
                else:
                    save_path = path
#                self.debug('        adding %s', save_path)
                src_files.add(save_path)
        try:
            return super(Component, self).save_to_egg(name, version,
                                                      force_relative,
                                                      src_dir, src_files,
                                                      dst_dir, format, proto,
                                                      tmp_dir)
        finally:
            # If any component config has been modified, restore it.
            for comp, path in fixup_dirs:
                comp.directory = path
            for meta, path in fixup_meta:
                meta['path'] = path
            for fvar, path in fixup_fvar:
                fvar.set_value(path)

    def get_file_vars (self):
        """Return list of FileVariables owned by this component."""

        def _recurse_get_file_vars (container, file_vars, visited):
            """Scan both normal __dict__ and _pub."""
            objs = container.__dict__.values()
            objs.extend(container._pub.values())
            for obj in objs:
                if id(obj) in visited:
                    continue
                visited.append(id(obj))
                if isinstance(obj, FileVariable):
                    file_vars.add(obj)
                elif isinstance(obj, Component):
                    continue
                elif isinstance(obj, Variable):
                    continue
                elif isinstance(obj, Container):
                    _recurse_get_file_vars(obj, file_vars, visited)

        file_vars = set()
        visited = []
        _recurse_get_file_vars(self, file_vars, visited)
        return file_vars

    def _relpath (self, path1, path2):
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

    @staticmethod
    def load (instream, format=SAVE_CPICKLE, do_post_load=True):
        """Load object(s) from instream."""
# This doesn't work:
#    AttributeError: 'super' object has no attribute 'load'
#        top = super(Component).load(instream, format)
        top = Container.load(instream, format, False)

        if IComponent.providedBy(top):
            top.directory = os.getcwd()
            for component in [c for c in top.values(pub=False, recurse=True)
                                                if IComponent.providedBy(c)]:
                directory = component.get_directory()
                if not os.path.exists(directory):
                    os.makedirs(directory)

        if do_post_load:
            top.post_load()
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
    
    
