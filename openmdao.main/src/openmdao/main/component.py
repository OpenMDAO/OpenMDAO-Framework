
#public symbols
__all__ = ['Component', 'SimulationRoot']


import fnmatch
import glob
import logging
import os.path
from os.path import isabs, isdir, dirname, exists, join, normpath, relpath
import pkg_resources
import sys
import weakref

from enthought.traits.trait_base import not_event, not_none
from enthought.traits.api import Bool, List, Str, Instance, TraitError, on_trait_change

from openmdao.main.container import Container
from openmdao.main.filevar import FileMetadata, FileRef
from openmdao.main.expression import Expression, ExpressionList
from openmdao.util.eggsaver import SAVE_CPICKLE
from openmdao.util.eggobserver import EggObserver


class SimulationRoot (object):
    """Singleton object used to hold root directory."""

    # Execution root directory. Root of all legal file paths.
    __root = None

    @staticmethod
    def chroot (path):
        """Change to directory `path` and set the singleton's root.
        Normally not called but useful in special situations."""
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
        """Return True if `path` is legal (descendant of our root)."""
        if SimulationRoot.__root is None:
            SimulationRoot.__root = os.getcwd()
        return path.startswith(SimulationRoot.__root)
    

class DirectoryContext(object):
    """Supports using the 'with' statement in place of try-finally for
    :meth:`self.push_dir` and subsequent :meth:`self.pop_dir`."""

    def __init__(self, component):
        self.component = weakref.ref(component)

    def __enter__(self):
        return self.component().push_dir()

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.component().pop_dir()

    def __getstate__(self):
        state = self.__dict__.copy()
        state['component'] = self.component()
        return state

    def __setstate__(self, state):
        self.__dict__.update(state)
        self.component = weakref.ref(self.component)


class Component (Container):
    """This is the base class for all objects containing Traits that are \
    accessible to the OpenMDAO framework and are "runnable."

    - If `create_instance_dir` is True, then this component needs a unique \
      per-instance execution directory.  The created directory is filled \
      based on `external_files` and `directory` (which should contain the \
      required files).
    - `directory` is a string specifying the directory to execute in. \
      If it is a relative path, it is relative to its parent's directory.
    - `external_files` is a list of :class:`FileMetadata` objects for external \
      files used by the component.  The *path* attribute can be a \
      :mod:`glob`-style pattern.
    """

    create_instance_dir = Bool(False, desc='If True, create a unique'
                               ' per-instance execution directory',
                               iotype='in')
    directory = Str('', desc='If non-blank, the directory to execute in.', 
                    iotype='in')
    external_files = List(FileMetadata)
        
    def __init__(self, doc=None, directory=''):
        super(Component, self).__init__(doc)
        
        # contains validity flag for each io Trait
        self._valid_dict = dict([(name,False) for name,t in self.class_traits().items() if t.iotype])
        
        self._stop = False
        self._call_check_config = True
        self._call_execute = True
        
        self._input_names = None
        self._output_names = None
        self._container_names = None
        
        if directory:
            self.directory = directory
        
        self._dir_stack = []
        self._dir_context = None

    @property
    def dir_context(self):
        """Returns the :class:`DirectoryContext` for this component."""
        if self._dir_context is None:
            self._dir_context = DirectoryContext(self)
        return self._dir_context

    def _input_changed(self, name):
        if self.get_valid(name):  # if var is not already invalid
            self.invalidate_deps([name], notify_parent=True)
        
    def check_config (self):
        """Verify that this component is fully configured to execute.
        This function is called once prior to the first execution of this
        component and may be called explicitly at other times if desired. 
        Classes that override this function must still call the base class
        version.
        """
        for name, value in self._traits_meta_filter(required=True).items():
            if value.is_trait_type(Instance) and getattr(self, name) is None:
                self.raise_exception("required plugin '%s' is not present" %
                                     name, TraitError)
    
    def tree_rooted(self):
        """Calls the base class version of *tree_rooted()*, checks our
        directory for validity, and creates the directory if it doesn't exist.
        """
        super(Component, self).tree_rooted()

        if self.create_instance_dir:
            # Create unique subdirectory of parent based on our name.
            parent_dir = self.parent.get_abs_directory()
            new_name = self.name
            new_suffix = ''
            counter = 1
            new_dir = ''.join((new_name, new_suffix))
            path = os.path.join(parent_dir, new_dir)
            while os.path.exists(path):
                counter += 1
                new_suffix = '_%d' % counter
                new_dir = ''.join((new_name, new_suffix))
                path = os.path.join(parent_dir, new_dir)
            try:
                os.makedirs(path)
            except OSError, exc:
                self.raise_exception("Can't create execution directory '%s': %s"
                                     % (path, exc.strerror), OSError)
            
            # Populate with external files from config directory.
            config_dir = self.directory
            self.directory = new_dir
            try:
                self._restore_files(config_dir, '', [], from_egg=False)
            except Exception:
                self.directory = config_dir
                raise
            self.create_instance_dir = False

        elif self.directory:
            path = self.get_abs_directory()
            if not exists(path):
                # Make sure it's legal path before creating.
                self.check_path(path)
                try:
                    os.makedirs(path)
                except OSError, exc:
                    self.raise_exception(
                        "Can't create execution directory '%s': %s"
                        % (path, exc.strerror), OSError)
            else:
                self.check_path(path, check_dir=True)

    def _pre_execute (self):
        """Prepares for execution by calling *tree_rooted()* and *check_config()* if
        their "dirty" flags are set, and by requesting that the parent Assembly
        update this Component's invalid inputs.
        
        Overrides of this function must call this version.
        """
        if self._call_tree_rooted:
            self.tree_rooted()
            
        if self._call_check_config:
            self.check_config()
            self._call_check_config = False
        
        if self.parent is None: # if parent is None, we're not part of an Assembly
                                # so Variable validity doesn't apply. Just execute.
            self._call_execute = True
            for name in self.list_inputs():
                self.set_valid(name, True)
        else:
            invalid_ins = self.list_inputs(valid=False)
            if len(invalid_ins) > 0:
                self._call_execute = True
                name = self.name
                self.parent.update_inputs(['.'.join([name, n]) for n in invalid_ins])
                valids = self._valid_dict
                for name in invalid_ins:
                    valids[name] = True
            elif self._call_execute == False and len(self.list_outputs(valid=False)):
                self._call_execute = True

    def execute (self):
        """Perform calculations or other actions, assuming that inputs 
        have already been set. This must be overridden in derived classes.
        """
        raise NotImplementedError('%s.execute' % self.get_pathname())
    
    def _post_execute (self):
        """Update output variables and anything else needed after execution. 
        Overrides of this function must call this version.
        """
        # make our output Variables valid again
        for name in self.list_outputs():
            self.set_valid(name, True)
        self._call_execute = False
        
    def run (self, force=False):
        """Run this object. This should include fetching input variables,
        executing, and updating output variables. Do not override this function.
        """
        if self.directory:
            self.push_dir()

        self._stop = False
        try:
            self._pre_execute()
            if self._call_execute or force:
                #if __debug__: self._logger.debug('execute %s' % self.get_pathname())
                self.execute()
                self._post_execute()
        finally:
            if self.directory:
                self.pop_dir()
 
    def add(self, name, obj):
        """Override of base class version to force call to *check_config* after
        any child containers are added.
        Returns the added Container object.
        """
        self._config_changed()
        return super(Component, self).add(name, obj)
        
    def remove_container(self, name):
        """Override of base class version to force call to *check_config* after
        any child containers are removed.
        """
        obj = super(Component, self).remove_container(name)
        self._config_changed()
        return obj

    def add_trait(self, name, trait):
        """Overrides base definition of add_trait in order to
        force call to *check_config* prior to execution when new traits are
        added.
        """
        super(Component, self).add_trait(name, trait)
        self._config_changed()
        if trait.iotype:
            self._valid_dict[name] = False
        
    def remove_trait(self, name):
        """Overrides base definition of add_trait in order to
        force call to *check_config* prior to execution when a trait is
        removed.
        """
        super(Component, self).remove_trait(name)
        self._config_changed()
        try:
            del self._valid_dict[name]
        except KeyError:
            pass

    def is_valid(self):
        """Return False if any of our public variables is invalid."""
        if self._call_execute:
            return False
        for val in self._valid_dict.values():
            if val is False:
                return False
        return True

    def _config_changed(self):
        """Call this whenever the configuration of this Component changes,
        for example, children are added or removed.
        """
        self._input_names = None
        self._output_names = None
        self._container_names = None
        self._call_check_config = True

    def list_inputs(self, valid=None):
        """Return a list of names of input values. If valid is not None,
        the the list will contain names of inputs with matching validity.
        """
        if self._input_names is None:
            self._input_names = self.keys(iotype='in')
            
        if valid is None:
            return self._input_names
        else:
            fval = self.get_valid
            return [n for n in self._input_names if fval(n)==valid]
        
    def list_outputs(self, valid=None):
        """Return a list of names of output values. If valid is not None,
        the the list will contain names of outputs with matching validity.
        """
        if self._output_names is None:
            self._output_names = self.keys(iotype='out')
            
        if valid is None:
            return self._output_names
        else:
            fval = self.get_valid
            return [n for n in self._output_names if fval(n)==valid]
        
    def list_containers(self):
        """Return a list of names of child Containers."""
        if self._container_names is None:
            self._container_names = [n for n,v in self.items() 
                                                   if isinstance(v,Container)]            
        return self._container_names
    
    def get_expr_names(self, iotype=None):
        """Return a list of names of all Expression and ExpressionList traits
        in this instance.
        """
        if iotype is None:
            checker = not_none
        else:
            checker = iotype
        
        return [n for n,v in self._traits_meta_filter(iotype=checker).items() 
                    if v.is_trait_type(Expression) or 
                       v.is_trait_type(ExpressionList)]
    
    def get_expr_depends(self):
        """Returns a list of tuples of the form (src_comp_name, dest_comp_name)
        for each dependency introduced by any *input* Expression or ExpressionList 
        traits in this Component.
        """
        conn_list = []
        exprs = self.get_expr_names()
        selfname = self.name
        for name in exprs:
            exprobj = getattr(self, name)
            if isinstance(exprobj, basestring): # a simple Expression
                cnames = exprobj.get_referenced_compnames()
            else:  # an ExpressionList
                cnames = []
                for entry in exprobj:
                    cnames += entry.get_referenced_compnames()
            if self.trait(name).iotype == 'in':
                for cname in cnames:
                    conn_list.append((cname, selfname))
            else:
                for cname in cnames:
                    conn_list.append((selfname, cname))
                
        return conn_list

    def check_path(self, path, check_dir=False):
        """Verify that the given path is a directory and is located
        within the allowed area (somewhere within the simulation root path).
        """
# pylint: disable-msg=E1101
        if not SimulationRoot.legal_path(path):
            self.raise_exception("Illegal path '%s', not a descendant of '%s'."
                                 % (path, SimulationRoot.get_root()),
                                 ValueError)
        elif check_dir and not isdir(path):
            self.raise_exception(
                "Execution directory path '%s' is not a directory."
                % path, ValueError)
# pylint: enable-msg=E1101
        return path
    
    def get_abs_directory (self):
        """Return absolute path of execution directory."""
        path = self.directory
        if not isabs(path):
            if self._call_tree_rooted:
                self.raise_exception("can't call get_abs_directory before hierarchy is defined",
                                     RuntimeError)
            if self.parent is not None and isinstance(self.parent, Component):
                parent_dir = self.parent.get_abs_directory()
            else:
                parent_dir = SimulationRoot.get_root()
            path = join(parent_dir, path)
            
        return path

    def push_dir (self, directory=None):
        """Change directory to dir, remembering the current directory for a
        later :meth:`pop_dir`. Returns the new absolute directory path."""
        if not directory:
            directory = self.get_abs_directory()
        cwd = os.getcwd()
        if not isabs(directory):
            directory = join(self.get_abs_directory(), directory)
        self.check_path(directory, True)
        try:
            os.chdir(directory)
        except OSError, err:
            self.raise_exception("Can't push_dir '%s': %s" % (directory, err),
                                 OSError)
        self._dir_stack.append(cwd)
        return directory

    def pop_dir (self):
        """Return to previous directory saved by :meth:`push_dir`."""
        try:
            newdir = self._dir_stack.pop()
        except IndexError:
            self.raise_exception('Called pop_dir() with nothing on the dir stack',
                                 IndexError)
        os.chdir(newdir)

    def checkpoint (self, outstream, fmt=SAVE_CPICKLE):
        """Save sufficient information for a restart. By default, this
        just calls *save()*.
        """
        self.save(outstream, fmt)

    def restart (self, instream):
        """Restore state using a checkpoint file. The checkpoint file is
        typically a delta from a full saved state file. If checkpoint is
        overridden, this should also be overridden.
        """
        self.load(instream)

    def save_to_egg(self, name, version, py_dir=None, require_relpaths=True,
                    child_objs=None, dst_dir=None, fmt=SAVE_CPICKLE,
                    proto=-1, use_setuptools=False, observer=None):
        """Save state and other files to an egg.  Typically used to copy all or
        part of a simulation to another user or machine.  By specifying child
        components in `child_objs`, it will be possible to create instances of
        just those components from the installed egg.  Child component names
        should be specified relative to this component.

        - `name` must be an alphanumeric string.
        - `version` must be an alphanumeric string.
        - `py_dir` is the (root) directory for local Python files. \
          It defaults to the current directory.
        - If `require_relpaths` is True, any path (directory attribute,
          external file, or file trait) which cannot be made relative to this \
          component's directory will raise ValueError. Otherwise such paths \
          generate a warning and the file is skipped.
        - `child_objs` is a list of child objects for additional entry points.
        - `dst_dir` is the directory to write the egg in.
        - `fmt` and `proto` are passed to :meth:`eggsaver.save`.
        - `use_setuptools` is passed to :meth:`eggsaver.save_to_egg`.
        - `observer` will be called via an :class:`EggObserver`.

        After collecting files and possibly modifying their paths, this
        calls :meth:`container.save_to_egg`.
        Returns ``(egg_filename, required_distributions, orphan_modules)``.
        """
        observer = EggObserver(observer, self._logger)

        src_dir = self.get_abs_directory()
        src_files = set()

        fixup_dirs = []  # These are used to restore original component config.
        fixup_meta = []
        fixup_fvar = []

        # Process all components in bottom-up order.
        # We have to check relative paths like '../somedir' and if
        # we do that after adjusting a parent, things can go bad.
        components = [self]
        components.extend([obj for obj in self.values(recurse=True)
                                       if isinstance(obj, Component)])
        try:
            for comp in sorted(components, reverse=True,
                               key=lambda comp: comp.get_pathname()):
                try:
                    comp_dir = comp.get_abs_directory()
                    self._fix_directory(comp, comp_dir, src_dir,
                                        require_relpaths, fixup_dirs)
                    self._fix_external_files(comp, comp_dir, src_dir,
                                             require_relpaths, fixup_meta,
                                             src_files)
                    self._fix_file_vars(comp, comp_dir, src_dir,
                                        require_relpaths, fixup_fvar, src_files)
                except Exception, exc:
                    observer.exception(str(exc))
                    raise

            # Save relative directory for any entry points. Some oddness with
            # parent weakrefs seems to prevent reconstruction in load().
            if child_objs is not None:
                for child in child_objs:
                    if not isinstance(child, Component):
                        continue
                    rel_path = child.directory
                    obj = child.parent
                    if obj is None:
                        msg = 'Entry point object has no parent!'
                        observer.exception(msg)
                        raise RuntimeError(msg)
                    while obj.parent is not None and \
                          isinstance(obj.parent, Component):
                        rel_path = join(obj.directory, rel_path)
                        obj = obj.parent
                    child._rel_dir_path = rel_path

            return super(Component, self).save_to_egg(
                       name, version, py_dir, src_dir, src_files,
                       child_objs, dst_dir, fmt, proto, use_setuptools,
                       observer.observer)
        finally:
            # If any component config has been modified, restore it.
            for comp, path in fixup_dirs:
                comp.directory = path
            for meta, path in fixup_meta:
                meta.path = path
            for comp, name, path in fixup_fvar:
                comp.set(name+'.path', path, force=True)

    def _fix_directory(self, comp, comp_dir, root_dir, require_relpaths,
                       fixup_dirs):
        """Ensure execution directory for `comp` is in relative form."""
        if comp_dir.startswith(root_dir):
            if comp_dir == root_dir and comp.directory:
                fixup_dirs.append((comp, comp.directory))
                comp.directory = ''
            elif isabs(comp.directory):
                parent_dir = comp.parent.get_abs_directory()
                fixup_dirs.append((comp, comp.directory))
                comp.directory = relpath(comp_dir, parent_dir)
                self._logger.debug("    %s.directory reset to '%s'", 
                           comp.name, comp.directory)
        elif require_relpaths:
            self.raise_exception(
                "Can't save, %s directory '%s' doesn't start with '%s'."
                % (comp.get_pathname(), comp_dir, root_dir), ValueError)

        else:
            self._logger.warning("%s directory '%s' can't be made relative to '%s'.",
                         comp.get_pathname(), comp_dir, root_dir)

    def _fix_external_files(self, comp, comp_dir, root_dir, require_relpaths,
                            fixup_meta, src_files):
        """Ensure external files for `comp` are in relative form and update
        src_files to include all matches."""
        for metadata in comp.external_files:
            path = metadata.path
            if not path:
                continue
            if not isabs(path):
                path = join(comp_dir, path)
            path = normpath(path)
            if path.startswith(root_dir):
                if isabs(metadata.path):
                    new_path = relpath(path, comp_dir)
                    fixup_meta.append((metadata, metadata.path))
                    metadata.path = new_path
                for path in glob.glob(path):
                    src_files.add(relpath(path, root_dir))
            elif require_relpaths:
                self.raise_exception(
                    "Can't save, %s file '%s' doesn't start with '%s'."
                    % (comp.get_pathname(), path, root_dir), ValueError)
            else:
                self._logger.warning("%s file '%s' can't be made relative to '%s'.",
                             comp.get_pathname(), path, root_dir)

    def _fix_file_vars(self, comp, comp_dir, root_dir, require_relpaths,
                       fixup_fvar, src_files):
        """Ensure File traits for `comp` are in relative form and add to
        src_files."""
        for fvarname, fvar, ftrait in comp.get_file_vars():
            if fvar.owner is not comp:
                continue
            path = fvar.path
            if not path:
                continue
            if not isabs(path):
                path = join(comp_dir, path)
            path = normpath(path)
            if path.startswith(root_dir):
                if exists(path):
                    src_files.add(relpath(path, root_dir))
                if isabs(fvar.path):
                    path = relpath(path, comp_dir)
                    fixup_fvar.append((comp, fvarname, fvar.path))
                    comp.set(fvarname+'.path', path, force=True)
            elif require_relpaths:
                self.raise_exception(
                    "Can't save, %s path '%s' doesn't start with '%s'."
                    % ('.'.join((comp.get_pathname(), fvarname)),
                       path, root_dir), ValueError)
            else:
                self._logger.warning("%s path '%s' can't be made relative to '%s'.",
                             '.'.join((comp.get_pathname(), fvarname)),
                             path, root_dir)

    def get_file_vars(self):
        """Return list of (filevarname,filevarvalue,file trait) owned by this
        component."""

        def _recurse_get_file_vars(container, file_vars, visited, scope):
            for name, obj in container.items(type=not_event):
                if id(obj) in visited:
                    continue
                visited.add(id(obj))
                if isinstance(obj, FileRef):
                    ftrait = container.trait(name)
                    if self is scope:
                        file_vars.append((name, obj, ftrait))
                    else:
                        rel_path = container.get_pathname(rel_to_scope=scope)
                        file_vars.append(('.'.join((rel_path, name)),
                                          obj, ftrait))
                elif isinstance(obj, Container) and \
                     not isinstance(obj, Component):
                    _recurse_get_file_vars(obj, file_vars, visited, scope)

        file_vars = []
        _recurse_get_file_vars(self, file_vars, set(), self)
        return file_vars

    @staticmethod
    def load(instream, fmt=SAVE_CPICKLE, package=None,
             call_post_load=True, top_obj=True, name='', observer=None):
        """Load object(s) from `instream`.  If `instream` is an installed
        package name, then any external files referenced in the object(s)
        are copied from the package installation to appropriate directories.
        If an external file has metadata attribute 'constant' == True and
        the machine supports it, a symlink is used rather than a file copy.
        The `package` and `top_obj` arguments are normally used by a loader
        script (generated by :meth:`save_to_egg`) to load a sub-component from
        the egg.  `name` is set when creating an instance via a factory.
        In this case, external files are copied to a `name` directory and the
        component's directory attribute set accordingly.  Existing files
        are not overwritten.
        """
        observer = EggObserver(observer, logging.getLogger())
        try:
            top = Container.load(instream, fmt, package, False, name=name)
        except Exception, exc:
            observer.exception(str(exc))
            raise

        observer.logger = top._logger
        if isinstance(top, Component):
            # Get path relative to real top before we clobber directory attr.
            if top_obj:
                rel_path = '.'
            else:
                if top.parent is not None:
                    rel_path = top.directory
                    obj = top.parent
                    while obj.parent is not None and \
                          isinstance(obj.parent, Component):
                        rel_path = join(obj.directory, rel_path)
                        obj = obj.parent
                elif top.trait('_rel_dir_path'):
                    top.warning('No parent, using saved relative directory')
                    rel_path = top._rel_dir_path  # Set during save_to_egg().
                else:
                    top.warning('No parent, using null relative directory')
                    rel_path = ''

            # Set top directory.
            orig_dir = os.getcwd()
            if name:
                top.name = name
                # New instance via create(name) gets new directory.
                if not exists(name):
                    os.mkdir(name)
                os.chdir(name)
            # TODO: (maybe) Seems like we should make top.directory relative
            # here # instead of absolute, but it doesn't work...
            #top.directory = relpath(os.getcwd(), SimulationRoot.get_root())
            top.directory = os.getcwd()
            
            try:
                # Create any missing subdirectories.
                for component in [c for c in top.values(recurse=True)
                                          if isinstance(c, Component)]:
                    directory = component.get_abs_directory()
                    if not exists(directory):
                        os.makedirs(directory)

                # If necessary, copy files from installed egg.
                if isinstance(instream, basestring) and \
                   not exists(instream) and not isabs(instream):
                    # If we got this far, then the stuff below "can't" fail.
                    if not package:
                        dot = instream.rfind('.')
                        package = instream[:dot]
                    top._restore_files(package, rel_path, [], observer=observer)
            finally:
                os.chdir(orig_dir)
                if name and not glob.glob(join(name, '*')):
                    # Cleanup unused directory.
                    os.rmdir(name)
                    top.directory = ''
                    
        if call_post_load:
            top.post_load()

        observer.complete(name)
        return top

    def _restore_files(self, package, rel_path, file_list, do_copy=True,
                       observer=None, from_egg=True):
        """Restore external files from installed egg or config directory."""
        with self.dir_context:
            fvars = self.get_file_vars()
            if self.external_files or fvars:
                self._logger.info('Checking files in %s', os.getcwd())

            for metadata in self.external_files:
                pattern = metadata.path
                if pattern:
                    is_input = getattr(metadata, 'input', False)
                    const = getattr(metadata, 'constant', False)
                    binary = getattr(metadata, 'binary', False)
                    self._list_files(pattern, package, rel_path, is_input,
                                     const, binary, file_list, from_egg)

            for fvarname, fvar, ftrait in fvars:
                path = fvar.path
                if path:
                    is_input = ftrait.iotype == 'in'
                    self._list_files(path, package, rel_path, is_input, False,
                                     ftrait.binary, file_list, from_egg)
            if from_egg:
                for component in [c for c in self.values(recurse=False)
                                          if isinstance(c, Component)]:
                    path = rel_path
                    if component.directory:
                        # Always use '/' for resources.
                        path += '/'+component.directory
                    component._restore_files(package, path, file_list,
                                             do_copy=False)
            if do_copy:
                # Only copy once we've gotten the complete list.
                self._copy_files(package, file_list, observer, from_egg)

    def _list_files(self, pattern, package, rel_path, is_input, const, binary,
                    file_list, from_egg):
        """List files from installed egg or config dir matching pattern."""
        symlink = const and sys.platform != 'win32'
        sep = '/' if from_egg else os.sep

        directory = dirname(pattern)
        pattern = os.path.basename(pattern)
        if directory:
            if not exists(directory):
                os.makedirs(directory)
            rel_path = ''.join((rel_path, sep, directory))

        rel_path = normpath(rel_path)
        if from_egg:
            if not pkg_resources.resource_exists(package, rel_path):
                return
            if not pkg_resources.resource_isdir(package, rel_path):
                return
            pkg_files = pkg_resources.resource_listdir(package, rel_path)
        else:
            pkg_files = os.listdir(os.path.join(package, rel_path))

        if directory:
            self.push_dir(directory)
        cwd = os.getcwd()
        try:
            found = False
            for filename in pkg_files:
                if fnmatch.fnmatch(filename, pattern):
                    found = True
                    if exists(filename):
                        # Don't overwrite existing files (reloaded instance).
                        self._logger.debug("    '%s' exists", filename)
                        continue

                    src_name = ''.join((rel_path, sep, filename))
                    if from_egg:
                        src_path = pkg_resources.resource_filename(package,
                                                                   src_name)
                    else:
                        src_path = os.path.join(package, src_name)
                    size = os.path.getsize(src_path)
                    if symlink:
                        mode = 'symlink'
                    else:
                        mode = 'wb' if binary else 'w'
                    file_list.append((src_name, mode, size,
                                      os.path.join(cwd, filename)))
            if not found and is_input:
                self._logger.warning("No files found for '%s'", pattern)
        finally:
            if directory:
                self.pop_dir()

    def _copy_files(self, package, file_list, observer, from_egg):
        """Copy/symlink files in `file_list`."""
        total_files = float(len(file_list))
        total_bytes = 0.
        for i, info in enumerate(file_list):
            src_name, mode, size, dst_name = info
            total_bytes += size

        dst_dir = ''
        completed_bytes = 0.
        for i, info in enumerate(file_list):
            src_name, mode, size, dst_name = info
            if dirname(dst_name) != dst_dir:
                dst_dir = dirname(dst_name)
                self._logger.info('Restoring files in %s', dst_dir)
            if observer is not None:
                observer.copy(src_name, i/total_files,
                              completed_bytes/total_bytes)
            if mode == 'symlink':
                if from_egg:
                    src_path = pkg_resources.resource_filename(package,
                                                               src_name)
                else:
                    src_path = os.path.join(package, src_name)
                os.symlink(src_path, dst_name)
            else:
                if from_egg:
                    src = pkg_resources.resource_stream(package, src_name)
                else:
                    src_mode = 'rb' if 'b' in mode else 'r'
                    src = open(os.path.join(package, src_name), src_mode)
                dst = open(dst_name, mode)
                chunk = 1 << 20  # 1MB
                data = src.read(chunk)
                while data:
                    dst.write(data)
                    data = src.read(chunk)
                src.close()
                dst.close()
            completed_bytes += size

    def step (self):
        """For Components that run other components (e.g., Assembly or Drivers),
        this will run one Component and return. For simple components, it is
        the same as *run()*.
        """
        self.run()

    def stop (self):
        """Stop this component."""
        self._stop = True

    def get_valid(self, name):
        """Get the value of the validity flag for the io trait with the given
        name.
        """
        try:
            return self._valid_dict[name]
        except KeyError:
            self.raise_exception(
                "cannot get valid flag of '%s' because it's not "
                "an io trait." % name, RuntimeError)
            
        #valid = self._valid_dict.get(name, Missing)
        #if valid is Missing:
            #trait = self.trait(name)
            #if trait and trait.iotype:
                #self._valid_dict[name] = False
                #return False
            #else:
                #self.raise_exception(
                    #"cannot get valid flag of '%s' because it's not "
                    #"an io trait." % name, RuntimeError)
        #return valid
    
    def get_valids(self, names):
        """Get a list of validity flags for the io traits with the given
        names.
        """
        return [self.get_valid(v) for v in names]

    def set_valid(self, name, valid):
        """Mark the io trait with the given name as valid or invalid."""
        try:
            self._valid_dict[name] = valid
        except KeyError:
            self.raise_exception(
                "cannot set valid flag of '%s' because "
                "it's not an io trait." % name, RuntimeError)
            
        #if name in self._valid_dict:
            #self._valid_dict[name] = valid
        #else:
            #trait = self.trait(name)
            #if trait and trait.iotype:
                #self._valid_dict[name] = valid
            #else:
                #self.raise_exception(
                    #"cannot set valid flag of '%s' because "
                    #"it's not an io trait." % name, RuntimeError)

    def invalidate_deps(self, varlist, notify_parent=False):
        """Invalidate all of our valid outputs."""
        valid_outs = self.list_outputs(valid=True)
        
        self._call_execute = True
        
        for var in varlist:
            self.set_valid(var, False)
            
        if notify_parent and self.parent and len(valid_outs) > 0:
            self.parent.invalidate_deps(['.'.join([self.name,n]) for n in valid_outs], 
                                        notify_parent)
        for out in valid_outs:
            self._valid_dict[out] = False
            
        return valid_outs

    def update_outputs(self, outnames):
        """Do what is necessary to make the specified output Variables valid.
        For a simple Component, this will result in a *run()*.
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

