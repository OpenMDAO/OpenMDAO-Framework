""" Class definition for Component. """

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

# pylint: disable=E0611,F0401
from traits.trait_base import not_event
from traits.api import Property

from openmdao.main.array_helpers import flattened_value
from openmdao.main.container import Container
from openmdao.main.derivatives import applyJ, applyJT
from openmdao.main.interfaces import implements, obj_has_interface, \
                                     IAssembly, IComponent, IDriver
from openmdao.main.hasconstraints import HasConstraints, HasEqConstraints, \
                                         HasIneqConstraints
from openmdao.main.hasobjective import HasObjective, HasObjectives
from openmdao.main.file_supp import FileMetadata
from openmdao.main.rbac import rbac
from openmdao.main.mp_support import has_interface, is_instance
from openmdao.main.datatypes.api import Bool, List, Str, Int, Slot, \
                                        FileRef, Enum
from openmdao.main.vartree import VariableTree
from openmdao.main.mpiwrap import MPI_info

from openmdao.util.eggsaver import SAVE_CPICKLE
from openmdao.util.eggobserver import EggObserver

import openmdao.util.log as tracing

__missing__ = object()


class SimulationRoot(object):
    """Singleton object used to hold root directory."""

    # Execution root directory. Root of all legal file paths.
    __root = None

    @staticmethod
    def chroot(path):
        """Change to directory `path` and set the singleton's root.
        Normally not called but useful in special situations.

        path: string
            Path to move to.
        """
        os.chdir(path)
        SimulationRoot.__root = None
        SimulationRoot.get_root()

    @staticmethod
    def get_root():
        """Return this simulation's root directory path."""
        if SimulationRoot.__root is None:
            SimulationRoot.__root = os.path.realpath(os.getcwd())
            if sys.platform == 'win32':  # pragma no cover
                SimulationRoot.__root = SimulationRoot.__root.lower()
        return SimulationRoot.__root

    @staticmethod
    def legal_path(path):
        """Return True if `path` is legal (descendant of our root).

        path: string
            Path to check.
        """
        root = SimulationRoot.get_root()
        if sys.platform == 'win32':  # pragma no cover
            return os.path.realpath(path).lower().startswith(root)
        else:
            return os.path.realpath(path).startswith(root)


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


_iodict = {'out': 'output', 'in': 'input'}


class Component(Container):
    """This is the base class for all objects containing Traits that are
    accessible to the OpenMDAO framework and are "runnable."
    """

    implements(IComponent)

    directory = Str('', desc='If non-blank, the directory to execute in.',
                    framework_var=True, iotype='in', deriv_ignore=True)
    external_files = List(FileMetadata,
                          desc='FileMetadata objects for external files used'
                               ' by this component.', deriv_ignore=True)
    force_fd = Bool(False, iotype='in', framework_var=True, deriv_ignore=True,
                    desc="If True, always finite difference this component.")

    # this will automagically call _get_log_level and _set_log_level when needed
    log_level = Property(desc='Logging message level')

    exec_count = Int(0, iotype='out', framework_var=True, deriv_ignore=True,
                     desc='Number of times this Component has been executed.')

    derivative_exec_count = Int(0, iotype='out', framework_var=True, deriv_ignore=True,
                     desc="Number of times this Component's derivative "
                          "function has been executed.")

    itername = Str('', iotype='out', desc='Iteration coordinates.', deriv_ignore=True,
                   framework_var=True)

    # TODO: add 'fd' option to missing_deriv_policy
    missing_deriv_policy = Enum(['error', 'assume_zero'], iotype='in',
                                framework_var=True, deriv_ignore=True,
                                desc='Determines behavior when some '
                                     'analytical derivatives are provided '
                                     'but some are missing')

    create_instance_dir = Bool(False)

    def __init__(self):
        super(Component, self).__init__()

        self.mpi = MPI_info()

        self._exec_state = None
        self._stop = False
        self._new_config = True

        # cached configuration information
        self._input_names = None
        self._output_names = None
        self._container_names = None

        self._dir_stack = []
        self._dir_context = None

        # Flags and caching used by the derivatives calculation
        self._provideJ_bounds = None

        self._case_id = ''
        self._case_uuid = ''

    @property
    def dir_context(self):
        """The :class:`DirectoryContext` for this component."""
        if self._dir_context is None:
            self._dir_context = DirectoryContext(self)
        return self._dir_context

    def _set_exec_state(self, state):
        if self._exec_state != state:
            self._exec_state = state

    @rbac(('owner', 'user'))
    def get_itername(self):
        """Return current 'iteration coordinates'."""
        return self.itername

    @rbac(('owner', 'user'))
    def set_itername(self, itername):
        """Set current 'iteration coordinates'. Typically called by the
        current workflow just before running the component.

        itername: string
            Iteration coordinates.
        """
        self.itername = itername

    def _input_trait_modified(self, obj, name, old, new):
        if name.endswith('_items'):
            n = name[:-6]
            if hasattr(self, n):
                name = n
        self._input_updated(name)

    def _input_updated(self, name, fullpath=None):
        pass

    def __deepcopy__(self, memo):
        """ For some reason, deepcopying does not set the trait callback
        functions. We need to do this manually. """

        result = super(Component, self).__deepcopy__(memo)

        for name, trait in result.class_traits().items():
            if trait.iotype == 'in':
                result._set_input_callback(name)

        return result

    def __getstate__(self):
        """Return dict representing this container's state."""
        state = super(Component, self).__getstate__()
        state['_input_names'] = None
        state['_output_names'] = None
        state['_container_names'] = None

        return state

    def __setstate__(self, state):
        super(Component, self).__setstate__(state)

        # make sure all input callbacks are in place.  If callback is
        # already there, this will have no effect.
        for name, trait in self._alltraits().items():
            if trait.iotype == 'in':
                self._set_input_callback(name)

    @rbac(('owner', 'user'))
    def is_differentiable(self):
        """Return True if analytical derivatives can be
        computed for this Component.
        """
        if self.force_fd:
            return False

        return hasattr(self, 'provideJ')

    @rbac(('owner', 'user'))
    def get_req_default(self, self_required=None):
        """Returns a list of all inputs that are required but still have
        their default value.
        """
        req = []
        for name, trait in self.traits(type=not_event).items():
            if trait.iotype in ['in', 'state']:
                obj = getattr(self, name)
                if is_instance(obj, VariableTree):
                    if self.name:
                        req.extend(['.'.join((self.name, n))
                                     for n in obj.get_req_default(trait.required)])
                    else:
                        req.extend(obj.get_req_default(trait.required))
                elif trait.required is True:
                    try:
                        trait = trait.trait_type
                    except:
                        unset = (obj == trait.default)
                    else:
                        unset = (obj == trait.default_value)
                    if not isinstance(unset, bool):
                        try:
                            unset = unset.all()
                        except:
                            pass
                    if unset:
                        if self.name:
                            req.append('.'.join((self.name, name)))
                        else:
                            req.append(name)
        return req

    @rbac(('owner', 'user'))
    def check_config(self, strict=False):
        """
        Verify that this component and all of its children are properly
        configured to execute. This function is called prior the first
        component execution.  If strict is True, any warning or error
        should raise an exception.

        If you override this function to do checks specific to your class,
        you must call this function.
        """

        # derivatives related checks
        if hasattr(self, 'apply_deriv') or hasattr(self, 'apply_derivT'):
            if not hasattr(self, 'provideJ'):
                self.raise_exception("required method 'provideJ' is missing")
            self._check_deriv_vars()
            if not hasattr(self, 'apply_deriv'):
                self.raise_exception("method 'apply_deriv' must be also specified "
                                     " if 'apply_derivT' is specified")
            if not hasattr(self, 'apply_derivT'):
                self.raise_exception("method 'apply_derivT' must be also specified "
                                     " if 'apply_deriv' is specified")
        elif hasattr(self, 'provideJ'):
            self._check_deriv_vars()

        visited = set([id(self), id(self.parent)])

        for name, trait in self.traits(type=not_event).items():
            obj = getattr(self, name)
            #self._check_req_trait(name, obj, trait)
            if trait.required is True and trait.is_trait_type(Slot):
                if obj is None:
                    self.raise_exception("required plugin '%s' is not"
                                         " present" % name, RuntimeError)

            if has_interface(obj, IComponent) and id(obj) not in visited:
                visited.add(id(obj))
                obj.check_config(strict=strict)

        if self.parent is None:
            reqs = self.get_req_default()
            if reqs:
                self.raise_exception("required variables %s were"
                                     " not set" % reqs, RuntimeError)

        self._new_config = False

    def _check_deriv_var(self, var_name):
        try:
            val = self.get(var_name)
        except AttributeError:
            msg = "'{var_name}' was given in 'list_deriv_vars' "\
                  "but '{var_name}' is undefined"

            msg = msg.format(var_name=var_name, comp_name=self.__class__.__name__)
            self.raise_exception(msg)

        var_type_name = self.get_metadata(var_name).get('vartypename')

        if var_type_name == 'VarTree':
            msg = "'{var_name}', of type '{var_type}', was given in 'list_deriv_vars' but you must declare "\
                  "sub-vars of a vartree individually"

            msg = msg.format(var_name=var_name, var_type=var_type_name)
            self.raise_exception(msg)

        try:
            flattened_value(var_name, val)
        except Exception:
            msg = "'{var_name}', of type '{var_type}', was given in 'list_deriv_vars' "\
                  "but variables must be of a type convertable to a 1D float array"

            msg = msg.format(var_name=var_name, var_type=var_type_name)
            self.raise_exception(msg)

    def _check_deriv_vars(self):
        try:
            inputs, outputs = self.list_deriv_vars()
        except AttributeError:
            self.raise_exception("required method 'list_deriv_vars' is missing")

        for var_name in inputs:
            self._check_deriv_var(var_name)

        for var_name in outputs:
            self._check_deriv_var(var_name)

    @rbac(('owner', 'user'))
    def cpath_updated(self):
        """Calls the base class version of *cpath_updated()*, checks our
        directory for validity, and creates the directory if it doesn't exist.
        """
        super(Component, self).cpath_updated()

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

        if self._call_configure:
            self.configure()
            self._call_configure = False

    def _pre_execute(self):
        """Prepares for execution by calling various initialization methods
        if necessary.

        Overrides of this function must call this version.
        """

        if self._call_cpath_updated:
            self.cpath_updated()

        if self._new_config:
            self._setup()

        if self.parent is None and has_interface(self, IAssembly):
            self.configure_recording(self.recording_options)

    def _setup(self, inputs=None, outputs=None):
        self.check_config()

    def execute(self):
        """Perform calculations or other actions, assuming that inputs
        have already been set. This must be overridden in derived classes.
        """
        raise NotImplementedError('%s.execute' % self.get_pathname())

    def linearize(self, first=False, second=False):
        """Component wrapper for the ProvideJ hook. This function should not
        be overriden.

        first: Bool
            Set to True to calculate first derivatives.

        second: Bool
            Set to True to calculate second derivatives. This is not cuurrently supported.
        """

        J = None

        # Allow user to force finite difference on a comp.
        if self.force_fd is True:
            return

        # Calculate first derivatives using the new API.
        if first and hasattr(self, 'provideJ'):
            J = self.provideJ()
            self.derivative_exec_count += 1
        else:
            return

        return J

    def applyJ(self, system, variables):
        """ Wrapper for component derivative specification methods.
        Forward Mode.
        """
        applyJ(system, variables)

    def applyJT(self, system, variables):
        """ Wrapper for component derivative specification methods.
        Adjoint Mode.
        """
        applyJT(system, variables)

    def name_changed(self, old, new):
        pass

    def _post_execute(self):
        """Update output variables and anything else needed after execution.
        Overrides of this function must call this version.  This is only
        called if execute() actually ran.
        """
        pass

    def _post_run(self):
        """"Runs at the end of the run function, whether execute() ran or not."""
        pass

    @rbac('*', 'owner')
    def run(self, case_uuid=''):
        """Run this object. This should include fetching input variables
        (if necessary), executing, and updating output variables.
        Do not override this function.

        case_uuid: str
            Identifier for the Case that is associated with this run.
        """

        if self.directory:
            self.push_dir()

        self._stop = False
        self._case_uuid = case_uuid

        try:
            self._pre_execute()
            self._set_exec_state('RUNNING')

            # Component executes as normal
            self.exec_count += 1
            if tracing.TRACER is not None and \
               not obj_has_interface(self, IDriver, IAssembly):
                tracing.TRACER.debug(self.get_itername())
                #tracing.TRACER.debug(self.get_itername() + '  ' + self.name)

            self.execute()
            self._post_execute()
            self._post_run()
        except Exception:
            info = sys.exc_info()
            self._set_exec_state('INVALID')
            raise info[0], info[1], info[2]
        finally:
            # If this is the top-level component, perform run termination.
            if self.parent is None:
                self._run_terminated()
            if self.directory:
                self.pop_dir()

    @rbac(('owner', 'user'))
    def _run_terminated(self):
        """ Executed at end of top-level run. """
        if hasattr(self, 'recorders'):
            for recorder in self.recorders:
                recorder.close()

    def add(self, name, obj):
        """Override of base class version to force call to *check_config*
        after any child containers are added. The base class version is still
        called.

        Returns the added Container object.
        """
        if has_interface(obj, IDriver) and not has_interface(self, IAssembly):
            raise Exception("A Driver may only be added to an Assembly")

        try:
            super(Component, self).add(name, obj)
        finally:
            self.config_changed()

        return obj

    def remove(self, name):
        """Override of base class version to force call to *check_config* after
        any child containers are removed.
        """
        try:
            return super(Component, self).remove(name)
        finally:
            self.config_changed()

    def replace(self, target_name, newobj):
        """Replace one object with another, attempting to mimic the replaced
        object as much as possible.
        """
        tobj = getattr(self, target_name)

        if hasattr(newobj, 'mimic'):
            try:
                # this should copy inputs, delegates and set name
                newobj.mimic(tobj)
            except Exception:
                self.reraise_exception("Couldn't replace '%s' of type %s with"
                                       " type %s" % (target_name,
                                                     type(tobj).__name__,
                                                     type(newobj).__name__),
                                        sys.exc_info())

        self.add(target_name, newobj)  # this will remove the old object

    def add_trait(self, name, trait, refresh=True):
        """Overrides base definition of *add_trait* in order to
        force call to *check_config* prior to execution when new traits are
        added.
        """
        super(Component, self).add_trait(name, trait, refresh)

        self.config_changed()

        if trait.iotype and self.parent:
            self.parent.child_config_changed(self, removing=False)

    def _set_input_callback(self, name, remove=False):

        self.on_trait_change(self._input_trait_modified, name, remove=remove)

        # Certain containers get an additional listener for access by index.
        # Currently, List and Dict are supported, as well as any other
        # Enthought or user-defined trait whose handler supports it.
        # Array is not supported yet.
        t = self.trait(name)
        if t.handler.has_items:
            name = name + '_items'
            self.on_trait_change(self._input_trait_modified, name,
                                 remove=remove)

    def remove_trait(self, name):
        """Overrides base definition of *remove_trait* in order to
        force call to *check_config* prior to execution when a trait is
        removed.
        """
        try:
            super(Component, self).remove_trait(name)
        finally:
            self.config_changed()

    @rbac(('owner', 'user'))
    def config_changed(self, update_parent=True):
        """Call this whenever the configuration of this Component changes,
        for example, children are added or removed.
        """
        if update_parent and hasattr(self, '_parent') and self._parent:
            self.parent.config_changed(update_parent)
        self._input_names = None
        self._output_names = None
        self._container_names = None
        self._new_config = True
        self._provideJ_bounds = None

    @rbac(('owner', 'user'))
    def list_inputs(self):
        """Return a list of names of input values."""
        if self._input_names is None:
            self._input_names = [k for k, v in self.items(iotype='in')]

        return self._input_names[:]

    @rbac(('owner', 'user'))
    def list_outputs(self):
        """Return a list of names of output values."""
        if self._output_names is None:
            self._output_names = [k for k, v in self.items(iotype='out')]

        return self._output_names[:]

    def list_containers(self):
        """Return a list of names of child Containers."""
        if self._container_names is None:
            visited = set([id(self)])
            if hasattr(self, '_parent'):  # fix for weird unpickling bug
                visited.add(id(self.parent))
            names = []
            for n, v in self.__dict__.items():
                if is_instance(v, Container) and id(v) not in visited:
                    visited.add(id(v))
                    names.append(n)
            self._container_names = names
        return self._container_names

    @rbac(('owner', 'user'))
    def list_deriv_vars(self):
        return (), ()

    @rbac(('owner', 'user'))
    def mimic(self, target):
        """Initialize what we can from the given target object. Copy any
        inputs that we share with the target and initialize our delegates with
        any matching delegates from the target.
        """
        if isinstance(target, Container) and target.name != '':
            self.name = target.name

        # update any delegates that we share with the target
        if hasattr(target, '_delegates_') and hasattr(self, '_delegates_'):
            groups = [(HasConstraints, HasEqConstraints, HasIneqConstraints),
                      (HasObjective, HasObjectives)]
            matches = {}

            # should be safe assuming only one delegate of each type here,
            # since multiples would simply overwrite each other
            for tname, tdel in target._delegates_.items():
                for sname, sdel in self._delegates_.items():
                    if sname in matches:
                        continue
                    if tname == sname:
                        matches[sname] = target._delegates_[tname]
                    else:
                        for g in groups:
                            if isinstance(tdel, g) and isinstance(sdel, g):
                                matches[sname] = target._delegates_[tname]
                                break
                    if sname in matches:
                        break
                else:
                    # current tname wasn't matched to anything in self._delegates_
                    if hasattr(tdel, '_item_count') and tdel._item_count() > 0:
                        self.raise_exception("target delegate '%s' has no match"
                                             % tname, RuntimeError)

            for sname, tdel in matches.items():
                delegate = self._delegates_[sname]
                if hasattr(delegate, 'mimic'):
                    delegate.mimic(tdel)  # use target delegate as target

        # # now update any matching inputs from the target
        for inp in target.list_inputs():
            if hasattr(self, inp):
                setattr(self, inp, getattr(target, inp))

        # Update slots that aren't inputs.
        target_inputs = target.list_inputs()
        target_slots = [n for n, v in target.traits().items()
                                   if v.is_trait_type(Slot)]
        my_slots = [n for n, v in self.traits().items()
                               if v.is_trait_type(Slot)]
        for name in target_slots:
            if name not in target_inputs and name in my_slots:
                if hasattr(self, name):
                    myobj = getattr(self, name)
                    if hasattr(myobj, 'mimic'):
                        myobj.mimic(getattr(target, name))
                        continue
                self.add(name, getattr(target, name))

        # Update List(Slot) traits.
        target_lists = [n for n, v in target.traits().items()
                                   if v.is_trait_type(List) and
                                      v.inner_traits[-1].is_trait_type(Slot)]
        my_lists = [n for n, v in self.traits().items()
                                   if v.is_trait_type(List) and
                                      v.inner_traits[-1].is_trait_type(Slot)]
        for name in target_lists:
            if name in my_lists:
                setattr(self, name, getattr(target, name))

    @rbac(('owner', 'user'))
    def get_expr_depends(self):
        """Return a list of tuples of the form (src_comp_name, dest_comp_name)
        for each dependency resulting from ExprEvaluators in this Component.
        """
        conn_list = []
        if hasattr(self, '_delegates_'):
            for name, dclass in self._delegates_.items():
                delegate = getattr(self, name)
                if hasattr(delegate, 'get_expr_depends'):
                    conn_list.extend(delegate.get_expr_depends())
        return conn_list

    def check_path(self, path, check_dir=False):
        """Verify that the given path is a directory and is located
        within the allowed area (somewhere within the simulation root path).
        """
# pylint: disable=E1101
        if not SimulationRoot.legal_path(path):
            self.raise_exception("Illegal path '%s', not a descendant of '%s'."
                                 % (path, SimulationRoot.get_root()),
                                 ValueError)
        elif check_dir and not isdir(path):
            self.raise_exception(
                "Execution directory path '%s' is not a directory."
                % path, ValueError)
# pylint: enable=E1101
        return path

    @rbac('owner')
    def get_abs_directory(self):
        """Return absolute path of execution directory."""
        path = self.directory
        if not isabs(path):
            if self._call_cpath_updated:
                self.raise_exception("can't call get_abs_directory before"
                                     " hierarchy is defined", RuntimeError)
            if self.parent is not None and is_instance(self.parent, Component):
                parent_dir = self.parent.get_abs_directory()
            else:
                parent_dir = SimulationRoot.get_root()
            path = join(parent_dir, path)

        return path

    def push_dir(self, directory=None):
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

    def pop_dir(self):
        """Return to previous directory saved by :meth:`push_dir`."""
        try:
            newdir = self._dir_stack.pop()
        except IndexError:
            self.raise_exception('Called pop_dir() with nothing on the dir'
                                 ' stack', IndexError)
        os.chdir(newdir)

    def checkpoint(self, outstream, fmt=SAVE_CPICKLE):
        """Save sufficient information for a restart. By default, this
        just calls *save()*.
        """
        self.save(outstream, fmt)

    def restart(self, instream):
        """Restore state using a checkpoint file. The checkpoint file is
        typically a delta from a full saved state file. If checkpoint is
        overridden, this should also be overridden.
        """
        self.load(instream)

    def save_to_egg(self, name, version, py_dir=None, require_relpaths=True,
                    child_objs=None, dst_dir=None, observer=None,
                    need_requirements=True):
        """Save state and other files to an egg. Typically used to copy all or
        part of a simulation to another user or machine. By specifying child
        components in `child_objs`, it will be possible to create instances of
        just those components from the installed egg. Child component names
        should be specified relative to this component.

        name: string
            Name for egg; must be an alphanumeric string.

        version: string
            Version for egg; must be an alphanumeric string.

        py_dir: string
            The (root) directory for local Python files. It defaults to
            the current directory.

        require_relpaths: bool
            If True, any path (directory attribute, external file, or file
            trait) which cannot be made relative to this component's directory
            will raise ValueError. Otherwise, such paths generate a warning and
            the file is skipped.

        child_objs: list
            List of child objects for additional entry points.

        dst_dir: string
            The directory to write the egg in.

        observer: callable
            Will be called via an :class:`EggObserver`.

        need_requirements: bool
            Passed to :meth:`eggsaver.save_to_egg`.

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
        components.extend([obj for n, obj in self.items(recurse=True)
                                               if is_instance(obj, Component)])
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
                    if not is_instance(child, Component):
                        continue
                    rel_path = child.directory
                    obj = child.parent
                    if obj is None:
                        msg = 'Entry point object has no parent!'
                        observer.exception(msg)
                        raise RuntimeError(msg)
                    while obj.parent is not None and \
                          is_instance(obj.parent, Component):
                        rel_path = join(obj.directory, rel_path)
                        obj = obj.parent
                    child._rel_dir_path = rel_path

            return super(Component, self).save_to_egg(
                       name, version, py_dir, src_dir, src_files,
                       child_objs, dst_dir, observer.observer,
                       need_requirements)
        finally:
            # If any component config has been modified, restore it.
            for comp, path in fixup_dirs:
                comp.directory = path
            for meta, path in fixup_meta:
                meta.path = path
            for comp, name, path in fixup_fvar:
                comp.set(name + '.path', path)

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
            if hasattr(ftrait, 'local_path') and ftrait.local_path:
                path = ftrait.local_path
            else:
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
                    comp.set(fvarname + '.path', path)
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
                    ftrait = container.get_trait(name)
                    if self is scope:
                        file_vars.append((name, obj, ftrait))
                    else:
                        rel_path = container.get_pathname(rel_to_scope=scope)
                        file_vars.append(('.'.join((rel_path, name)),
                                          obj, ftrait))
                elif is_instance(obj, Container) and \
                     not is_instance(obj, Component):
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
        are not overwritten. Returns the root object.

        instream: file or string
            Stream to load from.

        fmt: int
            Format of state data.

        package: string
            Name of package to look for `instream` if `instream` is a string
            that is not an existing file.

        call_post_load: bool
            If True, call :meth:`post_load`.

        top_obj: bool
            Set True if loading the default entry, False if loading a
            child entry point object.

        name: string
            Name for the root object.

        observer: callable
            Will be called via an :class:`EggObserver`.
        """
        observer = EggObserver(observer, logging.getLogger())
        try:
            top = Container.load(instream, fmt, package, False, name=name)
        except Exception, exc:
            observer.exception(str(exc))
            raise

        observer.logger = top._logger
        if is_instance(top, Component):
            # Get path relative to real top before we clobber directory attr.
            if top_obj:
                rel_path = '.'
            else:
                if top.parent is not None:
                    rel_path = top.directory
                    obj = top.parent
                    while obj.parent is not None and \
                          is_instance(obj.parent, Component):
                        rel_path = join(obj.directory, rel_path)
                        obj = obj.parent
                elif '_rel_dir_path' in top.traits():
                    top._logger.warning('No parent, using saved relative directory')
                    rel_path = top._rel_dir_path  # Set during save_to_egg().
                else:
                    top._logger.warning('No parent, using null relative directory')
                    rel_path = ''

            # Set top directory.
            orig_dir = os.getcwd()
            if name:
                top.name = name
                # New instance via create(name) gets new directory.
                if not exists(name):
                    os.mkdir(name)
                os.chdir(name)

            try:
                top._trait_change_notify(False)

                # TODO: (maybe) Seems like we should make top.directory relative
                # here instead of absolute, but it doesn't work...
                #top.directory = relpath(os.getcwd(), SimulationRoot.get_root())
                top.directory = os.getcwd()

                # Create any missing subdirectories.
                for component in [c for n, c in top.items(recurse=True)
                                              if is_instance(c, Component)]:
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
                top._trait_change_notify(True)
                os.chdir(orig_dir)
                if name and not glob.glob(join(name, '*')):
                    # Cleanup unused directory.
                    os.rmdir(name)
                    # setting the directory attribute below was causing an
                    # exception because the parent was unaware of the 'top'
                    # object. It doesn't seem valid that a newly loaded object
                    # would ever have a parent, but setting the parent to None
                    # up above breaks things...
                    if getattr(top.parent, name, None) is not top:
                        # our parent doesn't know us, so why do we have a parent?
                        top.parent = None
                    top.directory = ''

        if call_post_load:
            top.post_load()

        observer.complete(name)

        top.parent = None
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
                for component in [c for n, c in self.items(recurse=False)
                                              if is_instance(c, Component)]:
                    path = rel_path
                    if component.directory:
                        # Always use '/' for resources.
                        path += '/' + component.directory
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
                observer.copy(src_name, i / total_files,
                              completed_bytes / total_bytes)
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

    def stop(self):
        """Stop this component."""
        self._stop = True

    def _get_log_level(self):
        """Return logging message level."""
        return self._logger.level

    def _set_log_level(self, level):
        """Set logging message level."""
        self._logger.level = level

    def check_gradient(self, inputs=None, outputs=None,
                       stream=sys.stdout, mode='auto',
                       fd_form='forward', fd_step=1.0e-6,
                       fd_step_type='absolute'):
        """Compare the OpenMDAO-calculated gradient with one calculated
        by straight finite-difference. This provides the user with a way
        to validate his derivative functions (apply_deriv and provideJ.)

        inputs: (optional) iter of str or None
            Names of input variables. The calculated gradient will be
            the matrix of values of the output variables with respect
            to these input variables. If no value is provided for inputs,
            they will be determined based on the inputs of this component.

        outputs: (optional) iter of str or None
            Names of output variables. The calculated gradient will be
            the matrix of values of these output variables with respect
            to the input variables. If no value is provided for outputs,
            they will be determined based on the outputs of this component.

        stream: (optional) file-like object, str, or None
            Where to write to, default stdout. If a string is supplied,
            that is used as a filename.  If None, no output is written.

        mode: (optional) str or None
            Set to 'forward' for forward mode, 'adjoint' for adjoint mode,
            or 'auto' to let OpenMDAO determine the correct mode.
            Defaults to 'auto'.

        fd_form: str
            Finite difference mode. Valid choices are 'forward', 'adjoint',
            'central'. Default is 'forward'

        fd_step: float
            Default step_size for finite difference. Default is 1.0e-6.

        fd_step_type: str
            Finite difference step type. Set to 'absolute' or 'relative'.
            Default is 'absolute'.

        Returns the finite difference gradient, the OpenMDAO-calculated gradient,
        a list of the gradient names, and a list of suspect inputs/outputs.
        """
        if self.parent is None or not self.name:
            from openmdao.main.assembly import Assembly, set_as_top
            asm = set_as_top(Assembly())
            orig_name = self.name
            try:
                asm.add('comp', self)
                asm.driver.workflow.add('comp')
                asm.run()
                return asm.check_gradient(name=self.name,
                                         inputs=inputs, outputs=outputs,
                                         stream=stream, mode=mode,
                                         fd_form=fd_form, fd_step=fd_step,
                                         fd_step_type=fd_step_type)
            finally:
                self.parent = None
                if orig_name:
                    self.name = orig_name
        else:
            return self.parent.check_gradient(name=self.name,
                                              inputs=inputs, outputs=outputs,
                                              stream=stream, mode=mode,
                                              fd_form=fd_form, fd_step=fd_step,
                                              fd_step_type=fd_step_type)

    @rbac(('owner', 'user'))
    def get_req_cpus(self):
        """Return requested_cpus"""
        return self.mpi.requested_cpus

    @rbac(('owner', 'user'))
    def setup_systems(self):
        return ()

    @rbac(('owner', 'user'))
    def setup_communicators(self, comm):
        self.mpi.comm = comm

    @rbac(('owner', 'user'))
    def get_full_nodeset(self):
        """Return the node in the depgraph
        belonging to this component.
        """
        return set((self.name,))

    @rbac(('owner', 'user'))
    def setup_depgraph(self, inputs=None, outputs=None):
        pass

    @rbac(('owner', 'user'))
    def setup_init(self):
        self._provideJ_bounds = None

    @rbac(('owner', 'user'))
    def init_var_sizes(self):
        pass

    @rbac(('owner', 'user'))
    def post_setup(self):
        pass
