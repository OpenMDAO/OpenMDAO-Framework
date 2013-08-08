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
import re

try:
    from numpy import inner
    from numpy import ndarray
except ImportError as err:
    import logging
    logging.warn("In %s: %r", __file__, err)
    from openmdao.main.numpy_fallback import inner

# pylint: disable-msg=E0611,F0401
from traits.trait_base import not_event
from traits.api import Property

from openmdao.main.container import Container
from openmdao.main.derivatives import Derivatives, \
                                      flattened_size, flattened_value
from openmdao.main.expreval import ConnectedExprEvaluator
from openmdao.main.interfaces import implements, obj_has_interface, \
                                     IAssembly, IComponent, IDriver, \
                                     IHasCouplingVars, IHasObjectives, \
                                     IHasParameters, IHasConstraints, \
                                     IHasEqConstraints, IHasIneqConstraints, \
                                     IHasEvents, ICaseIterator, ICaseRecorder
from openmdao.main.hasparameters import ParameterGroup
from openmdao.main.hasconstraints import HasConstraints, HasEqConstraints, \
                                         HasIneqConstraints
from openmdao.main.hasobjective import HasObjective, HasObjectives
from openmdao.main.file_supp import FileMetadata
from openmdao.main.depgraph import DependencyGraph
from openmdao.main.rbac import rbac
from openmdao.main.mp_support import has_interface, is_instance
from openmdao.main.datatypes.api import Bool, List, Str, Int, Slot, Dict, \
                                        FileRef
from openmdao.main.publisher import Publisher
from openmdao.main.vartree import VariableTree

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

# this key in publish_vars indicates a subscriber to the Component attributes
__attributes__ = '__attributes__'


class Component(Container):
    """This is the base class for all objects containing Traits that are
    accessible to the OpenMDAO framework and are "runnable."
    """

    implements(IComponent)

    directory = Str('', desc='If non-blank, the directory to execute in.',
                    framework_var=True, iotype='in')
    external_files = List(FileMetadata,
                          desc='FileMetadata objects for external files used'
                               ' by this component.')
    force_execute = Bool(False, iotype='in', framework_var=True,
                         desc="If True, always execute even if all IO traits are valid.")

    # this will automagically call _get_log_level and _set_log_level when needed
    log_level = Property(desc='Logging message level')

    exec_count = Int(0, iotype='out', framework_var=True,
                     desc='Number of times this Component has been executed.')

    derivative_exec_count = Int(0, iotype='out', framework_var=True,
                     desc="Number of times this Component's derivative "
                          "function has been executed.")

    itername = Str('', iotype='out', desc='Iteration coordinates.',
                   framework_var=True)

    create_instance_dir = Bool(False)

    def __init__(self):
        super(Component, self).__init__()

        self._exec_state = 'INVALID'  # possible values: VALID, INVALID, RUNNING

        # register callbacks for all of our 'in' traits
        for name, trait in self.class_traits().items():
            if trait.iotype == 'in':
                self._set_input_callback(name)

        # contains validity flag for each io Trait (inputs are valid since
        # they're not connected yet, and outputs are invalid)
        self._valid_dict = dict([(name, t.iotype == 'in')
            for name, t in self.class_traits().items() if t.iotype])

        # dependency graph between us and our boundaries (bookkeeps connections
        # between our variables and external ones).
        # This replaces self._depgraph from Container.
        self._depgraph = DependencyGraph()

        # Components with input CaseIterators will be forced to execute whenever run() is
        # called on them, even if they don't have any invalid inputs or outputs.
        self._num_input_caseiters = 0
        for name, trait in self.class_traits().items():
            # isinstance(trait.trait_type.klass,ICaseIterator) doesn't work here...
            if trait.iotype == 'in' and trait.trait_type and trait.trait_type.klass is ICaseIterator:
                self._num_input_caseiters += 1

        self._stop = False
        self._call_check_config = True
        self._call_execute = True

        # cached configuration information
        self._input_names = None
        self._output_names = None
        self._container_names = None
        self._expr_sources = None
        self._connected_inputs = None
        self._connected_outputs = None

        self._dir_stack = []
        self._dir_context = None

        self.ffd_order = 0
        self._case_id = ''

        self._publish_vars = {}  # dict of varname to subscriber count
        
        # Derivatives helper object. Mostly used for the older differentiators.
        self.derivatives = Derivatives(self)

    @property
    def dir_context(self):
        """The :class:`DirectoryContext` for this component."""
        if self._dir_context is None:
            self._dir_context = DirectoryContext(self)
        return self._dir_context

    def _set_exec_state(self, state):
        if self._exec_state != state:
            self._exec_state = state
            pub = Publisher.get_instance()
            if pub:
                pub.publish('.'.join([self.get_pathname(), 'exec_state']), state)

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

    # call this if any trait having 'iotype' metadata of 'in' is changed
    def _input_trait_modified(self, obj, name, old, new):

        if name.endswith('_items'):
            n = name[:-6]
            if n in self._valid_dict:
                name = n

        self._input_check(name, old)
        self._input_updated(name)

    def _input_updated(self, name, fullpath=None):
        self._call_execute = True
        if self._valid_dict[name]:  # if var is not already invalid
            outs = self.invalidate_deps(varnames=[name])
            if (outs is None) or outs:
                if self.parent:
                    self.parent.child_invalidated(self.name, outs)

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
        state['_expr_sources'] = None
        state['_connected_inputs'] = None
        state['_connected_outputs'] = None

        return state

    def __setstate__(self, state):
        super(Component, self).__setstate__(state)

        # make sure all input callbacks are in place.  If callback is
        # already there, this will have no effect.
        for name, trait in self._alltraits().items():
            if trait.iotype == 'in':
                self._set_input_callback(name)

    @rbac(('owner', 'user'))
    def check_configuration(self):
        """
        Verify that this component and all of its children are properly
        configured to execute. This function is called prior to each
        component execution, but is a no-op unless self._call_check_config is
        True.

        Do not override this function.

        This function calls check_config(), which may be overridden by
        inheriting classes to perform more specific configuration checks.
        """
        if self._call_check_config:
            self.check_config()

            visited = set([id(self), id(self.parent)])
            for name, value in self.traits(type=not_event).items():
                obj = getattr(self, name)
                if value.is_trait_type(Slot) and value.required is True and obj is None:
                    self.raise_exception("required plugin '%s' is not present" %
                                         name, RuntimeError)
                if has_interface(obj, IComponent) and id(obj) not in visited:
                    visited.add(id(obj))
                    obj.check_configuration()

            self._call_check_config = False

    def check_config(self):
        """
        Override this function to perform configuration checks specific to your class.
        Bad configurations should raise an exception.
        """
        pass

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

    def _pre_execute(self, force=False):
        """Prepares for execution by calling *cpath_updated()* and *check_config()* if
        their "dirty" flags are set and by requesting that the parent Assembly
        update this Component's invalid inputs.

        Overrides of this function must call this version.
        """
        if self._call_cpath_updated:
            self.cpath_updated()

        if force:
            outs = self.invalidate_deps()
            if (outs is None) or outs:
                if self.parent:
                    self.parent.child_invalidated(self.name, outs)
        else:
            if not self.is_valid():
                self._call_execute = True
            elif self._num_input_caseiters > 0:
                self._call_execute = True
                # we're valid, but we're running anyway because of our input CaseIterators,
                # so we need to notify downstream comps so they grab our new outputs
                outs = self.invalidate_deps()
                if (outs is None) or outs:
                    if self.parent:
                        self.parent.child_invalidated(self.name, outs)

        if self.parent is None:  # if parent is None, we're not part of an Assembly
                                 # so Variable validity doesn't apply. Just execute.
            self._call_execute = True
            valids = self._valid_dict
            for name in self.list_inputs():
                valids[name] = True
        else:
            valids = self._valid_dict
            invalid_ins = [inp for inp in self.list_inputs(connected=True)
                                    if valids.get(inp) is False]
            if invalid_ins:
                self._call_execute = True
                self.parent.update_inputs(self.name, invalid_ins)
                for name in invalid_ins:
                    valids[name] = True
            elif self._call_execute is False and len(self.list_outputs(valid=False)):
                self._call_execute = True

        self.check_configuration()

    def execute(self):
        """Perform calculations or other actions, assuming that inputs
        have already been set. This must be overridden in derived classes.
        """
        raise NotImplementedError('%s.execute' % self.get_pathname())

    def _execute_ffd(self, ffd_order):
        """During Fake Finite Difference, instead of executing, a component
        can use the available derivatives to calculate the output efficiently.
        Before FFD can execute, calc_derivatives must be called to save the
        baseline state and the derivatives at that baseline point.

        This method approximates the output using a Taylor series expansion
        about the saved baseline point.

        ffd_order: int
            Order of the derivatives to be used (1 or 2).
        """

        for name in self.derivatives.out_names:
            setattr(self, name,
                    self.derivatives.calculate_output(name, ffd_order))

    def calc_derivatives(self, first=False, second=False, savebase=False):
        """Prepare for Fake Finite Difference runs by calculating all needed
        derivatives, and saving the current state as the baseline if
        requested. The user must supply *calculate_first_derivatives()*
        and/or *calculate_second_derivatives()* in the component.
        
        This function should not be overriden.
        
        first: Bool
            Set to True to calculate first derivatives.
        
        second: Bool
            Set to True to calculate second derivatives.
            
        savebase: Bool
            If set to true, then we save our baseline state for fake finite
            difference.
        """
        
        executed = False
        
        # Calculate first derivatives using the new API.
        # TODO: unify linearize & calculate_first_derivatives'
        if first and hasattr(self, 'linearize'):
            self.linearize()
            self.derivative_exec_count += 1
            
        # Calculate first derivatives in user-defined function
        if first and hasattr(self, 'calculate_first_derivatives'):
            self.calculate_first_derivatives()
            self.derivative_exec_count += 1
            executed = True
            
        # Calculate second derivatives in user-defined function
        if second and hasattr(self, 'calculate_second_derivatives'):
            self.calculate_second_derivatives()
            self.derivative_exec_count += 1
            executed = True
            
        # Save baseline state
        if savebase and executed:
            self.derivatives.save_baseline(self)

    def check_derivatives(self, order, driver_inputs, driver_outputs):
        """ComponentsWithDerivatives overloads this function to check for
        missing derivatives.

        This function is overridden by ComponentWithDerivatives.
        """

        pass

    def _post_execute(self):
        """Update output variables and anything else needed after execution.
        Overrides of this function must call this version.  This is only
        called if execute() actually ran.
        """
        # make our output Variables valid again
        valids = self._valid_dict
        for name in self.list_outputs(valid=False):
            valids[name] = True
        ## make sure our inputs are valid too
        for name in self.list_inputs(valid=False):
            valids[name] = True
        self._call_execute = False
        self._set_exec_state('VALID')
        self.publish_vars()

    def _post_run(self):
        """"Runs at the end of the run function, whether execute() ran or not."""
        pass

    @rbac('*', 'owner')
    def run(self, force=False, ffd_order=0, case_id=''):
        """Run this object. This should include fetching input variables
        (if necessary), executing, and updating output variables.
        Do not override this function.

        force: bool
            If True, force component to execute even if inputs have not
            changed. (Default is False.)

        ffd_order: int
            Order of the derivatives to be used during Fake
            Finite Difference (typically 1 or 2). During regular execution,
            ffd_order should be 0. (Default is 0.)

        case_id: str
            Identifier for the Case that is associated with this run. (Default is ''.)
            If applied to the top-level assembly, this will be prepended to
            all iteration coordinates.
        """
        if self.directory:
            self.push_dir()

        if self.force_execute:
            force = True

        self._stop = False
        self.ffd_order = ffd_order
        self._case_id = case_id
        try:
            self._pre_execute(force)
            self._set_exec_state('RUNNING')

            if self._call_execute or force:
                #print 'execute: %s' % self.get_pathname()

                if ffd_order == 1 and \
                   hasattr(self, 'calculate_first_derivatives'):
                    # During Fake Finite Difference, the available derivatives
                    # are used to approximate the outputs.
                    self._execute_ffd(1)

                elif ffd_order == 2 and \
                   hasattr(self, 'calculate_second_derivatives'):
                    # During Fake Finite Difference, the available derivatives
                    # are used to approximate the outputs.
                    self._execute_ffd(2)

                else:
                    # Component executes as normal
                    self.exec_count += 1
                    if tracing.TRACER is not None and \
                       not obj_has_interface(self, IAssembly) and \
                       not obj_has_interface(self, IDriver):

                        tracing.TRACER.debug(self.get_itername())

                    self.execute()

                self._post_execute()
            #else:
                #print 'skipping: %s' % self.get_pathname()
            self._post_run()
        except:
            self._set_exec_state('INVALID')
            raise
        finally:
            # If this is the top-level component, perform run termination.
            if self.parent is None:
                self._run_terminated()
            if self.directory:
                self.pop_dir()

    def _run_terminated(self):
        """ Executed at end of top-level run. """
        def _recursive_close(container, visited):
            """ Close all case recorders. """
            # Using ._alltraits() since .items() won't pickle.
            # and we may be traversing a distributed tree.
            for name in container._alltraits():
                obj = getattr(container, name)
                if id(obj) in visited:
                    continue
                visited.add(id(obj))
                if obj_has_interface(obj, IDriver):
                    for recorder in obj.recorders:
                        recorder.close()
                elif obj_has_interface(obj, ICaseRecorder):
                    obj.close()
                if isinstance(obj, Container):
                    _recursive_close(obj, visited)
        visited = set((id(self),))
        _recursive_close(self, visited)

    def add(self, name, obj):
        """Override of base class version to force call to *check_config*
        after any child containers are added. The base class version is still
        called.

        Returns the added Container object.
        """
        if has_interface(obj, IDriver) and not has_interface(self, IAssembly):
            raise Exception("A Driver may only be added to an Assembly")

        self.config_changed()
        super(Component, self).add(name, obj)
        if is_instance(obj, Container) and not is_instance(obj, Component):
            self._depgraph.add(name)
        return obj

    def remove(self, name):
        """Override of base class version to force call to *check_config* after
        any child containers are removed.
        """
        obj = super(Component, self).remove(name)
        if is_instance(obj, Container) and name in self._depgraph and not is_instance(obj, Component):
            self._depgraph.remove(name)
        self.config_changed()
        return obj

    def replace(self, target_name, newobj):
        """Replace one object with another, attempting to mimic the replaced
        object as much as possible.
        """
        tobj = getattr(self, target_name)

        if hasattr(newobj, 'mimic'):
            try:
                newobj.mimic(tobj)  # this should copy inputs, delegates and set name
            except Exception:
                self.reraise_exception("Couldn't replace '%s' of type %s with type %s"
                                       % (target_name, type(tobj).__name__,
                                          type(newobj).__name__))

        self.add(target_name, newobj)  # this will remove the old object

    def add_trait(self, name, trait):
        """Overrides base definition of *add_trait* in order to
        force call to *check_config* prior to execution when new traits are
        added.
        """
        super(Component, self).add_trait(name, trait)

        # if it's an input trait, register a callback to be called whenever it's changed
        if trait.iotype == 'in':
            self._set_input_callback(name)

        self.config_changed()
        if name not in self._valid_dict:
            if trait.iotype:
                self._valid_dict[name] = trait.iotype == 'in'
            if trait.iotype == 'in' and trait.trait_type and trait.trait_type.klass is ICaseIterator:
                self._num_input_caseiters += 1

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
        """Overrides base definition of *add_trait* in order to
        force call to *check_config* prior to execution when a trait is
        removed.
        """
        trait = self.get_trait(name)

        # remove the callback if it's an input trait
        if trait and trait.iotype == 'in':
            self._set_input_callback(name, remove=True)

        super(Component, self).remove_trait(name)
        self.config_changed()

        try:
            del self._valid_dict[name]
        except KeyError:
            pass

        if trait and trait.iotype == 'in' and trait.trait_type \
           and trait.trait_type.klass is ICaseIterator:
            self._num_input_caseiters -= 1

    @rbac(('owner', 'user'))
    def is_valid(self):
        """Return False if any of our variables is invalid."""
        if self._call_execute:
            return False
        if False in self._valid_dict.values():
            self._call_execute = True
            return False
        if self.parent is not None:
            srccomps = [n for n, v in self.get_expr_sources()]
            if srccomps:
                counts = self.parent.exec_counts(srccomps)
                for count, tup in zip(counts, self._expr_sources):
                    if count != tup[1]:
                        self._call_execute = True  # to avoid making this same
                        # check unnecessarily later, update the count
                        # information since we've got it, to avoid making
                        # another call
                        for i, tup in enumerate(self._expr_sources):
                            self._expr_sources[i] = (tup[0], count)
                        return False
        return True

    @rbac(('owner', 'user'))
    def config_changed(self, update_parent=True):
        """Call this whenever the configuration of this Component changes,
        for example, children are added or removed.
        """
        if update_parent and hasattr(self, 'parent') and self.parent:
            self.parent.config_changed(update_parent)
        self._input_names = None
        self._output_names = None
        self._connected_inputs = None
        self._connected_outputs = None
        self._container_names = None
        self._expr_sources = None
        self._call_check_config = True
        self._call_execute = True

    def list_inputs(self, valid=None, connected=None):
        """Return a list of names of input values.

        valid: bool (optional)
            If valid is not None, the list will contain names
            of inputs with matching validity.

        connected: bool (optional)
            If connected is not None, the list will contain names
            of inputs with matching *external* connectivity status.
        """
        if self._connected_inputs is None:
            nset = set([k for k, v in self.items(iotype='in')])
            self._connected_inputs = self._depgraph.get_connected_inputs()
            nset.update(self._connected_inputs)
            self._input_names = list(nset)
        self._input_names = [name_ for name_ in self._input_names
                                             if "[" not in name_]

        if valid is None:
            if connected is None:
                return self._input_names
            elif connected is True:
                return self._connected_inputs
            else:  # connected is False
                return [n for n in self._input_names
                                if n not in self._connected_inputs]

        valids = self._valid_dict
        ret = self._input_names
        ret = [n for n in ret if valids[n] == valid]

        if connected is True:
            return [n for n in ret if n in self._connected_inputs]
        elif connected is False:
            return [n for n in ret if n not in self._connected_inputs]

        return ret  # connected is None, valid is not None

    def list_outputs(self, valid=None, connected=None):
        """Return a list of names of output values.

        valid: bool (optional)
            If valid is not None, the list will contain names
            of outputs with matching validity.

        connected: bool (optional)
            If connected is not None, the list will contain names
            of outputs with matching *external* connectivity status.
        """
        if self._connected_outputs is None:
            nset = set([k for k, v in self.items(iotype='out')])
            self._connected_outputs = self._depgraph.get_connected_outputs()
            nset.update(self._connected_outputs)
            self._output_names = list(nset)
        self._output_names = [name_ for name_ in self._output_names
                                              if "[" not in name_]

        if valid is None:
            if connected is None:
                return self._output_names
            elif connected is True:
                return self._connected_outputs
            else:  # connected is False
                return [n for n in self._output_names
                                if n not in self._connected_outputs]

        valids = self._valid_dict
        ret = self._output_names
        ret = [n for n in ret if valids[n] == valid]

        if connected is True:
            return [n for n in ret if n in self._connected_outputs]
        elif connected is False:
            return [n for n in ret if n not in self._connected_outputs]

        return ret  # connected is None, valid is not None

    def list_containers(self):
        """Return a list of names of child Containers."""
        if self._container_names is None:
            visited = set([id(self), id(self.parent)])
            names = []
            for n, v in self.__dict__.items():
                if is_instance(v, Container) and id(v) not in visited:
                    visited.add(id(v))
                    names.append(n)
            self._container_names = names
        return self._container_names

    @rbac(('owner', 'user'))
    def connect(self, srcexpr, destexpr):
        """Connects one source expression to one destination expression.
        When a name begins with 'parent.', that indicates
        it is referring to a variable outside of this object's scope.

        srcexpr: str or ExprEvaluator
            Source expression object or expression string.

        destexpr: str or ExprEvaluator
            Destination expression object or expression string.
        """
        if isinstance(srcexpr, basestring):
            srcexpr = ConnectedExprEvaluator(srcexpr, self)
        if isinstance(destexpr, basestring):
            destexpr = ConnectedExprEvaluator(destexpr, self, is_dest=True)

        destpath = destexpr.text

        valid_updates = []
        if not srcexpr.refs_parent():
            if srcexpr.text not in self._valid_dict:
                valid_updates.append((srcexpr.text, True))
            self._connected_outputs = None  # reset cached value of connected outputs
        if not destpath.startswith('parent.'):
            valid_updates.append((destpath, False))
            self.config_changed(update_parent=False)

        super(Component, self).connect(srcexpr, destexpr)

        # this is after the super connect call so if there's a
        # problem we don't have to undo it
        for valids_update in valid_updates:
            self._valid_dict[valids_update[0]] = valids_update[1]

    @rbac(('owner', 'user'))
    def disconnect(self, srcpath, destpath):
        """Removes the connection between one source variable and one
        destination variable.
        """
        super(Component, self).disconnect(srcpath, destpath)
        if destpath in self._valid_dict:
            if '.' in destpath or '[' in destpath or ']' in destpath:
                del self._valid_dict[destpath]
            else:
                self._valid_dict[destpath] = True  # disconnected boundary outputs are always valid
        self.config_changed(update_parent=False)

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

            # should be safe assuming only one delegate of each type here, since
            # multiples would simply overwrite each other
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
                else:  # current tname wasn't matched to anything in self._delegates_
                    if hasattr(tdel, '_item_count') and tdel._item_count() > 0:
                        self.raise_exception("target delegate '%s' has no match"
                                             % tname, RuntimeError)

            for sname, tdel in matches.items():
                delegate = self._delegates_[sname]
                if hasattr(delegate, 'mimic'):
                    delegate.mimic(tdel)  # use target delegate as target

        # now update any matching inputs from the target
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
                #setattr(self, name, getattr(target, name))
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

    @rbac(('owner', 'user'))
    def get_expr_sources(self):
        """Return a list of tuples containing the names of all upstream
        components that are referenced in any of our ExprEvaluators, along with
        an initial exec_count of 0.
        """
        if self._expr_sources is None:
            self._expr_sources = [(u, 0)
                for u, v in self.get_expr_depends() if v == self.name]
        return self._expr_sources

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
                comp.set(name + '.path', path, force=True)

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
                    comp.set(fvarname + '.path', path, force=True)
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

    def step(self):
        """For Components that run other components (e.g., Assembly or Drivers),
        this will run one Component and return. For simple components, it is
        the same as *run()*.
        """
        self.run()

    def stop(self):
        """Stop this component."""
        self._stop = True

    @rbac(('owner', 'user'))
    def get_valid(self, names):
        """Get the value of the validity flag for the specified variables.
        Returns a list of bools.

        names: iterator of str
            Names of variables whose validity is requested.
        """
        valids = self._valid_dict
        return [valids[n] for n in names]

    def set_valid(self, names, valid):
        """Mark the io traits with the given names as valid or invalid."""
        valids = self._valid_dict
        for name in names:
            valids[name] = valid

    @rbac(('owner', 'user'))
    def invalidate_deps(self, varnames=None, force=False):
        """Invalidate all of our outputs if they're not invalid already.
        For a typical Component, this will always be all or nothing, meaning
        there will never be partial validation of outputs.

        NOTE: Components supporting partial output validation must override
        this function.

        Returns None, indicating that all outputs are newly invalidated, or [],
        indicating that no outputs are newly invalidated.
        """
        outs = self.list_outputs()
        valids = self._valid_dict

        self._call_execute = True
        self._set_exec_state('INVALID')

        # only invalidate connected inputs. inputs that are not connected
        # should never be invalidated
        if varnames is None:
            for var in self.list_inputs(connected=True):
                valids[var] = False
        else:
            conn = self.list_inputs(connected=True)
            if conn:
                for var in varnames:
                    if var in conn:
                        valids[var] = False

        # this assumes that all outputs are either valid or invalid
        if not force and outs and (valids[outs[0]] is False):
            # nothing to do because our outputs are already invalid
            return []

        for out in outs:
            valids[out] = False

        return None  # None indicates that all of our outputs are invalid.

    def update_outputs(self, outnames):
        """Do what is necessary to make the specified output Variables valid.
        For a simple Component, this will result in a *run()*.
        """
        self.run()

    def _get_log_level(self):
        """Return logging message level."""
        return self._logger.level

    def _set_log_level(self, level):
        """Set logging message level."""
        self._logger.level = level

    def register_published_vars(self, names, publish=True):
        if isinstance(names, basestring):
            names = [names]
        for name in names:
            obj = None
            parts = name.split('.', 1)
            if len(parts) == 1:
                if not name == __attributes__:
                    try:
                        obj = self.get(name)
                    except AttributeError:
                        self.raise_exception("%s has no attribute named '%s'"
                                              % (self.get_pathname(), name),
                                             NameError)
                    else:
                        if has_interface(obj, IComponent):
                            obj.register_published_vars(__attributes__, publish)
                            return

                if publish:
                    Publisher.register('.'.join([self.get_pathname(), name]),
                                       obj)
                    if name in self._publish_vars:
                        self._publish_vars[name] += 1
                    else:
                        self._publish_vars[name] = 1
                else:
                    Publisher.unregister('.'.join([self.get_pathname(), name]))
                    if name in self._publish_vars:
                        self._publish_vars[name] -= 1
                        if self._publish_vars[name] < 1:
                            del self._publish_vars[name]
            else:
                obj = getattr(self, parts[0])
                obj.register_published_vars(parts[1], publish)

    def publish_vars(self):
        pub = Publisher.get_instance()
        if pub:
            pub_vars = self._publish_vars.keys()
            if len(pub_vars) > 0:
                pname = self.get_pathname()
                lst = []
                for var in pub_vars:
                    if var == __attributes__:
                        lst.append((pname, self.get_attributes()))
                    else:
                        lst.append(('.'.join([pname, var]), getattr(self, var)))
                pub.publish_list(lst)

    def get_attributes(self, io_only=True):
        """ Get attributes of component. Includes inputs and ouputs and, if
        *io_only* is not true, a dictionary of attributes for each interface
        implemented by the component.

        io_only: Bool
            Set to true if we only want to populate the input and output
            fields of the attributes dictionary.
        """
        array_regex = re.compile("(?P<index>\[\d+\])(?P=index)*")

        attrs = {}
        attrs['type'] = type(self).__name__

        parameters = {}
        implicit = {}
        partial_parameters = {} 
        # We need connection information before we process the variables.
        if self.parent is None:
            connected_inputs = []
            connected_outputs = []
        else:
            connected_inputs = self._depgraph.get_connected_inputs()
            connected_outputs = self._depgraph.get_connected_outputs()

        # Additionally, we need to know if anything is connected to a
        # param, objective, or constraint.
        # Objectives and constraints are "implicit" connections. Parameters
        # are as well, though they lock down their variable targets.
        if self.parent and has_interface(self.parent, IAssembly):
            dataflow = self.parent.get_dataflow()
            for parameter, target in dataflow['parameters']:
                if "[" in target:
                    target_name = target.split('[')[0]
                    if not target_name in partial_parameters:
                        partial_parameters[target_name] = []
                    partial_parameters[target_name].append(parameter)
                else:
                    if not target in parameters:
                        parameters[target] = []
                    parameters[target].append(parameter)

            for target, objective in dataflow['objectives']:
                if target not in implicit:
                    implicit[target] = []
                implicit[target].append(objective)

            for target, constraint in dataflow['constraints']:
                if target not in implicit:
                    implicit[target] = []
                implicit[target].append(constraint)

        inputs = []
        outputs = []
        slots = []

        inputs_list  = self.list_inputs()
        outputs_list = self.list_outputs()
        io_list      = inputs_list + outputs_list

        for name in io_list:

            #for variable trees
            if '.' in name:
                continue

            trait = self.get_trait(name)
            meta = self.get_metadata(name)
            value = getattr(self, name)
            ttype = trait.trait_type

            # If this is a passthrough, reach in to get the trait
            if 'validation_trait' in meta:
                trait = meta['validation_trait']
                ttype = trait.trait_type

            # Each variable type provides its own basic attributes
            io_attr, slot_attr = ttype.get_attribute(name, value, trait, meta)

            io_attr['id'] = name

            # Framework variables
            if 'framework_var' in meta:
                io_attr['id'] = '~' + name

            io_attr['indent'] = 0

            io_attr['valid'] = self.get_valid([name])[0]
            io_attr['connected'] = ''
            io_attr['connection_types'] = 0

            connected = []
            partially_connected = []
            partially_connected_indices = []
            
            for inp in connected_inputs:
                cname = inp.split('[')[0]  # Could be 'inp[0]'.

                if cname == name:
                    connections = self._depgraph._var_connections(inp)
                    connections = [src for src, dst in connections]
                    connected.extend(connections)

                    if '[' not in inp:
                        io_attr['connection_types'] = io_attr['connection_types'] | 1

                    if '[' in inp:

                        io_attr['connection_types'] = io_attr['connection_types'] | 2
                        array_indices = re.findall("\[\d+\]", inp)
                        array_indices = [ index.split('[')[1].split(']')[0] for index in array_indices ]
                        array_indices = [ int(index) for index in array_indices ]
                       
                        dimensions = self.get(name).ndim - 1
                        shape = self.get(name).shape
                        column_index = 0

                        for dimension, array_index in enumerate( array_indices[:-1] ):
                            column_index = column_index + ( shape[-1] ** ( dimensions - dimension ) * array_index )
                        
                        column_index = column_index + array_indices[-1]

                        partially_connected_indices.append( column_index )

            if connected:
                io_attr['connected'] = str(connected).replace('@xin.', '')
            
            if partially_connected_indices:
                io_attr['partially_connected_indices'] = str(partially_connected_indices)

            if name in connected_outputs:  # No array element indications.
                connections = self._depgraph._var_connections(name)
                io_attr['connected'] = \
                    str([dst for src, dst in connections]).replace('@xout.', '')
            
            io_attr['implicit'] = []

            if "%s.%s" % (self.name, name) in partial_parameters:
                implicit_partial_indices = []
                shape = self.get(name).shape
                io_attr['connection_types'] = io_attr['connection_types'] | 8
                
                for key, target in partial_parameters.iteritems():
                    for value in target:
                        array_indices = re.findall("\[\d+\]", value)
                        array_indices = [index.split('[')[1].split(']')[0] for index in array_indices]
                        array_indices = [int(index) for index in array_indices]
                            
                        dimensions = self.get(name).ndim - 1
                        shape = self.get(name).shape
                        column_index = 0

                        for dimension, array_index in enumerate( array_indices[:-1] ):
                            column_index = column_index + ( shape[-1] ** ( dimensions - dimension ) * array_index )
                        
                        column_index = column_index + array_indices[-1]
                        implicit_partial_indices.append( column_index )

                io_attr['implicit_partial_indices'] = str(implicit_partial_indices)
                
                io_attr['implicit'].extend([driver_name.split('.')[0] for
                    driver_name in partial_parameters["%s.%s" % (self.name, name)]])

            if "%s.%s" % (self.name, name) in parameters:
                io_attr['connection_types'] = io_attr['connection_types'] | 4

                io_attr['implicit'].extend([driver_name.split('.')[0] for
                    driver_name in parameters["%s.%s" % (self.name, name)]])

                io_attr['implicit'] = str(io_attr['implicit'])

            if "%s.%s" % (self.name, name) in implicit:
                io_attr['connection_types'] = io_attr['connection_types'] | 4

                io_attr['implicit'] = str([driver_name.split('.')[0] for
                    driver_name in implicit["%s.%s" % (self.name, name)]])

            if not io_attr['implicit']:
                io_attr['implicit'] = ''

            # indicate that this var is the top element of a variable tree
            if io_attr.get('ttype') == 'vartree':
                vartable = self.get(name)
                if isinstance(vartable, VariableTree):
                    io_attr['vt'] = 'vt'

            if name in inputs_list:
                inputs.append(io_attr)
            else:
                outputs.append(io_attr)

            # Process singleton and contained slots.
            if not io_only and slot_attr is not None:
                # We can hide slots (e.g., the Workflow slot in drivers)
                if 'hidden' not in meta or meta['hidden'] is False:
                    slots.append(slot_attr)

            # For variables trees only: recursively add the inputs and outputs
            # into this variable list
            if 'vt' in io_attr:
                vt_attrs = vartable.get_attributes(io_only, indent=1,
                                                   parent=name,
                                                   valid=io_attr['valid'])
                if name in inputs_list:
                    inputs += vt_attrs.get('Inputs', [])
                else:
                    outputs += vt_attrs.get('Outputs', [])

        attrs['Inputs'] = inputs
        attrs['Outputs'] = outputs

        # Find any event traits

        tset1 = set(self._alltraits(events=True))
        tset2 = set(self._alltraits(events=False))
        event_set = tset1.difference(tset2)
        # Remove the Enthought events common to all has_traits objects
        event_set.remove('trait_added')
        event_set.remove('trait_modified')

        events = []
        for name in event_set:

            trait = self.get_trait(name)
            meta = self.get_metadata(name)
            ttype = trait.trait_type

            event_attr = ttype.get_attribute(name, meta)
            events.append(event_attr)

        if len(events) > 0:
            attrs['Events'] = events

        if not io_only:
            # Add Slots that are not inputs or outputs
            for name, value in self.traits().items():
                if name not in io_list and (value.is_trait_type(Slot)
                                            or value.is_trait_type(List)
                                            or value.is_trait_type(Dict)):
                    trait = self.get_trait(name)
                    meta = self.get_metadata(name)
                    value = getattr(self, name)
                    ttype = trait.trait_type
                    # We can hide slots (e.g., the Workflow slot in drivers)
                    if 'hidden' not in meta or meta['hidden'] is False:
                        io_attr, slot_attr = ttype.get_attribute(name, value, trait, meta)
                        if slot_attr is not None:
                            slots.append(slot_attr)

            if has_interface(self, IAssembly):
                attrs['Dataflow'] = self.get_dataflow()

            if has_interface(self, IDriver):
                attrs['Workflow'] = self.get_workflow()

            if has_interface(self, IHasCouplingVars):
                couples = []
                objs = self.list_coupling_vars()
                for indep, dep in objs:
                    attr = {}
                    attr['independent'] = indep
                    attr['dependent'] = dep
                    couples.append(attr)
                attrs['CouplingVars'] = couples

            if has_interface(self, IHasObjectives):
                objectives = []
                objs = self.get_objectives()
                for key in objs.keys():
                    attr = {}
                    attr['name'] = str(key)
                    attr['expr'] = objs[key].text
                    attr['scope'] = objs[key].scope.name
                    objectives.append(attr)
                attrs['Objectives'] = objectives

            if has_interface(self, IHasParameters):
                parameters = []
                for key, parm in self.get_parameters().items():
                    attr = {}

                    if isinstance(parm, ParameterGroup):
                        attr['target'] = str(tuple(parm.targets))
                    else:
                        attr['target'] = parm.target

                    attr['name']    = str(key)
                    attr['low']     = parm.low
                    attr['high']    = parm.high
                    attr['scaler']  = parm.scaler
                    attr['adder']   = parm.adder
                    attr['fd_step'] = parm.fd_step
                    #attr['scope']   = parm.scope.name
                    parameters.append(attr)

                attrs['Parameters'] = parameters

            constraints = []
            has_constraints = False
            if has_interface(self, IHasConstraints) or \
               has_interface(self, IHasEqConstraints):
                has_constraints = True
                cons = self.get_eq_constraints()
                for key, con in cons.iteritems():
                    attr = {}
                    attr['name']    = str(key)
                    attr['expr']    = str(con)
                    attr['scaler']  = con.scaler
                    attr['adder']   = con.adder
                    constraints.append(attr)

            if has_interface(self, IHasConstraints) or \
               has_interface(self, IHasIneqConstraints):
                has_constraints = True
                cons = self.get_ineq_constraints()
                for key, con in cons.iteritems():
                    attr = {}
                    attr['name']    = str(key)
                    attr['expr']    = str(con)
                    attr['scaler']  = con.scaler
                    attr['adder']   = con.adder
                    constraints.append(attr)

            if has_constraints:
                attrs['Constraints'] = constraints

            if has_interface(self, IHasEvents):
                attrs['Triggers'] = [dict(target=path)
                                   for path in self.get_events()]

        if len(slots) > 0:
            attrs['Slots'] = slots

        return attrs

    def applyJ(self, arg, result):
        """Multiply an input vector by the Jacobian. For an Explicit Component,
        this automatically forms the "fake" residual, and calls into the
        function hook "apply_deriv.
        """
        for key in result:
            result[key] = -arg[key]

        if hasattr(self, 'apply_deriv'):
            self.apply_deriv(arg, result)
            return

        # Optional specification of the Jacobian
        input_keys, output_keys, J = self.provideJ()

        ibounds = {}
        nvar = 0
        for key in input_keys:
            val = self.get(key)
            width = flattened_size('.'.join((self.name, key)), val)
            ibounds[key] = (nvar, nvar+width)
            nvar += width

        obounds = {}
        nvar = 0
        for key in output_keys:
            val = self.get(key)
            width = flattened_size('.'.join((self.name, key)), val)
            obounds[key] = (nvar, nvar+width)
            nvar += width

        for okey in result:
            for ikey in arg:
                if ikey not in result:
                    i1, i2 = ibounds[ikey]
                    o1, o2 = obounds[okey]
                    if i2 - i1 == 1:
                        if o2 - o1 == 1:
                            Jsub = float(J[o1, i1])
                            result[okey] += Jsub*arg[ikey]
                        else:
                            Jsub = J[o1:o2, i1:i2]
                            tmp = Jsub*arg[ikey]
                            result[okey] += tmp.reshape(result[okey].shape)
                    else:
                        tmp = flattened_value('.'.join((self.name, ikey)),
                                              arg[ikey]).reshape(1, -1)
                        Jsub = J[o1:o2, i1:i2]
                        tmp = inner(Jsub, tmp)
                        if o2 - o1 == 1:
                            result[okey] += float(tmp)
                        else:
                            result[okey] += tmp.reshape(result[okey].shape)


def _show_validity(comp, recurse=True, exclude=None, valid=None):  # pragma no cover
    """Prints out validity status of all input and output traits
    for the given object, optionally recursing down to all of its
    Component children as well.
    """
    exclude = exclude or set()

    def _show_validity_(comp, recurse, exclude, valid, result):
        pname = comp.get_pathname()
        for name, val in comp._valid_dict.items():
            if name not in exclude and (valid is None or val is valid):
                if '.' in name:  # mark as fake boundary var
                    result['.'.join([pname, '*%s*' % name])] = val
                else:
                    result['.'.join([pname, name])] = val
        if recurse:
            for name in comp.list_containers():
                obj = getattr(comp, name)
                if is_instance(obj, Component):
                    _show_validity_(obj, recurse, exclude, valid, result)
    result = {}
    _show_validity_(comp, recurse, exclude, valid, result)
    for name, val in sorted([(n, v) for n, v in result.items()], key=lambda v: v[0]):
        print '%s: %s' % (name, val)
