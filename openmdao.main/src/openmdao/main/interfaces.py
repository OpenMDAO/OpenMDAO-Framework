
"""
Interfaces for the OpenMDAO project.
"""

# pylint: disable=E0213,E0211,W0232

from zope.interface import implements, classImplements, Attribute, Interface,\
                           directlyProvides

from openmdao.main.constants import SAVE_CPICKLE


class IArchitecture(Interface):

    parent = Attribute("parent Assembly")
    data_recorders = Attribute("List of CaseRecorder instances where data from "
                               "the optimization should be stored.")
    param_types = Attribute("list of types of allowed parameters.  "
                            "Valid values are: ['continuous','discrete','enum']")
    constraint_types = Attribute("list of types of allowed constraints. "
                                 " Valid values are: ['eq', 'ineq']")
    num_allowed_objectives = Attribute("number of objectives supported.")
    has_coupling_vars = Attribute("True if coupling variables are required.")
    has_global_des_vars = Attribute("True if the architecture requires a problem "
                                    "formulation with global design variables in it")

    def configure():
        """sets up drivers,workflows, and data connections in
        the assembly to configure the architecture
        """

    def clear():
        """removes all the drivers, workflows, and data connections in the
        assembly, leaving the assembly cleaned up.
        """


class IContainer(Interface):
    """Interface for an object containing variables and other IContainers."""

    parent = Attribute("parent of this Container (or None)")
    name = Attribute("name of this Container")

    def add(name, obj):
        """Add a Container object to this Container.
        Returns the added Container object.
        """

    def contains(path):
        """Return True if the child specified by the given dotted path
        name is contained in this Container.
        """

    def get_dyn_trait(pathname, iotype=None, trait=None):
        """Returns a trait if a trait with the given pathname exists, possibly
        creating the trait "on-the-fly." If an attribute exists with the given
        pathname but no trait is found or can be created, or if pathname
        references a trait in a parent scope, None will be returned. If no
        attribute exists with the given pathname within this scope, an
        AttributeError will be raised.

        pathname: str
            Pathname of the desired trait.  May contain dots.

        iotype: str (optional)
            Expected iotype of the trait.

        trait: TraitType (optional)
            Trait to be used for validation.
        """

    def get(path):
        """Return the object specified by the given
        path, which may contain '.' characters.
        """

    def get_pathname(rel_to_scope=None):
        """ Return full path name to this container, relative to scope
        *rel_to_scope*. If *rel_to_scope* is *None*, return the full pathname.
        """

    def items(recurse=False, **metadata):
        """Return a list of tuples of the form (rel_pathname, obj) for each
        trait of this Container that matches the given metadata. If recurse is
        True, also iterate through all child Containers of each Container
        found.
        """

    def list_containers():
        """Return a list of names of child Containers."""

    def get_metadata(traitpath, metaname=None):
        """Retrieve the metadata associated with the trait found using
        traitpath.  If metaname is None, return the entire metadata dictionary
        for the specified trait. Otherwise, just return the specified piece
        of metadata.  If the specified piece of metadata is not part of
        the trait, None is returned.
        """

    def get_trait(name, copy=False):
        """Returns the trait indicated by name, or None if not found.  No recursive
        search is performed if name contains dots.  This is a replacement
        for the trait() method on HasTraits objects, because that method
        can return traits that shouldn't exist. Do not use the trait() function
        unless you are certain that the named trait exists.
        """

    def pre_delete():
        """Perform any required operations before being deleted."""

    def post_load():
        """Perform any required operations after model has been loaded."""

    def remove(name):
        """Remove the specified child from this container and remove any
        public trait objects that reference that child. Notify any
        observers."""

    def revert_to_defaults(recurse=True):
        """Sets the values of all of the inputs to their default values."""

    def save(outstream, fmt=SAVE_CPICKLE, proto=-1):
        """Save the state of this object and its children to the given
        output stream. Pure Python classes generally won't need to
        override this because the base class version will suffice, but
        Python extension classes will have to override. The format
        can be supplied in case something other than cPickle is needed.

        outstream: file or string
            Stream to save to.

        fmt: int
            Format for saved data.

        proto: int
            Protocol used.
        """

    def set(path, value):
        """Set the value of the Variable specified by the given path, which
        may contain '.' characters. The Variable will be set to the given
        value, subject to validation and constraints.
        """

    def cpath_updated():
        """Called whenever this Container's position in the Container hierarchy changes."""

    def configure():
        """Called once, after this Container has been placed in a rooted Container hierarchy."""


class IContainerProxy(IContainer):
    """Marker interface for proxy containers."""


class IVariableTree(IContainer):
    """Marker interface for VariableTrees."""


class IComponent(IContainer):
    """Interface for an IContainer object that can be executed to update the values of
    its output variables based on the values of its input variables.
    """

    def check_config(strict=False):
        """Verify that this component is properly configured to execute.
        Classes overriding this method must call the base class method.
        If strict is True, even configuration warnings should raise an exception.
        """

    def run(force=False):
        """Run this object. This should include fetching input variables,
        executing, and updating output variables. Do not override this function.
        """

    def list_inputs(valid=None):
        """Return a list of names of input values. If valid is not None,
        the the list will contain names of inputs with matching validity.
        """

    def list_outputs(valid=None):
        """Return a list of names of output values. If valid is not None,
        the the list will contain names of outputs with matching validity.
        """

    def get_expr_depends():
        """Returns a list of tuples of the form (src_comp_name, dest_comp_name)
        for each dependency resulting from ExprEvaluators in this Component.
        """

    def get_abs_directory():
        """Return absolute path of execution directory."""

    def checkpoint(outstream, fmt=SAVE_CPICKLE):
        """Save sufficient information for a restart. By default, this
        just calls *save()*.
        """

    def restart(instream):
        """Restore state using a checkpoint file. The checkpoint file is
        typically a delta from a full saved state file. If checkpoint is
        overridden, this should also be overridden.
        """

    def get_file_vars():
        """Return list of (filevarname, filevarvalue, file trait) owned by this
        component."""

    def stop():
        """Stop this component."""


class IImplicitComponent(IComponent):
    """An interface for a component that represents an implicit function
    """

    def list_states(self):
        """Return a list of names of state variables."""

    def list_residuals(self):
        """Return a list of names of residual variables."""

    def evaluate(self):
        """run a single step to calculate the residual
        values for the given state var values.
        """

class IPseudoComp(IContainer):
    """Special interface for Pseudocomps for checking.
    """

    def check_config(strict=False):
        """Verify that this component is properly configured to execute.
        Classes overriding this method must call the base class method.
        If strict is True, even configuration warnings should raise an exception.
        """

    def run(force=False):
        """Run this object. This should include fetching input variables,
        executing, and updating output variables. Do not override this function.
        """

    def list_inputs(valid=None):
        """Return a list of names of input values. If valid is not None,
        the the list will contain names of inputs with matching validity.
        """

    def list_outputs(valid=None):
        """Return a list of names of output values. If valid is not None,
        the the list will contain names of outputs with matching validity.
        """


class IDriver(IComponent):
    """An interface for objects that manage the iteration of workflows.
    """

    workflow = Attribute("object that knows how to run a single iteration over"
                         " this Driver's iteration set")

    def iteration_set(self):
        """Return a set of names (not pathnames) containing all Components
        in this Driver's workflow or any of its sub-workflows.
        """

    def requires_derivs(self):
        """ Returns True if this Driver requires derivatives. """


class ISolver(IDriver):
    """An interface for drivers that are solvers.
    """

    pass


class IOptimizer(IDriver):
    """An interface for drivers that are optimizers.
    """

    pass


class IAssembly(IComponent):
    """An interface for objects that contain a driver and its workflow components."""

    driver = Attribute("object that manage's the iteration of a workflow")

    def connect(srcpath, destpath):
        """Connects one source variable to one destination variable.

        srcpath: str
            Name of source variable.

        destpath: str
            Name of destination variable.
        """

    def disconnect(srcpath, destpath):
        """Removes the connection between one source variable and one
        destination variable.
        """


class IFactory(Interface):
    """An object that creates and returns objects based on a type string."""

    def create(self, typname, version=None, server=None,
               res_desc=None, **ctor_args):
        """Return an object of type *typname,* (or a proxy to it if it resides
        in another process) using the specified package version, server
        location, and resource description. Returns None if this factory is
        unable to create the specified type.
        """

    def get_available_types(self, groups=None):
        """Return a list tuples of the form (typename, meta_dict) for all
        available types based on the given list of entry point groups. If
        groups is None, all types matching any openmdao entry point group will
        be returned.
        """

    def get_signature(self, typname, version=None):
        """Return constructor argument signature for *typname,* using the
        specified package version. The return value is a dictionary:

        args: list
            List of 1 or 2-element lists. The first element is the argument
            name, the second element is the default value.

        varargs: string
            The name of the '*' argument.

        kwargs: string
            The name of the '**' argument.
        """

    def cleanup(self):
        """This function is optional, but if present it will be called by
        the FactoryManager prior to the factory being removed from the
        list of active factories.
        """


class IResourceAllocator(Interface):
    """An object responsible for allocating CPU/disk resources for a particular
    host, cluster, load balancer, etc."""

    def max_servers(resource_desc):
        """Return the maximum number of servers which could be deployed for
        `resource_desc`.  The value needn't be exact, but performance may
        suffer if it overestimates.  The value is used to limit the number
        of concurrent evaluations."""

    def time_estimate(resource_desc):
        """Return `(estimate, criteria)` indicating how well this resource
        allocator can satisfy the `resource_desc` request.  The estimate will
        be:

        - >0 for an estimate of walltime (seconds).
        -  0 for no estimate.
        - -1 for no resource at this time.
        - -2 for no support for `resource_desc`.

        The returned criteria is a dictionary containing information related
        to the estimate, such as load averages, unsupported resources, etc."""

    def deploy(name, resource_desc, criteria):
        """Deploy a server suitable for `resource_desc`.
        `criteria` is the dictionary returned by :meth:`time_estimate`.
        Returns a proxy to the deployed server."""

    def list_allocated_components():
        """Return a list of tuples `(hostname, pid, component_name)` for each
        Component currently allocated by this allocator."""


class ICaseIterator(Interface):
    """An iterator that returns Case objects."""

    def __iter__():
        """Returns an iterator of Cases"""


class ICaseRecorder(Interface):
    """A recorder of Cases."""

    def startup():
        """Perform any operations required to start-up this recorder."""

    def register(driver, inputs, outputs):
        """Register names for input and output data coming from `driver`."""

    def record_constants(constants):
        """Record constant data."""

    def record(driver, inputs, outputs, exc, case_uuid, parent_uuid):
        """Record input and output data from `driver`."""

    def get_iterator():
        """Return an iterator that matches the format that this recorder uses."""

    def close():
        """Perform any operations required to shut-down this recorder."""


class ICaseFilter(Interface):
    """Selects cases."""

    def select(seqno, case):
        """Returns True if `case` should be used, where `seqno` is the index
        of `case` in the sequence of cases."""


class IDOEgenerator(Interface):
    """An iterator that returns lists of normalized values that are mapped
    to design variables by a Driver.
    """

    num_parameters = Attribute("number of parameters in the DOE")

    def __iter__():
        """Return an iterator object where each iteration returns
        a set of values in the range [0., 1.].
        """


class IUncertainVariable(Interface):
    """A variable which supports uncertainty"""
    def getvalue():
        """Returns either value from expected() or from sample() depending on
        the global or local uncertainty setting."""

    def expected():
        """Calculates the expected value of the uncertainty distribution."""

    def sample():
        """Generates a random number from an uncertain distribution."""


class IHasCouplingVars(Interface):
    """An interface to support the declaration of coupling variables
    """

    def add_coupling_var(self, indep, dep):
        """adds a new pair (indep/dep) of coupling variables

        indep: str
            name of the independent variable, or the variable
            that should be varied, to meet the coupling constraint
        dep: str
            name of the dependent variable, or the variable that
            needs to be forced to be consistent with the independent
        """

    def remove_coupling_var(self, couple):
        """removes the pair (indep/dep) of coupling variables.

        couple: tuple of str
            two tuple of (<indep>,<dep>) to be removed
        """

    def list_coupling_vars(self):
        """returns a ordered list of names of the coupling variables"""

    def clear_coupling_vars(self):
        """removes all coupling variables"""

#class IHasGlobalDesVars(Interface):
    #"""Interface for managing global design variables in assemblies

    #parent: Assembly
        #containing assembly where the HasGlobalDesVars lives.
    #"""

    #def add_global_des_var(name,targets,low,high,scalar=1.0,adder=0.0):
        #"""adds a global design variable to the assembly

        #name: str
            #name given to the global design variable
        #targets: list of str
            #names of the component variables that this global design variable should link to
        #low: float
            #minimum allowed value for the global design variable
        #high: float
            #maximum allowed value for the global design variable
        #scalar: float (optional)
            #default: 1.0. scalar value which is multiplied by the value of the global design
            #variable before setting target values
        #adder: float (optiona)
            #default: 0.0. amount which is added to the value of the global
            #design variable before setting target values
        #"""

    #def remove_global_des_var(name):
        #"""removed the global design variable from the assembly"""

    #def clear_global_des_vars():
        #"""removes all global design variables from the assembly"""

    #def list_global_des_vars():
        #"""returns a list of all the names of global design variable objects in the assembly"""
        
class IPredictor(Interface):

    def get_uncertain_value(value):
        """Converts a deterministic value into an uncertain quantity which
        matches the uncertain variable type the surrogate predicts."""

    def predict(X):
        """Predicts a value of from the surrogate model for the given independent values in X.

        X: list
            The input values for which the predicted output is requested.

        Returns the predicted output value.
        """

class ISurrogate(IPredictor):

    def train(X, Y):
        """Trains the surrogate model, based on the given training data set.

        X: iterator of lists
            Values representing the training case input history.
        y: iterator
            Training case output history for this surrogate's output,
            which corresponds to the training case input history given by X.
        """


class IMultiFiSurrogate(IPredictor):

    def train_multifi(X, Y):
        """Trains the surrogate model, based on the given training data set.

        X: list of (m samples, n inputs) lists of lists
            Values representing the multi-fidelity training case input history.
        y: list of lists
            Training case output history for this surrogate's output,
            which corresponds to the multi-fidelity training case input history 
            given by X.
        """


class IHasParameters(Interface):

    def add_parameter(param_name, low=None, high=None):
        """Adds a parameter to the driver.

        param_name: str
            Name of the parameter to add.
        low: number (optional)
            Minimum allowed value the optimzier can use for this parameter. If not specified,
            then the *low* value from the variable is used.
        high: number (optional)
            Maximum allowed value the optimizer can use for this parameter. If not specified,
            then the *high* value from the variable is used.
        """

    def remove_parameter(param_name):
        """Removes the specified parameter. Raises a KeyError if param_name is not found.

        param_name: str
            Name of the parameter to remove.
        """

    def list_param_targets():
        """Lists the targets of all parameters."""

    def clear_parameters():
        """Removes all parameters."""

    def get_parameters():
        """Returns an ordered dict of parameter objects."""

    def set_parameters(X):
        """Pushes the values in the X input array into the corresponding
        variables in the model.

        X: iterator
            iterator of input values with an order defined to match the order
            of parameters returned by the get_parameters method. X must support
             the len() function.
        """

    def total_parameters(self):
        """Returns the total number of values to be set."""

    def eval_parameters(self, scope=None, dtype='d'):
        """Return evaluated parameter values.

        dtype: string or None
            If not None, return an array of this dtype. Otherwise just return
            a list (useful if parameters may be of different types).
        """

    def get_lower_bounds(self, dtype='d'):
        """Return lower bound values.

        dtype: string or None
            If not None, return an array of this dtype. Otherwise just return
            a list (useful if parameters may be of different types).
        """

    def get_upper_bounds(self, dtype='d'):
        """Return upper bound values.

        dtype: string or None
            If not None, return an array of this dtype. Otherwise just return
            a list (useful if parameters may be of different types).
        """

    def get_fd_steps(self, dtype='d'):
        """Return fd_step values, they may include None.

        dtype: string or None
            If not None, return an array of this dtype. Otherwise just return
            a list (useful if it's valid to have None for a step size).
        """


class IHasEvents(Interface):
    def add_event(name):
        """Adds an event variable to be set when set_events is called.

        name: str
            Name of the event variable that should be set during execution.
        """

    def remove_event(name):
        """Removes the specified event variable.

        name: str
            Name of the event to be removed.
        """

    def get_events():
        """Return the list of event variables to be set."""

    def clear_events():
        """Remove all event variables from the list."""

    def set_events():
        """Set all events in the event list."""


class IHasEqConstraints(Interface):
    """An Interface for objects containing equality constraints."""

    def add_constraint(expr_string):
        """Adds an equality constraint.

        expr_string: str
            A string containing an assignment, e.g., 'a = 2*b+5'
        """

    def remove_constraint(expr_string):
        """Removes the given constraint.

        expr_string: str
            A string matching the constraint to be
            removed.  Whitespace is ignored when matching.
        """

    def clear_constraints():
        """Removes all constraints."""

    def get_eq_constraints():
        """Returns an ordered dictionary of equality constraint objects."""

    def total_eq_constraints(self):
        """Returns the total number of equality constraint values."""

    def eval_eq_constraints(scope=None):
        """Evaluates the constraint expressions and returns a list of values.
        The form of the constraint is transformed if necessary such that the
        right-hand-side is 0.0.  The values returned are the evaluation of the
        left-hand-side.
        """


class IHasIneqConstraints(Interface):
    """An Interface for objects containing inequality constraints."""

    def add_constraint(expr_string):
        """Adds an inequality constraint as a string containing an inequality,
        for example, 'a > b'.
        """

    def remove_constraint(expr_string):
        """Removes the constraint matching the given string. Whitespace is ignored."""

    def clear_constraints():
        """Removes all constraints."""

    def get_ineq_constraints():
        """Returns an ordered dict of inequality constraint objects."""

    def total_ineq_constraints(self):
        """Returns the total number of inequality constraint values."""

    def eval_ineq_constraints(scope=None):
        """Evaluates the constraint expressions and returns a list of values. Constraints
        are coerced into a form where the right-hand-side is 0., and the value returned
        is the evaluation of the left-hand-side.
        """


class IHasConstraints(IHasEqConstraints, IHasIneqConstraints):
    """An Interface for objects containing both equality and inequality constraints."""

    def add_constraint(expr_string):
        """Adds a constraint as a string containing
        an assignment or an inequality, e.g., 'a=b' or 'a<=b'.
        """

    def get_constraints():
        """Returns an ordered dict of constraint objects."""

    def total_constraints(self):
        """Returns the total number of constraint values."""

    def eval_constraints(scope=None):
        """Evaluates the constraint expressions and returns a list of values."""


class IHas2SidedConstraints(Interface):
    """An Interface for objects that can accept constraints defined like
    a < x < b, where x is a variable and a and b are constants."""

    def add_2sided_constraint(lhs, center, rhs, rel, name=None, scope=None,
                               linear=False):
        """Adds an 2-sided constraint as four strings; a left-hand side, a
        center, a right-hand side, and a comparator ('<','>','<=', or '>=')
        """

    def get_2sided_constraints(linear=None):
        """Returns an ordered dict of inequality constraint objects.

        linear: obj
            Set to True or False to return linear or nonlinear constraints.
            Default is None, for all constraints."""

    def list_2sided_constraints(self):
        """Return a list of strings containing constraint expressions."""

    def list_2sided_constraint_targets(self):
        """Returns a list of outputs suitable for calc_gradient()."""


class IHasObjectives(Interface):
    """An Interface for objects having a multiple objectives."""

    def add_objectives(obj_iter):
        """Takes an iterator of objective strings and creates
        objectives for them in the driver.
        """

    def add_objective(expr):
        """Adds an objective to the driver.

        expr: string
            String containing the objective expression.
         """

    def remove_objective(expr):
        """Removes the specified objective expression. Spaces within
        the expression are ignored.
        """

    def clear_objectives():
        """Removes all objectives."""

    def eval_objectives():
        """Returns a list of values of the evaluated objectives."""

    def get_expr_depends():
        """Returns a list of tuples of the form (src_comp_name, dest_comp_name)
        for each dependency introduced by our objectives.
        """


class IHasObjective(IHasObjectives):
    """An Interface for objects having a single objective."""

    def eval_objective():
        """Returns the value of the evaluated objective."""


class IHasResponses(Interface):
    """An Interface for objects having a responses."""

    def add_responses(response_iter):
        """Takes an iterator of response strings and creates
        responses for them in the driver.
        """

    def add_response(expr):
        """Adds an response to the driver.

        expr: string
            String containing the response expression.
         """

    def remove_response(expr):
        """Removes the specified response expression. Spaces within
        the expression are ignored.
        """

    def clear_responses():
        """Removes all responses."""

    def eval_responses():
        """Returns a list of values of the evaluated responses."""

    def get_expr_depends():
        """Returns a list of tuples of the form (src_comp_name, dest_comp_name)
        for each dependency introduced by our responses.
        """


class IVariable(Interface):
    def validate(obj, name, value):
        """ Validates that the specified value is valid and can be assigned
        to the data value corresponding to this Variable.
        """

class ISystem(Interface):
    pass

def obj_has_interface(obj, *ifaces):
    """Returns True if the specified object implements one of the interfaces
    specified."""
    for iface in ifaces:
        if iface.providedBy(obj):
            return True
    return False
