
"""
Interfaces for the OpenMDAO project.
"""

# pylint: disable-msg=E0213,E0211,W0232

from zope.interface import implements, Attribute, Interface

from openmdao.main.workflow import Workflow
from openmdao.main.constants import SAVE_CPICKLE

class IArchitecture(Interface):
    
    parent = Attribute("parent Assembly")
    
    def configure(): 
        """sets up drivers,workflows, and data connections in 
        the assembly to configure the architecture
        """
   
    def clear(): 
        """removes all the drivers, workflows, and data connections in the assembly, 
        leaving the assembly cleaned up. 
        """

class IContainer(Interface):
    """Interface for an object containing variables and other IContainers."""
    
    parent = Attribute("parent of this Container (or None)")
    name = Attribute("name of this Container")
    
    def add(name, obj, **kw_args):
        """Add a Container object to this Container.
        Returns the added Container object.
        """
        
    def add_trait(name, trait):
        """Overrides HasTraits definition of *add_trait* in order to
        keep track of dynamically added traits for serialization.
        """

    def connect(srcpath, destpath):
        """Connects one source variable to one destination variable. 
        When a pathname begins with 'parent.', that indicates
        that it is referring to a variable outside of this object's scope.
        
        srcpath: str
            Pathname of source variable.
            
        destpath: str
            Pathname of destination variable.
        """
        
    def contains(path):
        """Return True if the child specified by the given dotted path
        name is contained in this Container. 
        """
    
    def create_alias(path, alias, iotype=None, trait=None):
        """Create a trait that maps to some internal variable designated by a
        dotted path. If a trait is supplied as an argument, use that trait as
        a validator for the aliased value. The resulting trait will have the
        alias as its name and will be added to 
        self.  An exception will be raised if the trait already exists.
        """

    def disconnect(srcpath, destpath):
        """Removes the connection between one source variable and one 
        destination variable.
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

    def get(path, index=None):
        """Return the object specified by the given 
        path, which may contain '.' characters.  *index*, if not None,
        should be a list of container indices and/or single entry lists of attribute 
        names.  For example, to get something like comp.x[2]['mykey'].child.value, 
        *index* would look like:  [2,'mykey',['child'],['value']].  Attribute names
        are placed in sublists because strings are valid container indices.
        """

    def get_pathname(rel_to_scope=None):
        """ Return full path name to this container, relative to scope
        *rel_to_scope*. If *rel_to_scope* is *None*, return the full pathname.
        """
        
    def get_wrapped_attr(name):
        """If the named trait can return an AttrWrapper, then this
        function will return that, with the value set to the current value of
        the variable. Otherwise, it functions like *getattr*, just
        returning the value of the variable. Raises an exception if the
        variable cannot be found. The value will be copied if the variable has
        a 'copy' metadata attribute that is not None. Possible values for
        'copy' are 'shallow' and 'deep'.
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
        
    def get_trait (name, copy = False):
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
        
    def remove_trait(name):
        """Overrides HasTraits definition of remove_trait in order to
        keep track of dynamically added traits for serialization.
        """

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

    def set(path, value, index=None, src=None, force=False):
        """Set the value of the Variable specified by the given path, which
        may contain '.' characters. The Variable will be set to the given
        value, subject to validation and constraints. *index*, if not None,
        should be a list of container indices and/or single entry lists of attribute 
        names.  For example, to get something like comp.x[2]['mykey'].child.value, 
        *index* would look like:  [2,'mykey',['child'],['value']].  Attribute names
        are placed in sublists to avoid ambiguity with string container indices.
        """ 

    def tree_rooted():
        """Called after the hierarchy containing this Container has been
        defined back to the root. This does not guarantee that all sibling
        Containers have been defined. It also does not guarantee that this
        component is fully configured to execute.
        """
            
    
class IComponent(IContainer):
    """Interface for an IContainer object that can be executed to update the values of
    its output variables based on the values of its input variables.
    """

    def check_config ():
        """Verify that this component is fully configured to execute.
        This function is called once prior to the first execution of this
        component and may be called explicitly at other times if desired. 
        Classes that override this function must still call the base class
        version.
        """
    
    def run (force=False):
        """Run this object. This should include fetching input variables,
        executing, and updating output variables. Do not override this function.
        """
 
    def is_valid():
        """Return False if any of our variables is invalid."""

    def list_inputs(valid=None):
        """Return a list of names of input values. If valid is not None,
        the the list will contain names of inputs with matching validity.
        """
        
    def list_outputs(valid=None):
        """Return a list of names of output values. If valid is not None,
        the the list will contain names of outputs with matching validity.
        """
            
    def connect(srcpath, destpath):
        """Connects one source variable to one destination variable. 
        When a pathname begins with 'parent.', that indicates
        that it is referring to a variable outside of this object's scope.
        
        srcpath: str
            Pathname of source variable.
            
        destpath: str
            Pathname of destination variable.
        """
        
    def disconnect(srcpath, destpath):
        """Removes the connection between one source variable and one 
        destination variable.
        """
    
    def get_expr_depends():
        """Returns a list of tuples of the form (src_comp_name, dest_comp_name)
        for each dependency resulting from ExprEvaluators in this Component.
        """

    def get_expr_sources():
        """Return a list of tuples containing the names of all upstream components that are 
        referenced in any of our objectives, along with an initial exec_count of 0.
        """

    def get_abs_directory():
        """Return absolute path of execution directory."""

    def checkpoint (outstream, fmt=SAVE_CPICKLE):
        """Save sufficient information for a restart. By default, this
        just calls *save()*.
        """

    def restart (instream):
        """Restore state using a checkpoint file. The checkpoint file is
        typically a delta from a full saved state file. If checkpoint is
        overridden, this should also be overridden.
        """

    def get_file_vars():
        """Return list of (filevarname, filevarvalue, file trait) owned by this
        component."""

    def step ():
        """For Components that run other components (e.g., Assembly or Drivers),
        this will run one Component and return. For simple components, it is
        the same as *run()*.
        """

    def stop ():
        """Stop this component."""

    def get_valid(names):
        """Get the value of the validity flag for each of the named io traits."""
                
    def set_valid(names, valid):
        """Mark the io traits with the given names as valid or invalid."""
            
    def invalidate_deps(varnames=None, force=False):
        """Invalidate all of our outputs if they're not invalid already.
        For a typical Component, this will always be all or nothing, meaning
        there will never be partial validation of outputs.  Components
        supporting partial output validation must override this function.
        
        Returns None, indicating that all outputs are invalidated.
        """

    def update_outputs(outnames):
        """Do what is necessary to make the specified output Variables valid.
        For a simple Component, this will result in a *run()*.
        """
    
    
class IDriver(Interface):
    """A marker interface for Drivers. To make a usable IDriver plug-in,
    you must still inherit from Driver.
    """
    
    workflow = Attribute("object that knows how to run a single iteration over this Driver's iteration set")
    
    def iteration_set(self):
        """Return a set of names (not pathnames) containing all Components
        in this Driver's workflow.
        """

class IFactory (Interface):
    """An object that creates and returns objects based on a type string."""

    def create (typ):
        """Create an object of the specified type and return it, or a proxy
        to it if it resides in another process."""


#class IGeomQueryObject (Interface):
    #"""A Component representing an object having physical dimensions and
    #shape that can be queried for geometric information like surfaces, curves,
    #etc.
    
    #The exact API is still to be determined, but will probably be based 
    #largely on the querying portion of the CAPRI API.
    
    #"""

    #modelID = Attribute("Identifies an assembly or a part.")



#class IGeomModifier (Interface):
    #"""An interface to a geometry kernel that allows new geometry to be
    #created and modified.
    
    #The API is still to be determined.
    #"""
    
class IResourceAllocator (Interface):
    """An object responsible for allocating CPU/disk resources for a particular
    host, cluster, load balancer, etc."""

    def max_servers (resource_desc):
        """Return the maximum number of servers which could be deployed for
        `resource_desc`.  The value needn't be exact, but performance may
        suffer if it overestimates.  The value is used to limit the number
        of concurrent evaluations."""

    def time_estimate (resource_desc):
        """Return `(estimate, criteria)` indicating how well this resource
        allocator can satisfy the `resource_desc` request.  The estimate will
        be:

        - >0 for an estimate of walltime (seconds).
        -  0 for no estimate.
        - -1 for no resource at this time.
        - -2 for no support for `resource_desc`.

        The returned criteria is a dictionary containing information related
        to the estimate, such as load averages, unsupported resources, etc."""

    def deploy (name, resource_desc, criteria):
        """Deploy a server suitable for `resource_desc`.
        `criteria` is the dictionary returned by :meth:`time_estimate`.
        Returns a proxy to the deployed server."""

    def list_allocated_components ():
        """Return a list of tuples `(hostname, pid, component_name)` for each
        Component currently allocated by this allocator."""

    
class ICaseIterator(Interface):
    """An iterator that returns Case objects."""
    
    def __iter__():
        """Returns an iterator of Cases"""

        
class IDOEgenerator(Interface):
    """An iterator that returns lists of normalized values that are mapped
    to design variables by a Driver.
    """
    
    num_parameters = Attribute("number of parameters in the DOE")
    
    def __iter__():
        """Return an iterator object where each iteration returns
        a set of values in the range [0., 1.].
        """

class IDifferentiator(Interface):
    """A plugin to driver that can determine derivatives between a driver's
    parameters and its objectives and constraints."""
    
    def calc_gradient():
        """Returns the gradient vectors for this Driver's workflow"""

    def calc_hessian():
        """Returns the Hessian matrix for this Driver's workflow"""
        
        
class IUncertainVariable(Interface):
    """A variable which supports uncertainty"""
    def getvalue():
        """Returns either value from expected() or from sample() depending on 
        the global or local uncertainty setting."""
    
    def expected():
        """Calculates the expected value of the uncertainty distribution."""
    
    def sample():
        """Generates a random number from an uncertain distribution."""

class ICaseRecorder(Interface):
    """A recorder of Cases."""
    
    def record(case):
        """Record the given Case."""
        
    def get_iterator():
        """Return an iterator that matches the format that this recorder uses."""
        
#class IHasCouplingVars(Interface): 
    #"""An interface for assemblies to support the declaration of coupling vars"""
    
    #def add_coupling_var(indep,constraint,tollerance=.0001,scalar=1.0,adder=0.0):
        #"""adds a new coupling var to the assembly
        
        #indep: str
            #name of the independent variable, or the variable that should be varied to meet the coupling 
            #constraint
        #constraint: str
            #constraint equation, meeting the requirements of the IHasConstraints interface, which must be met 
            #to enforce the coupling
        #tolerance: float (optional)
            #default value of .0001, specifies the tolerance to which the coupling constraint must be met to be 
            #statisfied
        #scalar: float (optional)
            #default value of 1.0, specifies the scalar value that the constraint equation will be multiplied by 
            #before being returned
        #adder: float (optional)
            #default value of 0.0, specifies the value which will be added to the constraint before being returned
        #"""        
    
    #def remove_coupling_var(indep):
        #"""removes the coupling var, idenfied by the indepent name, from the assembly. 
        
        #indep: str 
            #name of the independent variable from the CouplingVar   
        #"""
    
    #def list_coupling_vars(): 
        #"""returns a ordered list of names of the coupling vars in the assembly"""
    
    #def clear_coupling_vars(): 
        #"""removes all coupling variables from the assembly"""
    
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
        
class ISurrogate(Interface):
    
    def get_uncertain_value(value): 
        """Converts a deterministic value into an uncertain quantity which 
        matches the uncertain variable type the surrogate predicts."""
    
    def predict(X):
        """Predicts a value of from the surrogate model for the given independent values in X.
            
        X: list
            The input values for which the predicted output is requested.
            
        Returns the predicted output value.
        """

    def train(X, Y): 
        """Trains the surrogate model, based on the given training data set.
        
        X: iterator of lists
            Values representing the training case input history.
        y: iterator
            Training case output history for this surrogate's output,
            which corresponds to the training case input history given by X.
        """
    
class IHasParameters(Interface):
    
    def add_parameter(param_name,low=None,high=None):
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

    def eval_eq_constraints(): 
        """Evaluates the constraint expressions and returns a list of tuples of the 
        form (lhs, rhs, operator, is_violated), where rhs is the right-hand side
        of the equality, lhs is the left-hand side of the equality, operator is 
        the string '=', and is_violated is a boolean which is True if the constraint
        is currently violated.  The operator entry in the tuple is always the same
        for an equality constraint, but is included for consistency with the 
        eval_ineq_constraints function used for inequality constraints.
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

    def eval_ineq_constraints(): 
        """Evaluates the constraint expressions and returns a list of tuples of the 
        form (lhs, rhs, relation, is_violated).
        """

class IHasConstraints(IHasEqConstraints, IHasIneqConstraints):
    """An Interface for objects containing both equality and inequality constraints."""
    
    def add_constraint(expr_string):
        """Adds a constraint as a string containing
        an assignment or an inequality, e.g., 'a=b' or 'a<=b'.
        """

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


def obj_has_interface(obj, *ifaces):
    """Returns True if the specified object implements one of the interfaces
    specified."""
    for iface in ifaces:
        if issubclass(iface, Interface) and iface.providedBy(obj):
            return True
    return False
    
