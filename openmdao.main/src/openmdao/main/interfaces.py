
"""
Interfaces for the OpenMDAO project.
"""

# pylint: disable-msg=E0213,E0211,W0232


from enthought.traits.api import Interface, Instance, Int, Str

from openmdao.main.workflow import Workflow
from openmdao.main.constants import SAVE_CPICKLE

# to check if an interface is implemented, you can call
# validate_implements(obj, klass) from enthought.traits.trait_types
# or if the object you're checking inherits from HasTraits, you can call 
# obj.has_traits_interface(*ifaces) on it.
# Note that validate_implements checks for existence of attributes and member 
# functions but does not type check attributes. It also doesn't care whether
# a class calls 'implements' or not.  has_traits_interface, on the other hand,
# believes whatever the class says it implements and doesn't verify anything.

class IContainer(Interface):
    """Interface for an object containing variables and other IContainers."""
    # FIXME: figure out how to declare parent as a reference to an IContainer. Syntax below doesn't work.
    #parent = Instance(IContainer)
    name = Str('')
    
    def add(self, name, obj, **kw_args):
        """Add a Container object to this Container.
        Returns the added Container object.
        """
        
    def add_trait(self, name, trait):
        """Overrides HasTraits definition of *add_trait* in order to
        keep track of dynamically added traits for serialization.
        """

    #def build_trait(self, pathname, iotype=None, trait=None):
        #"""Asks the object to dynamically create a trait for the 
        #attribute given by pathname, based on whatever knowledge the
        #component has of that attribute.
        
        #pathname: str
            #The dotted path to the specified attribute.
            
        #iotype: str, optional
            #The data direction, either 'in' or 'out'.
            
        #trait: TraitType, optional
            #A validation trait for the given attribute.
        #"""
        
    def connect(self, srcpath, destpath):
        """Connects one source variable to one destination variable. 
        When a pathname begins with 'parent.', that indicates
        that it is referring to a variable outside of this object's scope.
        
        srcpath: str
            Pathname of source variable
            
        destpath: str
            Pathname of destination variable
        """
        
    def contains(self, path):
        """Return True if the child specified by the given dotted path
        name is contained in this Container. 
        """
    
    def create_alias(self, path, alias, iotype=None, trait=None):
        """Create a trait that maps to some internal variable designated by a
        dotted path. If a trait is supplied as an argument, use that trait as
        a validator for the aliased value. The resulting trait will have the
        alias as its name and will be added to 
        self.  An exception will be raised if the trait already exists.
        """

    def disconnect(self, srcpath, destpath):
        """Removes the connection between one source variable and one 
        destination variable.
        """
        
    def get_dyn_trait(self, pathname, iotype=None, trait=None):
        """Returns a trait if a trait with the given pathname exists, possibly
        creating the trait 'on-the-fly'. If an attribute exists with the given
        pathname but no trait is found or can be created, or if pathname
        references a trait in a parent scope, None will be returned. If no
        attribute exists with the given pathname within this scope, an
        AttributeError will be raised.
        
        pathname: str
            Pathname of the desired trait.  May contain dots.
            
        iotype: str, optional
            Expected iotype of the trait.
            
        trait: TraitType, optional
            Trait to be used for validation
        """

    def get(self, path, index=None):
        """Return the object specified by the given 
        path, which may contain '.' characters.  
        """

    def get_pathname(self, rel_to_scope=None):
        """ Return full path name to this container, relative to scope
        *rel_to_scope*. If *rel_to_scope* is *None*, return the full pathname.
        """
        
    def get_wrapped_attr(self, name):
        """If the named trait can return a TraitValWrapper, then this
        function will return that, with the value set to the current value of
        the named variable. Otherwise, it functions like *getattr*, just
        returning the value of the named variable. Raises an exception if the
        named trait cannot be found. The value will be copied if the trait has
        a 'copy' metadata attribute that is not None. Possible values for
        'copy' are 'shallow' and 'deep'.
        """
        
    def items(self, recurse=False, **metadata):
        """Return a list of tuples of the form (rel_pathname, obj) for each
        trait of this Container that matches the given metadata. If recurse is
        True, also iterate through all child Containers of each Container
        found.
        """

    def list_containers(self):
        """Return a list of names of child Containers."""

    def get_metadata(self, traitpath, metaname=None):
        """Retrieve the metadata associated with the trait found using
        traitpath.  If metaname is None, return the entire metadata dictionary
        for the specified trait. Otherwise, just return the specified piece
        of metadata.  If the specified piece of metadata is not part of
        the trait, None is returned.
        """
        
    def get_trait ( self, name, copy = False ):
        """Returns the trait indicated by name, or None if not found.  No recursive
        search is performed if name contains dots.  This is a replacement
        for the trait() method on HasTraits objects, because that method
        can return traits that shouldn't exist. Do not use the trait() function
        unless you are certain that the named trait exists.
        """

    def invoke(self, path, *args, **kwargs):
        """Call the callable specified by **path**, which may be a simple
        name or a dotted path, passing the given arguments to it, and 
        return the result.
        """
    
    def pre_delete(self):
        """Perform any required operations before being deleted."""
    
    def post_load(self):
        """Perform any required operations after model has been loaded."""

    def remove(self, name):
        """Remove the specified child from this container and remove any
        public trait objects that reference that child. Notify any
        observers."""
        
    def remove_trait(self, name):
        """Overrides HasTraits definition of remove_trait in order to
        keep track of dynamically added traits for serialization.
        """

    def revert_to_defaults(self, recurse=True):
        """Sets the values of all of the inputs to their default values."""
            
    def save(self, outstream, fmt=SAVE_CPICKLE, proto=-1):
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

    def set(self, path, value, index=None, src=None, force=False):
        """Set the value of the Variable specified by the given path, which
        may contain '.' characters. The Variable will be set to the given
        value, subject to validation and constraints. *index*, if not None,
        should be a list of ints, at most one for each array dimension of the
        target value.
        """ 

    def tree_rooted(self):
        """Called after the hierarchy containing this Container has been
        defined back to the root. This does not guarantee that all sibling
        Containers have been defined. It also does not guarantee that this
        component is fully configured to execute.
        """
            
    
class IComponent(IContainer):
    """Interface for an IContainer object that can be executed to update the values of
    its output variables based on the values of its input variables.
    """

    def check_config (self):
        """Verify that this component is fully configured to execute.
        This function is called once prior to the first execution of this
        component and may be called explicitly at other times if desired. 
        Classes that override this function must still call the base class
        version.
        """
    
    def run (self, force=False):
        """Run this object. This should include fetching input variables,
        executing, and updating output variables. Do not override this function.
        """
 
    def is_valid(self):
        """Return False if any of our variables is invalid."""

    def list_inputs(self, valid=None):
        """Return a list of names of input values. If valid is not None,
        the the list will contain names of inputs with matching validity.
        """
        
    def list_outputs(self, valid=None):
        """Return a list of names of output values. If valid is not None,
        the the list will contain names of outputs with matching validity.
        """
            
    def connect(self, srcpath, destpath):
        """Connects one source variable to one destination variable. 
        When a pathname begins with 'parent.', that indicates
        that it is referring to a variable outside of this object's scope.
        
        srcpath: str
            Pathname of source variable
            
        destpath: str
            Pathname of destination variable
        """
        
    def disconnect(self, srcpath, destpath):
        """Removes the connection between one source variable and one 
        destination variable.
        """
    
    def get_expr_depends(self):
        """Returns a list of tuples of the form (src_comp_name, dest_comp_name)
        for each dependency resulting from ExprEvaluators in this Component.
        """

    def get_expr_sources(self):
        """Return a list of tuples containing the names of all upstream components that are 
        referenced in any of our objectives, along with an initial exec_count of 0.
        """

    def get_abs_directory (self):
        """Return absolute path of execution directory."""

    def checkpoint (self, outstream, fmt=SAVE_CPICKLE):
        """Save sufficient information for a restart. By default, this
        just calls *save()*.
        """

    def restart (self, instream):
        """Restore state using a checkpoint file. The checkpoint file is
        typically a delta from a full saved state file. If checkpoint is
        overridden, this should also be overridden.
        """

    def get_file_vars(self):
        """Return list of (filevarname,filevarvalue,file trait) owned by this
        component."""

    def step (self):
        """For Components that run other components (e.g., Assembly or Drivers),
        this will run one Component and return. For simple components, it is
        the same as *run()*.
        """

    def stop (self):
        """Stop this component."""

    def get_valid(self, names):
        """Get the value of the validity flag for each of the named io traits."""
                
    def set_valid(self, names, valid):
        """Mark the io traits with the given names as valid or invalid."""
            
    def invalidate_deps(self, varnames=None, force=False):
        """Invalidate all of our outputs if they're not invalid already.
        For a typical Component, this will always be all or nothing, meaning
        there will never be partial validation of outputs.  Components
        supporting partial output validation must override this function.
        
        Returns None, indicating that all outputs are invalidated.
        """

    def update_outputs(self, outnames):
        """Do what is necessary to make the specified output Variables valid.
        For a simple Component, this will result in a *run()*.
        """

    
    
class IDriver(Interface):
    """A marker interface for Drivers. To make a usable IDriver plug-in,
    you must still inherit from Driver.
    """
    
    workflow = Instance(Workflow, allow_none=True)
    
    def iteration_set(self):
        """Return a set of names (not pathnames) containing all Components
        in all of the workflows managed by this Driver
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

    #modelID = Int(desc="Identifies an assembly or a part.")



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
        """Return ``(estimate, criteria)`` indicating how well this resource
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
        """Return a list of tuples ``(hostname, pid, component_name)`` for each
        Component currently allocated by this allocator."""

    
class ICaseIterator(Interface):
    """An iterator that returns Case objects."""
    
    # unfortunately we can't just use __iter__ here because
    # double underscore members are ignored when the implementation
    # is validated, meaning that ANY class would match this interface
    # if we just used __iter__.
    def get_iter():
        """Return an iterator of Case objects."""
        
        
class IDOEgenerator(Interface):
    """An iterator that returns arrays of normalized values that are mapped
    to design variables by a Driver.
    """
    
    num_parameters = Int(desc="number of parameters in the DOE")
    
    def __iter__():
        """Return an iterator object."""


class IUncertainVariable(Interface):
    """A variable which supports uncertainty"""
    def getvalue():
        """returns either value from expected() or from sample() depending on 
        the golbal or local uncertainty setting"""
    
    def expected():
        """Calculates the expected value of the uncertainty distribution"""
    
    def sample():
        """Generates a random number from an uncertain distribution"""

class ICaseRecorder(Interface):
    """A recorder of Cases."""
    
    def record(case):
        """Record the given Case."""
        
    def get_iterator():
        """Return an iterator that matches the format that this recorder uses."""
        
class ISurrogate(Interface):
    
    def get_uncertain_value(self,value): 
        """Converts a deterministic value into an uncertain quantity which 
        matches the uncertain variable type the surrogate predicts"""
    
    def predict(self, X):
        """Predicts a value of from the surrogate model, for the given independent values in X.
            
        X: list
            the input values for which the predicted output is requested.
            
        Returns the predicted output value
        """

    def train(self, X, Y): 
        """Trains the surrogate model, based on the given training data set.
        
        X: iterator of lists
            Values representing the training case input history.
        y: iterator
            Training case output history for this surrogate's output,
            which corresponds to the training case input history given by X.
        """
    
class IHasParameters(Interface):
    
    def add_parameter(self,param_name,low=None,high=None):
        """Adds a parameter to the driver.
        
        param_name: str 
            Name of the parameter to add.
        low: number, optional
            Minimum allowed value the optimzier can use for this parameter. If not specified, 
            then the *low* value from the variable is used. 
        high: number, optional
            Maximum allowed value the optimizer can use for this parameter. If not specified, 
            then the *high* value from the variable is used.
        """
        
    def add_parameters(self, param_iter):
        """Adds the given iterator of parameters to the driver.
        
        param_iter: Iterator returning entries of the form (param_name, low, high)
            Adds each parameter in the iterator to the driver, setting lower and
            upper bounds based on the values of *low* and *high*.
        """

    def remove_parameter(self,param_name):
        """Removes the specified parameter. Raises a KeyError if param_name is not found.
        
        param_name: str
            Name of the parameter to remove.
        """
        
    def list_parameters(self):
        """Lists all the parameters."""
        
    def clear_parameters(self):
        """Removes all parameters."""
        
    def get_parameters(self):
        """Returns an ordered dict of parameter objects."""

    def set_parameters(self, X): 
        """Pushes the values in the X input array into the corresponding 
        variables in the model.
        
        X: iterator
            iterator of input values with an order defined to match the order of parameters returned 
            by the list_parameter method. X must support the len() function.
        """
        
class IHasEvents(Interface):
    def add_event(self, name):
        """Adds an event variable to be set when set_events is called.
        
        name: str
            Name of the event variable that should be set during execution.
        """
            
    def remove_event(self, name):
        """Removes the specified event variable.
        
        name: str
            Name of the event to be removed.
        """
        
    def get_events(self):
        """Return the list of event variables to be set."""
    
    def clear_events(self):
        """Remove all event variables from the list."""
        
    def set_events(self):
        """Set all events in the event list."""


class IHasEqConstraints(Interface):
    """An Interface for objects containing equality constraints."""
    
    def add_constraint(self, expr_string):
        """Adds an equality constraint.
        
        expr_string: str
            A string containing an assignment, e.g., 'a = 2*b+5'
        """

    def add_eq_constraint(self, lhs, rhs):
        """Adds an equality constraint.
        
        lhs: str
            Left hand side of the equality
            
        rhs: str
            Right hand side of the equality
        """
        
    def remove_constraint(self, expr_string):
        """Removes the given constraint.
        
        expr_string: str
            A string matching the constraint the constraint to be
            removed.  Whitespace is ignored when matching.
        """
        
    def clear_constraints(self):
        """Removes all constraints."""
        
    def get_eq_constraints(self):
        """Returns an ordered dictionary of equality constraint objects."""
        return self._constraints

    def eval_eq_constraints(self): 
        """Evaluates the constraint expressions and returns a list of tuples of the 
        form (lhs, rhs, operator, is_violated), where lhs is the right hand side
        of the equality, lhs is the left hand side of the equality, operator is 
        the string '=', and is_violated is a boolean which is True if the constraint
        is currently violated.  The operator entry in the tuple is always the same
        for an equality constraint, but is included for consistency with the 
        eval_ineq_constraints function used for inequality constraints.
        """

    
class IHasIneqConstraints(Interface):
    """An Interface for objects containing inequality constraints."""
    
    def add_constraint(self, expr_string):
        """Adds an inequality constraint as a string containing an inequality, 
        for example, 'a > b'.
        """

    def add_ineq_constraint(self, lhs, rel, rhs):
        """Adds an inequality constraint as three strings; a left hand side,
        a right hand side, and a relation.  The relation must be one of the 
        following: '<', '>', '<=', or '>='.
        """

    def remove_constraint(self, expr_string):
        """Removes the constraint matching the given string. Whitespace is ignored."""
        
    def clear_constraints(self):
        """Removes all constraints."""
        
    def get_ineq_constraints(self):
        """Returns an ordered dict of inequality constraint objects."""

    def eval_ineq_constraints(self): 
        """Evaluates the constraint expressions and returns a list of tuples of the 
        form (lhs, rhs, relation, is_violated).
        """

class IHasConstraints(IHasEqConstraints, IHasIneqConstraints):
    """An Interface for objects containing both equality and inequality constraints."""
    
    def add_constraint(self, expr_string):
        """Adds a constraint as a string containing
        an assignment or an inequality, e.g., 'a=b' or 'a<=b'.
        """

class IHasObjective(Interface): 
    """An Interface for objects having a single objective."""

    def add_objective(self, expr):
        """Sets the objective of this driver to be the specified expression.
        If there is a preexisting objective in this driver, it is replaced.
        
        expr: string
            String containing the objective expression.
         """
            
    def list_objective(self):
        """Returns the expression string for the objective."""
    
    def get_objective(self):
        """Returns the objective object."""
    
    def eval_objective(self):
        """Returns the value of the evaluated objective."""

    def get_expr_depends(self):
        """Returns a list of tuples of the form (src_comp_name, dest_comp_name)
        for each dependency introduced by our objective. 
        """
    

class IHasObjectives(object): 
    """An Interface for objects having a multiple objectives."""

    def add_objectives(self, obj_iter):
        """Takes an iterator of objective strings and creates
        objectives for them in the driver.
        """

    def add_objective(self, expr):
        """Adds an objective to the driver. 
        
        expr: string
            String containing the objective expression.
         """
            
    def remove_objective(self, expr):
        """Removes the specified objective expression. Spaces within
        the expression are ignored.
        """
        
    def list_objectives(self):
        """Returns a list of objective expression strings."""
    
    def clear_objectives(self):
        """Removes all objectives."""
        
    def get_objectives(self):
        """Returns an ordered dict of objective objects."""

    def eval_objectives(self):
        """Returns a list of values of the evaluated objectives."""
 
    def get_expr_depends(self):
        """Returns a list of tuples of the form (src_comp_name, dest_comp_name)
        for each dependency introduced by our objectives.
        """
        

