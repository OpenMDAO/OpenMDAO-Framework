
"""
Interfaces for the OpenMDAO project.
"""

# pylint: disable-msg=E0213,E0211,W0232


from enthought.traits.api import Interface, Instance, Int

# to check if an interface is implemented, you can call
# validate_implements(obj, klass) from enthought.traits.trait_types
# or if the object you're checking inherits from HasTraits, you can call 
# obj.has_traits_interface(*ifaces) on it.
# Note that validate_implements checks for existence of attributes and member 
# functions but does not type check attributes. It also doesn't care whether
# a class calls 'implements' or not.  has_traits_interface, on the other hand,
# believes whatever the class says it implements and doesn't verify anything.

class IComponent(Interface):
    """A marker interface for Components."""
    
class IWorkflow(Interface):
    """An object that can run a group of components in some order. """
    
    scope = Instance(IComponent, allow_none=True)
    
    def __iter__():
        """Return an iterator object that iterates over components in
        the desired execution order.
        """
        
    def __contains__(self, comp):
        """Return True if this workflow contains the given Component."""
        
    def __len__(self):
        """Returns the number of components in this workflow."""
        
    def add(self, comp):
        """Add the Component to this workflow."""
        
    def remove(self, comp):
        """Remove the Component from this workflow.  Do not raise
        an exception if the component is not found.
        """
        
    def clear(self):
        """Remove all Components from this workflow."""
        
    def contents(self):
        """Return a list of all Components in this Workflow. No
        ordering is assumed.
        """

    def run(self):
        """ Run the components in the workflow. """
    
    def step(self):
        """Run a single component in the Workflow."""

    def stop(self):
        """
        Stop all components in this workflow.
        We assume it's OK to to call stop() on something that isn't running.
        """
        
class IDriver(Interface):
    """A marker interface for Drivers. To make a usable IDriver plug-in,
    you must still inherit from Driver.
    """
    
    workflow = Instance(IWorkflow, allow_none=True)
    
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
    
    def __iter__():
        """Return an iterator object."""
        
    def next():
        """Return the next Case. If no Cases remain, raise a StopIteration
        exception.
        """
        
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
        """Generates a random number from an uncertainty distribution"""

class ICaseRecorder(Interface):
    """A recorder of Cases."""
    
    def record(case):
        """Record the given Case."""
        
class ISurrogate(Interface):
    
    def get_uncertain_value(self,value): 
        """converts a deterministic value into an uncertain quantity which 
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
            values representing the training case input history
        y: iterator
            training case output history for this surrogate's output,
            which corresponds to the training case input history given by X
        """
    
class IHasParameters(Interface):
    
    def add_parameter(self,param_name,low=None,high=None):
        """adds a parameter to the driver. 
        
        param_name : str 
            name of the parameter to add to the driver
        low : number, optional
            minimum allowed value the optimzier can use for this parameter. If not specified, 
            then the 'low' value from the public variable is used. 
        high : number, optional
            maximum allowed value the optimizer can use for this parameter. If not specified, 
            then the 'high' value from the public variable is used.
        """
        
    def remove_parameter(self,param_name):
        """removes the specified parameter. Raises a KeyError if param_name is not found
        
        param_name: str
            the name of the parameter to remove
        """
        
    def list_parameters(self):
        """Lists all the parameters"""
        
    def clear_parameters(self):
        """Removes all parameters"""


def obj_has_interface(obj, *ifaces):
    """Returns True if the specified object inherits from HasTraits and
    implements one or more of the specified interfaces.
    """
    try:
        if not obj.has_traits_interface(*ifaces):
            return False
    except Exception:
        return False
    return True
    

