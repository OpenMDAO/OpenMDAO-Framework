
"""
Interfaces for the OpenMDAO project.
"""

# pylint: disable-msg=E0213,E0211,W0232

#public symbols
__all__ = ['IContainer', 'IComponent', 'IAssembly', 'IDriver', 'IFactory',
           'IGeomQueryObject', 'IGeomModifier', 'IResourceAllocator',
           'IVariable', 'IWorkflow', 'ICaseIterator']

__version__ = "0.1"


from zope.interface import Interface, Attribute


class IContainer (Interface):
    """The interface to an object that allows set/get of its public
    attributes through the framework. This interface is provided by the
    Container class."""

    name = Attribute('the name of the Container')

    parent = Attribute('the IContainer object containing this object, or None')

    def create (type_name, name, version=None, factory=None):
        """Create an object with the given type and the given name within
        this Container."""

    def delete (name):
        """Remove the named object from this container and notify any
        observers"""

    def get_objs (iface, recurse=False, attrdict=None):
        """Return a list of objects with the specified interface that also have
        attributes with values that match those passed in the attrdict
        dictionary."""

    def get_pathname ():
        """Return the name (dot delimited) that uniquely
        identifies this object's location within a hierarchy of IContainers"""

    def get (name):
        """return the value of a public variable"""

    def set (name, value):
        """Set the value of a public variable"""

    def save_state (outstream, format='cPickle'):
        """Save the state of this object and its children to the given
        output stream. Pure python classes generally won't need to
        override this because the base class version will suffice, but
        python extension classes will have to override. The format
        can be supplied in case something other than cPickle is needed."""

    def load (instream, format='cPickle'):
        """Replace the current object in the hierarchy with the object
        loaded from the input stream. Pure python classes generally
        won't need to override this, but extensions will. The format
        can be supplied in case something other than cPickle is needed."""

    def config_from_obj (obj):
        """This is intended to allow a newer version of a component to
        configure itself based on an older version. By default, values
        of dictionary entries from the old object will be copied to the
        new one."""



class IComponent (Interface):
    """A runnable Container. This interface is provided by the Component
    class"""

    state =  Attribute('the current state of this object '+
                       '(UNKNOWN,IDLE,RUNNING,WAITING)')

    resource_desc = Attribute('a dict containing key-value pairs that are used'+
                              'to select a ResourceAllocator')

    directory = Attribute('If non-null, the directory to execute in.')

    def add_socket (name, iface, doc=''):
        """Specify a named placeholder for a component with the given
        interface."""

    def remove_socket (name):
        """Remove an existing Socket"""

    def post_config ():
        """Perform any final initialization after configuration has been set,
        and verify that the configuration is correct."""

    def update_inputs ():
        """Fetch input variables."""

    def execute ():
        """Perform calculations or other actions."""

    def update_outputs ():
        """Update output variables"""

    def run ():
        """Run this object. This should include fetching input variables,
        executing, and updating output variables."""

    def get_directory ():
        """Return absolute path of execution directory."""

    def push_dir (directory):
        """Change directory to dir, remembering current for later pop_dir()."""

    def pop_dir ():
        """Return to previous directory saved by push_dir()."""

    def checkpoint (outstream):
        """Save sufficient information for a restart. By default, this
        just calls save_state()"""

    def restart (instream):
        """Restore state using a checkpoint file. The checkpoint file is
        typically a delta from a full saved state file."""

    def step ():
        """For Components that contain Workflows (e.g., Assembly), this will run
        one Component in the Workflow and return. For simple components, it is the
        same as run()."""

    def require_gradients (varname, gradients):
        """Requests that the component be able to provide (after execution) a
        list of gradients of a variable w.r.t. a list of variables. The format
        of the gradients list is [dvar_1, dvar_2, ..., dvar_n]. The component
        should return a list with entries of either a name, a tuple of the
        form (name,index) or None.  None indicates that the component cannot
        compute the specified derivative. name indicates the name of a
        scalar variable in the component that contains the gradient value, and
        (name,index) indicates the name of an array variable and the index of
        the entry containing the gradient value. If the component cannot
        compute any gradients of the requested varname, it can just return
        None."""

    def require_hessians (varname, deriv_vars):
        """Requests that the component be able to provide (after execution)
        the hessian of a variable w.r.t. a list of variables. The format of
        deriv_vars is [dvar_1, dvar_2, ..., dvar_n]. The component should
        return one of the following:
          1) a name, which would indicate that the component contains
                      a 2D array variable or matrix containing the hessian
          2) an array of the form [[dx1dx1, dx1dx2, ... dx1dxn],
                                            ...
                                   [dxndx1, dxndx2, ... dxndxn]]
             with entries of either name, (name,index), or None. name
             indicates that a scalar variable in the component contains the
             desired hessian matrix entry. (name,index) indicates that
             an array variable contains the value at the specified index.
             If index is a list with two entries, that indicates that
             the variable containing the entry is a 2d array or matrix.
          3) None, which means the the component cannot compute any values
             of the hessian."""



class IAssembly (Interface):
    """Contains a workflow, a driver, and a collection of child 
    Compnents/Containers and manages connections between its children."""
    
    def update_inputs(comp_name):
        """update the inputs for the named child component."""
    
    
class IDriver (Interface):
    """Executes a Workflow until certain criteria are met."""

    workflow = Attribute('the object that orders execution of'+
                         'components that are driven by this driver')



class IFactory (Interface):
    """An object that creates and returns objects based on a type string."""

    def create (typ):
        """Create an object of the specified type and return it, or a proxy
        to it if it resides in another process."""



class IGeomQueryObject (Interface):
    """A Component representing an object having physical dimensions and
    shape that can be queried for geometric information like surfaces, curves,
    etc.
    
    The exact API is still to be determined, but will probably be based 
    largely on the querying portion of the CAPRI API.
    
    """

    modelID = Attribute("Identifies the model. "+
                        "This can either be a part or an assembly of parts")



class IGeomModifier (Interface):
    """An interface to a geometry kernel that allows new geometry to be
    created and modified.
    
    The API is still to be determined.
    """
    
class IResourceAllocator (Interface):
    """An object responsible for allocating CPU/disk resources for a particular
    host, cluster, load balancer, etc."""

    def time_estimate (resource_desc):
        """Return the estimated time (wall clock) to perform the specified
        computation. A return of -1 indicates that the computation cannot
        be performed using this resource. A return of 0 indicates that
        the computation can be performed, but there is no time estimate."""

    def deploy (resource_desc):
        """Execute the process described in the resource description on the
        computing resource associated with this object."""

    def list_allocated_components ():
        """Return a list of tuples (hostname, pid, component_name) for each
        Component currently allocated by this allocator."""




class IVariable (Interface):
    """ An object representing data to be passed between Components within
    the framework. It will perform validation when assigned to another
    IVariable. It can notify other objects when its value is modified.
    """

    value = Attribute('the value')

    default = Attribute('the default value')

    current = Attribute('if False, the value is not current')

    def revert ():
        """ Return this Variable to its default value."""

    def validate (variable):
        """ Raise an exception if the assigned variable is not compatible"""

    def add_observer (obs_funct, *args, **metadata):
        """ Add a function to be called when this variable is modified"""

    def notify_observers ():
        """Call data_changed(self,args,metadata) on all of this object's
        observers."""


class IWorkflow(Interface):
    """An object that executes its nodes in a specified order.
    """
    def add_node(node):
        """add a new node to this workflow"""

    
class ICaseIterator(Interface):
    """An iterator that returns Case objects"""
    
    def __iter__():
        """Return an iterator object."""
        
    def next():
        """Return the next item. If no items remain, raise a StopIteration
        exception.
        """

