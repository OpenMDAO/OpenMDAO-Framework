.. index:: API


.. _Application-Programming-Interface-(API):

Application Programming Interface (API)
---------------------------------------

Python objects that provide any of the following interfaces can be used to
extend the functionality of the framework. As discussed in an earlier section,
these objects are called :term:`plug-ins`. In this framework, plug-ins will be
registered by putting them in a directory or an egg file that is in the search
path of the  :term:`FactoryManager` or in the search path of one of the
ObjServerFactories that is itself registered with the :term:`FactoryManager`.

Each of the interfaces below will be provided by a class within the framework, so a
developer will generally just inherit from a given framework class and override a
small number of methods in order to have a fully functional framework plug-in. For
diagrams of these classes and their relationships, see  :ref:`Class-Diagrams`.
These interfaces are subject to change as the design of the framework matures, but
they should give a good indication of our intent.

.. index:: IContainer 

.. _IContainer: 

::

    class IContainer (Interface):
	"""The interface to an object that allows set/get of its public attributes through the framework. 
	This interface is provided by the Container class."""

	name = Attribute('the name of the Container')

	parent = Attribute('the IContainer object containing this object, or None')

    def add_child(obj, private=False):
            """Add an object (must provide IContainer interface) to this
            Container, and make it a member of this Container's public
            interface if private is False."""
            
    def remove_child(name):
            """Remove the specified child from this container and remove any
            Variable objects from _pub that reference that child."""

    def create (typ_name, name, version=None, factory=None):
            """Create an object with the given type and the given name 
            within this Container."""
        
    def delete (name):
            """Remove the named object from this container and notify any
            observers"""

    def contains (path):
            """Return True if the child specified by the given dotted path
            name is publicly accessibly and is contained in this Container. 
            """
        
    def get_objs_by_type (typ, recurse=False, attrdict):
            """Return a list of objects of the specified type that also have
            attributes with values that match those passed in the attrdict
            dictionary. If type is an Interface, then a list of objects providing
            that interface will be returned."""

    def get_pathname ():
            """Return the name (dot delimited) that uniquely
            identifies this object's location within a hierarchy of IContainers"""

    def get (name):
            """Return the value of a public Variable."""

    def getvar (name):
            """Return a public Variable."""

    def set (name, value):
            """Set the value of a public variable"""

    def setvar (name, varobj):
            """Set the value of a public Variable to the value of the Variable var."""

    def save (out, format=constants.SAVE_CPICKLE):
            """Save the state of this object and its children to the given 
            output stream. Pure Python classes generally won't need to 
            override this because the base class version will suffice, but
            Python extension classes will have to override. The format
            can be supplied in case something other than cPickle is needed."""

    def load (input, format=constants.SAVE_CPICKLE):
            """Replace the current object in the hierarchy with the object
            loaded from the input stream. Pure Python classes generally 
            won't need to override this, but extensions will. The format
            can be supplied in case something other than cPickle is needed."""

    def config_from_obj (obj):
            """This is intended to allow a newer version of a component to
            configure itself based on an older version. By default, values
            of dictionary entries from the old object will be copied to the
            new one."""


-------

.. index:: IComponent

.. _IComponent:

::

    class IComponent (Interface):
        """A runnable Container. This interface is provided by the Component 
        class"""

    state =  Attribute('the current state of this object(UNKNOWN,IDLE,RUNNING,WAITING)')

    resource_desc = Attribute('a dict containing key-value pairs that are used to select a ResourceAllocator')


    def add_socket (name, iface, desc=''):
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

    def checkpoint (out):
            """Save sufficient information for a restart. By default, this
            just calls save_state()"""

    def restart (input):
            """Restore state using a checkpoint file. The checkpoint file is typically a delta 
	    from a full saved state file."""

    def step ():
            """For Components that contain Workflows (e.g., Assembly), this will run
            one Component in the Workflow and return. For simple components, it is the
            same as run()."""

    def require_gradients (varname, gradients):
            """Requests that the component be able to provide (after execution) a
            list of gradients w.r.t. a list of variables. The format
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
            the hessian w.r.t. a list of variables. The format of
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


-------

.. index:: IDriver

.. _IDriver:

::

    class IDriver (Interface):
        """Executes a Workflow until certain criteria are met."""

        workflow = Attribute('the object that orders execution of components that are driven by this driver')
	 	 
-------

.. index:: IFactory

.. _IFactory:

::

    class IFactory (Interface):
        """An object that creates and returns objects based on a type string"""

    def create (type):
            """Create an object of the specified type and return it, or a proxy
            to it if it resides in another process."""

-------

.. index:: IGeomObject


.. _IGeomObject:

::

    class IGeomObject (Interface):
        """A Component representing an object having physical dimensions and
        shape, with parameters that can be manipulated by other Components or 
        Drivers to modify its properties."""

        modelID = Attribute('Identifies the model. This can either be a part or an assembly of parts')

        # API to be determined

-------

.. index:: IResourceAllocator

.. _IResourceAllocator:

::

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
        
--------

.. index:: IVariable

.. _IVariable:

::

    class IVariable (Interface):
        """ An object representing data to be passed between Components within
        the framework. It will perform validation when assigned to another
        IVariable. It can notify other objects when its value is modified."""

    value = Attribute('the value')

    default = Attribute('the default value')

    current = Attribute('if False, the value is not current')

    def revert ():
        """ Return this Variable to its default value"""

    def validate (variable):
        """ Raise an exception if the assigned variable is not compatible"""

    def add_observer (obs_funct, *args, **metadata):
        """ Add a function to be called when this variable is modified"""

    def notify_observers ():
        """Call data_changed(self,args,metadata) on all of this object's 
        observers."""


