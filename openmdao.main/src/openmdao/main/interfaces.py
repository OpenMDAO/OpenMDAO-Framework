
"""
Interfaces for the OpenMDAO project.
"""

# pylint: disable-msg=E0213,E0211,W0232

#public symbols
__all__ = ['IFactory',
           'IGeomQueryObject', 'IGeomModifier', 'IResourceAllocator',
           'ICaseIterator']

__version__ = "0.1"


from enthought.traits.api import Interface, Int

# to check if an interface is provided, you can call
# validate_implements(value,klass) from enthought.traits.trait_types

class IDriver(Interface):
    """A marker interface for Drivers. To make a usable IDriver plug-in,
    you must still inherit from Driver.
    """
 
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

    modelID = Int(desc="Identifies an assembly or a part.")



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

    
class ICaseIterator(Interface):
    """An iterator that returns Case objects"""
    
    def __iter__():
        """Return an iterator object."""
        
    def next():
        """Return the next Case. If no Cases remain, raise a StopIteration
        exception.
        """

