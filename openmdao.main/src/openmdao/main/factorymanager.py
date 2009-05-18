
"""
Manages the creation of framework objects, either locally or remotely.
"""


#public symbols
__all__ = ["create", "get_factory_names", "register_factory"]

__version__ = "0.1"


from openmdao.main import ImportFactory
from openmdao.main import PkgResourcesFactory

_factories = []
search_path = []

def create(typname, name=None, version=None, server=None, res_desc=None):
    """Create and return an object specified by the given type, name,
    version, etc.
    """
    obj = None
    for fct in _factories:
        obj = fct.create(typname, version, server, res_desc)
        if obj is not None:
            if isinstance(name, basestring):
                obj.name = name
            return obj
    
    raise NameError("unable to create object of type '"+typname+"'")


def get_factory_names():
    """Return a list of names of Factory objects managed by this manager."""
    names = []
    for fct in _factories:
        names.append(type(fct).__name__)
        
    return names


def register_factory(fct):
    """Add a Factory to the factory list."""
    if fct not in _factories:
        _factories.append(fct)      
          

# by default, register factories that create things via pkg_resources 
# and simple imports
register_factory(PkgResourcesFactory())
register_factory(ImportFactory())
