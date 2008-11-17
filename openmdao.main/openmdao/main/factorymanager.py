
"""
A manager over the creation of framework objects, either locally or remotely.
"""


#public symbols
__all__ = ["create", "get_factory_names", "register_factory"]

__version__ = "0.1"


_factories = []
search_path = []

def create(typname, name=None, version=None, server=None, res_desc=None):
    obj = None
    for fct in _factories:
        obj = fct.create(typname, version, server, res_desc)
        if obj is not None:
            if isinstance(name, basestring):
                obj.name = name
            return obj
    
    raise NameError("unable to create object of type '"+typname+"'")


def find_components(search_path):
    _factories += egglib.get_plugin_factories(search_path, 'openmdao.components')
            


def get_factory_names():
    names = []
    for fct in _factories:
        names.append(type(fct).__name__)
        
    return names


def register_factory(fct):
    if fct not in _factories:
       _factories.append(fct)      
          
