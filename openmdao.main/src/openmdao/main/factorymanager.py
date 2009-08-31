
"""
Manages the creation of framework objects, either locally or remotely.
"""


#public symbols
__all__ = [ "create", "register_factory", "get_available_types", "plugin_path" ]


import os

from openmdao.main.importfactory import ImportFactory
from openmdao.main.pkg_res_factory import PkgResourcesFactory

_factories = []
_pkg_res_factory = None

# this list should contain all openmdao entry point groups for Containers
_container_groups = [ 'openmdao.container',
                      'openmdao.component',
                      'openmdao.driver',
                    ]

def _parse_plugin_path(pathstr):
    if pathstr is None:
        return []
    if ';' in pathstr or ':\\' in pathstr:
        return pathstr.split(';')
    else:
        return pathstr.split(':')
    
pluginpath = _parse_plugin_path(os.environ.get('OPENMDAO_PLUGIN_PATH'))
_old_pluginpath = pluginpath


def create(typname, version=None, server=None, res_desc=None, **ctor_args):
    """Create and return an object specified by the given type, name,
    version, etc.
    """
    global pluginpath, _old_pluginpath
    
    if _old_pluginpath != pluginpath:  # update the pkg_res_factory with new search path
        _pkg_res_factory.update_search_path(pluginpath)
        _old_pluginpath = pluginpath
        
    obj = None
    for fct in _factories:
        obj = fct.create(typname, version, server, res_desc, **ctor_args)
        if obj is not None:
            return obj
    
    raise NameError("unable to create object of type '"+typname+"'")


def register_factory(fct):
    """Add a Factory to the factory list."""
    if fct not in _factories:
        _factories.append(fct)      
          
def get_available_types(groups=None):
    """Return a set of tuples of the form (typename, dist_version), one
    for each available plugin type in the given entry point groups.
    If groups is None, return the set for all openmdao entry point groups.
    """
    if groups is None:
        groups = _container_groups
    types = []
    for fct in _factories:
        types.extend(fct.get_available_types(groups))
    return types


# register factory that loads plugins via pkg_resources
_pkg_res_factory = PkgResourcesFactory(groups=_container_groups,
                                       search_path=pluginpath)   
register_factory(_pkg_res_factory)

# register factory for simple imports
register_factory(ImportFactory())
