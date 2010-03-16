
"""
Manages the creation of framework objects, either locally or remotely.
"""


#public symbols
__all__ = [ "create", "register_factory", "get_available_types" ]


import os

from pkg_resources import parse_version

from openmdao.main.importfactory import ImportFactory
from openmdao.main.pkg_res_factory import PkgResourcesFactory

_factories = []
_pkg_res_factory = None



# this list should contain all openmdao entry point groups for Containers
_container_groups = [ 'openmdao.container',
                      'openmdao.component',
                      'openmdao.driver',
                    ]


def create(typname, version=None, server=None, res_desc=None, **ctor_args):
    """Create and return an object specified by the given type, name,
    version, etc.
    """
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

def _cmp(tup1, tup2):
    s1 = tup1[0].lower()
    s2 = tup2[0].lower()
    if s1 < s2: return -1
    elif s1 > s2: return 1
    else: # s1 == s2
        return cmp(parse_version(tup1[1]), parse_version(tup2[1]))

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
    return sorted(types, _cmp)


# register factory that loads plugins via pkg_resources
_pkg_res_factory = PkgResourcesFactory(groups=_container_groups)   
register_factory(_pkg_res_factory)

# register factory for simple imports
register_factory(ImportFactory())
