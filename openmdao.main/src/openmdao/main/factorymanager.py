
"""
Manages the creation of framework objects, either locally or remotely.
"""


#public symbols
__all__ = [ "create", "register_class_factory", "get_available_types" ]


import os

from pkg_resources import parse_version

from openmdao.main.importfactory import ImportFactory
from openmdao.main.pkg_res_factory import PkgResourcesFactory, plugin_groups

_factories = []
_pkg_res_factory = None


def create(typname, version=None, server=None, res_desc=None, **ctor_args):
    """Create and return an object specified by the given type,
    version, etc.
    """
    obj = None
    for fct in _factories:
        obj = fct.create(typname, version, server, res_desc, **ctor_args)
        if obj is not None:
            return obj
    
    raise NameError("unable to create object of type '"+typname+"'")


def register_class_factory(factory):
    """Add a Factory to the factory list."""
    if factory not in _factories:
        _factories.append(factory)
        
def remove_class_factory(factory):
    """Remove a Factory from the factory list."""
    for fct in _factories:
        if fct is factory:
            if hasattr(factory, 'cleanup'):
                factory.cleanup()
            _factories.remove(factory)
            return

def _cmp(tup1, tup2):
    s1 = tup1[0].lower()
    s2 = tup2[0].lower()
    if s1 < s2: return -1
    elif s1 > s2: return 1
    else: # s1 == s2
        return cmp(parse_version(tup1[1].get('version','')), 
                   parse_version(tup2[1].get('version','')))

def get_available_types(groups=None):
    """Return a set of tuples of the form (typename, dist_version), one
    for each available plugin type in the given entry point groups.
    If groups is None, return the set for all openmdao entry point groups.
    """
    if groups is None:
        groups = plugin_groups.keys()
    else:
        badgroups = []
        for group in groups:
            if group not in plugin_groups:
                badgroups.append(group)
        if badgroups:
            raise RuntimeError("Didn't recognize the following entry point groups: %s. Allowed groups are: %s" %
                               (badgroups, plugin_groups.keys()))
    types = []
    for fct in _factories:
        types.extend(fct.get_available_types(groups))
    return sorted(types, _cmp)


# register factory that loads plugins via pkg_resources
_pkg_res_factory = PkgResourcesFactory()
register_class_factory(_pkg_res_factory)

# register factory for simple imports
register_class_factory(ImportFactory())

