
#public symbols
__all__ = ['find_var_class','add_var_type_map']

__version__ = "0.1"

import inspect

from openmdao.main.interfaces import IVariable


# a mapping of Variable classes to corresponding types, for example, Float maps to float
_var_type_map = {}


def find_var_class(typ, name, parent, iostatus, ref_name=None):
    """Look up a Variable class that can be used to wrap the given type.
    
    Returns a new Variable instance or None if none can be found.
    """
    klasses = inspect.getmro(typ)
    for klass in klasses:
        if klass in _var_type_map:
            vclass = _var_type_map[klass]
            if ref_name is None:
                ref_name = name
            return vclass(name, parent, iostatus=iostatus, ref_name=ref_name)


def add_var_type_map(varclass, typ):
    """Add a Variable class to the Variable-to-type map"""
    if typ not in _var_type_map:
        if IVariable.implementedBy(varclass):
            _var_type_map[typ] = varclass
        else:
            raise TypeError('attempted to add non-IVariable class '+str(varclass)+
                            ' to var_type_map')
    else:
        raise NameError('type '+str(typ)+' is already mapped in the Variable to type map')

    
    