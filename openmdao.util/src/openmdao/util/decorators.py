
import functools
from inspect import getmembers, ismethod
from types import MethodType


def add_delegate(delegate_dict):
    """A class decorator that takes a dict with delegate names mapped
    to their corresponding classes.  All of the public functions from
    the delegates will be added to the class.  Conflicts will cause
    an exception to be raised.
    """
    def _add_delegate(cls):
        added_set = set([n for n,v in getmembers(cls)])
    
        for delegatename,delegate in delegate_dict.items():
            for memname,mem in getmembers(delegate, ismethod):
                if not memname.startswith('_'):
                    if memname in added_set:
                        raise NameError("'%s' is already present so we can't add it to class %s from delegate '%s'"%
                                        (memname, cls.__name__, delegatename))
                    added_set.add(memname)
                    setattr(cls, memname, MethodType(mem, None, cls))
    return _add_delegate
