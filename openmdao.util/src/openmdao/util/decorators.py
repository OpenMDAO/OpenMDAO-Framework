"""
Some useful decorators
"""

import functools
from inspect import getmembers, ismethod
from types import MethodType


def add_delegate(*delegates):
    """A class decorator that takes a list of name,delegate tuples.  
    All of the public functions from the delegates will be added to the class.
    Conflicts will cause a NameError exception to be raised.
    """
    def forwarder(fname, delegatename):
        """Forwards calls on the scoping object to calls on the delegate object"""
        def _forward(obj, *args, **kwargs):
            return getattr(getattr(obj, delegatename),fname)(*args, **kwargs)
        return _forward

    def init_wrapper(fnc, delegatelist):
        """Wrapper for the wrapped class' __init__ function"""
        def new_init(obj, *args, **kwargs):
            fnc(obj, *args, **kwargs)
            for name,delegate in delegatelist:
                try:
                    delinst = delegate(obj) # try passing scoping object
                except Exception:
                    delinst = delegate()    # try without passing scoping object
                setattr(obj, name, delinst)
        return new_init
        
    def _add_delegate(cls):
        added_set = set([n for n,v in getmembers(cls) if not n.startswith('_')])
    
        listofdels = []
        for tup in delegates:
            if isinstance(tup, tuple):
                delegatename, delegate = tup
            else: # assume no name given
                delegate = tup
                delegatename = None
            if not isinstance(delegate, type):
                raise TypeError("Can't add delegate to class %s because delegate is not a class"%
                                cls.__name__)
            if not delegatename:
                delegatename = '_%s' % delegate.__name__.lower()
                
            listofdels.append((delegatename, delegate))
            
            for memname,mem in getmembers(delegate, ismethod):
                if not memname.startswith('_'):
                    if memname in added_set:
                        raise NameError("'%s' is already present so we can't add it to class %s from delegate '%s'"%
                                        (memname, cls.__name__, delegatename))
                    added_set.add(memname)
                    setattr(cls, memname, forwarder(memname, delegatename))
            cls.__init__ = init_wrapper(cls.__init__, listofdels)
        return cls
    return _add_delegate
