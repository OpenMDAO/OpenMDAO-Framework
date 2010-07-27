"""
Some useful decorators
"""

from decorator import decorator
from inspect import getmembers, ismethod, getargspec

def add_delegate(*delegates):
    """A class decorator that takes delegate classes or (name,delegate) tuples as
    args. For each tuple, an instance with the given name will be created in the
    wrapped __init__ function of the class.  If only the delegate class is provided,
    then the instance created in the wrapped __init__ function will be named using
    an underscore (_) followed by the lower case name of the class. All of the public 
    functions from the delegate classes will be added to the class
    unless there is an attribute or function in the class with the same name. In that
    case the delegate function will be ignored.
    """

    def forwarder(fname, delegatename):
        """Returns a function that forwards calls on the scoping object to calls 
        on the delegate object.
        """
        def _forward(obj, *args, **kwargs):
            return getattr(getattr(obj, delegatename),fname)(*args, **kwargs)
        return _forward

    def init_wrapper(fnc, delegate_class_list):
        """Returns a function that calls the wrapped class' __init__ function
        and then creates an instance of each delegate class in delegate_class_list.
        """
        def new_init(obj, *args, **kwargs):
            fnc(obj, *args, **kwargs)
            for name,delegate in delegate_class_list:
                setattr(obj, name, delegate(obj))
        return new_init
        
    def _add_delegate(cls):
        """Returns the given class, updated with a new __init__ function that wraps the
        old one and adds instantiation of the delegate, and with new member functions added
        that match the public functions in the delegate class.
        """
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
                        continue   # skip adding member if it's already part of the class
                    added_set.add(memname)
                    f = forwarder(memname, delegatename)
                    f.__doc__ = getattr(delegate, memname).__doc__
                    setattr(cls, memname, f)
            cls.__init__ = init_wrapper(cls.__init__, listofdels)
        return cls
    return _add_delegate
