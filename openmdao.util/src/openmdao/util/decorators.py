"""
Some useful decorators
"""
import sys
import types
import time

from decorator import FunctionMaker
from inspect import getmembers, ismethod, isfunction, isclass, getargspec, formatargspec, getmro

# this decorator is based on a code snippet by vegaseat at daniweb.
# See http://www.daniweb.com/code/snippet216689.html
def print_timing(func):
    def wrapper(*args, **kwargs):
        t1 = time.time()
        res = func(*args, **kwargs)
        t2 = time.time()
        print '%s took %0.3f ms' % (func.func_name, (t2-t1)*1000.0)
        return res
    return wrapper

# not a decorator
def forwarder(cls, fnc, delegatename):
    """Returns a method that forwards calls on the scoping object to calls 
    on the delegate object.  The signature of the delegate method is preserved
    in the forwarding method.
    """
    fname = fnc.__name__
    spec = getargspec(fnc)
    sig = formatargspec(*spec)
    body = 'return self.%s.%s(%s)' % (delegatename, fname, ','.join(spec[0][1:]))
    f = FunctionMaker.create('%s%s' % (fname,sig), body, {}, defaults=spec[3],
                             doc=fnc.__doc__)
    return types.MethodType(f, None, cls)


def funct_replacer(fnc, body):
    """Returns a function with a new body that replaces the given function. The
    signature of the original function is preserved in the new function.
    """
    fname = fnc.__name__
    spec = getargspec(fnc)
    sig = formatargspec(*spec)
    return FunctionMaker.create('%s%s' % (fname,sig), body, {}, defaults=spec[3],
                                doc=fnc.__doc__)
        
    
def stub_if_missing_deps(*deps):
    """A class decorator that will try to import the specified modules and in
    the event of failure will stub out the class, raising a RuntimeError that
    explains the missing dependencies whenever an attempt is made to
    instatiate the class.
    
    deps: str args
        args in deps may have the form a.b.c or a.b.c:attr, where attr would be 
        searched for within the module a.b.c after a.b.c is successfully imported.
    """
    
    def _find_failures():
        failed = []
        for dep in deps:
            parts = dep.split(':')
            modname = parts[0]
            attrname = parts[1] if len(parts)>1 else None
            
            try:
                __import__(modname)
            except ImportError:
                failed.append(modname)
                continue
            
            if attrname and attrname not in sys.modules[modname]:
                failed.append('.'.join([modname, attrname]))
        return failed
    
    def _stub_if_missing(obj):
        failed = _find_failures()        
        if failed:
            if isclass(obj):
                def _error(obj, *args, **kwargs):
                    msg = "The %s class depends on the following modules or attributes which were not found on your system: %s"
                    raise RuntimeError(msg % (obj.__name__, failed))
                obj.__new__ = staticmethod(_error)
            elif isfunction(obj):
                body = "raise RuntimeError(\"The %s function depends on the following modules or attributes which were not found on your system: %s\")" 
                return funct_replacer(obj, body % (obj.__name__, failed))
        return obj
            
    return _stub_if_missing

    
def add_delegate(*delegates):
    """A class decorator that takes delegate classes or (name,delegate) tuples as
    args. For each tuple, an instance with the given name will be created in the
    wrapped ``__init__`` method of the class.  If only the delegate class is provided,
    then the instance created in the wrapped ``__init__`` method will be named using
    an underscore (_) followed by the lower case name of the class. All of the public 
    methods from the delegate classes will be added to the class
    unless there is an attribute or method in the class with the same name. In that
    case the delegate method will be ignored.
    """

    def init_wrapper(cls, delegate_class_list):
        """Returns a function that calls the wrapped class' __init__ function
        and then creates an instance of each delegate class in delegate_class_list.
        """
        fnc = cls.__init__
        spec = getargspec(fnc)
        sig = formatargspec(*spec)
        template = [
            'old_init__(%s)' % ','.join(spec[0]),
            ]
        for name, delegate in delegate_class_list:
            template.append('self.%s = %s(self)' % (name, delegate.__name__))
            template.append("if not hasattr(self, '_delegates_'): self._delegates_ = {}")
            template.append("self._delegates_['%s'] = self.%s" % (name,name))
        f = FunctionMaker.create('__init__%s' % sig, '\n'.join(template), 
                                 dict([(c.__name__,c) for n,c in delegate_class_list]+
                                      [('old_init__',fnc)]),
                                 doc=fnc.__doc__, defaults=spec[3])
        return types.MethodType(f, None, cls)
        
    def _add_delegate(cls):
        """Returns the given class, updated with a new __init__ function that wraps the
        old one and adds instantiation of the delegate, and adds new member functions
        that match the public members in the delegate class.  Any public members in the
        delegate that have a name matching anything in the scoping object are ignored.
        """
        member_set = set()

        for klass in getmro(cls):
            member_set.update(klass.__dict__.keys())
    
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
            
            alldict = {}
            for klass in getmro(delegate):
                if hasattr(klass, '_do_not_promote'):
                    skip = klass._do_not_promote
                else:
                    skip = []
                for k,v in getmembers(klass, ismethod):
                    if not k.startswith('_') and k not in alldict and k not in member_set and k not in skip:
                        member_set.add(k)
                        setattr(cls, k, forwarder(cls, v, delegatename))
            cls.__init__ = init_wrapper(cls, listofdels)
        return cls
    return _add_delegate


#def on_condition(cond, dec, *args, **kwargs):
    #"""This is actually a decorator of decorators.  It will cause the wrapped decorator
    #to be applied only if the supplied value is True.
    #"""
    #def _wrap_on_condition(fnc):
        #if cond:
            #return dec(*args, **kwargs)(fnc)
        #else:
            #return fnc
    #return _wrap_on_condition


