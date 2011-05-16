"""
Trait for a Socket meant to contain an object of a particular type
or having a particular interface (either a Traits interface or a
zope.interface).  
"""

# The regular Instance class that comes with Traits will only check public
# methods on an object if that object is not a HasTraits object, which means
# we get essentially no error checking for things like Case iterators where
# their API doesn't include any public methods. If we use zope.interface we
# don't have this problem.

#public symbols
__all__ = ["Socket"]

# pylint: disable-msg=E0611,F0401
from enthought.traits.api import TraitType, Instance, TraitError, Interface
import zope.interface

class Socket(TraitType):
    """A trait for an object of a particular type or implementing a particular
    interface. Both Traits Interfaces and zope.interface.Interfaces are
    supported.
    """
    
    def __init__(self, klass = None, allow_none = True, **metadata):
        if issubclass(klass, zope.interface.Interface):
            self.klass = klass
            self.allow_none = allow_none
            self._instance = None
        else:
            self._instance = Instance(klass=klass, allow_none=allow_none, 
                                      **metadata)
        super(Socket, self).__init__(default_value=None, **metadata)

    def validate ( self, obj, name, value ):
        """ Validates that the value is a valid object instance."""
        if value is None:
            if self._allow_none:
                return value
            self.validate_failed( obj, name, value )

        if self._instance is None:  # our iface is a zope.interface
            if self.klass.providedBy(value):
                return value
            else:
                self._iface_error(obj, name, self.klass.__name__)
        else:
            try:
                return self._instance.validate(obj, name, value)
            except TraitError:
                if issubclass(self._instance.klass, Interface):
                    self._iface_error(obj, name, self._instance.klass.__name__)
                else:
                    obj.raise_exception("%s must be an instance of class '%s'" %
                                        (name, self._instance.klass.__name__), TraitError)

    def _iface_error(self, obj, name, iface_name):
        obj.raise_exception("%s must provide interface '%s'" % 
                            (name, iface_name), TraitError)
        