"""
Trait for a Slot meant to contain an object of a particular type
or having a particular interface (either a Traits interface or a
zope.interface).
"""

# The regular Instance class that comes with Traits will only check public
# methods on an object if that object is not a HasTraits object, which means
# we get essentially no error checking for things like Case iterators where
# their API doesn't include any public methods. If we use zope.interface we
# don't have this problem.

#public symbols
__all__ = ["Instance"]

# pylint: disable-msg=E0611,F0401
from inspect import isclass

# pylint: disable-msg=E0611,F0401

from traits.api import Interface
import traits

import zope.interface

from openmdao.main.variable import Variable, gui_excludes
from openmdao.main.mp_support import has_interface
from openmdao.main.interfaces import IContainer

class Base(Variable):

    """A trait for an object of a particular type or implementing a particular
    interface. Both Traits Interfaces and zope.interface.Interfaces are
    supported.
    """

    def __init__(self, klass=object, allow_none=True, factory=None,
                 args=None, kw=None, **metadata):

        default_value = None
        try:
            iszopeiface = issubclass(klass, zope.interface.Interface)
        except TypeError:
            iszopeiface = False
            if not isclass(klass):
                default_value = klass
                klass = klass.__class__

        metadata.setdefault('copy', 'deep')

        self._allow_none = allow_none
        self.klass = klass

        if has_interface(klass, IContainer) or \
           (isclass(klass) and IContainer.implementedBy(klass)):
            self._is_container = True
        else:
            self._is_container = False

        if iszopeiface:
            self._instance = None
            self.factory = factory
            self.args = args
            self.kw = kw
        else:
            self._instance = traits.api.Instance(klass=klass, allow_none=allow_none,
                                      factory=factory, args=args, kw=kw,
                                      **metadata)
            if default_value:
                self._instance.default_value = default_value
            else:
                default_value = self._instance.default_value

            if klass.__name__ == 'VariableTree':
                raise TypeError('Slotting of VariableTrees is not supported,'
                                ' please use VarTree instead')

        super(Base, self).__init__(default_value, **metadata)

    def validate(self, obj, name, value):
        ''' wrapper around Enthought validate method'''

        if value is None:
            if self._allow_none:
                return value
            self.validate_failed(obj, name, value)

        if self._instance is None:  # our iface is a zope.interface
            if not self.klass.providedBy(value):
                self._iface_error(obj, name, self.klass.__name__)
        else:
            try:
                value = self._instance.validate(obj, name, value)
            except Exception:
                if issubclass(self._instance.klass, Interface):
                    self._iface_error(obj, name, self._instance.klass.__name__)
                else:
                    obj.raise_exception("%s must be an instance of class '%s', got %s" %
                                        (name, self._instance.klass.__name__, type(value)),
                                        TypeError)

        return value

    def post_setattr(self, obj, name, value):
        '''Containers must know their place within the hierarchy, so set their
        parent here.  This keeps side effects out of validate()'''

        if self._is_container and value is not None:
            if value.parent is not obj:
                value.parent = obj

    def _iface_error(self, obj, name, iface_name):
        obj.raise_exception("%s must provide interface '%s'" %
                            (name, iface_name), TypeError)

    def get_attribute(self, name, value, trait, meta):
        """Return the attribute dictionary for this variable. This dict is
        used by the GUI to populate the edit UI. Slots also return an
        attribute dictionary for the slot pane.

        name: str
          Name of variable

        value: object
          The value of the variable

        trait: CTrait
          The variable's trait

        meta: dict
          Dictionary of metadata for this variable
        """

        raise NotImplementedError("Subclasses should implement this")

class Instance(Base):

    """A trait for an object of a particular type or implementing a particular
    interface. Both Traits Interfaces and zope.interface.Interfaces are
    supported.
    """

    def __init__(self, klass=object, allow_none=True, factory=None,
                 args=None, kw=None, **metadata):

        super(Instance, self).__init__(klass, allow_none, factory, args, kw, **metadata)

    def get_attribute(self, name, value, trait, meta):
        """Return the attribute dictionary for this variable. This dict is
        used by the GUI to populate the edit UI. Slots also return an
        attribute dictionary for the slot pane.

        name: str
          Name of variable

        value: object
          The value of the variable

        trait: CTrait
          The variable's trait

        meta: dict
          Dictionary of metadata for this variable
        """

        io_attr = {}
        io_attr['name'] = name
        io_attr['type'] = trait.trait_type.klass.__name__
        io_attr['ttype'] = 'instance'

        for field in meta:
            if field not in gui_excludes:
                io_attr[field] = meta[field]

        return io_attr, {}
