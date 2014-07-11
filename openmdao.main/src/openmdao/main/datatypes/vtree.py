"""
Variable meant to contain a VariableTree of a particular type.
"""

#public symbols
__all__ = ["VarTree"]

from traits.api import Instance

from openmdao.main.variable import Variable, gui_excludes


class VarTree(Variable):
    """ A Variable for a :class:`VariableTree` of a particular type. """

    def __init__(self, default_value, allow_none=True, **metadata):
        from openmdao.main.vartree import VariableTree # Break import loop on VariableTree
        if isinstance(default_value, VariableTree):
            klass = default_value.__class__
            if 'iotype' in metadata:
                default_value._iotype = metadata['iotype']
            else:
                metadata['iotype'] = default_value.iotype
        else:
            raise TypeError('default_value must be an instance of VariableTree'
                            ' or subclass')

        metadata.setdefault('copy', 'deep')
        self._allow_none = allow_none
        self.klass = klass
        self._instance = Instance(klass=klass, allow_none=False, factory=None,
                                  args=None, kw=None, **metadata)
        self._instance.default_value = default_value
        super(VarTree, self).__init__(default_value, **metadata)

    def validate(self, obj, name, value):
        """ Validates that a specified value is valid for this trait. """
        if value is None:
            if self._allow_none:
                return value
            self.validate_failed(obj, name, value)

        try:
            value = self._instance.validate(obj, name, value)
        except Exception:
            obj.raise_exception('%r must be an instance of %s.%s, not %r' %
                                (name, self._instance.klass.__module__,
                                 self._instance.klass.__name__, type(value)),
                                TypeError)
        return value

    def post_setattr(self, obj, name, value):
        """ VariableTrees must know their place within the hierarchy, so set
        their parent here.  This keeps side effects out of validate(). """
        if value.parent is not obj:
            value.parent = obj
            value.name = name
        value._iotype = self.iotype

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
        io_attr['ttype'] = 'vartree'

        for field in meta:
            if field not in gui_excludes:
                io_attr[field] = meta[field]

        return io_attr, None

