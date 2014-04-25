
from openmdao.main.variable import Variable
from openmdao.main.mp_support import has_interface
from openmdao.main.interfaces import IUncertainVariable
from openmdao.main.attrwrapper import AttrWrapper, UnitsAttrWrapper

class UncertainDistVar(Variable):
    """A variable wrapper for an UncertainDistribution variable.
       """
    
    def __init__(self, default_value=None, iotype=None, desc=None, **metadata):

        # Put iotype in the metadata dictionary
        if iotype is not None:
            metadata['iotype'] = iotype
            
        # Put desc in the metadata dictionary
        if desc is not None:
            metadata['desc'] = desc
            
        super(UncertainDistVar, self).__init__(default_value=default_value, **metadata)

    def validate(self, obj, name, value):
        """ Use the Enthought trait's validate.
        """
        if not has_interface(value, IUncertainVariable):
            raise ValueError("'%s' does not implement the IUncertainVariable interface" %
                             name)
        return value

    def get_val_wrapper(self, value, index=None):
        """Return a UnitsAttrWrapper object.  Its value attribute
        will be filled in by the caller.
        """
        if index is not None:
            raise ValueError("UncertainDistVar does not support indexing")
        # pylint: disable-msg=E1101
        if self.units is None:
            return value
        return UnitsAttrWrapper(value, units=self.units)
            

