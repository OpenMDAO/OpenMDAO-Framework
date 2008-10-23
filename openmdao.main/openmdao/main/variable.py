
#public symbols
__all__ = ["Variable"]

__version__ = "0.1"


from zope.interface import implements, Attribute

from openmdao.main.interfaces import IContainer, IVariable
from openmdao.main.exceptions import ConstraintError
from openmdao.main.hierarchy import HierarchyMember


class Variable(HierarchyMember):
    """ An object representing data to be passed between Components within
    the framework. It will perform validation when assigned to another
    Variable. It can notify other objects when its value is modified.
    """

    implements(IContainer, IVariable)
    
    def __init__(self, name, ref_name=None, parent=None, default=None):
        HierarchyMember.__init__(self, name, parent)        

        if ref_name is None:
            self.ref_name = name
        else:
            self.ref_name = ref_name
        self.current = False
        self.permission = None
        self.observers = []
        
        self.__value = default
        self.default = default


    def _set_value(self, var):
        """Assign this Variable's value to the value of another Variable or directly
        to another value."""
        self.__value = self._pre_assign(var)
        if self._parent is not None:
            setattr(self._parent, self.ref_name, self.__value)
        self._notify_observers()

    def _get_value(self):
        return self.__value
    
    value = property(_get_value,_set_value)
        
        
    def revert(self):
        """ Return this Variable to its default value"""
        self.__value = self.default


    def _pre_assign(self, var):
        """This should be overridden to perform transformations or
        validations at assignment time.  If validations fail, they should raise a
        ValueError. Note that var can be a Variable object or just a simple value.
        
        Returns the transformed, validated value.
        """
        if IVariable.providedBy(var):
            return var.__value
        else:
            return var
    
    
    def validate_connection(self, variable):
        """Raise a TypeError if the connecting variable is not compatible with this one. 
        This function should be overridden in any derived classes that require validation.
        """
        pass


    def add_observer(self, obs_funct):
        """ Add a function to be called when this variable is modified. The
        function should accept this Variable as an argument.
        """
        self.observers.append(obs_funct)


    def _notify_observers(self):
        """Call each observer on the observers list."""
        for ob in self.observers:
            ob(self)
        
        

class Float(Variable):
    """A float Variable"""
    
    def __init__(self, name, ref_name=None, parent=None, default=None,
                 units=None, min_limit=None, max_limit=None):
        Variable.__init__(self, name, ref_name, parent, default)
        self.units = units
        self.min_limit = min_limit
        self.max_limit = max_limit


    def incompatible_units(self, variable, units):
        raise TypeError(variable.get_pathname()+' units ('+
                        units+') are '+
                        'incompatible with units ('+self.units+') of '+ 
                        self.get_pathname())
        
            
    def _pre_assign(self, var):
        """Return the value of the specified Variable after
        converting units if necessary and checking against min and max limits.
        """
        if IVariable.providedBy(var):
            if self.units is not None:
                # ??? eventually do unit conversion here
                newval = var.value
            else:
                newval = var.value
        else:
            newval = var
            
        # check against min and max limits
        if self.min_limit is not None and newval < self.min_limit:
            raise ConstraintError(self.get_pathname()+' min_limit violated: '+
                                  str(newval)+' < '+str(self.min_limit))
        if self.max_limit is not None and newval > self.max_limit:
            raise ConstraintError(self.get_pathname()+' max_limit violated: '+
                                  str(newval)+' > '+str(self.max_limit))
            
        return newval
        
        
    def validate_connection(self, variable):
        """Raise a TypeError if the connecting Variable is incompatible."""
        typ = type(variable.value)
        if typ == float or typ == int:
            # ??? until we add unit conversion stuff, force units to be the same
            if self.units is not None:
                if hasattr(variable,'units'):
                    vunits = variable.units
                else:
                    vunits = None
                if vunits != self.units:
                    self.incompatible_units(variable, vunits)
        else:
            raise TypeError(variable.get_pathname()+' is not compatible with '+
                            self.get_pathname())


