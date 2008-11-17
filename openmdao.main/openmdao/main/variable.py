
#public symbols
__all__ = []
__version__ = "0.1"


import weakref
from zope.interface import implements, Attribute

from openmdao.main.interfaces import IContainer, IVariable
from openmdao.main.exceptions import ConstraintError
from openmdao.main.hierarchy import HierarchyMember


INPUT = 1
OUTPUT = 2


class Variable(HierarchyMember):
    """ An object representing data to be passed between Components within
    the framework. It will perform validation when assigned to another
    Variable. It can notify other objects when its value is modified.
    """

    implements(IContainer, IVariable)
    
    def __init__(self, name, parent, iostatus, 
                 val_type=None, ref_name=None, default=None, desc=None):
        """Note that Variable calls _pre_assign from here, so if _pre_assign requires any
        attributes from a derived class, those attributes must be set before the Variable
        __init__ function is called.
        """
        HierarchyMember.__init__(self, name, parent, desc)        

        if ref_name is None:
            self.ref_name = name
        else:
            self.ref_name = ref_name
        self.changed = True
        self.permission = None
        self.iostatus = iostatus
        self.observers = None
        self.val_type = val_type
        
        if parent is None:
            val = default
        else:
            val = getattr(parent, self.ref_name)
            parent.add_child(self)
        self.__value = self._pre_assign(val)
        if default is None: 
            self.default = self.__value
        else:
            self.default = self._pre_assign(default)


    def _set_value(self, var):
        """Assign this Variable's value to the value of another Variable or 
        directly to another value."""
        if self.iostatus == OUTPUT:
            raise RuntimeError(self.get_pathname()+
                               'is an OUTPUT Variable and cannot be set.')
        self.__value = self._pre_assign(var)
        if self._parent is not None:
            setattr(self._parent, self.ref_name, self.__value)
        if self.observers is not None:
            self._notify_observers()


    def _get_value(self):
        if self._parent is not None:
            return getattr(self._parent, self.ref_name)
        else:
            return self.__value
    
    value = property(_get_value,_set_value)
        
        
    def revert(self):
        """ Return this Variable to its default value"""
        self.__value = self.default


    def _pre_assign(self, var, attribname=None):
        """This should be overridden to perform necessary transformations or
        validations at assignment time.  If validations fail, they should raise a
        ValueError. Note that var can be a Variable object or just a simple value.
        By default, this routine performs a simple type check on the value if the
        self.val_type attribute has been set.
        
        Returns the validated value.
        """
        if IVariable.providedBy(var):
            val = var.value
        else:
            val = var
            
        if self.val_type is not None and not isinstance(var.value,self.val_type):
            raise ValueError(self.get_pathname()+': incompatible with type '+
                             str(type(var.value)))
        return var
    
        
    def connect(self, variable, attribname=None):
        """Raise a ValueError if the iostatus of the two variables is not compatible.
        """
        if self.iostatus == variable.iostatus:
            raise ValueError(self.get_pathname()+' and '+
                               variable.get_pathname()+' have incompatible iostatus')
        self._pre_connect(variable, attribname)
    
        
    def _pre_connect(self, variable, attribname=None):
        """Raise a ValueError if the connecting variable is not compatible with this one.
        This function should be overridden in any derived classes that require validation 
        beyond the default at connect time.
        """
        self._pre_assign(variable, attribname)

    def add_observer(self, obs_funct):
        """ Add a function to be called when this variable is modified. The
        function should accept this Variable as an argument.
        """
        if self.observers is None:
            self.observers = [obs_funct]
        else:
            self.observers.append(obs_funct)


    def set(self, name, value):
        """Set the value of the attribute specified by the given name."""
        if name is None: # they're setting this Variable
            setattr(self, 'value', value)
        else:  # they're setting an attribute (value, units, etc.)
            setattr(self, name, value)
            
    def get(self, name):
        """Return the named attribute"""
        return getattr(self, name) 
        
    def _notify_observers(self):
        """Call each observer on the observers list."""
        for ob in self.observers:
            ob(self)
        
#    def sync_with_parent(self):
#        if self._parent is not None:
#            self.__value = getattr(self._parent, self.ref_name)

    def add_child(self, child):
        raise RuntimeError(self._error_msg('you cannot add children to a Variable'))            
            
