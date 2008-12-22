
#public symbols
__all__ = ['Variable', 'UNDEFINED']
__version__ = "0.1"

from zope.interface import implements

from openmdao.main.interfaces import IContainer, IVariable
from openmdao.main.exceptions import ConstraintError
from openmdao.main.hierarchy import HierarchyMember


INPUT = 1
OUTPUT = 2

class _Undefined ( object ):
    """A class used to define a special undefined object."""
    def __repr__ ( self ):
        return '<undefined>'
    
# use this to represent undefined value rather than None    
UNDEFINED = _Undefined()

class Variable(HierarchyMember):
    """ An object representing data to be passed between Components within
    the framework. It will perform validation when assigned to another
    Variable. It can notify other objects when its value is modified.
    """

    implements(IVariable, IContainer)
    
    def __init__(self, name, parent, iostatus, 
                 val_type=None, ref_name=None, refparent=None, 
                 default=UNDEFINED, desc=None):
        """Note that Variable calls _pre_assign from here, so if _pre_assign
        requires any attributes from a derived class, those attributes must be
        set before the Variable __init__ function is called.
        """
        HierarchyMember.__init__(self, name, parent, desc)        

        # by default, name of the variable is the same as the obj it refers to
        if ref_name is None:
            self.ref_name = name
        else:
            self.ref_name = ref_name
        # the variable can reference an obj inside of some object other 
        # than the variable's parent
        if refparent is None:
            self._refparent = parent
        else:
            self._refparent = refparent
            
        self.observers = None
        self.permission = None
        self._constraints = []
        self.iostatus = iostatus
        self._set_count = 0
        if val_type is not None and not isinstance(val_type, tuple):
            self.val_type = (val_type,)
        else:
            self.val_type = val_type
        
        if IContainer.providedBy(parent):
            val = getattr(self._refparent, self.ref_name)
            parent.make_public(self)
        else:
            raise TypeError("parent of Variable '"+name+
                            "' is not an IContainer")

        if default is UNDEFINED: 
            self.default = self._pre_assign(val)
        elif default is None:
            self.default = None
        else:
            self.default = self._pre_assign(default)

    def contains(self, path):
        """Return True if the object identified by path is found
        within this object.
        """
        if '.' in path:
            return False
        else:
            return hasattr(self, path)
    
    def _set_value(self, var):
        """Assign this Variable's value to the value of another Variable or 
        directly to another value.  Checks validity of the new value before assignment.
        Called by setting the 'value' property.
        """
        if self.iostatus == OUTPUT:
            raise RuntimeError(self.get_pathname()+
                               'is an OUTPUT Variable and cannot be set.')
        setattr(self._refparent, self.ref_name, self._pre_assign(var))
        self._set_count += 1
        if self.observers is not None:
            self._notify_observers()


    def _get_value(self):
        """"Called when getting the 'value' property."""
        return getattr(self._refparent, self.ref_name)
    
    value = property(_get_value, _set_value)
        
        
    def revert_to_default(self):
        """ Return this Variable to its default value"""
        self.value = self.default

    def add_constraint(self, con):
        """Add a Constraint object to the list of Constraint objects for
        this Variable.
        """
        self._constraints.append(con)

    def _pre_assign(self, val):
        """This should be overridden to perform necessary validations at
        assignment time but this base version should still be called from within the
        overridden version. If validations fail, they should raise a ValueError.
        Note that val should just be a simple value, not a Variable. This
        routine performs a simple type check on the value if the self.val_type
        attribute has been set.
        
        Returns the validated value.
        """
        if self.val_type is not None:
            match = [x for x in self.val_type if isinstance(val, x)]
            if len(match) == 0:
                self.raise_exception('incompatible with type '+
                                     str(type(val), ValueError))
        
        # test against any constraints placed on this variable
        try:
            for con in self._constraints:
                con.test(val)
        except ConstraintError, err:
            self.raise_exception(str(err), ConstraintError)
        
        return val

    
    def _pre_assign_entry(self, val):
        """Perform validation before assignment of a value to an entry.
        This is only applicable for array Variables.
        """
        self.raise_exception('_pre_assign_entry', NotImplemented)            

        
    def validate_var(self, variable):
        """Raise a TypeError if the given Variable is incompatible. This is
        called on the INPUT side of a connection. The value attribute of the
        Variable is not checked at this time."""
        if not isinstance(variable, type(self)):
            # could try to obtain adapter here...
            self.raise_exception("assignment to incompatible variable '"+
                                 variable.get_pathname()+"' of type '"+
                                 str(type(variable))+"'", TypeError)
        if self.iostatus != INPUT or self.iostatus == variable.iostatus:
            self.raise_exception('incompatible iostatus with '+
                                 variable.get_pathname(),ValueError)

    def _convert(self, variable):
        """Some Variables, e.g., Float, will need to override this in order to
        convert units from those in connected variables. By default, no 
        conversion is performed.
        """
        return variable.value
    
    def setvar(self, name, var):
        """Assign this Variable to another Variable, which generally just
        means to assign our value to the value of var. Some Variables will
        override _convert to handle things like unit conversion."""
        if name is None: # they're setting this Variable
            self.validate_var(var)
            self.value = self._convert(var)
        else:
            self.raise_exception("cannot assign a Variable to attribute '"+
                                 name+"'", RuntimeError)
        
    def set(self, name, value, index=None):
        """Set the value of the attribute specified by the given name. value
        is assumed to be a value and not a Variable object. Assignment to
        'value' will force a check against any constraints registered with
        this Variable."""
        if name is None or name == value: # they're setting this Variable
            if index is None:
                self.value = value
            else:
                self.set_entry(value, index)                    
        else:  # they're setting an attribute (units, etc.)
            setattr(self, name, value)
            
    def getvar(self, name=None):
        """Retrieved a named Variable from this object, or return 
        self if the name is None.
        """
        if name is None:
            return self
        else:
            self.raise_exception("'"+name+"' is not a Variable object", 
                                 NameError)        
        
    def get(self, name=None, index=None):
        """Return the named attribute"""
        if name is None:
            if index is None:
                return self.value
            else:
                return self.get_entry(index)
        else:
            if index is None:
                return getattr(self, name)
            else:
                val = getattr(self, name)
                for i in index:
                    val = val[i]
                return val

    def get_entry(self, index):
        """Retrieve the entry indicated by index."""
        return self.value[index]
    
    def set_entry(self, val, index):
        """Set the value of the entry indicated by index.
        Validation will be performed prior to assignment.
        """
        tmp = self._pre_assign_entry(val, index)
        try:
            self.value[index] = tmp
        except Exception:
            self.raise_exception("assigning index "+str(index)+
                                 " to a value of type "+
                                 str(type(val))+" failed", ValueError)        
    
    def make_public(self, child):
        """Make a given object a member of this object's pubic interface."""
        self.raise_exception('make_public', NotImplemented)            
            
    def add_observer(self, obs_funct):
        """ Add a function to be called when this variable is modified. The
        function should accept this Variable as an argument.
        """
        if self.observers is None:
            self.observers = [obs_funct]
        else:
            self.observers.append(obs_funct)

    def _notify_observers(self):
        """Call each observer on the observers list."""
        for observer in self.observers:
            observer(self)
        
    
