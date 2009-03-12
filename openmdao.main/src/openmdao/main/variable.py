#public symbols
__all__ = ['Variable', 'UNDEFINED']
__version__ = "0.1"

from zope.interface import implements

from openmdao.main.interfaces import IContainer, IVariable
from openmdao.main.exceptions import ConstraintError
from openmdao.main import HierarchyMember


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
                 val_type=object, ref_name=None, ref_parent=None, 
                 default=UNDEFINED, doc=None):
        """Note that Variable calls _pre_assign from here, so if _pre_assign
        requires any attributes from a derived class, those attributes must be
        set before the Variable __init__ function is called.
        """
        super(Variable, self).__init__(name, parent, doc)        

        # by default, name of the variable is the same as the obj it refers to
        if ref_name is None:
            self.ref_name = name
        else:
            self.ref_name = ref_name
        # the variable can reference an obj inside of some object other 
        # than the variable's parent
        if ref_parent is None:
            self._refparent = parent
        else:
            self._refparent = ref_parent
            
        self.observers = None
        self.permission = None
        self._constraints = []
        self.iostatus = iostatus
        self._set_count = 0
        
        if IContainer.providedBy(parent):
            parent.make_public(self)
        else:
            raise TypeError("parent of Variable '"+name+
                            "' is not an IContainer")

        if isinstance(val_type, (tuple, type)):
            self.val_type = val_type
        elif isinstance(val_type, list):
            self.val_type = tuple(val_type)
        else:
            self.raise_exception('val_type must be a type, a class, or a tuple',
                                 TypeError) 
            
        # Create the real object if it doesn't already exist.
        if not hasattr(self._refparent, self.ref_name):
            if default is UNDEFINED or default is None:
                self.raise_exception('default must be supplied'
                                     ' with implicit creation.', ValueError)
            else:
                setattr(self._refparent, self.ref_name,
                        self._pre_assign(default))

        # we'll check the validity of the default value here, but
        # we don't know the constraints yet, so the derived class
        # will have to do it again if it has constraints
        self.set_default(default)

    def _get_ref_value(self):
        return getattr(self._refparent, self.ref_name)        
        
    def set_default(self, default):
        try:
            if default is UNDEFINED:
                self.default = self._pre_assign(self._get_ref_value())
            elif default is None:
                self.default = None
            else:
                self.default = self._pre_assign(default)
        except (ValueError, TypeError), err:
            self.raise_exception('invalid default value: '+ 
                        str(err).replace(self.get_pathname()+':','',1),
                        type(err))
        
    def contains(self, path):
        """Return True if the object identified by path is an attribute of
        this object.
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
                               ' is an OUTPUT Variable and cannot be set.')
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
        
    def remove_constraint(self, con):
        """Remove a Constraint object from the list of Constraint objects for
        this Variable.
        """
        self._constraints.remove(con)

    def _pre_assign(self, val):
        """This should be overridden to perform necessary validations at
        assignment time but this base version should still be called from within the
        overridden version. If validations fail, they should raise a ValueError.
        Note that val should just be a simple value, not a Variable. This
        routine performs a simple type check on the value if the self.val_type
        attribute has been set.
        
        Returns the validated value.
        """
        if not isinstance(val, self.val_type):        
            self.raise_exception('incompatible with type '+
                                 str(type(val)), ValueError)
        
        # test against any constraints placed on this variable
        try:
            for con in self._constraints:
                con.test(val)
        except ConstraintError, err:
            self.raise_exception(str(err), ConstraintError)
        
        return val

    
    def _pre_assign_entry(self, val, index):
        """Perform validation before assignment of a value to an entry.
        This is only applicable for array Variables.
        """
        self.raise_exception('_pre_assign_entry', NotImplementedError)            

        
    def validate_var(self, variable):
        """Raise a TypeError if the given Variable is incompatible. This is
        called on the INPUT side of a connection. The value attribute of the
        Variable is not checked at this time."""
        if not isinstance(variable, type(self)):
            # TODO: could try to obtain adapter here...
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
        if name is None or name == 'value': # they're setting this Variable
            if index is None:
                self.value = value
            else:
                self.set_entry(value, index)                    
        else:  # they're setting an attribute (units, etc.)
            if index is None:
                setattr(self, name, value)
            else:
                self.raise_exception("array indexing of Variable attributes not supported", 
                                     ValueError)
            
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
        else:  # getting an attribute of this Variable
            if index is None:
                return getattr(self, name)
            else:
                val = getattr(self, name)
                for i in index:
                    val = val[i]
                return val

    def get_entry(self, index):
        """Retrieve the entry indicated by index."""
        val = self.value
        for i in index:
            val = val[i]
        return val
    
    def set_entry(self, val, index):
        """Set the value of the entry indicated by index.
        Validation will be performed prior to assignment.
        """
        tmp = self._pre_assign_entry(val, index)
        try:
            val = self.value
            for i in index[:-1]:
                val = val[i]
            val[index[len(index)-1]] = tmp
        except TypeError:
            self.raise_exception("assigning index "+str(index)+
                                 " to a value of type "+
                                 str(type(val))+" failed", ValueError)        
        except IndexError:
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
        
    
