#public symbols
__all__ = ['Variable', 'UNDEFINED', 
           'VariableChangedEvent', 'VariableRemovePendingEvent']
__version__ = "0.1"

import copy
from zope.interface import implements

from openmdao.main.interfaces import IContainer, IVariable
from openmdao.main.exceptions import ConstraintError
from openmdao.main import HierarchyMember


INPUT = 1
OUTPUT = 2

# use this to represent undefined value rather than None    
UNDEFINED = '<undefined>'

# some events to allow specific types of observer notification

class VariableChangedEvent(object):
    def __init__(self, var):
        self.var = var
        
class VariableRemovePendingEvent(object):
    def __init__(self, var):
        self.var = var

    
class Variable(HierarchyMember):
    """ An object representing data to be passed between Components within
    the framework. It will perform validation when assigned to another
    Variable. It can notify other objects when its value is modified.
    """

    implements(IVariable, IContainer)
    
    def __init__(self, name, parent, iostatus, 
                 val_types=None, ref_name=None, ref_parent=None, 
                 default=UNDEFINED, doc=None, implicit_creation=True):
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
        self.iostatus = iostatus
        self._constraints = []
        self.valid = False
        self._passthru = None
        
        if IContainer.providedBy(parent):
            parent.make_public(self)
        else:
            raise TypeError("parent of Variable '"+name+
                            "' is not an IContainer")

        self._set_val_types(val_types, default)
                    
        # Create the real object if it doesn't already exist.
        if implicit_creation and self._find(self._refparent, self.ref_name) is None:
            if default is UNDEFINED or default is None:
                self.raise_exception('default must be supplied'
                                     ' with implicit creation.', ValueError)
            else:
                self._find_and_set(self._refparent, self.ref_name,
                                   self._pre_assign(default))

        # we'll check the validity of the default value here, but
        # we don't know the constraints yet, so the derived class
        # will have to do it again if it has constraints
        self.set_default(default)

    def _set_val_types(self, val_types, default):
        """Set self.val_types to what was provided, or guess based on default
        value or referenced value.
        """
        if isinstance(val_types, tuple):
            self.val_types = val_types
        elif isinstance(val_types, list):
            self.val_types = tuple(val_types)
        elif isinstance(val_types, type):
            self.val_types = (val_types,)
        elif val_types is None and default is not None:
            if default is UNDEFINED:
                self.val_types = (type(self.get_value()),)
            elif default is not None:
                self.val_types = (type(default),)
        else:
            self.raise_exception('val_types must be a type, a class, or a tuple',
                                 TypeError) 
        
    def _find(self, scope, path):
        """Locate the object specified by the dotted pathname 'path' within the
        object called scope. Return the object or None if the object is not 
        found.
        """
        obj = scope
        for part in path.split('.'):
            try:
                obj = getattr(obj, part)
            except AttributeError:
                return None
        return obj

    def _find_and_set(self, scope, path, value):
        """Locate the object specified by the dotted pathname 'path' within the
        object called scope and set it to value. If the last entry in the 
        dotted path doesn't exist, it will be created.
        """
        try:
            partpath,endpath = path.rsplit('.', 1)
        except ValueError:
            setattr(scope, path, value)
            return
        obj = self._find(scope, partpath)
        if obj is None:
            self.raise_exception('object %s does not exist' % partpath, NameError)
        setattr(obj, endpath, value)
            
    def set_default(self, default):
        if default is None:
            self.default = None
            return
        try:
            if default is UNDEFINED:
                try: # it may be too early for get_value() to work properly, so
                     # just try it and see
                    tmp = self.get_value()
                except Exception:
                    self.default = None
                    return
            else:
                tmp = default
            # if val_types isn't set yet, set it based on the default value
            if self.val_types is None:
                self.val_types = (type(tmp),)
            self.default = self._pre_assign(tmp)
        except (ValueError, TypeError), err:
            self.raise_exception('invalid default value: '+ 
                        str(err).replace(self.get_pathname()+':','',1),
                        type(err))

    def contains(self, path):
        """Return True if the object identified by path is an attribute of
        this object.
        """
        return hasattr(self, path)
    
    def set_value(self, val):
        """Assign this Variable's value to the value of another Variable or 
        directly to another value.  Checks validity of the new value before assignment.
        """
        self._logger.debug('setting %s to value of %s' % (self.get_pathname(),str(val)))
        if self.iostatus == OUTPUT:
            raise RuntimeError(self.get_pathname()+
                               ' is an OUTPUT Variable and cannot be set.')
        if self._passthru is None:
            setattr(self._refparent, self.ref_name, self._pre_assign(val))
        else:
            self._passthru.set_value(val)
            
        if self.valid is True:
            self._logger.debug('invalidating %s'%self.get_pathname())
            self.valid = False
            # since we've been newly invlidated, notify our parent (or it's parent) so dependent vars
            # can also be invalidated
            if self.parent and hasattr(self.parent, 'invalidate_deps'):
                self.parent.invalidate_deps([self], notify_parent=True)
            
        if self.observers is not None:
            self._notify_observers()
        
    def get_value(self):
        """"Called when getting the 'value' of this Variable. """
        if self._passthru is None:
            return getattr(self._refparent, self.ref_name)
        else:
            return self._passthru.get_value()
         
    def revert_to_default(self):
        """ Return this Variable to its default value"""
        self.set_value(self.default)

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
        routine performs a simple type check on the value if the self.val_types
        attribute has been set.
        
        Returns the validated value.
        """
        if type(val) not in self.val_types:        
            self.raise_exception('incompatible type %s is not one of %s' %
                                 (str(type(val)),
                                  str([x.__name__ for x in self.val_types])) , ValueError)
        
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
        if type(variable) != type(self):
            # TODO: could try to obtain adapter here...
            self.raise_exception("assignment to incompatible variable '"+
                                 variable.get_pathname()+"' of type '"+
                                 str(type(variable))+"'", TypeError)
            
    def _convert(self, variable):
        """Some Variables, e.g., Float, will need to override this in order to
        convert units from those in connected variables. By default, no 
        conversion is performed.
        """
        return variable.get_value()
    
    def setvar(self, name, var):
        """Assign this Variable to another Variable, which generally just
        means to assign our value to the value of var. Some Variables will
        override _convert to handle things like unit conversion."""
        if name is None: # they're setting this Variable
            if self._passthru is not var:
                self.validate_var(var)
                self.set_value(self._convert(var))
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
                self.set_value(value)
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
        if name is None or name == 'value':
            if index is None:
                return self.get_value()
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
        l = len(index)
        if l == 1:
            return self.get_value()[index[0]]
        elif l == 2:
            return self.get_value()[index[0]][index[1]]
        elif l == 3:
            return self.get_value()[index[0]][index[1]][index[2]]
        else:
            val = self.get_value()
            for i in index:
                val = val[i]
            return val
    
    def set_entry(self, val, index):
        """Set the value of the entry indicated by index.
        Validation will be performed prior to assignment.
        """
        tmp = self._pre_assign_entry(val, index)
        try:
            l = len(index)
            if l == 1:
                self.get_value()[index[0]] = tmp
            elif l == 2:
                self.get_value()[index[0]][index[1]] = tmp
            elif l == 3:
                self.get_value()[index[0]][index[1]][index[2]] = tmp
            else:
                value = self.get_value()
                for i in index[:-1]:
                    value = value[i]
                self.get_value()[index[len(index)-1]] = tmp
        except (TypeError, IndexError):
            self.raise_exception("assigning index "+str(index)+
                                 " to a value of type "+
                                 str(type(val))+" failed", ValueError)        
    
    def make_public(self, child):
        """Make a given object a member of this object's public interface."""
        self.raise_exception('make_public', NotImplemented)            

    def add_observer(self, obs_funct, event_class):
        """ Add a function to be called when this variable is modified. The
        function should accept the specified event_class_name as an argument.
        """
        if self.observers is None:
            self.observers = { event_class: [obs_funct] }
        else:
            evlist = self.observers.get(event_class, None)
            if evlist:
                evlist.append(obs_funct)
            else:                
                self.observers[event_class] = { event_class: [obs_funct] }

    def _notify_observers(self, event):
        """Call each observer on the observers list."""
        for obs in self.observers.get(event.__class__.__name__, []):
            obs(self)
        
    def create_passthru(self, parent, name=None):
        """Create a passthru version of self that can be made public in the 
        containing scope.
        """
        newvar = copy.copy(self)
        
        if name is not None:
            newvar.name = name
            
        newvar.parent = parent
        newvar._passthru = self
        return newvar
    
