
"""

This is the base class for all objects containing Variables that are accessible
to the OpenMDAO framework.

"""

#public symbols
__all__ = ["Container"]

__version__ = "0.1"

import copy
import weakref
# the following is a monkey-patch to correct a problem with copying/deepcopying weakrefs
# There is an issue in the python issue tracker regarding this, but it isn't fixed yet.
copy._copy_dispatch[weakref.ref] = copy._copy_immutable
copy._deepcopy_dispatch[weakref.ref] = copy._deepcopy_atomic
copy._deepcopy_dispatch[weakref.KeyedRef] = copy._deepcopy_atomic

from zope.interface import implements

from openmdao.main.interfaces import IContainer, IVariable
from openmdao.main.hierarchy import HierarchyMember
import openmdao.main.factorymanager as factorymanager
import openmdao.main.constants as constants


class Container(HierarchyMember):
    """ Base class for all objects having Variables that are visible 
    to the framework"""
   
    implements(IContainer)
   
    def __init__(self, name, parent, desc=None):
        HierarchyMember.__init__(self, name, parent, desc)
        self._data_objs = {}
            

    def _error_msg(self, msg):
        return self.get_pathname()+': '+msg

    
    def add_child(self, child):
        """Adds the given child as a framework-accessible object of 
        this Container. The child must provide the IContainer interface.
        
        Returns None.
        """
        if IContainer.providedBy(child):
            child._parent = self
            if IVariable.providedBy(child):
                self._data_objs[child.name] = child
            else:
                setattr(self, child.name, child)
        else:
            raise TypeError(self._error_msg(
                                'child does not provide the IContainer interface'))

        
    def create(self, type_name, name, version=None, server=None, res_desc=None):
        """Create a new object of the specified type inside of this
        Container. The object must have a 'name' attribute.
        
        Returns the new object.
        
        """
        obj = factorymanager.create(type_name, name, version, server, res_desc)
        self.add_child(obj)
        return obj

        
    def get(self, path):
        """Return the framework-accessible object specified by the given 
        path, which may contain '.' characters.
        
        """
        try:
            base, name = path.split('.',1)
        except ValueError:
            base = path
            name = None

        if base in self._data_objs:
            scope = self._data_objs[base]
        else:
            scope = getattr(self,base)
            
        if not IContainer.providedBy(scope):
            raise NameError(self._error_msg("'"+path+
                                            "' not a framework-accessible object"))
        if name is None:
            return scope
        else:
            return scope.get(name)

    def set(self, path, value):
        """Set the value of the framework-accessible object specified by the 
        given path, which may contain '.' characters.
        
        """ 
        if path is None:
            if IVariable.providedBy(value):
                value = value.get(None)
            if IContainer.providedBy(value):
                newcont = copy.deepcopy(value)
                self._parent.add_child(value)  # this will replace this container
                return
        
        try:
            base, name = path.split('.',1)
        except ValueError:
            base = path
            name = None
        
        if base in self._data_objs:
            scope = self._data_objs[base]
        else:
            scope = getattr(self,base)
            
        if not IContainer.providedBy(scope):
            raise NameError(self._error_msg("'"+path+
                                            "' not a framework-accessible object"))        
        scope.set(name, value)

    
    def get_objs(self, iface, recurse=False, **kwargs):
        """Return a list of objects with the specified interface that
        also have attributes with values that match those passed as named
        args.
        
        """
        
        def matches(obj, dct):
            for key,val in dct.items():
                try:
                    if getattr(obj, key) != val:
                        return False
                except:
                    return False
            return True
            
        def recurse_get_objs(obj, iface, visited, **kwargs):
            objs = []
            for child in obj.__dict__.values():
                if id(child) in visited:
                    continue
                visited.add(id(child))
                if iface.providedBy(child):
                    objs.append(child)
                if IContainer.providedBy(child):
                    objs.extend(recurse_get_objs(child, iface, visited, **kwargs))
            return objs
            
        objs = []
        visited = set()
        
        if recurse:
            objs = recurse_get_objs(self, iface, visited, **kwargs)
        else:
            objs = [child for child in self.__dict__.values() if iface.providedBy(child)]
            
        if len(kwargs) > 0:
            return [x for x in objs if matches(x,kwargs)]
        else:
            return objs

        
    def get_names(self, iface, recurse=False, **kwargs):
        """Return a list of objects that provide the specified interface and
        also have attributes with values that match those passed as named
        args"""
        return [x.get_pathname() 
                    for x in self.get_objs(iface, recurse, **kwargs)]
        
    
    def remove_child(self, name):
        """Remove the named object from this container and notify any 
        observers.
        
        """
        if name in self._data_objs:
            del self._data_objs[name]
        else:
            delattr(self, name)
 
    def save (self, outstream, format=constants.SAVE_PICKLE):
        """Save the state of this object and its children to the given
        output stream. Pure python classes generally won't need to
        override this because the base class version will suffice, but
        python extension classes will have to override. The format
        can be supplied in case something other than cPickle is needed."""
        raise NotImplementedError('save_state')
    
    @staticmethod
    def load (instream, format=constants.SAVE_PICKLE):
        """Load an object of this type from the input stream. Pure python 
        classes generally won't need to override this, but extensions will. 
        The format can be supplied in case something other than cPickle is 
        needed."""
        raise NotImplementedError('restore_state')
