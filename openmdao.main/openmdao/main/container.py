
"""

This is the base class for all objects containing Variables that are accessible
to the OpenMDAO framework.

"""

#public symbols
__all__ = ["Container"]

__version__ = "0.1"


from zope.interface import implements

from openmdao.main.interfaces import IContainer, IVariable
from openmdao.main.hierarchy import HierarchyMember
from openmdao.main.variable import Variable
import openmdao.main.factorymanager as factorymanager
import openmdao.main.constants as constants


#??? check out operator.attrgetter (in 2.6) which allows dotted names

class Container(HierarchyMember):
    """ Base class for all objects having Variables that are visible 
    to the framework"""
   
    implements(IContainer)
   
    def __init__(self, name, parent=None):
        HierarchyMember.__init__(self, name, parent)
        self._framework_objs = {}
            
#    def bind_variable(self, name, var):
#        """Bind a Variable object to an object in self."""
#        if isinstance(var, Variable):
#            self._framework_objs[name] = var
#        else:
#            raise TypeError('binding object must be a Variable')

        
    def add_child(self, child):
        """Adds the given child object to the dict of this Container.
        The child must provide the IContainer interface."""
        if IContainer.providedBy(child):
            child._parent = self
            self._framework_objs[child.name] = child
            setattr(self, child.name, child)
        else:
            raise TypeError('child does not provide the IContainer interface')

        
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
        scope = self
        for name in path.split('.'):
            try:
                scope = scope._framework_objs[name]
            except:
                raise NameError("'"+path+"' not a framework-accessible object")
        return scope    

    
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
            
            
        objs = []
        
        if recurse:
            for child in self._framework_objs.values():
                if iface.providedBy(child):
                    objs.append(child)
                if IContainer.providedBy(child):
                    objs.extend(child.get_objs(iface, recurse, **kwargs))
        else:
            for child in self._framework_objs.values():
                if iface.providedBy(child):
                    objs.append(child)
            return objs
            
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
        obj = getattr(self, name)
        self._framework_objs.remove(obj)
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
        """Replace the current object in the hierarchy with the object
        loaded from the input stream. Pure python classes generally
        won't need to override this, but extensions will. The format
        can be supplied in case something other than cPickle is needed."""
        raise NotImplementedError('restore_state')
