
"""

This is the base class for all objects containing Variables that are accessible
to the OpenMDAO framework.

"""

#public symbols
__all__ = ["Container"]

__version__ = "0.1"


from zope.interface import implements

from openmdao.main.interfaces import IContainer
from openmdao.main.hierarchy import HierarchyMember
import openmdao.main.factorymanager as factorymanager
import openmdao.main.constants as constants


#??? check out operator.attrgetter (in 2.6) which allows dotted names

class Container(HierarchyMember):
    """ Base class for all objects having Variables that are visible 
    to the framework"""
   
    implements(IContainer)
   
    def __init__(self, name, parent, desc=None):
        HierarchyMember.__init__(self, name, parent, desc)
        self._framework_objs = {}
            

    def _error_msg(self, msg):
        return self.get_pathname()+': '+msg

    
    def add_child(self, child):
        """Adds the given child as a framework-accessible object of 
        this Container. The child must provide the IContainer interface."""
        if IContainer.providedBy(child):
            child._parent = self
            self._framework_objs[child.name] = child
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
        try:
            scope = self._framework_objs[base]
        except KeyError:
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
        try:
            base, name = path.split('.',1)
        except ValueError:
            base = path
            name = None
        try:
            scope = self._framework_objs[base]
        except:
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
        del self._framework_objs[name]
 
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
