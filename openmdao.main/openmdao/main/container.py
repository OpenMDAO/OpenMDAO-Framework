
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
from openmdao.main.variable import INPUT, OUTPUT
from openmdao.main.vartypemap import find_var_class
import openmdao.main.factorymanager as factorymanager
import openmdao.main.constants as constants


class Container(HierarchyMember):
    """ Base class for all objects having Variables that are visible 
    to the framework"""
   
    implements(IContainer)
    
   
    def __init__(self, name, parent, desc=None):
        HierarchyMember.__init__(self, name, parent, desc)            
        self._pub = {}  # A container for framework accessible objects.
        
    def add_child(self, obj, private=False):
        if IContainer.providedBy(obj):
            setattr(self, obj.name, obj)
            obj.parent = self
            if private is False:
                self.make_public(obj)
        else:
            self.raise_exception("'"+str(type(obj))+
                                 "' object has does not provide the IContainer interface",
                                 TypeError)
        
    def remove_child(self, name):
        """Remove the specified child from this container and remove any Variable objects
        from _pub that reference that child."""
        dels = []
        for key,val in self._pub.items():
            if val.ref_name == name:
                dels.append(key)
        for name in dels:
            del self._pub[name]
        delattr(self, name)
        
    def make_public(self, obj_info):
        """Adds the given object(s) as framework-accessible data object(s) of this
        Container. obj_info can be a single non-Variable object, a list of names of objects
        in this container instance, or a list of tuples of the form (name, alias, iostatus),
        where name is the name of an object within this container instance.  If iostatus is
        not supplied, the default value is INPUT.  This function attempts to locate an object
        with an IVariable interface that can wrap each object passed into the function.
        
        Returns None.
        """
        if isinstance(obj_info, list):
            lst = obj_info
        else:
            lst = [obj_info]
            
        for entry in lst:
            if isinstance(entry, basestring):
                name = entry
                dobj = find_var_class(type(getattr(self, name)), name, self, 
                                      iostatus=INPUT)
            elif isinstance(entry, tuple):
                name = entry[0]  # wrapper name
                ref_name = entry[1]  # internal name
                if len(entry) > 2:
                    iostatus = entry[2]
                else:
                    iostatus = INPUT
                dobj = find_var_class(type(getattr(self, ref_name)), name, self, 
                                      iostatus=iostatus,
                                      ref_name=ref_name)
            else:
                dobj = entry
                if hasattr(dobj, 'name'):
                    name = dobj.name
                else:
                    name = None
                if not IVariable.providedBy(dobj):
                    dobj = find_var_class(type(dobj), name, self, iostatus=INPUT)
            
            if IVariable.providedBy(dobj):
                dobj.parent = self
                self._pub[dobj.name] = dobj
            else:
                self.raise_exception('no IVariable interface available for the object named '+
                                     str(name), TypeError)

    def make_private(self, name):
        """Remove the named object from the _pub container, which will make it
        no longer accessible to the framework. 
        
        Returns None.
        """
        del self._pub[name]

    def contains(self, path):
        try:
            base, name = path.split('.',1)
        except ValueError:
            return path in self._pub
        if base in self._pub:
            return self._pub[base].contains(name)
        return False
            
    def create(self, type_name, name, version=None, server=None, private=False, res_desc=None):
        """Create a new object of the specified type inside of this
        Container. The object must have a 'name' attribute.
        
        Returns the new object.        
        """
        obj = factorymanager.create(type_name, name, version, server, res_desc)
        self.add_child(obj, private)
        return obj

    def get(self, path, index=None):
        """Return any public object specified by the given 
        path, which may contain '.' characters.  If the name matches the name of a Variable,
        the value of the Variable will be returned. Attributes of variables may also be 
        returned.
        
        """
        assert(isinstance(path,basestring))
        
        try:
            base, name = path.split('.',1)
        except ValueError:
            try:
                if index is not None:
                    return self._pub[path].get_entry(index)
                else:
                    return self._pub[path].value
            except KeyError:
                self.raise_exception("object has no attribute '"+path+"'", AttributeError)

        return self._pub[base].get(name, index)

    
    def getvar(self, path):
        """Return the public Variable specified by the given 
        path, which may contain '.' characters.  Only returns Variables, not attributes or
        other Containers.
        """
        assert(isinstance(path,basestring))
        try:
            base, name = path.split('.',1)
        except ValueError:
            try:
                return self._pub[path]
            except KeyError:
                self.raise_exception("object has no attribute '"+path+"'", AttributeError)

        return self._pub[base].getvar(name)
        
                
    def set(self, path, value, index=None):
        """Set the value of the data object specified by the 
        given path, which may contain '.' characters.  If path specifies a Variable, then
        its value attribute will be set to the given value, subject to validation and 
        constraints.
        
        """ 
        assert(isinstance(path,basestring))
        try:
            base, name = path.split('.',1)
        except ValueError:
            base = path
            name = None
           
        try:
            obj = self._pub[base]
        except KeyError:
            self.raise_exception("object has no attribute '"+base+"'", AttributeError)
        except TypeError:
            self.raise_exception("object has no attribute '"+str(base)+"'", AttributeError)
            
        obj.set(name, value, index)        


    def setvar(self, path, variable):
        try:
            base, name = path.split('.',1)
        except ValueError:
            base = path
            name = None
        try:
            obj = self._pub[base]
        except KeyError:
            self.raise_exception("object has no attribute '"+
                                 base+"'", AttributeError)
        except TypeError:
            self.raise_exception("object has no attribute '"+
                                 str(base)+"'", AttributeError)
        obj.setvar(name, variable)        
        
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
        
    
    def save (self, outstream, format=constants.SAVE_PICKLE):
        """Save the state of this object and its children to the given
        output stream. Pure python classes generally won't need to
        override this because the base class version will suffice, but
        python extension classes will have to override. The format
        can be supplied in case something other than cPickle is needed."""
        self.raise_exception('save', NotImplementedError)
    
    @staticmethod
    def load (instream, format=constants.SAVE_PICKLE):
        """Load an object of this type from the input stream. Pure python 
        classes generally won't need to override this, but extensions will. 
        The format can be supplied in case something other than cPickle is 
        needed."""
        raise NotImplementedError('load')
