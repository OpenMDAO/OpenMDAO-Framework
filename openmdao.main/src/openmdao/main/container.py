

#public symbols
__all__ = ["Container"]

__version__ = "0.1"

import copy
import pickle
import cPickle
import yaml
try:
    from yaml import CLoader as Loader
    from yaml import CDumper as Dumper
    _libyaml = True
except ImportError:
    from yaml import Loader, Dumper
    _libyaml = False

import weakref
# the following is a monkey-patch to correct a problem with
# copying/deepcopying weakrefs There is an issue in the python issue tracker
# regarding this, but it isn't fixed yet.

# pylint: disable-msg=W0212
copy._copy_dispatch[weakref.ref] = copy._copy_immutable  
copy._deepcopy_dispatch[weakref.ref] = copy._deepcopy_atomic
copy._deepcopy_dispatch[weakref.KeyedRef] = copy._deepcopy_atomic
# pylint: enable-msg=W0212

from zope.interface import implements

from openmdao.main.interfaces import IContainer, IVariable
from openmdao.main import HierarchyMember
from openmdao.main.variable import INPUT
from openmdao.main.vartypemap import find_var_class
from openmdao.main.log import logger
from openmdao.main.factorymanager import create as fmcreate
from openmdao.main.constants import SAVE_YAML, SAVE_LIBYAML
from openmdao.main.constants import SAVE_PICKLE, SAVE_CPICKLE

class Container(HierarchyMember):
    """ Base class for all objects having Variables that are visible 
    to the framework"""
   
    implements(IContainer)
    
    def __init__(self, name, parent=None, doc=None, add_to_parent=True):
        super(Container, self).__init__(name, parent, doc)            
        self._pub = {}  # A container for framework accessible objects.
        if parent is not None and \
           IContainer.providedBy(parent) and add_to_parent:
            parent.add_child(self)
        
    def add_child(self, obj, private=False):
        """Add an object (must provide IContainer interface) to this
        Container, and make it a member of this Container's public
        interface if private is False.
        """
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
        """Remove the specified child from this container and remove any
        Variable objects from _pub that reference that child. Notify any
        observers."""
        dels = []
        for key, val in self._pub.items():
            if val.ref_name == name:
                dels.append(key)
        for dname in dels:
            del self._pub[dname]
        delattr(self, name)
        
    def make_public(self, obj_info):
        """Adds the given object(s) as framework-accessible data object(s) of
        this Container. obj_info can be a single non-Variable object, a list
        of names of objects in this container instance, or a list of tuples of
        the form (name, alias, iostatus), where name is the name of an object
        within this container instance. If iostatus is not supplied, the
        default value is INPUT. This function attempts to locate an object
        with an IVariable interface that can wrap each object passed into the
        function.
        
        Returns None.
        """            
# pylint: disable-msg=R0912
        if isinstance(obj_info, list):
            lst = obj_info
        else:
            lst = [obj_info]
        
        for i, entry in enumerate(lst):
            ref_name = None
            iostat = INPUT
            dobj = None

            if isinstance(entry, basestring):
                name = entry
                typ = type(getattr(self, name))
                    
            elif isinstance(entry, tuple):
                name = entry[0]  # wrapper name
                ref_name = entry[1]  # internal name
                if not ref_name:
                    ref_name = name
                if len(entry) > 2:
                    iostat = entry[2] # optional iostatus
                typ = type(getattr(self, ref_name))
                
            else:
                dobj = entry
                if hasattr(dobj, 'name'):
                    name = dobj.name
                    typ = type(dobj)
                else:
                    self.raise_exception(
                     'no IVariable interface available for the object at %d' % \
                      i, TypeError)
                    
            if not IVariable.providedBy(dobj):
                dobj = find_var_class(typ, name, self, iostatus=iostat, 
                                      ref_name=ref_name)
            
            if IVariable.providedBy(dobj):
                dobj.parent = self
                self._pub[dobj.name] = dobj
            else:
                self.raise_exception(
                    'no IVariable interface available for the object named '+
                    str(name), TypeError)

    def make_private(self, name):
        """Remove the named object from the _pub container, which will make it
        no longer accessible to the framework. 
        
        Returns None.
        """
        del self._pub[name]

    def contains(self, path):
        """Return True if the child specified by the given dotted path
        name is publicly accessibly and is contained in this Container. 
        """
        try:
            base, name = path.split('.', 1)
        except ValueError:
            return path in self._pub
        obj = self._pub.get(base)
        if obj is not None:
            return obj.contains(name)
        return False
            
    def create(self, type_name, name, version=None, server=None, 
               private=False, res_desc=None):
        """Create a new object of the specified type inside of this
        Container.
        
        Returns the new object.        
        """
        obj = fmcreate(type_name, name, version, server, res_desc)
        self.add_child(obj, private)
        return obj

    def get(self, path, index=None):
        """Return any public object specified by the given 
        path, which may contain '.' characters.  
        
        Returns the value specified by the name. This will either be the value
        of a Variable or some attribute of a Variable.
        
        """
        assert(isinstance(path, basestring))
        
        try:
            base, name = path.split('.', 1)
        except ValueError:
            try:
                if index is not None:
                    return self._pub[path].get_entry(index)
                else:
                    return self._pub[path].value
            except KeyError:
                self.raise_exception("object has no attribute '"+path+"'",
                                     AttributeError)

        return self._pub[base].get(name, index)

    
    def getvar(self, path):
        """Return the public Variable specified by the given 
        path, which may contain '.' characters.  
        
        Returns the specified Variable object.
        """
        assert(isinstance(path, basestring))
        try:
            base, name = path.split('.', 1)
        except ValueError:
            try:
                return self._pub[path]
            except KeyError:
                self.raise_exception("object has no attribute '"+path+"'", 
                                     AttributeError)
        try:
            return self._pub[base].getvar(name)
        except KeyError:
            try:
                return self._pub[path]
            except KeyError:
                self.raise_exception("object has no attribute '"+path+"'", 
                                     AttributeError)
                
    def set(self, path, value, index=None):
        """Set the value of the data object specified by the  given path, which
        may contain '.' characters.  If path specifies a Variable, then its
        value attribute will be set to the given value, subject to validation
        and  constraints. index, if not None, should be a list of ints, at
        most one for each array dimension of the target value.
        
        """ 
        assert(isinstance(path, basestring))
        try:
            base, name = path.split('.', 1)
        except ValueError:
            base = path
            name = None
           
        try:
            obj = self._pub[base]
        except KeyError:
            self.raise_exception("object has no attribute '"+base+"'", 
                                 AttributeError)
        except TypeError:
            self.raise_exception("object has no attribute '"+str(base)+
                                 "'", AttributeError)
            
        obj.set(name, value, index)        


    def setvar(self, path, variable):
        """Set the value of a Variable in this Container with another Variable.
        This differs from setting to a simple value, because the destination
        Variable can use info from the source Variable to perform conversions
        if necessary, as in the case of Float Variables with differing units.
        """
        try:
            base, name = path.split('.', 1)
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

        
    def get_objs(self, matchfunct, recurse=False):
        """Return a list of objects that return a value of True when passed
        to matchfunct.
        
        """            
        def _recurse_get_objs(obj, matchfunct, visited):
            objs = []
            for child in obj.__dict__.values():
                if id(child) in visited:
                    continue
                visited.add(id(child))
                if matchfunct(child):
                    objs.append(child)
                if IContainer.providedBy(child):
                    objs.extend(_recurse_get_objs(child, matchfunct, visited))
            return objs
            
        objs = []
        visited = set()
        
        if recurse:
            return _recurse_get_objs(self, matchfunct, visited)
        else:
            return [child for child in self.__dict__.values() 
                                               if matchfunct(child)]
            
       
    def get_names(self, matchfunct, recurse=False):
        """Return a list of objects that provide the specified interface and
        also have attributes with values that match those passed as named
        args"""
        return [x.get_pathname() 
                    for x in self.get_objs(matchfunct, recurse)]
        
    def config_from_obj(self, obj):
        """This is intended to allow a newer version of a component to
        configure itself based on an older version. By default, values
        of dictionary entries from the old object will be copied to the
        new one."""
        raise NotImplementedError("config_from_obj")
    
    def save (self, outstream, format=SAVE_CPICKLE):
        """Save the state of this object and its children to the given
        output stream. Pure python classes generally won't need to
        override this because the base class version will suffice, but
        python extension classes will have to override. The format
        can be supplied in case something other than cPickle is needed."""
        if format is SAVE_CPICKLE:
            cPickle.dump(self, outstream, -1) # -1 means use highest protocol
        elif format is SAVE_PICKLE:
            pickle.dump(self, outstream, -1)
        elif format is SAVE_YAML:
            yaml.dump(self, outstream)
        elif format is SAVE_LIBYAML:
            if _libyaml is False:
                self.warning('libyaml not available, using yaml instead')
            yaml.dump(self, outstream, Dumper=Dumper)
        else:
            raise RuntimeError('cannot save object using format '+str(format))
    
    @staticmethod
    def load (instream, format=SAVE_CPICKLE):
        """Load an object of this type from the input stream. Pure python 
        classes generally won't need to override this, but extensions will. 
        The format can be supplied in case something other than cPickle is 
        needed."""
        if format is SAVE_CPICKLE:
            return cPickle.load(instream)
        elif format is SAVE_PICKLE:
            return pickle.load(instream)
        elif format is SAVE_YAML:
            return yaml.load(instream)
        elif format is SAVE_LIBYAML:
            if _libyaml is False:
                logger.warn('libyaml not available, using yaml instead')
            return yaml.load(instream, Loader=Loader)
        else:
            raise RuntimeError('cannot load object using format '+str(format))

    def post_load(self):
        """ Perform any required operations after model has been loaded. """
        subcontainers = self.get_objs(IContainer.providedBy)
        for child in subcontainers:
            child.post_load()

    def pre_delete(self):
        """ Perform any required operations before the model is deleted. """
        subcontainers = self.get_objs(IContainer.providedBy)
        for child in subcontainers:
            child.pre_delete()

