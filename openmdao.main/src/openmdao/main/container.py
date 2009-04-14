

#public symbols
__all__ = ["Container"]

__version__ = "0.1"

import sys
import copy
import pprint
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
from openmdao.main.vartypemap import make_variable_wrapper
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

    def items(self, pub=True, recurse=False):
        """Return an iterator that returns a list of tuples of the form 
        (rel_pathname, obj) for each
        child of this Container. If pub is True, only iterate through the public
        dict of any Container. If recurse is True, also iterate through all
        child Containers of each Container found based on the value of pub.
        """
        if pub:
            return self._get_pub_items(recurse)
        else:
            return self._get_all_items(set(), recurse)
    
    def keys(self, pub=True, recurse=False):
        """Return an iterator that will return the relative pathnames of
        children of this Container. If pub is True, only children from
        the pub dict will be included. If recurse is True, child Containers
        will also be iterated over.
        """
        for entry in map(lambda x: x[0], self.items(pub,recurse)):
            yield entry
        
    def values(self, pub=True, recurse=False):
        """Return an iterator that will return the
        children of this Container. If pub is True, only children from
        the pub dict will be included. If recurse is True, child Containers
        will also be iterated over.
        """
        for entry in map(lambda x: x[1], self.items(pub,recurse)):
            yield entry
        
                    
    def add_child(self, obj, private=False):
        """Add an object (must provide IContainer interface) to this
        Container, and make it a member of this Container's public
        interface if private is False.
        """
        if obj == self:
            self.raise_exception('cannot make an object a child of itself',
                                 RuntimeError)
        if IContainer.providedBy(obj):
            # if an old child with that name exists, remove it
            if self.contains(obj.name):
                self.remove_child(obj.name)
            setattr(self, obj.name, obj)
            obj.parent = self
            if private is False:
                self.make_public(obj)
        else:
            self.raise_exception("'"+str(type(obj))+
                    "' object has does not provide the IContainer interface",
                    TypeError)
        
    def remove_child(self, name, delete=True):
        """Remove the specified child from this container and remove any
        public Variable objects that reference that child. Notify any
        observers."""
        dels = []
        for key, val in self._pub.items():
            if val.ref_name == name:
                dels.append(key)
        for dname in dels:
            del self._pub[dname]
        # TODO: notify observers
        
        if delete:
            delattr(self, name)
        
    def make_public(self, obj_info, iostatus=INPUT):
        """Adds the given object(s) as framework-accessible data object(s) of
        this Container. obj_info can be an object, the name of an object, a list
        of names of objects in this container instance, or a list of tuples of
        the form (name, alias, iostatus), where name is the name of an object
        within this container instance. This function attempts to locate an object
        with an IVariable interface that can wrap each object passed into the
        function.
        
        Returns None.
        """            
# pylint: disable-msg=R0912
        if isinstance(obj_info, list):
            lst = obj_info
        else:
            lst = [obj_info]
        
        for entry in lst:
            ref_name = None
            iostat = iostatus
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
                     'no IVariable interface available for %s' % \
                      str(entry), TypeError)
                    
            if not IVariable.providedBy(dobj):
                dobj = make_variable_wrapper(typ, name, self, iostatus=iostat, 
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
        assert(isinstance(path, basestring))
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


    def config_from_obj(self, obj):
        """This is intended to allow a newer version of a component to
        configure itself based on an older version. By default, values
        of dictionary entries from the old object will be copied to the
        new one."""
        raise NotImplementedError("config_from_obj")
    
    def save (self, outstream, format=SAVE_CPICKLE, proto=-1):
        """Save the state of this object and its children to the given
        output stream. Pure python classes generally won't need to
        override this because the base class version will suffice, but
        python extension classes will have to override. The format
        can be supplied in case something other than cPickle is needed."""
        if isinstance(outstream, basestring):
            if format is SAVE_CPICKLE or format is SAVE_PICKLE:
                mode = 'wb'
            else:
                mode = 'w'
            try:
                outstream = open(outstream, mode)
            except IOError, exc:
                self.raise_exception(exc.args, type(exc))

        if format is SAVE_CPICKLE:
            cPickle.dump(self, outstream, proto) # -1 means use highest protocol
        elif format is SAVE_PICKLE:
            pickle.dump(self, outstream, proto)
        elif format is SAVE_YAML:
            yaml.dump(self, outstream)
        elif format is SAVE_LIBYAML:
            if _libyaml is False:
                self.warning('libyaml not available, using yaml instead')
            yaml.dump(self, outstream, Dumper=Dumper)
        else:
            self.raise_exception('cannot save object using format '+str(format),
                                 RuntimeError)
    
    @staticmethod
    def load (instream, format=SAVE_CPICKLE):
        """Load an object of this type from the input stream. Pure python 
        classes generally won't need to override this, but extensions will. 
        The format can be supplied in case something other than cPickle is 
        needed."""
        if isinstance(instream, basestring):
            if format is SAVE_CPICKLE or format is SAVE_PICKLE:
                mode = 'rb'
            else:
                mode = 'r'
            instream = open(instream, mode)

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
        [x.post_load() for x in self.values(pub=False) 
                                                if isinstance(x,Container)]

    def pre_delete(self):
        """ Perform any required operations before the model is deleted. """
        [x.pre_delete() for x in self.values(pub=False) 
                                                if isinstance(x,Container)]

    def _get_pub_items(self, recurse=False):
        """Generate a list of tuples of the form (rel_pathname, obj) for each
        child of this Container. Only iterate through the public
        dict of any Container. If recurse is True, also iterate through all
        public child Containers of each Container found.
        """
        for name,obj in self._pub.items():
            yield (name, obj)
            cont = None
            if hasattr(obj,'value') and isinstance(obj.value,Container):
                cont = obj.value
            elif isinstance(obj, Container):
                cont = obj
            if cont and recurse:
                for chname, child in cont._get_pub_items(recurse):
                    yield ('.'.join([name,chname]), child)
                   
    def _get_all_items(self, visited, recurse=False):
        """Generate a list of tuples of the form (rel_pathname, obj) for each
        child of this Container.  If recurse is True, also iterate through all
        child Containers of each Container found.
        """
        for name,obj in self.__dict__.items():
            if not name.startswith('_') and id(obj) not in visited:
                visited.add(id(obj))
                yield (name, obj)
                if isinstance(obj, Container) and recurse:
                    for chname, child in obj._get_all_items(visited, recurse):
                        yield ('.'.join([name,chname]), child)
                   
