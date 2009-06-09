
#public symbols
__all__ = ["Container"]

__version__ = "0.1"

import copy
import sys

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
import networkx as nx

from openmdao.main.interfaces import IContainer, IVariable
from openmdao.main import HierarchyMember
from openmdao.main.variable import Variable, INPUT, OUTPUT
from openmdao.main.vartypemap import make_variable_wrapper
from openmdao.main.log import logger
from openmdao.main.factorymanager import create as fmcreate
from openmdao.main.constants import SAVE_CPICKLE

import openmdao.util.save_load

EGG_SERVER_URL = 'http://torpedo.grc.nasa.gov:31001'


class Container(HierarchyMember):
    """ Base class for all objects having Variables that are visible 
    to the framework"""
   
    implements(IContainer)
    
    def __init__(self, name, parent=None, doc=None, add_to_parent=True):
        super(Container, self).__init__(name, parent, doc)            
        self._pub = {}  # A container for framework accessible objects.
        self._io_graph = None
        if parent is not None and \
           isinstance(parent, Container) and add_to_parent:
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
        for entry in self.items(pub, recurse):
            yield entry[0]
        
    def values(self, pub=True, recurse=False):
        """Return an iterator that will return the
        children of this Container. If pub is True, only children from
        the pub dict will be included. If recurse is True, child Containers
        will also be iterated over.
        """
        for entry in self.items(pub, recurse):
            yield entry[1]
    
    def has_invalid_inputs(self):
        """Return True if this object has any invalid input Variables."""
        return len(self.get_inputs(valid=False)) > 0
    
    def get_inputs(self, valid=None):
        """Return a list of input Variables. If valid is True or False, 
        return only inputs with a matching .valid attribute.
        If valid is None, return inputs regardless of validity.
        """
        if valid is None:
            return [x for x in self._pub.values() if isinstance(x, Variable) 
                                                     and x.iostatus == INPUT]
        else:
            return [x for x in self._pub.values() if isinstance(x, Variable) 
                                                     and x.iostatus == INPUT 
                                                     and x.valid == valid]
        
    def has_invalid_outputs(self):
        """Return True if this object has any invalid output Variables."""
        return len(self.get_outputs(valid=False)) > 0
    
    def get_outputs(self, valid=None):
        """Return a list of output Variables. If valid is True or False, 
        return only outputs with a matching .valid attribute.
        If valid is None, return outputs regardless of validity.
        """
        if valid is None:
            return [x for x in self._pub.values() if isinstance(x, Variable)
                                                     and x.iostatus == OUTPUT]
        else:
            return [x for x in self._pub.values() if isinstance(x, Variable) 
                                                     and x.iostatus == OUTPUT 
                                                     and x.valid == valid]
    
    def add_child(self, obj):
        """Add an object (must provide IContainer interface) to this
        Container, and make it a member of this Container's public
        interface.
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
            self.make_public(obj)
        else:
            self.raise_exception("'"+str(type(obj))+
                    "' object has does not provide the IContainer interface",
                    TypeError)
        return obj
        
    def remove_child(self, name, delete=True):
        """Remove the specified child from this container and remove any
        public Variable objects that reference that child. Notify any
        observers."""
        dels = []
        for key, val in self._pub.items():
            if IVariable.providedBy(val) and val.ref_name == name:
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
        within this container instance. If either type of tuple is supplied,
        this function attempts to locate an object with an IVariable interface 
        that can wrap each object named in the tuple.
        
        Returns a list of objects added to the public area.
        """            
# pylint: disable-msg=R0912
        pubs = []
        if isinstance(obj_info, list):
            lst = obj_info
        else:
            lst = [obj_info]
        
        for entry in lst:
            need_wrapper = True
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
                    if IContainer.providedBy(dobj):
                        need_wrapper = False
                else:
                    self.raise_exception(
                     'cannot make %s a public framework object' % \
                      str(entry), TypeError)
                    
            if need_wrapper and not IVariable.providedBy(dobj):
                dobj = make_variable_wrapper(typ, name, self, iostatus=iostat, 
                                      ref_name=ref_name)
            
            if IContainer.providedBy(dobj):
                dobj.parent = self
                self._pub[dobj.name] = dobj
                pubs.append(dobj)
            else:
                self.raise_exception(
                    'no IVariable interface available for the object named '+
                    str(name), TypeError)
        return pubs

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
               res_desc=None):
        """Create a new object of the specified type inside of this
        Container.
        
        Returns the new object.        
        """
        obj = fmcreate(type_name, name, version, server, res_desc)
        self.add_child(obj)
        return obj

    def get(self, path, index=None):
        """Return any public object specified by the given 
        path, which may contain '.' characters.  
        
        Returns the value specified by the name. This will either be the value
        of a Variable or some attribute of a Variable.
        
        """
        assert(path is None or isinstance(path, basestring))
        
        if path is None:
            if index is None:
                return self
            else:
                self.raise_exception('%s is not a Variable. Cannot retrieve index %s'%
                                     (self.get_pathname(),str(index)), AttributeError)
        
        try:
            base, name = path.split('.', 1)
        except ValueError:
            try:
                if index is None:
                    return self._pub[path].get(None)
                else:
                    return self._pub[path].get_entry(index)
            except KeyError:
                self.raise_exception("object has no attribute '%s'" % path, 
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

    def save_to_egg(self, name=None, version=None, force_relative=True,
                    src_dir=None, src_files=None, dst_dir=None,
                    format=SAVE_CPICKLE, proto=-1, tmp_dir=None):
        """Save state and other files to an egg.

        - `name` defaults to the name of the container.
        - `version` defaults to the container's module __version__.
        - If `force_relative` is True, all paths are relative to `src_dir`.
        - `src_dir` is the root of all (relative) `src_files`.
        - `dst_dir` is the directory to write the egg in.
        - `tmp_dir` is the directory to use for temporary files.

        The resulting egg can be unpacked on UNIX via 'sh egg-file'.
        Returns the egg's filename.

        NOTE: References to types defined in module __main__ can't be saved.
              Also, references to old-style class types can't be restored
              correctly.  These issues are typically related to the Variable
              var_types attribute.
        """
        if name is None:
            name = self.name
        if version is None:
            try:
                version = sys.modules[self.__module__].__version__
            except AttributeError:
                pass
        parent = self.parent
        self.parent = None
        try:
            return openmdao.util.save_load.save_to_egg(self, name, version,
                                                       src_dir, src_files,
                                                       dst_dir, format, proto,
                                                       tmp_dir, self._logger)
        except Exception, exc:
            self.raise_exception(str(exc), type(exc))
        finally:
            self.parent = parent

    def save(self, outstream, format=SAVE_CPICKLE, proto=-1):
        """Save the state of this object and its children to the given
        output stream. Pure Python classes generally won't need to
        override this because the base class version will suffice, but
        Python extension classes will have to override. The format
        can be supplied in case something other than cPickle is needed."""
        parent = self.parent
        self.parent = None
        try:
            openmdao.util.save_load.save(self, outstream, format, proto,
                                         self._logger)
        except Exception, exc:
            self.raise_exception(str(exc), type(exc))
        finally:
            self.parent = parent
    
    @staticmethod
    def load_from_egg(filename, install=True):
        """Load state and other files from an egg, returns top object."""
        return openmdao.util.save_load.load_from_egg(filename, install, logger)

    @staticmethod
    def load(instream, format=SAVE_CPICKLE, do_post_load=True):
        """Load object(s) from the input stream. Pure python 
        classes generally won't need to override this, but extensions will. 
        The format can be supplied in case something other than cPickle is 
        needed."""
        top = openmdao.util.save_load.load(instream, format, logger)
        if do_post_load:
            top.post_load()
        return top

    def post_load(self):
        """Perform any required operations after model has been loaded."""
        [x.post_load() for x in self.values(pub=False) 
                                          if isinstance(x,Container)]

    def pre_delete(self):
        """Perform any required operations before the model is deleted."""
        [x.pre_delete() for x in self.values(pub=False) 
                                          if isinstance(x,Container)]

    def _get_pub_items(self, recurse=False):
        """Generate a list of tuples of the form (rel_pathname, obj) for each
        child of this Container. Only iterate through the public
        dict of any Container. If recurse is True, also iterate through all
        public child Containers of each Container found.
        """
        for name, obj in self._pub.items():
            yield (name, obj)
            if recurse:
                cont = None
                if hasattr(obj, 'get_value') and \
                   isinstance(obj.get_value(), Container):
                    cont = obj.get_value()
                elif isinstance(obj, Container):
                    cont = obj
                if cont:
                    for chname, child in cont._get_pub_items(recurse):
                        yield ('.'.join([name, chname]), child)
                   
    def _get_all_items(self, visited, recurse=False):
        """Generate a list of tuples of the form (rel_pathname, obj) for each
        child of this Container.  If recurse is True, also iterate through all
        child Containers of each Container found.
        """
        for name, obj in self.__dict__.items():
            if not name.startswith('_') and id(obj) not in visited:
                visited.add(id(obj))
                yield (name, obj)
                if recurse and isinstance(obj, Container):
                    for chname, child in obj._get_all_items(visited, recurse):
                        yield ('.'.join([name, chname]), child)
                   

    def get_io_graph(self):
        """Return a graph connecting our input variables to our output variables.
        In the case of a simple Container, all input variables are predecessors to
        the Container, and all output variables are successors to the Container.
        """
        if self._io_graph is None:
            self._io_graph = nx.LabeledDiGraph()
            io_graph = self._io_graph
            varlist = [x for x in self._pub.values() if isinstance(x, Variable)]
            ins = ['.'.join([self.name, x.name]) for x in varlist if x.iostatus == INPUT]
            outs = ['.'.join([self.name, x.name]) for x in varlist if x.iostatus == OUTPUT]
            
            # add a node for the component
            io_graph.add_node(self.name, data=self)
            
            # add nodes for all of the variables
            for var in varlist:
                io_graph.add_node('%s.%s' % (self.name, var.name), data=var)
            
            # specify edges, with all inputs as predecessors to the component node,
            # and all outputs as successors to the component node
            io_graph.add_edges_from([(i, self.name) for i in ins])
            io_graph.add_edges_from([(self.name, o) for o in outs])
        return self._io_graph
    
