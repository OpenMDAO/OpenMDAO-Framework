
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

import datetime
import modulefinder
import os
import pkg_resources
import platform
import shutil
import site
import subprocess
import sys
import tempfile
import zc.buildout.easy_install
import zipfile
import traceback
import re

import weakref
# the following is a monkey-patch to correct a problem with
# copying/deepcopying weakrefs There is an issue in the python issue tracker
# regarding this, but it isn't fixed yet.

# pylint: disable-msg=W0212
copy._copy_dispatch[weakref.ref] = copy._copy_immutable  
copy._deepcopy_dispatch[weakref.ref] = copy._deepcopy_atomic
copy._deepcopy_dispatch[weakref.KeyedRef] = copy._deepcopy_atomic
# pylint: enable-msg=W0212

import networkx as nx
from enthought.traits.api import HasTraits, implements, Str, Missing, TraitError,\
                                 BaseStr, Undefined, push_exception_handler,\
                                 on_trait_change, WeakRef, TraitType
from enthought.traits.trait_handlers import NoDefaultSpecified
from enthought.traits.has_traits import _SimpleTest, FunctionType
from enthought.traits.trait_base import not_event

from openmdao.main.log import Logger, logger, LOG_DEBUG
from openmdao.main.interfaces import IContainer
from openmdao.main.factorymanager import create as fmcreate
from openmdao.main.constants import SAVE_YAML, SAVE_LIBYAML
from openmdao.main.constants import SAVE_PICKLE, SAVE_CPICKLE
from openmdao.main.unitsfloat import convert_units, UnitsFloat

# FIXME - shouldn't have a hard wired URL in the code
EGG_SERVER_URL = 'http://torpedo.grc.nasa.gov:31001'

# regex to check for valid names.  Added '.' as allowed because
# npsscomponent uses it...
_namecheck_rgx = re.compile('([_a-zA-Z][_a-zA-Z0-9]*)+(\.[_a-zA-Z][_a-zA-Z0-9]*)*')

# this causes any exceptions occurring in trait handlers to be re-raised. Without
# this, the default behavior is for the exception to be logged and not re-raised.
push_exception_handler(handler = lambda o,t,ov,nv: None,
                       reraise_exceptions = True,
                       main = True,
                       locked = True )

# _io_trait_changed won't do anything unless this is True
_io_side_effects = True

class IMHolder(object):
    """Holds an instancemethod object in a pickleable form."""

    def __init__(self, obj):
        self.name = obj.__name__
        self.im_self = obj.im_self
        if obj.im_self:
            self.im_class = None  # Avoid possible __main__ issues.
        else:
            # TODO: handle __main__ for im_class.__module__.
            self.im_class = obj.im_class

    def method(self):
        """Return instancemethod corresponding to saved state."""
        if self.im_self:
            return getattr(self.im_self, self.name)
        else:
            return getattr(self.im_class, self.name)


#class ContainerProperty(TraitType):
    #"""A trait that allows attributes in child Containers to be referenced
    #using an alias in a parent scope.
    #"""
    #def __init__ ( self, default_value = NoDefaultSpecified, **metadata ):
        #if not metadata.get('ref_name'):
            #raise TraitError("ContainerProperty constructor requires a 'ref_name' argument.")
        #super(ContainerProperty, self).__init__(default_value, **metadata)

    #def get(self, object, name):
        #return object.get(self.ref_name)

    #def set(self, object, name, value):
        #if self.iostatus == 'out':
            #raise TraitError('%s is an output trait and cannot be set' % name)
        
        #if self.trait is not None:
            #self.trait.validate(object, name, value)
            
        #object.set(self.ref_name, value)        

        
class ContainerName(BaseStr):
    """A string that must match the allowed regex for the name of a 
    Container.  This was necessary because the String class allowed 
    names with spaces when using the same regex.
    """
    
    def __init__(self, **metadata):
        super(ContainerName, self).__init__(**metadata)

    def validate(self, object, name, value):
        if value == '' or value == None:
            return value
        
        s = super(ContainerName, self).validate(object, name, value) # normal string validation
        m = _namecheck_rgx.search(s)
        if m is None or m.group() != s:
            raise TraitError("name '%s' contains illegal characters" % s)
        return s            
    

class Container(HasTraits):
    """ Base class for all objects having Traits that are visible 
    to the framework"""
   
    implements(IContainer)
    
    name = ContainerName()
    #parent = WeakRef(IContainer, allow_none=True, adapt='no')
    
    def __init__(self, name='', parent=None, doc=None, add_to_parent=True):
        super(Container, self).__init__() # don't forget to init HasTraits
                                          # or @on_trait_change decorator won't work!
        self._valid_dict = {}  # contains validity flag for each io Trait
        self._dests = {}  # for checking that destination traits cannot be 
                          # set by other objects
        self._added_traits = {}  # for keeping track of dynamically added traits for serialization
                          
        self.parent = parent
        self.name = name
        
        if doc is not None:
            self.__doc__ = doc

        # Replace pathname to keep loggers from interfering with each other.
        self._logger = Logger(self.get_pathname().replace('.', ','))
        self.log_level = LOG_DEBUG

        self._io_graph = None
        if parent is not None and name != '' and \
           isinstance(parent, Container) and add_to_parent:
            parent.add_child(self)
            
    #
    #  HasTraits overrides
    #
    
    def __getstate__(self):
        """Return dict representing this container's state."""
        state = super(Container, self).__getstate__()
        dct = {}
        for name,trait in state['_added_traits'].items():
            if trait.transient is not True:
                dct[name] = trait
        state['_added_traits'] = dct
        return state

    def __setstate__(self, state):
        """Restore this component's state."""
        super(Container, self).__setstate__({})
        self.__dict__.update(state)
        for name,trait in self._added_traits.items():
            self.add_trait(name, trait)

    def add_trait(self, name, *trait):
        """Overrides HasTraits definition of add_trait in order to
        keep track of dynamically added traits for serialization.
        """
        if len( trait ) == 0:
            raise ValueError, 'No trait definition was specified.'
        elif len(trait) > 1:
            trait = Trait(*trait)
        else:
            trait = trait[0]
            
        self._added_traits[name] = trait
        super(Container, self).add_trait(name, trait)
        
    def remove_trait(self, name):
        """Overrides HasTraits definition of remove_trait in order to
        keep track of dynamically added traits for serialization.
        """
        del self._added_traits[name]
        super(Container, self).remove_trait(name)
            
    def trait_get(self, *names, **metadata):
        """Override the HasTraits version of this because if we don't,
        HasTraits.__getstate__ won't return our instance traits.
        """
        if len(names) == 0:
            if 'transient' in metadata:
                meta = metadata
            else:
                meta = metadata.copy()
                meta.setdefault('type', not_event)
            names = self._traits_meta_filter(None, **meta).keys()
        return super(Container, self).trait_get(*names, **metadata)
        
        
    # call this if any trait having 'iostatus' metadata is changed    
    @on_trait_change('+iostatus') 
    def _io_trait_changed(self, obj, name, old, new):
        if _io_side_effects:
            # setting old to Undefined is a kludge to bypass the destination check
            # when we call this directly from Assembly as part of setting this attribute
            # from an existing connection.
            if self.trait(name).iostatus == 'in':
                if old is not Undefined and name in self._dests:
                    self.raise_exception(
                        "'%s' is already connected to source '%s' and cannot be directly set"%
                        (name, self._dests[name]), TraitError)
                self._execute_needed = True
            if self.get_valid(name):  # if var is not already invalid
                self.invalidate_deps([name], notify_parent=True)

    def get_valid(self, name):
        def _valid(self, name):
            tup = name.split('.',1)
            if len(tup) > 1:
                return _valid(getattr(self, tup[0]), tup[1])
            else:
                valid = self._valid_dict.get(name, Missing)
                if valid is Missing:
                    if self.trait(name) and self.trait(name).iostatus:
                        self._valid_dict[name] = False
                        return False
                    else:
                        self.raise_exception("cannot set valid flag of '%s' because it's not an io trait."%
                                             name, RuntimeError)
                else:
                    return valid
            
        if isinstance(name, basestring): 
            return _valid(self, name)
        else:
            return [_valid(self,v) for v in name]
    
    def set_valid(self, name, valid):
        if name in self._valid_dict:
            self._valid_dict[name] = valid
        else:
            trait = self.trait(name)
            if trait and trait.iostatus:
                self._valid_dict[name] = valid
            else:
                self.raise_exception("cannot set valid flag of '%s' because it's not an io trait."%
                                     name, RuntimeError)

    def add_child(self, obj):
        """Add a Container object to this Container, and make it a member of 
        this Container's public interface.
        """
        if obj == self:
            self.raise_exception('cannot make an object a child of itself',
                                 RuntimeError)
        if isinstance(obj, Container):
            # if an old child with that name exists, remove it
            if self.contains(obj.name):
                self.remove_child(obj.name)
            setattr(self, obj.name, obj)
            obj.parent = self
        else:
            self.raise_exception("'"+str(type(obj))+
                    "' object has does not provide the IContainer interface",
                    TypeError)
        return obj
        
    def remove_child(self, name, delete=True):
        """Remove the specified child from this container and remove any
        public Variable objects that reference that child. Notify any
        observers."""
        trait = self.trait(name)
        if trait is not None:
            self.remove_trait(name)
        else:
            self.raise_exception("cannot remove child '%s': not found"%
                                 name, TraitError)
    
    def unit_convert(self, name, units):
        desttrait = self.trait(name)
        if desttrait is None:
            self.raise_exception("attribute '%s' not found"%name,
                                 TraitError)
        destunits = desttrait.units
        if destunits is None:
            raise self.raise_exception("'%s' has no units" % name,
                                       TraitError)
        else:
            return convert_units(getattr(self, name), destunits, units)
    
    def items(self, recurse=False, **metadata):
        """Return a list of tuples of the form (rel_pathname, obj) 
        for each trait of this Container that matches
        the given metadata. If recurse is True, also iterate through all
        child Containers of each Container found.
        """
        mdata = { 'type': not_event } # exclude event traits, which are write-only
        mdata.update(metadata)
        return self._items(set([id(self.parent)]), recurse, **mdata)
        
    def keys(self, recurse=False, **metadata):
        """Return a list of the relative pathnames of
        children of this Container that match the given metadata. If recurse is 
        True, child Containers will also be iterated over.
        """
        mdata = { 'type': not_event } # exclude event traits, which are write-only
        mdata.update(metadata)
        return [tup[0] for tup in self._items(set([id(self.parent)]), 
                                              recurse, **mdata)]
        
    def values(self, recurse=False, **metadata):
        """Return a list of children of this Container that have matching 
        trait metadata. If recurse is True, child Containers will also be 
        iterated over.
        """
        mdata = { 'type': not_event } # exclude event traits, which are write-only
        mdata.update(metadata)
        return [tup[1] for tup in self._items(set([id(self.parent)]), 
                                              recurse, **mdata)]

    def get_all_traits(self):
        """Returns a dict containing all traits for this object, including
        instance traits. This function was written because property traits
        (TraitTypes with set/get defined) didn't seem to show up using
        the normal traits() function.
        """
        traits = self.__base_traits__.copy()
        instset = set(self.__dict__).union(self._instance_traits())
        
        # base traits are already included, so exclude them from loop
        for name in instset.difference(traits):  
            trait = self.trait( name )
            if trait is not None:
                traits[ name ] = trait
        return traits
    
    def _traits_meta_filter(self, traits=None, **metadata):
        """This returns a dict that contains all entries in the traits dict
        that match the given metadata.
        """
        if traits is None:
            traits = self.get_all_traits()
            
        if len( metadata ) == 0:
            return traits

        for meta_name, meta_eval in metadata.items():
            if type( meta_eval ) is not FunctionType:
                metadata[ meta_name ] = _SimpleTest( meta_eval )

        result = {}
        for name, trait in traits.items():
            for meta_name, meta_eval in metadata.items():
                if not meta_eval( getattr( trait, meta_name ) ):
                    break
            else:
                result[ name ] = trait

        return result
        
        
    def _items(self, visited, recurse=False, **metadata):
        """Return an iterator that returns a list of tuples of the form 
        (rel_pathname, obj) for each trait of this Container that matches
        the given metadata. If recurse is True, also iterate through all
        child Containers of each Container found.
        """
        if id(self) not in visited:
            visited.add(id(self))
            match_dict = self._traits_meta_filter(**metadata)
            
            if recurse:
                for name, obj in self.__dict__.items():
                    if isinstance(obj, Container) and id(obj) not in visited:
                        if name in match_dict:
                            yield(name, obj)
                        for chname, child in obj._items(visited, recurse, **metadata):
                            yield ('.'.join([name, chname]), child)
                            
            for name, trait in match_dict.items():
                obj = getattr(self, name)
                if id(obj) not in visited:
                    if isinstance(obj, Container):
                        if not recurse:
                            yield (name, obj)
                    elif trait.iostatus is not None:
                        yield (name, obj)

    
    def get_pathname(self, rel_to_scope=None):
        """ Return full path name to this container, relative to scope
        rel_to_scope. If rel_to_scope is None, return the full pathname.
        """
        path = []
        obj = self
        while obj is not None and obj != rel_to_scope and obj.name:
            path.append(obj.name)
            obj = obj.parent
        if len(path) > 0:
            return '.'.join(path[::-1])
        else:
            return ''
        
    def get_trait_pathname(self, traitname, rel_to_scope=None):
        return '.'.join([self.get_pathname(rel_to_scope=rel_to_scope),traitname])
    
    def contains(self, path):
        """Return True if the child specified by the given dotted path
        name is publicly accessibly and is contained in this Container. 
        """
        tup = path.split('.', 1)
        if len(tup) == 1:
            return getattr(self, path, Missing) is not Missing
        
        obj = getattr(self, tup[0], Missing)
        if obj is not Missing:
            if isinstance(obj, Container):
                return obj.contains(tup[1])
            else:
                return getattr(obj, tup[1], Missing) is not Missing
        return False
    
    def create(self, type_name, name, version=None, server=None, 
               res_desc=None):
        """Create a new object of the specified type inside of this
        Container.
        
        Returns the new object.        
        """
        obj = fmcreate(type_name, name, version, server, res_desc)
        obj.parent = self
        return obj

    def invoke(self, path, *args, **kwargs):
        if path is None:
            return self.__call__(*args, **kwargs)
        else:
            tup = path.split('.')
            if len(tup) == 1:
                return getattr(self, path)(*args, **kwargs)
            else:
                obj = getattr(self, tup[0], Missing)
                if obj is Missing:
                    self.raise_exception("object has no attribute '%s'" % tup[0], 
                                         AttributeError)
                if len(tup) == 2:
                    return getattr(obj, tup[1])(*args, **kwargs)
                else:
                    return obj.invoke('.'.join(tup[1:]), *args, **kwargs)
        
        
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
                self.raise_exception(
                    'Cannot retrieve items from Container %s using array notation.'%
                    self.get_pathname(), AttributeError)
        
        tup = path.split('.')
        if len(tup) == 1:
            if index is None:
                obj = getattr(self, path, Missing)
                if obj is Missing:
                    self.raise_exception("object has no attribute '%s'" % path, 
                                         AttributeError)
                return obj
            else:
                return self._array_get(path, index)
        else:
            obj = getattr(self, tup[0], Missing)
            if obj is Missing:
                self.raise_exception("object has no attribute '%s'" % tup[0], 
                                     AttributeError)
            if len(tup) == 2 and index is None:
                return getattr(obj, tup[1])
            
            if isinstance(obj, Container):
                return obj.get('.'.join(tup[1:]), index)
            elif index is None:
                return getattr(obj, '.'.join(tup[1:]))
            else:
                return obj._array_get('.'.join(tup[1:]), index)

     
    def add_destination(self, name, source):
        """Mark an io trait as a destination, which will prevent it from being
        set directly or connected to another source."""
        if name in self._dests:
            self.raise_exception("'%s' is already connected to source '%s'" % 
                                 (name, self._dests[name]), TraitError)
        self._dests[name] = source   
            
    def remove_destination(self, name):
        del self._dests[name]    
            
    def _check_trait_settable(self, name, srcname=None, force=False):
        if force:
            src = None
        else:
            src = self._dests.get(name)
        trait = self.trait(name)
        if not trait:
            self.raise_exception("object has no attribute '%s'" % name,
                                 TraitError)
        if trait.iostatus != 'in' and src is not None and src != srcname:
            self.raise_exception("'%s' is not an input trait and cannot be set" %
                                 name, TraitError)
            
        if src is not None and src != srcname:
            self.raise_exception(
                "'%s' is connected to source '%s' and cannot be set by source '%s'"%
                (name,src,srcname), TraitError)
                    
    def set(self, path, value, index=None, srcname=None, srcmeta=None, force=False):
        """Set the value of the data object specified by the  given path, which
        may contain '.' characters.  If path specifies a Variable, then its
        value attribute will be set to the given value, subject to validation
        and  constraints. index, if not None, should be a list of ints, at
        most one for each array dimension of the target value.       
        """ 
        assert(isinstance(path, basestring))
        
        if path is None:
            if index is None:
                # should never get down this far
                self.raise_exception('this object cannot replace itself')
            else:
                self.raise_exception(
                    'Cannot set value at index %s'%
                    str(index), AttributeError)
                    
        tup = path.split('.')
        if len(tup) == 1:
            self._check_trait_settable(path, srcname, force)
            if index is None:
                if self.trait(path) is None:
                    self.raise_exception("object has no attribute '%s'" %
                                         path, TraitError)
                # bypass the callback here and call it manually after 
                # with a flag to tell it not to check if it's a destination
                self._trait_change_notify(False)
                try:
                    if srcmeta is None or len(srcmeta) == 0:
                        setattr(self, path, value)
                    else:
                        val = getattr(self.trait(path).trait_type,
                                      'validate_with_metadata')(self, 
                                                                path, value,
                                                                srcmeta)
                        self.__dict__[path] = val # avoid repeat validation
                finally:
                    self._trait_change_notify(True)
                # now manually call the notifier with old set to Undefined
                # to avoid the destination check
                self._io_trait_changed(self, path, Undefined, getattr(self, path))
            else:
                self._array_set(path, value, index)
        else:
            obj = getattr(self, tup[0], Missing)
            if obj is Missing:
                self.raise_exception("object has no attribute '%s'" % tup[0], 
                                     TraitError)
            if len(tup) == 2:
                if isinstance(obj, Container):
                    obj.set(tup[1], value, index, 
                            srcname=srcname, srcmeta=srcmeta, force=force)
                elif index is None:
                    setattr(obj, tup[1], value)
                else:
                    obj._array_set(tup[1], value, index)
            else:
                if isinstance(obj, Container):
                    obj.set('.'.join(tup[1:]), value, index, 
                            srcmeta=srcmeta, force=force)
                else:
                    obj._array_set('.'.join(tup[1:]), value, index)

    def _array_set(self, name, value, index):
        arr = getattr(self, name)
        
        length = len(index)
        if length == 1:
            old = arr[index[0]]
            arr[index[0]] = value
        elif length == 2:
            old = arr[index[0]][index[1]]
            arr[index[0]][index[1]] = value
        elif length == 3:
            old = arr[index[0]][index[1]][index[2]]
            arr[index[0]][index[1]][index[2]] = value
        else:
            for idx in index[:-1]:
                arr = arr[idx]
            old = arr[index[length-1]]
            arr[index[length-1]] = value
                
        # setting of individual Array values doesn't seem to trigger
        # _io_trait_changed, so do it manually
        if old != value:
            self._io_trait_changed(self, name, arr, arr)
            
    def _array_get(self, name, index):
        arr = getattr(self, name)
        length = len(index)
        if length == 1:
            return arr[index[0]]
        elif length == 2:
            return arr[index[0]][index[1]]
        elif length == 3:
            return arr[index[0]][index[1]][index[2]]
        else:
            for idx in index:
                arr = arr[idx]
            return arr
    
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
        orig_dir = os.getcwd()
        if src_dir and not os.path.isabs(src_dir):
            src_dir = os.path.abspath(src_dir)

        if name is None:
            name = self.name
        if version is None:
            try:
                version = sys.modules[self.__class__.__module__].__version__
            except (AttributeError, KeyError):
                now = datetime.datetime.now()  # Could consider using utcnow().
                version = '%d.%02d.%02d.%02d.%02d' % \
                          (now.year, now.month, now.day, now.hour, now.minute)
        if dst_dir is None:
            dst_dir = orig_dir
        if not os.access(dst_dir, os.W_OK):
            self.raise_exception("Can't save to '%s', no write permission" %
                                 dst_dir, IOError)

        py_version = platform.python_version_tuple()
        py_version = '%s.%s' % (py_version[0], py_version[1])
        egg_name = '%s-%s-py%s.egg' % (name, version, py_version)
        self.debug('Saving to %s in %s...', egg_name, orig_dir)

        buildout = 'buildout.cfg'
        buildout_path = os.path.join(name, buildout)
        buildout_orig = buildout_path+'-orig'

        tmp_dir = None

        # Get a list of all objects we'll be saving.
        objs = self._get_objects()
        objs.append(self)

        # Check that each object can be pickled.
        errors = 0
        for obj in objs:
            if obj.__class__.__name__ == 'function':
                self.error("Can't save, can't pickle function %s.%s",
                           obj.__module__, obj.__name__)
                errors += 1
            # Hopefully all instancemethods are handled by HierarchyMember.
            elif obj.__class__.__name__ == 'instancemethod':
                self.error("Can't save, can't pickle instancemethod %s.%s.%s",
                           obj.im_class.__module__,
                           obj.im_class.__name__,
                           obj.__name__)
                errors += 1
            #else: # (Debug) actually try to pickle.
                #try:
                    #cPickle.dumps(obj)
                #except Exception, exc:
                    #self.error("Can't pickle obj %r %s",
                               #obj, obj.__class__.__name__)
                #errors += 1
        if errors:
            self.raise_exception("Can't save, %d objects cannot be pickled."
                                 % errors, RuntimeError)

        # Fixup objects, classes, & sys.modules for __main__ imports.
        fixup = self._fix_objects(objs)
        try:
            # Determine distributions and local modules required.
            required_distributions, local_modules = \
                self._get_distributions(objs)

            # Move to scratch area.
            tmp_dir = tempfile.mkdtemp(prefix='Egg_', dir=tmp_dir)
            os.chdir(tmp_dir)

            if src_dir:
                if sys.platform == 'win32':
                    # Copy original directory to object name.
                    shutil.copytree(src_dir, name)
                else:
                    # Just link original directory to object name.
                    os.symlink(src_dir, name)
            else:
                os.mkdir(name)

            # If orig_dir isn't src_dir, copy local modules from orig_dir.
            if orig_dir != src_dir:
                for path in local_modules:
                    path = orig_dir+os.sep+path
#                    self.debug('    copy %s -> %s', path, name)
                    shutil.copy(path, name)

            # Save state of object hierarchy.
            if format is SAVE_CPICKLE or format is SAVE_PICKLE:
                state_name = name+'.pickle'
            elif format is SAVE_LIBYAML or format is SAVE_YAML:
                state_name = name+'.yaml'
            else:
                self.raise_exception("Unknown format '%s'." % format,
                                     RuntimeError)

            state_path = os.path.join(name, state_name)
            try:
                self.save(state_path, format, proto)
            except Exception, exc:
                if os.path.exists(state_path):
                    os.remove(state_path)
                self.raise_exception("Can't save to '%s': %s" %
                                     (state_path, exc), type(exc))

            # Add state file to set of files to save.
            if src_files is None:
                src_files = set()
            src_files.add(state_name)

            # Create buildout.cfg
            if os.path.exists(buildout_path):
                os.rename(buildout_path, buildout_orig)
            self.debug('writing file %s...' % buildout_path)
            out = open(buildout_path, 'w')
            out.write("""\
[buildout]
parts = %(name)s
index = %(server)s
unzip = true

[%(name)s]
recipe = zc.recipe.egg:scripts
interpreter = python
eggs =
""" % {'name':name, 'server':EGG_SERVER_URL})
            for dist in required_distributions:
                out.write("    %s == %s\n" % (dist.project_name, dist.version))
            out.close()
            src_files.add(buildout)

            # If needed, make an empty __init__.py
            init_path = os.path.join(name, '__init__.py')
            if not os.path.exists(init_path):
                remove_init = True
                out = open(init_path, 'w')
                out.close()
            else:
                remove_init = False

            # Create loader script.
            loader = '%s_loader' % name
            loader_path = os.path.join(name, loader+'.py')
            self._write_loader_script(loader_path, state_name)

            # Save everything to an egg via setuptools.
            self._write_egg_via_setuptools(name, version, loader, src_files,
                                           required_distributions, dst_dir)
            self.debug('removing %s and %s' % (state_path,loader_path))
            os.remove(state_path)
            os.remove(loader_path)
            if remove_init:
                self.debug('removing %s' % init_path)
                os.remove(init_path)

        finally:
            if os.path.exists(buildout_orig):
                os.rename(buildout_orig, buildout_path)
            elif os.path.exists(buildout_path):
                self.debug('removing %s' % buildout_path)
                os.remove(buildout_path)
            os.chdir(orig_dir)
            if tmp_dir:
                self.debug('removing %s' % tmp_dir)
                shutil.rmtree(tmp_dir)
            self._restore_objects(fixup)

        return egg_name

    def _get_objects(self):
        """Get objects to be saved."""

        def _recurse_get_objects(parent_obj, objs, visited):
            """Use __getstate__(), or scan __dict__, or scan container."""
            try:
                state = parent_obj.__getstate__()
            except AttributeError:
                try:
                    state = parent_obj.__dict__
                except AttributeError:
                    if isinstance(parent_obj, dict) or \
                       isinstance(parent_obj, list) or \
                       isinstance(parent_obj, set)  or \
                       isinstance(parent_obj, tuple):
                        state = parent_obj
                    else:
                        return  # Some non-container primitive.
            except Exception, exc:
                self.error("During save_to_egg, _get_objects error %s: %s",
                           type(parent_obj), exc)
                return

            if isinstance(state, dict):
                state = state.values()

            for obj in state:
                if obj.__class__.__name__ == 'instancemethod':
                    self.error("Can't save, can't pickle instancemethod %s.%s.%s",
                               obj.im_class.__module__,
                               obj.im_class.__name__,
                               obj.__name__)
                if id(obj) in visited:
                    continue
                visited.add(id(obj))
                objs.append(obj)
                _recurse_get_objects(obj, objs, visited)

        objs = []
#        visited = [id(self._parent)]  # Don't include our parent.
        visited = set([id(self.parent)])  # Don't include our parent.
        _recurse_get_objects(self, objs, visited)
        return objs

    def _fix_objects(self, objs):
        """Fixup objects, classes, & sys.modules for __main__ imports."""
        fixup_objects = []
        fixup_classes = {}
        fixup_modules = set()

        for obj in objs:
            classname = obj.__class__.__name__
            if classname == 'instancemethod':
                obj = obj.im_self
            try:
                mod = obj.__module__
            except AttributeError:
                continue

            if mod == '__main__':
                if classname in ('function', 'type'):
                    self.raise_exception("Can't save: reference to %s defined "
                                         "in main module %r" % (classname, obj),
                                         RuntimeError)

                mod = obj.__class__.__module__
                if mod == '__main__' and \
                   (classname not in fixup_classes.keys()):
                    # Need to determine 'real' module name.
                    if '.' not in sys.path:
                        sys.path.append('.')
                    for arg in sys.argv:
                        if arg.endswith('.py'):
                            mod = os.path.basename(arg)[:-3]
                            try:
                                module = __import__(mod, fromlist=[classname])
                            except ImportError:
                                pass
                            else:
                                old = obj.__class__
                                new = getattr(module, classname)
                                fixup_classes[classname] = (old, new)
                                fixup_modules.add(mod)
                                break
                    else:
                        self._restore_objects((fixup_objects, fixup_classes,
                                               fixup_modules))
                        self.raise_exception("Can't find module for '%s'" %
                                             classname, RuntimeError)

                try:
                    obj.__class__ = fixup_classes[classname][1]
                except KeyError:
                    self.raise_exception("Can't fix %r, classname %s, module %s"
                                         % (obj, classname, mod), RuntimeError)
                    
                obj.__module__ = obj.__class__.__module__
                fixup_objects.append(obj)

        return (fixup_objects, fixup_classes, fixup_modules)

    def _restore_objects(self, fixup):
        """Restore objects, classes, & sys.modules for __main__ imports."""
        fixup_objects, fixup_classes, fixup_modules = fixup

        for obj in fixup_objects:
            obj.__module__ = '__main__'
            classname = obj.__class__.__name__
            obj.__class__ = fixup_classes[classname][0]

        for classname in fixup_classes.keys():
            new = fixup_classes[classname][1]
            del new

        for mod in fixup_modules:
            del sys.modules[mod]

    def _get_distributions(self, objs):
        """Return (distributions, local_modules) used by objs."""
#        working_set = pkg_resources.WorkingSet()
#        for dist in working_set:
#            self.debug('    %s %s', dist.project_name, dist.version)
#            self.debug('        %s', dist.location)
        distributions = set()
        local_modules = set()
        modules = ['__builtin__']
        prefixes = []
        site_lib = os.path.dirname(site.__file__)
        site_pkg = site_lib+os.sep+'site-packages'

        # For each object found...
        for obj in objs:
            try:
                name = obj.__module__
                if name is None:
                    continue
            except AttributeError:
                continue
#            self.debug('    obj module %s', name)
            if name in modules:
#                self.debug('        already known')
                continue
            modules.append(name)

            # Skip modules in distributions we already know about.
            path = sys.modules[name].__file__
            if path.startswith(site_lib) and not path.startswith(site_pkg):
#                self.debug("    skipping(1) '%s'", path)
                continue
            found = False
            for prefix in prefixes:
                if path.startswith(prefix):
                    found = True
#                    self.debug("    skipping(2) '%s'", path)
                    break
            if found:
                continue

            egg = path.find('.egg')
            if egg > 0:
                # We'll assume the egg has accurate dependencies.
                path = path[:egg+4]
                self._process_egg(path, distributions, prefixes)
            else:
                # Use module finder to get the modules this object requires.
                if path.endswith('.pyc') or path.endswith('.pyo'):
                    path = path[:-1]
                if not os.path.exists(path):
                    self.warning("    module path '%s' does not exist", path)
                    continue
                self.debug("    analyzing '%s'...", path)
                finder = modulefinder.ModuleFinder()
                try:
                    finder.run_script(path)
                except Exception:
                    self.exception("ModuleFinder for '%s'" % path)
                else:
                    self._process_found_modules(finder, modules,
                                                distributions,
                                                prefixes, local_modules)

        distributions = sorted(distributions,
                               key=lambda dist: dist.project_name)
        self.debug('    required distributions:')
        for dist in distributions:
            self.debug('        %s %s', dist.project_name, dist.version)
        return (distributions, local_modules)

    def _process_egg(self, path, distributions, prefixes):
        """Update distributions and prefixes based on egg data."""
        self.debug("    processing '%s'", path)
        dist = pkg_resources.Distribution.from_filename(path)
        distributions.add(dist)
        prefixes.append(path)

        for req in dist.requires():
            self.debug("    requires '%s'", req)
            dep = pkg_resources.get_distribution(req)
            distributions.add(dep)
            loc = dep.location
            if loc.endswith('.egg') and loc not in prefixes:
                prefixes.append(loc)

    def _process_found_modules(self, finder, modules, distributions, prefixes,
                               local_modules):
        """Use ModuleFinder data to update distributions and local_modules."""
        working_set = pkg_resources.WorkingSet()
        site_lib = os.path.dirname(site.__file__)
        site_pkg = site_lib+os.sep+'site-packages'
        py_version = 'python%d.%d' % (sys.version_info[0], sys.version_info[1])
        cwd = os.getcwd()
        not_found = set()

        for name, module in sorted(finder.modules.items(),
                                   key=lambda item: item[0]):
#            self.debug('    found %s', name)
            if name in modules:
#                self.debug('        already known')
                continue
            if name != '__main__':
                modules.append(name)
            try:
                path = module.__file__
            except AttributeError:
#                self.debug('        no __file__')
                continue
            if not path:
#                self.debug('        null __file__')
                continue

            # Skip modules in distributions we already know about.
            if path.startswith(site_lib) and not path.startswith(site_pkg):
#                self.debug("        skipping(1) '%s'", path)
                continue
            found = False
            for prefix in prefixes:
                if path.startswith(prefix):
                    found = True
#                    self.log_debug("    skipping(2) '%s'", path)
                    break
            if found:
                continue

            # Record distribution.
            for dist in working_set:
                loc = dist.location
                # Protect against a 'bare' location.
                if loc.endswith('site-packages') or loc.endswith(py_version):
                    loc += os.sep+dist.project_name
                if path.startswith(loc):
                    distributions.add(dist)
                    if loc.endswith('.egg'):
                        prefixes.append(loc)
#                    self.debug('        adding %s %s',
#                               dist.project_name, dist.version)
                    break
            else:
                if not os.path.isabs(path):
                    # No distribution expected.
                    if os.path.dirname(path) == '.':
                        # May need to be copied later.
                        local_modules.add(path)
                elif path.startswith(cwd):
                    # No distribution expected.
                    if os.path.dirname(path) == cwd:
                        # May need to be copied later.
                        local_modules.add(path)
                else:
                    dirpath = os.path.dirname(path)
                    if dirpath not in not_found:
                        if not dirpath.endswith('site-packages'):
                            not_found.add(dirpath)
                            path = dirpath
                        self.warning('        no distribution found for %s',
                                     path)

    def _write_loader_script(self, path, state_name):
        """Write script used for loading object(s)."""
        out = open(path, 'w')
        out.write("""\
import logging
import os
import sys
if not '.' in sys.path:
    sys.path.append('.')

try:
    from openmdao.main.api import Component
    from openmdao.main.constants import SAVE_CPICKLE, SAVE_LIBYAML
    from openmdao.main.log import enable_console
except ImportError:
    print 'No OpenMDAO distribution available.'
    if __name__ != '__main__':
        print 'You can unzip the egg to access the enclosed files.'
        print 'To get OpenMDAO, please visit openmdao.org'
    sys.exit(1)

def load():
    '''Create object(s) from state file.'''
    state_name = '%s'
    if state_name.endswith('.pickle'):
        return Component.load(state_name, SAVE_CPICKLE)
    elif state_name.endswith('.yaml'):
        return Component.load(state_name, SAVE_LIBYAML)
    raise RuntimeError("State file '%%s' is not a pickle or yaml save file.",
                       state_name)

def eggsecutable():
    '''Unpack egg.'''
    install = os.environ.get('OPENMDAO_INSTALL', '1')
    if install:
        install = int(install)
    debug = os.environ.get('OPENMDAO_INSTALL_DEBUG', '1')
    if debug:
        debug = int(debug)
    if debug:
        enable_console()
        logging.getLogger().setLevel(logging.DEBUG)
    try:
        Component.load_from_egg(sys.path[0], install=install)
    except Exception, exc:
        print str(exc)
        sys.exit(1)

def main():
    '''Load state and run.'''
    model = load()
    model.run()

if __name__ == '__main__':
    main()
""" % state_name)
        out.close()

    def _write_egg_via_setuptools(self, name, version, loader, src_files,
                                  distributions, dst_dir):
        """Save everything to an egg via setuptools."""
        out = open('setup.py', 'w')

        out.write('import setuptools\n')

        out.write('\npackage_files = [\n')
        for filename in sorted(src_files):
            path = os.path.join(name, filename)
            if not os.path.exists(path):
                self.raise_exception("Can't save, '%s' does not exist" %
                                     path, ValueError)
            out.write("    '%s',\n" % filename)
        out.write(']\n')

        out.write('\nrequirements = [\n')
        for dist in distributions:
            out.write("    '%s == %s',\n" % (dist.project_name, dist.version))
        out.write(']\n')

        if self.__doc__ is None:
            doc = ''
        else:
            doc = self.__doc__.strip()
        out.write("""
entry_points = {
    'openmdao.top' : [
        'top = %(loader)s:load',
    ],
    'openmdao.components' : [
        '%(name)s = %(loader)s:load',
    ],
    'setuptools.installation' : [
        'eggsecutable = %(name)s.%(loader)s:eggsecutable',
    ],
}

setuptools.setup(
    name='%(name)s',
    description='''%(doc)s''',
    version='%(version)s',
    packages=setuptools.find_packages(),
    package_data={'%(name)s' : package_files},
    zip_safe=False,
    install_requires=requirements,
    entry_points=entry_points,
)
""" % {'name':name, 'loader':loader, 'doc':doc, 'version':version})

        out.close()

        # Use environment since 'python' might not recognize '-u'.
        env = os.environ
        env['PYTHONUNBUFFERED'] = '1'
        proc = subprocess.Popen(['python', 'setup.py', 'bdist_egg',
                                 '-d', dst_dir], env=env,
                                stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT)
        output = []
        while proc.returncode is None:
            line = proc.stdout.readline()
            if line:
                line = line.rstrip()
                self.debug('    '+line)
                output.append(line)
            proc.poll()
        line = proc.stdout.readline()
        while line:
            line = line.rstrip()
            self.debug('    '+line)
            output.append(line)
            line = proc.stdout.readline()

        if proc.returncode != 0:
            if self.log_level > LOG_DEBUG:
                for line in output:
                    self.error('    '+line)
            self.error('save_to_egg failed due to setup.py error %d:',
                       proc.returncode)
            self.raise_exception('setup.py failed, check log for info.',
                                 RuntimeError)

    def save (self, outstream, format=SAVE_CPICKLE, proto=-1):
        """Save the state of this object and its children to the given
        output stream. Pure Python classes generally won't need to
        override this because the base class version will suffice, but
        Python extension classes will have to override. The format
        can be supplied in case something other than cPickle is needed."""
        if isinstance(outstream, basestring):
            if format is SAVE_CPICKLE or format is SAVE_PICKLE:
                mode = 'wb'
            else:
                mode = 'w'
            try:
                outstream = open(outstream, mode)
            except IOError, exc:
                self.raise_exception("Can't save to '%s': %s" % \
                                     (outstream, exc.strerror), type(exc))

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
    def load_from_egg(filename, install=True):
        """Load state and other files from an egg, returns top object."""
        logger.debug('Loading from %s in %s...', filename, os.getcwd())
        if not os.path.exists(filename):
            raise ValueError("'%s' not found." % filename)

        # Check for a distribution.
        distributions = \
            [dist for dist in pkg_resources.find_distributions(filename,
                                                               only=True)]
        if not distributions:
            raise RuntimeError("No distributions found in '%s'." % filename)

        dist = distributions[0]
        logger.debug('    project_name: %s', dist.project_name)
        logger.debug('    version: %s', dist.version)
        logger.debug('    py_version: %s', dist.py_version)
        logger.debug('    platform: %s', dist.platform)
        logger.debug('    requires:')
        for req in dist.requires():
            logger.debug('        %s', req)
        logger.debug('    entry points:')
        maps = dist.get_entry_map()
        for group in maps.keys():
            logger.debug('        group %s:' % group)
            for name in maps[group]:
                logger.debug('            %s', name)

        # Extract files.
        archive = zipfile.ZipFile(filename, 'r')
        name = archive.read('EGG-INFO/top_level.txt').split('\n')[0]
        logger.debug("    name '%s'", name)

        for info in archive.infolist():
            if not info.filename.startswith(name):
                continue  # EGG-INFO
            if info.filename.endswith('.pyc') or \
               info.filename.endswith('.pyo'):
                continue  # Don't assume compiled OK for this platform.

            logger.debug("    extracting '%s' (%d bytes)...",
                         info.filename, info.file_size)
            dirname = os.path.dirname(info.filename)
            dirname = dirname[len(name)+1:]
            if dirname and not os.path.exists(dirname):
                os.makedirs(dirname)
            path = info.filename[len(name)+1:]
            # TODO: use 2.6 ability to extract to filename.
            out = open(path, 'w')
            out.write(archive.read(info.filename))
            out.close()

        if install:
            # Locate the installation (eggs) directory.
# TODO: fix this hack!
            install_dir = \
                os.path.dirname(
                    os.path.dirname(
                        os.path.dirname(
                            os.path.dirname(zc.buildout.__file__))))
            logger.debug('    installing in %s', install_dir)

            # Grab any distributions we depend on.
            try:
                zc.buildout.easy_install.install(
                    [str(req) for req in dist.requires()], install_dir,
                    index=EGG_SERVER_URL,
                    always_unzip=True
                    )
            except Exception, exc:
                raise RuntimeError("Install failed: '%s'" % exc)

        # Invoke the top object's loader.
        info = dist.get_entry_info('openmdao.top', 'top')
        if info is None:
            raise RuntimeError("No openmdao.top 'top' entry point.")
        if info.module_name in sys.modules.keys():
            logger.debug("    removing existing '%s' in sys.modules",
                         info.module_name)
            del sys.modules[info.module_name]
        if not '.' in sys.path:
            sys.path.append('.')
        try:
            loader = dist.load_entry_point('openmdao.top', 'top')
            return loader()
        except pkg_resources.DistributionNotFound, exc:
            logger.error('Distribution not found: %s', exc)
            visited = []
            Container._check_requirements(dist, visited)
            raise exc
        except pkg_resources.VersionConflict, exc:
            logger.error('Version conflict: %s', exc)
            visited = []
            Container._check_requirements(dist, visited)
            raise exc
        except Exception, exc:
            logger.exception('Loader exception:')
            raise exc

    @staticmethod
    def _check_requirements(dist, visited, level=1):
        """Display requirements and note conflicts."""
        visited.append(dist)
        indent  = '    ' * level
        indent2 = '    ' * (level + 1)
        working_set = pkg_resources.WorkingSet()
        for req in dist.requires():
            logger.debug('%schecking %s', indent, req)
            dist = None
            try:
                dist = working_set.find(req)
            except pkg_resources.VersionConflict:
                dist = working_set.by_key[req.key]
                logger.debug('%sconflicts with %s %s', indent2,
                             dist.project_name, dist.version)
            else:
                if dist is None:
                    logger.debug('%sno distribution found', indent2)
                else: 
                    logger.debug('%s%s %s', indent2,
                                 dist.project_name, dist.version)
                    if not dist in visited:
                        Container._check_requirements(dist, visited, level+1)

    @staticmethod
    def load (instream, format=SAVE_CPICKLE, do_post_load=True):
        """Load object(s) from the input stream. Pure python 
        classes generally won't need to override this, but extensions will. 
        The format can be supplied in case something other than cPickle is 
        needed."""
        if isinstance(instream, basestring):
            if format is SAVE_CPICKLE or format is SAVE_PICKLE:
                mode = 'rb'
            else:
                mode = 'r'
            instream = open(instream, mode)

        _io_side_effects = False
        try:
            if format is SAVE_CPICKLE:
                top = cPickle.load(instream)
            elif format is SAVE_PICKLE:
                top = pickle.load(instream)
            elif format is SAVE_YAML:
                top = yaml.load(instream)
            elif format is SAVE_LIBYAML:
                if _libyaml is False:
                    logger.warn('libyaml not available, using yaml instead')
                top = yaml.load(instream, Loader=Loader)
            else:
                raise RuntimeError('cannot load object using format %s' % format)
    
            if do_post_load:
                top.post_load()
        finally:
            _io_side_effects = True
            
        return top

    def post_load(self):
        """Perform any required operations after model has been loaded."""
        [x.post_load() for x in self.values() 
                                          if isinstance(x,Container)]

    def pre_delete(self):
        """Perform any required operations before the model is deleted."""
        [x.pre_delete() for x in self.values() 
                                          if isinstance(x,Container)]

    def get_io_graph(self):
        """Return a graph connecting our input variables to our output variables.
        In the case of a simple Container, all input variables are predecessors to
        all output variables.
        """
        # NOTE: if the _io_graph changes, this function must return a NEW graph
        # object instead of modifying the old one, because object identity
        # is used in the parent assembly to determine of the graph has changed
        if self._io_graph is None:
            self._io_graph = nx.DiGraph()
            io_graph = self._io_graph
            name = self.name
            ins = ['.'.join([name, v]) for v in self.keys(iostatus='in')]
            outs = ['.'.join([name, v]) for v in self.keys(iostatus='out')]
            
            # add nodes for all of the variables
            io_graph.add_nodes_from(ins)
            io_graph.add_nodes_from(outs)
            
            # specify edges, with all inputs as predecessors to all outputs
            for invar in ins:
                io_graph.add_edges_from([(invar, o) for o in outs])
        return self._io_graph
    
    # error reporting stuff
    def _get_log_level(self):
        """Return logging message level."""
        return self._logger.level

    def _set_log_level(self, level):
        """Set logging message level."""
        self._logger.level = level

    log_level = property(_get_log_level, _set_log_level,
                         doc='Logging message level.')

    def raise_exception(self, msg, exception_class=Exception):
        """Raise an exception."""
        full_msg = '%s: %s' % (self.get_pathname(), msg)
        self._logger.error(msg)
        raise exception_class(full_msg)
    
    def exception(self, msg, *args, **kwargs):
        """Log traceback from within exception handler."""
        self._logger.critical(msg, *args, **kwargs)
        self._logger.critical(traceback.format_exc())

    def error(self, msg, *args, **kwargs):
        """Record an error message."""
        self._logger.error(msg, *args, **kwargs)
        
    def warning(self, msg, *args, **kwargs):
        """Record a warning message."""
        self._logger.warning(msg, *args, **kwargs)
        
    def info(self, msg, *args, **kwargs):
        """Record an informational message."""
        self._logger.info(msg, *args, **kwargs)
        
    def debug(self, msg, *args, **kwargs):
        """Record a debug message."""
        self._logger.debug(msg, *args, **kwargs)

