
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
import os
import pkg_resources
import platform
import shutil
import subprocess
import sys
import tempfile
import zipfile

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
            self.raise_exception("object has no attribute '"+str(base)+"'",
                                 AttributeError)
            
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
            self.raise_exception("object has no attribute '"+base+"'",
                                 AttributeError)
        except TypeError:
            self.raise_exception("object has no attribute '"+str(base)+"'",
                                 AttributeError)

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
            
        if recurse:
            visited = set()
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

    def save_to_egg(self, name=None, version=None, force_relative=True,
                    src_dir=None, src_files=None, dst_dir=None,
                    format=SAVE_CPICKLE, proto=-1, tmp_dir=None):
        """Save state and other files to an egg.
        name defaults to the name of the container.
        version defaults to the container's module __version__.
        If force_relative is True, all paths are relative to src_dir.
        src_dir is the root of all (relative) src_files.
        dst_dir is the directory to write the egg in.
        tmp_dir is the directory to use for temporary files.
        Returns the egg's filename."""
        # TODO: record required packages, buildout.cfg.
        orig_dir = os.getcwd()

        if name is None:
            name = self.name
        if version is None:
            try:
                version = sys.modules[self.__module__].__version__
            except AttributeError:
                now = datetime.datetime.now()  # Could consider using utcnow().
                version = '%d.%d.%d.%d.%d' % \
                          (now.year, now.month, now.day, now.hour, now.minute)
        if dst_dir is None:
            dst_dir = orig_dir
        if not os.access(dst_dir, os.W_OK):
            self.raise_exception("Can't save to '%s', no write permission" \
                                 % dst_dir, IOError)

        py_version = platform.python_version_tuple()
        py_version = '%s.%s' % (py_version[0], py_version[1])
        egg_name = '%s-%s-py%s.egg' % (name, version, py_version)
        self.debug('Saving to %s in %s...', egg_name, orig_dir)

        # Fixup objects, classes, & sys.modules for __main__ imports.
        fixup_objects = []
        fixup_classes = {}
        fixup_modules = set()

        objs = self.get_objs(IContainer.providedBy, recurse=True)
        objs.append(self)
        for obj in objs:
            mod = obj.__module__
            if mod == '__main__':
                classname = obj.__class__.__name__
                mod = obj.__class__.__module__
                if mod == '__main__':
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
                        self.raise_exception("Can't find module for '%s'" % \
                                             classname, RuntimeError)
                obj.__class__ = fixup_classes[classname][1]
                obj.__module__ = mod
                fixup_objects.append(obj)

        # Move to scratch area.
        tmp_dir = tempfile.mkdtemp(prefix='Egg_', dir=tmp_dir)
        os.chdir(tmp_dir)
        try:
            if src_dir:
                # Link original directory to object name.
                os.symlink(src_dir, name)
            else:
                os.mkdir(name)

            # Save state of object hierarchy.
            if format is SAVE_CPICKLE or format is SAVE_PICKLE:
                state_name = name+'.pickle'
            elif format is SAVE_LIBYAML or format is SAVE_YAML:
                state_name = name+'.yaml'
            else:
                self.raise_exception("Unknown format '%s'." % str(format),
                                     RuntimeError)

            state_path = os.path.join(name, state_name)
            try:
                self.save(state_path, format, proto)
            except Exception, exc:
                if os.path.exists(state_path):
                    os.remove(state_path)
#                self.exception("Can't save to '%s': %s", state_path, str(exc))
                self.raise_exception("Can't save to '%s': %s" % \
                                     (state_path, str(exc)), type(exc))

            # Add state file to set of files to save.
            if src_files is None:
                src_files = set()
            src_files.add(state_name)
            src_files = sorted(src_files)

            # If needed, make an empty __init__.py
            if not os.path.exists(os.path.join(name, '__init__.py')):
                remove_init = True
                out = open(os.path.join(name, '__init__.py'), 'w')
                out.close()
            else:
                remove_init = False

            # Create loader script.
            out = open(os.path.join(name, '%s_loader.py' % name), 'w')
            out.write("""\
import os.path
import sys
if not '.' in sys.path:
    sys.path.append('.')

from openmdao.main import Container, Component
from openmdao.main.constants import SAVE_CPICKLE, SAVE_LIBYAML

def load():
    state_name = '%s'
    if state_name.endswith('.pickle'):
        return Container.load(state_name, SAVE_CPICKLE)
    elif state_name.endswith('.yaml'):
        return Container.load(state_name, SAVE_LIBYAML)
    raise RuntimeError("State file '%%s' is not a pickle or yaml save file.",
                       state_name)

if __name__ == '__main__':
    model = Component.load_from_egg(os.path.join('..', '%s'))
    model.run()
""" % (state_name, egg_name))
            out.close()

            # Save everything to an egg via setuptools.
            out = open('setup.py', 'w')

            out.write('import setuptools\n')
            out.write('\n')

            out.write('package_files = [\n')
            for filename in src_files:
                path = os.path.join(name, filename)
                if not os.path.exists(path):
                    self.raise_exception("Can't save, '%s' does not exist" % \
                                         path, ValueError)
                out.write("    '%s',\n" % filename)
            out.write(']\n')
            out.write('\n')

            modules = []
            out.write('requirements = [\n')
            for module in modules:
                out.write("    '%s',\n" % module)
            out.write(']\n')
            out.write('\n')

            out.write("entry_points = {\n")
            out.write("    'openmdao.top' : [\n")
            out.write("        'top = %s_loader:load',\n" % name)
            out.write("    ],\n")
            out.write("    'openmdao.components' : [\n")
            out.write("        '%s = %s_loader:load',\n" % (name, name))
            out.write("    ],\n")
            out.write("}\n")
            out.write("\n")

            out.write("setuptools.setup(\n")
            out.write("    name='%s',\n" % name)
            out.write('    description="""%s""",\n' % self.__doc__.strip())
            out.write("    version='%s',\n" % version)
            out.write("    packages=['%s'],\n" % name)
            out.write("    package_data={'%s' : package_files},\n" % name)
            out.write("    zip_safe=False,\n")
            out.write("    install_requires=requirements,\n")
            out.write("    entry_points=entry_points,\n")
            out.write(")\n")
            out.write("\n")

            out.close()

            stdout = open('setup.py.out', 'w')
            subprocess.check_call(['python', 'setup.py', 'bdist_egg',
                                   '-d', dst_dir],
                                  stdout=stdout, stderr=subprocess.STDOUT)
            stdout.close()

            os.remove(os.path.join(name, state_name))
            os.remove(os.path.join(name, '%s_loader.py' % name))
            if remove_init:
                os.remove(os.path.join(name, '__init__.py'))

        finally:
            os.chdir(orig_dir)
            shutil.rmtree(tmp_dir)
            # Restore objects, classes, & sys.modules for __main__ imports.
            for obj in fixup_objects:
                obj.__module__ = '__main__'
                classname = obj.__class__.__name__
                obj.__class__ = fixup_classes[classname][0]
            for classname in fixup_classes.keys():
                new = fixup_classes[classname][1]
                del new
            for mod in fixup_modules:
                del sys.modules[mod]

        return egg_name

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
    def load_from_egg (filename):
        """Load state and other files from an egg, returns top object."""
        # TODO: handle required packages.
        logger.debug('Loading from %s in %s...', filename, os.getcwd())
        if not os.path.exists(filename):
            raise ValueError("'%s' not found." % filename)

        distributions = \
            [dist for dist in pkg_resources.find_distributions(filename,
                                                               only=True)]
        if not distributions:
            raise RuntimeError("No distributions found in '%s'." % filename)

        dist = distributions[0]
        logger.debug('    project_name: %s', dist.project_name)
        logger.debug('    key: %s', dist.key)
        logger.debug('    extras: %s', dist.extras)
        logger.debug('    version: %s', dist.version)
        logger.debug('    parsed_version: %s', dist.parsed_version)
        logger.debug('    py_version: %s', dist.py_version)
        logger.debug('    platform: %s', dist.platform)
        logger.debug('    precedence: %s', dist.precedence)
        logger.debug('    entry points:')
        maps = dist.get_entry_map()
        for group in maps.keys():
            logger.debug('        group %s:' % group)
            for name in maps[group]:
                logger.debug('            %s', name)

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

        info = dist.get_entry_info('openmdao.top', 'top')
        if info is None:
            raise RuntimeError("No openmdao.top 'top' entry point.")
        if info.module_name in sys.modules.keys():
            logger.debug("    removing existing '%s' module",
                         info.module_name)
            del sys.modules[info.module_name]            
        if not '.' in sys.path:
            sys.path.append('.')
        loader = dist.load_entry_point('openmdao.top', 'top')
        return loader()

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
        for child in self.get_objs(IContainer.providedBy):
            child.post_load()

    def pre_delete(self):
        """ Perform any required operations before the model is deleted. """
        for child in self.get_objs(IContainer.providedBy):
            child.pre_delete()

