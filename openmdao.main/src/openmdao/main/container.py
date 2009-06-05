
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
from openmdao.main.log import logger, LOG_DEBUG
from openmdao.main.factorymanager import create as fmcreate
from openmdao.main.constants import SAVE_YAML, SAVE_LIBYAML
from openmdao.main.constants import SAVE_PICKLE, SAVE_CPICKLE

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
        """
        orig_dir = os.getcwd()

        if name is None:
            name = self.name
        if version is None:
            try:
                version = sys.modules[self.__module__].__version__
            except AttributeError:
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

        # Fixup objects, classes, & sys.modules for __main__ imports.
        fixup = self._fix_objects(objs)
        try:
            # Determine distributions required.
            required_distributions = self._get_distributions(objs)

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
            os.remove(state_path)
            os.remove(loader_path)
            if remove_init:
                os.remove(init_path)

        finally:
            if os.path.exists(buildout_orig):
                os.rename(buildout_orig, buildout_path)
            elif os.path.exists(buildout_path):
                os.remove(buildout_path)
            os.chdir(orig_dir)
            if tmp_dir:
                shutil.rmtree(tmp_dir)
            self._restore_objects(fixup)

        return egg_name

    def _get_objects(self):
        """Get objects to be saved."""

        def _recurse_get_objects(obj, objs, visited):
            """Use __getstate__(), or scan __dict__, or scan container."""
            try:
                state = obj.__getstate__()
            except AttributeError:
                try:
                    state = obj.__dict__
                except AttributeError:
                    if isinstance(obj, dict) or \
                       isinstance(obj, list) or \
                       isinstance(obj, set)  or \
                       isinstance(obj, tuple):
                        state = obj
                    else:
                        return  # Some non-container primitive.
            except Exception, exc:
                self.error("During save_to_egg, _get_objects error %s: %s",
                           type(obj), exc)
                return

            if isinstance(state, dict):
                state = state.values()

            for obj in state:
                if id(obj) in visited:
                    continue
                visited.append(id(obj))
                objs.append(obj)
                _recurse_get_objects(obj, objs, visited)

        objs = []
        visited = [id(self._parent)]  # Don't include our parent.
        _recurse_get_objects(self, objs, visited)
        return objs

    def _fix_objects(self, objs):
        """Fixup objects, classes, & sys.modules for __main__ imports."""
        fixup_objects = []
        fixup_classes = {}
        fixup_modules = set()

        for obj in objs:
            try:
                mod = obj.__module__
            except AttributeError:
                continue
            if mod == '__main__':
                classname = obj.__class__.__name__
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
                obj.__class__ = fixup_classes[classname][1]
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
        """Return distributions used by objs."""
        working_set = pkg_resources.WorkingSet()
#        for dist in working_set:
#            self.debug('    %s %s', dist.project_name, dist.version)
#            self.debug('        %s', dist.location)
        distributions = set()
        modules = ['__builtin__']
        prefixes = []
        site_lib = os.path.dirname(site.__file__)
        site_pkg = site_lib+os.sep+'site-packages'
        py_version = 'python%d.%d' % (sys.version_info[0], sys.version_info[1])

        # For each object found...
        for obj in objs:
            try:
                name = obj.__module__
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
            for prefix in prefixes:
                if path.startswith(prefix):
#                    self.debug("    skipping(2) '%s'", path)
                    break
            else:
                # Use module finder to get modules that object requires.
                if path.endswith('.pyc') or path.endswith('.pyo'):
                    path = path[:-1]
#                self.debug("    analyzing '%s'...", path)
                finder = modulefinder.ModuleFinder()
                finder.run_script(path)

                # For each found module...
                for name, module in sorted(finder.modules.items(),
                                           key=lambda item: item[0]):
#                    self.debug('    found %s', name)
                    if name in modules:
#                        self.debug('        already known')
                        continue
                    if name != '__main__':
                        modules.append(name)
                    try:
                        path = module.__file__
                    except AttributeError:
#                        self.debug('        no __file__')
                        continue
                    if not path:
#                        self.debug('        null __file__')
                        continue
                    if path.startswith(site_lib) and \
                       not path.startswith(site_pkg):
#                        self.debug("        skipping '%s'", path)
                        continue

                    # Record distribution.
                    for dist in working_set:
                        loc = dist.location
                        # Protect against a 'bare' location.
                        if loc.endswith('site-packages') or \
                           loc.endswith(py_version):
                            loc += os.sep+dist.project_name
                        if path.startswith(loc):
                            distributions.add(dist)
                            if loc.endswith('.egg'):
                                prefixes.append(loc)
#                            self.debug('        adding %s %s',
#                                       dist.project_name, dist.version)
                            break
                    else:
                        self.warning("        no distribution found for '%s'",
                                     path)

        distributions = sorted(distributions,
                               key=lambda dist: dist.project_name)
        self.debug('    required distributions:')
        for dist in distributions:
            self.debug('        %s %s', dist.project_name, dist.version)
        return distributions

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
    from openmdao.main import Component
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
""" % {'name':name, 'loader':loader,
       'doc':self.__doc__.strip(), 'version':version})

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
            line = proc.stdout.readline().rstrip()
            self.debug('    '+line)
            output.append(line)
            proc.poll()
        for line in proc.stdout:
            if not line:
                break
            line = line.rstrip()
            self.debug('    '+line)
            output.append(line)

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
    def load_from_egg (filename, install=True):
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
    
