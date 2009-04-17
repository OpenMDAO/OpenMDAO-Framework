
#public symbols
#__all__ = ['Component']

__version__ = "0.1"

import datetime
import os
import os.path
import platform
import shutil
import subprocess
import sys
import tempfile
import zipfile

from zope.interface import implements

from openmdao.main.interfaces import IComponent, IAssembly
from openmdao.main import Container, String
from openmdao.main.log import logger
from openmdao.main.variable import INPUT
from openmdao.main.constants import SAVE_PICKLE, SAVE_CPICKLE, \
                                    SAVE_YAML, SAVE_LIBYAML

# Execution states.
STATE_UNKNOWN = -1
STATE_IDLE    = 0
STATE_RUNNING = 1
STATE_WAITING = 2


class Component (Container):
    """This is the base class for all objects containing Variables that are 
       accessible to the OpenMDAO framework and are 'runnable'.
    """

    implements(IComponent)
    
    def __init__(self, name, parent=None, doc=None, directory=''):
        super(Component, self).__init__(name, parent, doc)
        
        self.state = STATE_IDLE
        self._stop = False
        self._input_changed = True
        self._dir_stack = []
        self._sockets = {}

        # List of meta-data dictionaries.
        self.external_files = []

        # If we have no parent, then make directory absolute to
        # stop get_directory() scan.  Can be removed if we get a 'root' obj.
        if parent is None:
            if directory:
                if not os.path.isabs(directory):
                    directory = os.path.join(os.getcwd(), directory)
            else:
                directory = os.getcwd()

        String('directory', self, INPUT, default=directory,
               doc='If non-null, the directory to execute in.')

# pylint: disable-msg=E1101
        dirpath = self.get_directory()
        if dirpath:
            if not os.path.exists(dirpath):
# TODO: Security!
                try:
                    os.makedirs(dirpath)
                except OSError, exc:
                    self.raise_exception("Can't create execution directory '%s': %s" \
                                         % (dirpath, exc.strerror),
                                         OSError)
            else:
                if not os.path.isdir(dirpath):
                    self.raise_exception("Execution directory path '%s' is not a directory." \
                                         % dirpath, ValueError)
# pylint: enable-msg=E1101

    def _get_socket_plugin(self, name):
        """Return plugin for the named socket"""
        try:
            iface, plugin = self._sockets[name]
        except KeyError:
            self.raise_exception("no such socket '%s'" % name, AttributeError)
        else:
            if plugin is None:
                self.raise_exception("socket '%s' is empty" % name,
                                     RuntimeError)
            return plugin

    def _set_socket_plugin(self, name, plugin):
        """Set plugin for the named socket"""
        try:
            iface, current = self._sockets[name]
        except KeyError:
            self.raise_exception("no such socket '%s'" % name, AttributeError)
        else:
            if plugin is not None and iface is not None:
                if not iface.providedBy(plugin):
                    self.raise_exception("plugin does not support '%s'" % \
                                         iface.__name__, ValueError)
            self._sockets[name] = (iface, plugin)

    def add_socket (self, name, iface, doc=''):
        """Specify a named placeholder for a component with the given
        interface or prototype.
        """
        assert isinstance(name, basestring)
        self._sockets[name] = (iface, None)
        setattr(self.__class__, name,
                property(lambda self : self._get_socket_plugin(name),
                         lambda self, plugin : self._set_socket_plugin(name, plugin),
                         None, doc))

    def check_socket (self, name):
        """Return True if socket is filled"""
        try:
            iface, plugin = self._sockets[name]
        except KeyError:
            self.raise_exception("no such socket '%s'" % name, AttributeError)
        else:
            return plugin is not None

    def remove_socket (self, name):
        """Remove an existing Socket"""
        del self._sockets[name]

    def post_config (self):
        """Perform any final initialization after configuration has been set,
        and verify that the configuration is correct.
        """
        pass
    
    def pre_execute (self):
        """update input variables and anything else needed prior 
        to execution."""
        pass
    
    def execute (self):
        """Perform calculations or other actions, assuming that inputs 
        have already been set. 
        This should be overridden in derived classes.
        """
        pass
    
    def post_execute (self):
        """Update output variables and anything else needed after execution"""
        pass
    
    def run (self, force=False):
        """Run this object. This should include fetching input variables,
        executing, and updating output variables. Do not override this function.
        """
        directory = self.get_directory()
        try:
            self.push_dir(directory)
        except OSError, exc:
            msg = "Could not move to execution directory '%s': %s" % \
                  (directory, exc.strerror)
            self.raise_exception(msg, RuntimeError)

        self.state = STATE_RUNNING
        self._stop = False
        try:
            if self.parent is not None and IAssembly.providedBy(self.parent):
                self.parent.update_inputs(self)
            self.pre_execute()
            self.execute()
            self.post_execute()
        finally:
            self.state = STATE_IDLE
            self.pop_dir()

    def get_directory (self):
        """ Return absolute path of execution directory. """
        path = self.directory
        if not os.path.isabs(path):
            if self.parent is not None and IComponent.providedBy(self.parent):
                parent_dir = self.parent.get_directory()
            else:
# TODO: fix this hack by having some form of root object
#       that holds the absolute path of the server's directory.
#       (this would also be the root of the visible filesystem for security)
#       Normally we shouldn't get here due to the directory fix in __init__.
                if self.state == STATE_RUNNING:
                    parent_dir = self._dir_stack[-1]
                else:
                    parent_dir = os.getcwd()
            path = os.path.join(parent_dir, path)
        return path

    def push_dir (self, directory):
        """Change directory to dir, remembering current for later pop_dir()."""
        if not directory:
            directory = '.'
        cwd = os.getcwd()
# TODO: Security!
        os.chdir(directory)
        self._dir_stack.append(cwd)

    def pop_dir (self):
        """ Return to previous directory saved by push_dir(). """
        os.chdir(self._dir_stack.pop())

    def checkpoint (self, outstream, format=SAVE_PICKLE):
        """Save sufficient information for a restart. By default, this
        just calls save()
        """
        self.save(outstream, format)

    def restart (self, instream):
        """Restore state using a checkpoint file. The checkpoint file is
        typically a delta from a full saved state file. If checkpoint is
        overridden, this should also be overridden.
        """
        self.load(instream)

    def save_to_egg(self, name=None, version=None,
                    format=SAVE_CPICKLE, proto=-1, dst_dir=None):
        """ Save state and external files to an egg, returns egg name. """
        # TODO: check for setup.py errors!
        # TODO: handle custom class definitions.
        # TODO: scan for FileVariables.
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

        # Save top object's directory, we'll be clobbering it later.
        obj_dir = self.get_directory()

        # Get external files related to components.
        files = set()
        components = [self]
        components.extend(self.get_objs(IComponent.providedBy, True))
        for component in components:
            directory = component.get_directory()
            for i, metadata in enumerate(component.external_files):
                path = metadata['path']
                if not os.path.isabs(path):
                    path = os.path.join(directory, path)
                path = os.path.normpath(path)
                if path.startswith(obj_dir):
                    metadata['path'] = path[len(obj_dir)+1:]
                    component.external_files[i] = metadata
                    files.add(metadata['path'])
        files = sorted(files)

        # Move to scratch area.
        tmp_dir = tempfile.mkdtemp()
        os.chdir(tmp_dir)
        try:
            # Link original directory to object name.
            os.symlink(obj_dir, name)

            # Save state of object hierarchy.
            if format is SAVE_CPICKLE or format is SAVE_PICKLE:
                state_name = name+'.pickle'
            elif format is SAVE_LIBYAML or format is SAVE_YAML:
                state_name = name+'.yaml'
            else:
                self.raise_exception("Unknown format '%s'." % str(format),
                                     RuntimeError)

            self.directory = ''  # Must save in relative form.
            state_path = os.path.join(name, state_name)
            try:
                self.save(state_path, format, proto)
            except Exception, exc:
                if os.path.exists(state_path):
                    os.remove(state_path)
                self.raise_exception("Can't save to '%s': %s" % \
                                     (state_path, str(exc)), type(exc))

            # Save everything to an egg.
            package_files = [state_name]
            package_files.extend(files)

            if not os.path.exists(os.path.join(name, '__init__.py')):
                remove_init = True
                out = open(os.path.join(name, '__init__.py'), 'w')
                out.close()
            else:
                remove_init = False

            out = open('setup.py', 'w')

            out.write('import setuptools\n')
            out.write('\n')

            out.write('package_files = [\n')
            for filename in package_files:
                out.write("    '%s',\n" % filename)
            out.write('    ]\n')
            out.write('\n')

            out.write('setuptools.setup(\n')
            out.write("    name='%s',\n" % name)
            out.write("    description='%s',\n" % self.__doc__.strip())
            out.write("    version='%s',\n" % version)
            out.write("    packages=['%s'],\n" % name)
            out.write("    package_data={'%s' : package_files})\n" % name)
            out.write('\n')

            out.close()

            stdout = open('setup.py.out', 'w')
            subprocess.check_call(['python', 'setup.py', 'bdist_egg',
                                   '-d', dst_dir],
                                  stdout=stdout, stderr=subprocess.STDOUT)
            stdout.close()

            os.remove(os.path.join(name, state_name))
            if remove_init:
                os.remove(os.path.join(name, '__init__.py'))

        finally:
            self.directory = obj_dir
            os.chdir(orig_dir)
            shutil.rmtree(tmp_dir)

        return egg_name

    @staticmethod
    def load_from_egg(filename):
        """ Load state and external files from an egg, returns top object. """
        # TODO: handle custom class definitions.
        # TODO: handle required packages.
        logger.debug('Loading from %s in %s...', filename, os.getcwd())
        archive = zipfile.ZipFile(filename, 'r')
        name = archive.read('EGG-INFO/top_level.txt').split('\n')[0]
        logger.debug("    name '%s'", name)
        for info in archive.infolist():
            if not info.filename.startswith(name):
                continue  # EGG-INFO
            if info.filename.endswith('.pyc') or \
               info.filename.endswith('.pyo'):
                continue  # Don't assume compiled OK for this platform.
            logger.debug("    extracting '%s'...", info.filename)
            dirname = os.path.dirname(info.filename)
            dirname = dirname[len(name)+1:]
            if dirname and not os.path.exists(dirname):
                os.makedirs(dirname)
            path = info.filename[len(name)+1:]
            out = open(path, 'w')
            out.write(archive.read(info.filename))
            out.close()

        if os.path.exists(name+'.pickle'):
            top = Container.load(name+'.pickle', SAVE_CPICKLE)
        elif os.path.exists(name+'.yaml'):
            top = Container.load(name+'.yaml', SAVE_LIBYAML)
        else:
            raise RuntimeError('No top object pickle or yaml save file.')

        for component in top.get_objs(IComponent.providedBy, True):
            directory = component.get_directory()
            if not os.path.exists(directory):
                os.makedirs(directory)

        top.post_load()
        return top

    def step (self):
        """For Components that contain Workflows (e.g., Assembly), this will run
        one Component in the Workflow and return. For simple components, it is
        the same as run().
        """
        self.run()

    def stop (self):
        """ Stop this component. """
        self._stop = True

    def require_gradients (self, varname, gradients):
        """Requests that the component be able to provide (after execution) a
        list of gradients w.r.t. a list of variables. The format
        of the gradients list is [dvar_1, dvar_2, ..., dvar_n]. The component
        should return a list with entries of either a name, a tuple of the
        form (name,index) or None.  None indicates that the component cannot
        compute the specified derivative. name indicates the name of a
        scalar variable in the component that contains the gradient value, and
        (name,index) indicates the name of an array variable and the index of
        the entry containing the gradient value. If the component cannot
        compute any gradients of the requested varname, it can just return
        None.
        """
        return None

    def require_hessians (self, varname, deriv_vars):
        """ Requests that the component be able to provide (after execution)
        the hessian w.r.t. a list of variables. The format of
        deriv_vars is [dvar_1, dvar_2, ..., dvar_n]. The component should
        return one of the following:

            1) a name, which would indicate that the component contains
               a 2D array variable or matrix containing the hessian

            2) an array of the form 

               [[dx1dx1, dx1dx2, ... dx1dxn],
               [           ...             ],
               [dxndx1, dxndx2, ... dxndxn]]

               with entries of either name, (name,index), or None. name
               indicates that a scalar variable in the component contains the
               desired hessian matrix entry. (name,index) indicates that
               an array variable contains the value at the specified index.
               If index is a list with two entries, that indicates that
               the variable containing the entry is a 2d array or matrix.

            3) None, which means the the component cannot compute any values
               of the hessian.

             """
        return None
    
    
