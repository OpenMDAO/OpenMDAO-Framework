"""
Routines for handling 'projects' in python.
"""

import os
import sys
import inspect
import tarfile
import cPickle as pickle

from pkg_resources import get_distribution, DistributionNotFound

from openmdao.main.api import Assembly, set_as_top
from openmdao.main.component import SimulationRoot
from openmdao.util.fileutil import get_module_path


def _parse_archive_name(pathname):
    """Return the name of the project given the pathname of a project
    archive file.
    """
    return os.path.basename(pathname).split('.')[0]


def project_from_archive(archive_name, dest_dir):
    """Expand the given project archive file in the specified destination
    directory and return a Project object that points to newly
    expanded project.
    
    archive_name: str
        Path to the project archive to be expanded
        
    dest_dir: str
        Directory where the project directory for the expanded archive will
        reside.
    """
    projname = _parse_archive_name(archive_name)
    projpath = os.path.join(dest_dir, projname)
    os.mkdir(projpath)
    tf = tarfile.open(archive_name)
    try:
        tf.extractall(projpath)
    finally:
        tf.close()
    return Project(projpath)

    
class Project(object):
    def __init__(self, projpath):
        """Initializes a Project containing the project found in the 
        specified directory or creates a new project if one doesn't exist.
        
        projpath: str
            Path to the project's directory
        """
        self.path = projpath
        self.activate()
        if os.path.isdir(projpath):
            # locate the state file containing the state of the project
            statefile = os.path.join(projpath, '_project_state')
            if os.path.isfile(statefile):
                with open(statefile, 'r') as f:
                    self.__dict__ = pickle.load(f)
            else:
                self.top = Assembly()
        else:
            os.makedirs(projpath)
            self.top = Assembly()
            
        modeldir = os.path.join(self.path, 'model')
        if not os.path.isdir(modeldir):
            os.mkdir(modeldir)
            
        self.top.directory = modeldir
        SimulationRoot.chroot(self.top.directory)
        set_as_top(self.top)

        
    def clear(self):
        """Removes all project files and subdirectories in the project directory."""
        for f in os.listdir(self.path):
            if os.path.isdir(f):
                shutil.rmtree(f)
            else:
                os.remove(f)

    @property
    def name(self):
        return os.path.basename(self.path)
    
    def activate(self):
        """Puts this project's directory on sys.path."""
        if self.path not in sys.path:
            sys.path = [self.path]+sys.path
        
    def deactivate(self):
        """Removes this project's directory from sys.path."""
        try:
            sys.path.remove(self.path)
        except:
            pass

    def save(self):
        """Saves the state of the project to its project directory."""
        fname = os.path.join(self.path, '_project_state')
        with open(fname, 'w') as f:
            pickle.dump(self.__dict__, f)
        
    def export(self, destdir='.'):
        """Creates an archive of the current project for export.
        
        destdir: str
            The directory where the project archive will be placed.
        """
        ddir = os.path.abspath(destdir)
        if ddir.startswith(self.path):  # the project contains the dest directory... bad
            raise RuntimeError("Destination directory for export (%s) is within project directory (%s)" %
                               (ddir, self.path))
        self.save()
        startdir = os.getcwd()
        os.chdir(os.path.dirname(self.path))
        try:
            try:
                tf = tarfile.open(os.path.join(ddir,self.name+'.proj'), mode='w:gz')
                tf.add(os.path.basename(self.path))
            finally:
                tf.close()
        finally:
            os.chdir(startdir)
            
def find_distrib_for_obj(obj):
    """Return the name of the distribution containing the module that
    contains the given object, or None if it's not part of a distribution.
    """
    try:
        fname = inspect.getfile(obj)
    except TypeError:
        return None
    
    modpath = get_module_path(fname)
    parts = modpath.split('.')
    l = len(parts)
    for i in range(l):
        try:
            dist = get_distribution('.'.join(parts[:l-i]))
        except DistributionNotFound:
            continue
        return dist
    return None
    
    
def model_to_class(model, classname, stream):
    """Takes a model and creates a new class inherited from the model's
    class that is initialized with the current model's non-default
    configuration.  The class definition is written to the given stream.
    Note that this does not save the exact state of the model, but only
    key inputs and attributes recommended by the model and its children.
    """
    cfg = model.get_configinfo()
    cfg.save_as_class(stream, classname)
