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
from openmdao.util.fileutil import get_module_path, expand_path

# extension for project files
PROJ_FILE_EXT = '.proj'


def _parse_archive_name(pathname):
    """Return the name of the project given the pathname of a project
    archive file.
    """
    return os.path.basename(pathname).split('.')[0]


def project_from_archive(archive_name, proj_name=None, dest_dir=None):
    """Expand the given project archive file in the specified destination
    directory and return a Project object that points to the newly
    expanded project.
    
    archive_name: str
        Path to the project archive to be expanded
        
    proj_name: str (optional)
        Name of the new project. Defaults to the name of the project contained
        in the name of the archive.
        
    dest_dir: str (optional)
        Directory where the project directory for the expanded archive will
        reside. Defaults to the directory where the archive is located.
    """
    archive_name = expand_path(archive_name)

    if dest_dir is None:
        dest_dir = os.path.dirname(archive_name)
    else:
        dest_dir = expand_path(dest_dir)
        
    if proj_name is None:
        proj_name = _parse_archive_name(archive_name)

    projpath = os.path.join(dest_dir, proj_name)
    
    if os.path.exists(projpath):
        raise RuntimeError("Directory '%s' already exists" % projpath)

    os.mkdir(projpath)
    startdir = os.getcwd()
    tf = tarfile.open(archive_name)
    try:
        tf.extractall(projpath)
    finally:
        tf.close()
    proj = Project(projpath)
    return proj

#def find_distrib_for_obj(obj):
    #"""Return the name of the distribution containing the module that
    #contains the given object, or None if it's not part of a distribution.
    #"""
    #try:
        #fname = inspect.getfile(obj)
    #except TypeError:
        #return None
    
    #modpath = get_module_path(fname)
    #parts = modpath.split('.')
    #l = len(parts)
    #for i in range(l):
        #try:
            #dist = get_distribution('.'.join(parts[:l-i]))
        #except DistributionNotFound:
            #continue
        #return dist
    #return None
    
    
#def model_to_class(model, classname, stream):
    #"""Takes a model and creates a new class inherited from the model's
    #class that is initialized with the current model's non-default
    #configuration.  The class definition is written to the given stream.
    #Note that this does not save the exact state of the model, but only
    #key inputs and attributes recommended by the model and its children.
    #"""
    #cfg = model.get_configinfo()
    #cfg.save_as_class(stream, classname)


def _is_valid_project_dir(dpath):
    if not os.path.isdir(dpath):
        return False
    for path in ['model','_project_state']:
        if not os.path.exists(os.path.join(dpath,path)):
            return False
    return True
    
class Project(object):
    def __init__(self, projpath):
        """Initializes a Project containing the project found in the 
        specified directory or creates a new project if one doesn't exist.
        
        projpath: str
            Path to the project's directory
        """
        self.path = expand_path(projpath)
        modeldir = os.path.join(self.path, 'model')
        self.activate()
        if os.path.isdir(projpath):
            # I don't think model dir and state should be manadatory
            # e.g. when creating a project from an existing non-Project directory
            # if not _is_valid_project_dir(projpath):
                # raise RuntimeError("Directory '%s' is not a valid OpenMDAO project directory")

            # locate file containing state, create it if it doesn't exist
            statefile = os.path.join(projpath, '_project_state')
            if os.path.exists(statefile):
                try:
                    with open(statefile, 'r') as f:
                        self.__dict__ = pickle.load(f)
            	except Exception, e:
                    print 'Unable to restore project state:',e
                    self.top = Assembly()
            else:
                self.top = Assembly()

            
            self.path = expand_path(projpath) # set again in case loading project state changed it
        else:  # new project
            os.makedirs(projpath)
            os.mkdir(modeldir)
            self.top = Assembly()

        set_as_top(self.top)
        self.save()

        SimulationRoot.chroot(self.path)

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
        
    def export(self, projname=None, destdir='.'):
        """Creates an archive of the current project for export. 
        
        projname: str (optional)
            The name that the project in the archive will have. Defaults to
            the current project name.
        
        destdir: str (optional)
            The directory where the project archive will be placed. Defaults to
            the current directory.
        """
        
        ddir = expand_path(destdir)
        if projname is None:
            projname = self.name
        projpath = os.path.join(ddir, projname)
        
        if ddir.startswith(self.path):  # the project contains the dest directory... bad
            raise RuntimeError("Destination directory for export (%s) is within project directory (%s)" %
                               (ddir, self.path))
        
        self.save()
        startdir = os.getcwd()
        os.chdir(self.path)
        try:
            try:
                tf = tarfile.open(os.path.join(ddir,projname+PROJ_FILE_EXT), 
                                  mode='w:gz')
                for entry in os.listdir(self.path):
                    tf.add(entry)
            finally:
                tf.close()
        finally:
            os.chdir(startdir)
            
