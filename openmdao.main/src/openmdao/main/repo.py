import os
import shutil
import subprocess

from openmdao.main.project import Project, project_from_archive, PROJ_FILE_EXT

def in_dir(f):
    """Go to a specified directory before executing the function, then return 
    to the original directory.
    """
    def wrapper(self, *args, **kwargs):
        start = os.getcwd()
        os.chdir(self.dirpath)
        try:
            return f(self, *args, **kwargs)
        finally:
            os.chdir(start)
    return wrapper

class RepositoryBase(object):
    def __init__(self, dirpath='.'):
        self.dirpath = os.path.abspath(os.path.expanduser(dirpath))
        
    @classmethod
    def name(cls):
        return cls.__name__

class GitRepo(RepositoryBase):

    @staticmethod
    def is_present():
        proc = subprocess.Popen('git --help', stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
        proc.communicate()
        return proc.returncode == 0
    
    @in_dir
    def init_repo(self):
        subprocess.check_call('git init', shell=True)
    
    @in_dir
    def commit(self, comment):
        subprocess.check_call('git commit -a "%s"' % comment, shell=True)
    
    @in_dir
    def revert(self, commit_id=None):
        if commit_id is None:
            commit_id = 'HEAD'
        subprocess.check_call('git reset --hard %s' % commit_id, shell=True)
        
    
class BzrRepo(RepositoryBase):

    @staticmethod
    def is_present():
        proc = subprocess.Popen('bzr --help', stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
        proc.communicate()
        return proc.returncode == 0
    
    @in_dir
    def init_repo(self):
        subprocess.check_call('???', shell=True)
    
    @in_dir
    def commit(self, comment=''):
        pass
    
    @in_dir
    def revert(self, commit_id=None):
        pass
    

class HgRepo(RepositoryBase):

    @staticmethod
    def is_present():
        proc = subprocess.Popen('hg --help', stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
        proc.communicate()
        return proc.returncode == 0
    
    @in_dir
    def init_repo(self):
        pass
    
    @in_dir
    def commit(self, comment=''):
        pass
    
    @in_dir
    def revert(self, commit_id=None):
        pass

    
class DumbRepo(RepositoryBase):
    """A really simple repository that's used as a fallback if git, hg, bzr
    are not present.  It simply keeps an exported copy of the project in 
    a .projrepo directory and therefore only allows one level of 'revert'. A 
    commit just replaces the project copy.
    """
    repodir = '.projrepo'
    
    @staticmethod
    def is_present():
        return True
    
    @in_dir
    def init_repo(self):
        os.mkdir(self.repodir)
        self.commit()
    
    @in_dir
    def commit(self, comment=''):
        p = Project(self.dirpath)
        p.export(destdir=self.repodir)
    
    @in_dir
    def revert(self, commit_id=None):
        for projfile in os.listdir(self.repodir):
            if projfile.endswith(PROJ_FILE_EXT):
                # first, clean up existing project dir
                for f in os.listdir('.'):
                    if f == self.repodir:
                        continue
                    try:
                        if os.path.isdir(f):
                            shutil.rmtree(f)
                        else:
                            os.remove(f)
                    except Exception as err:
                        print str(err)
                # now untar the project archive over the current project directory
                project_from_archive(os.path.join(os.getcwd(), self.repodir, projfile),
                                     dest_dir=os.path.dirname(os.getcwd()), 
                                     create=False, overwrite=True)
                break
        else:
            raise RuntimeError("No project file to revert to!")



def get_repo(path):
    """Return the appropriate type of Repository object given the specified directory."""
    if os.path.exists(os.path.join(path, '.git')):
        return GitRepo(path)
    elif os.path.exists(os.path.join(path, '.hg')):
        return HgRepo(path)
    elif os.path.exists(os.path.join(path, '.bzr')):
        return BzrRepo(path)
    elif os.path.exists(os.path.join(path, DumbRepo.repodir)):
        return DumbRepo(path)


def find_vcs():
    """Return Repository objects based on what VCSs is found on the system."""
    #return [vcs for vcs in [GitRepo, HgRepo, BzrRepo, DumbRepo] if vcs.is_present()]
    return [DumbRepo]


if __name__ == '__main__':
    for vcs in find_vcs():
        print 'found %s' % vcs.name()
        
    