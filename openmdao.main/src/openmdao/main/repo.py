import os
import subprocess

def in_dir(f):
    """Go to a specified directory before executing the function, then return 
    to the original directory.
    """
    def wrapper(self, *args, **kwargs):
        start = os.getcwd()
        os.chdir(self.dirpath)
        try:
            return f(*args, **kwargs)
        finally:
            os.chdir(start)
    return wrapper

class RepositoryBase(object):
    def __init__(self, dirpath):
        self.dirpath = dirpath
        
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
        subprocess.check_call('git commit -a "%s"' % comment, stdout=None, shell=True)
    
    @in_dir
    def revert(self, commit_id=None):
        if commit_id is None:
            commit_id = 'HEAD'
        subprocess.check_call('git reset --hard %s' % commit_id, stdout=None, shell=True)
        
    
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
    def init_repo():
        pass
    
    @in_dir
    def commit():
        pass
    
    @in_dir
    def revert():
        pass

def get_repo(path):
    """Return the appropriate type of Repository object given the specified directory."""
    if os.path.exists(os.path.join(path, '.git')):
        return GitRepo(path)
    elif os.path.exists(os.path.join(path, '.hg')):
        return HgRepo(path)
    elif os.path.exists(os.path.join(path, '.bzr')):
        return BzrRepo(path)


def find_vcs():
    """Return Repository objects based on what VCSs is found on the system."""
    return [vcs for vcs in [GitRepo, HgRepo, BzrRepo] if vcs.is_present()]


if __name__ == '__main__':
    for vcs in find_vcs():
        print 'found %s' % vcs.name()
        
    