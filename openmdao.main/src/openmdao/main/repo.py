import os
import shutil
import subprocess

from openmdao.main.project import Project, project_from_archive, PROJ_FILE_EXT

class SimpleObj(object):
    def __init__(self, **kwargs):
        for arg,val in kwargs.items():
            setattr(self, arg, val)

def _run_command(cmd):
    proc = subprocess.Popen(cmd, shell=True)
    out = proc.communicate()
    if proc.returncode != 0:
        raise subprocess.CalledProcessError(proc.returncode, cmd, out[1])
    return SimpleObj(returncode=proc.returncode,
                     stdout=out[0], stderr=out[1])
            

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
        return _run_command('git --version').returncode == 0
    
    @in_dir
    def create_ignore_file(self):
        ignore = """
.coverage
openmdao_log.txt

~*
*~
*.egg-info
*.egg

# Compiled source #
###################
*.com
*.class
*.dll
*.exe
*.o
*.so
*.pyc
*.pyo

# Packages #
############
# it's better to unpack these files and commit the raw source
# git has its own built in compression methods
*.7z
*.dmg
*.gz
*.iso
*.jar
*.rar
*.tar
*.zip

# Logs and databases #
######################
*.log
*.sql
*.sqlite

# OS generated files #
######################
.DS_Store?
ehthumbs.db
Thumbs.db
        """
        with open('.gitignore', 'w') as f:
            f.write(ignore)
            
    @in_dir
    def init_repo(self):
        self.create_ignore_file()
        _run_command('git init')
    
    @in_dir
    def commit(self, comment):
        _run_command('git add .') # add any new files to the repo
        if not comment:
            comment = 'no comment'
        _run_command('git commit -a -m "%s"' % comment)
    
    @in_dir
    def revert(self, commit_id=None):
        if commit_id is None:
            commit_id = 'HEAD'
        _run_command('git reset --hard %s' % commit_id)
        
    
class BzrRepo(RepositoryBase):

    @staticmethod
    def is_present():
        return _run_command('bzr --help').returncode == 0
    
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
        return _run_command('hg --help').returncode == 0
    
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
    return [vcs for vcs in [GitRepo, HgRepo, BzrRepo, DumbRepo] if vcs.is_present()]


if __name__ == '__main__':
    for vcs in find_vcs():
        print 'found %s' % vcs.name()
        
    