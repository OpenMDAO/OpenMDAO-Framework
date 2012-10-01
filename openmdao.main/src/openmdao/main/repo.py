import os
import shutil
import subprocess
import tempfile

from openmdao.main.project import Project, project_from_archive, PROJ_FILE_EXT
from openmdao.util.log import logger

_ignore = """
.coverage
openmdao_log.txt

~*
*~
*.egg-info
*.egg

# Compiled source
*.com
*.class
*.dll
*.exe
*.o
*.so
*.pyc
*.pyo

# Logs and databases
*.log
*.sql
*.sqlite

# OS generated files
.DS_Store?
ehthumbs.db
Thumbs.db

# other vcs
.bzr
.hg
.svn
"""

class SimpleObj(object):
    def __init__(self, **kwargs):
        for arg,val in kwargs.items():
            setattr(self, arg, val)

def _run_command(cmd):
    fd, fname = tempfile.mkstemp()
    proc = subprocess.Popen(cmd, stdout=fd, stderr=subprocess.STDOUT, shell=True)
    proc.wait()
    os.close(fd)
    try:
        if proc.returncode != 0:
            with open(fname, 'rb') as f:
                out = f.read()
            logger.error("out: %s" % out)
            raise RuntimeError(out)
    finally:
        os.remove(fname)
    return proc.returncode
            

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
        return _run_command('git --version') == 0
    
    @in_dir
    def create_ignore_file(self):
        global _ignore
        with open('.gitignore', 'w') as f:
            f.write(_ignore)
            
    @in_dir
    def init_repo(self):
        if not os.path.isfile('.gitignore'):
            self.create_ignore_file()
        return _run_command('git init')
    
    @in_dir
    def commit(self, comment):
        _run_command('git add .') # add any new files to the repo
        if not comment:
            comment = 'no comment'
        return _run_command('git commit -a -m "%s"' % comment)
    
    @in_dir
    def revert(self, commit_id=None):
        if commit_id is None:
            commit_id = 'HEAD'
        return _run_command('git reset --hard %s' % commit_id)
        
    
#class BzrRepo(RepositoryBase):

    #@staticmethod
    #def is_present():
        #return _run_command('bzr --version') == 0
    
    #@in_dir
    #def init_repo(self):
        #subprocess.check_call('???', shell=True)
    
    #@in_dir
    #def commit(self, comment=''):
        #pass
    
    #@in_dir
    #def revert(self, commit_id=None):
        #pass
    

class HgRepo(RepositoryBase):

    @staticmethod
    def is_present():
        return _run_command('hg --version') == 0
    
    @in_dir
    def create_ignore_file(self):
        global _ignore
        with open('.hgignore', 'w') as f:
            f.write("syntax: glob\n"+_ignore)
            
    @in_dir
    def init_repo(self):
        if not os.path.isfile('.hgignore'):
            self.create_ignore_file()
        return _run_command('hg init')
    
    @in_dir
    def commit(self, comment=''):
        _run_command('hg add')
        if not comment:
            comment = 'no comment'
        return _run_command('hg commit -m "%s"' % comment)
    
    @in_dir
    def revert(self, commit_id=None):
        if commit_id is None:
            return _run_command('hg revert --all --no-backup')
        else:
            return _run_command('hg revert --all --no-backup --rev %s' % 
                                commit_id)

    
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
    if os.path.exists(os.path.join(path, '.git')) and GitRepo.is_present():
        return GitRepo(path)
    elif os.path.exists(os.path.join(path, '.hg')) and HgRepo.is_present():
        return HgRepo(path)
    #elif os.path.exists(os.path.join(path, '.bzr')) and BzrRepo.is_present():
        #return BzrRepo(path)
    elif os.path.exists(os.path.join(path, DumbRepo.repodir)):
        return DumbRepo(path)


def find_vcs():
    """Return Repository objects based on what version control systems
    are installed.
    """
    #return [vcs for vcs in [GitRepo, HgRepo, BzrRepo, DumbRepo] if vcs.is_present()]
    return [vcs for vcs in [HgRepo, DumbRepo] if vcs.is_present()]


if __name__ == '__main__':
    for vcs in find_vcs():
        print 'found %s' % vcs.name()
        
    