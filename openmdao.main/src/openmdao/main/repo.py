import os
import shutil
import subprocess
import tempfile

from openmdao.main.project import Project, project_from_archive, PROJ_FILE_EXT
from openmdao.util.log import logger
from openmdao.util.fileutil import onerror

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


def _run_command(cmd, silent=False):
    fd, fname = tempfile.mkstemp()
    proc = subprocess.Popen(cmd, stdout=fd, stderr=subprocess.STDOUT, shell=True)
    proc.wait()
    os.close(fd)
    try:
        if proc.returncode != 0 and not silent:
            with open(fname, 'rb') as f:
                out = f.read()
            logger.error("out: %s" % out)
            raise RuntimeError(out)
    finally:
        os.remove(fname)
    return proc.returncode


def in_dir(f):
    """Go to a specified directory before executing the function and then return
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
    """An object to interface with Git repositories."""

    @staticmethod
    def is_present():
        try:
            return _run_command('git --version', silent=True) == 0
        except:
            return False

    @in_dir
    def create_ignore_file(self):
        global _ignore
        with open('.gitignore', 'w') as f:
            f.write(_ignore)

    @in_dir
    def init_repo(self):
        if not os.path.isfile('.gitignore'):
            self.create_ignore_file()
        ret = _run_command('git init')
        if ret == 0:
            return self.commit("initial commit")
        return ret

    @in_dir
    def commit(self, comment):
        _run_command('git add .')  # add any new files to the repo
        if not comment:
            comment = 'no comment'
        return _run_command('git commit -a -m "%s"' % comment)

    @in_dir
    def revert(self, commit_id=None):
        if commit_id is None:
            commit_id = 'HEAD'
        return _run_command('git reset --hard %s' % commit_id)


class BzrRepo(RepositoryBase):
    """An object to interface with Bazaar repositories."""

    @staticmethod
    def is_present():
        try:
            return _run_command('bzr --version', silent=True) == 0
        except:
            return False

    @in_dir
    def create_ignore_file(self):
        global _ignore
        with open('.bzrignore', 'w') as f:
            f.write(_ignore)

    @in_dir
    def init_repo(self):
        if not os.path.isfile('.bzrignore'):
            self.create_ignore_file()
        ret = _run_command('bzr init')
        if ret == 0:
            return self.commit("initial commit")
        return ret

    @in_dir
    def commit(self, comment):
        _run_command('bzr add .')  # add any new files to the repo
        if not comment:
            comment = 'no comment'
        return _run_command('bzr commit -m "%s"' % comment)

    @in_dir
    def revert(self, commit_id=None):
        if commit_id is None:
            return _run_command('bzr revert --no-backup')
        else:
            return _run_command('bzr revert --no-backup -r %s' % commit_id)


class HgRepo(RepositoryBase):
    """An object to interface with Mercurial repositories."""

    @staticmethod
    def is_present():
        try:
            return _run_command('hg --version', silent=True) == 0
        except:
            return False

    @in_dir
    def create_ignore_file(self):
        global _ignore
        with open('.hgignore', 'w') as f:
            f.write("syntax: glob\n" + _ignore)

    @in_dir
    def init_repo(self):
        if not os.path.isfile('.hgignore'):
            self.create_ignore_file()
        ret = _run_command('hg init')
        if ret == 0:
            return self.commit("initial commit")
        return ret

    @in_dir
    def commit(self, comment=''):
        _run_command('hg add')
        if not comment:
            comment = 'no comment'
        try:
            return _run_command('hg commit -m "%s"' % comment)
        except RuntimeError as err:
            if 'no username supplied' in str(err):
                return _run_command('hg commit -u unknown@unknown.com -m "%s"' % comment)

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
    a .projrepo directory and, therefore, only allows one level of 'revert'. A
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
                            shutil.rmtree(f, onerror=onerror)
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
    repo_map = {
        '.git': GitRepo,
        '.hg': HgRepo,
        '.bzr': BzrRepo,
        DumbRepo.repodir: DumbRepo,
    }
    for repo, klass in repo_map.items():
        if os.path.exists(os.path.join(path, repo)) and klass.is_present():
            return klass(path)


def find_vcs():
    """Return Repository objects based on what version control systems
    are installed.
    """
    return [vcs for vcs in [GitRepo, HgRepo, BzrRepo, DumbRepo] if vcs.is_present()]


if __name__ == '__main__':
    for vcs in find_vcs():
        print 'found %s' % vcs.name()
