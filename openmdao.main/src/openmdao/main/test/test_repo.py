import os
import unittest
import tempfile
import shutil

from nose import SkipTest

from openmdao.main.repo import DumbRepo, GitRepo, BzrRepo, HgRepo
from openmdao.main.project import PROJ_FILE_EXT
from openmdao.util.fileutil import build_directory, onerror


def _build_project(topdir):
    dirstruct = {
        'myclass.py': """
from openmdao.main.api import Component
from openmdao.main.datatypes.api import Float
class MyClass(Component):
    x = Float(0.0, iotype='in')
    y = Float(0.0, iotype='out')
""",
       '_macros': {
             'default': """
top = set_as_top(create('openmdao.main.assembly.Assembly'))
top.add("P1",create("myclass.MyClass"))
top.add("P2",create("myclass.MyClass"))
top.connect("P1.y","P2.x")
"""
       }
    }
    build_directory(dirstruct, topdir=topdir)


class RepoTestCase(unittest.TestCase):
    def setUp(self):
        self.startdir = os.getcwd()
        self.tdir = tempfile.mkdtemp()
        self.projdir = os.path.join(self.tdir, 'myproject')
        os.mkdir(self.projdir)
        os.chdir(self.tdir)

    def tearDown(self):
        try:
            shutil.rmtree(self.tdir, onerror=onerror)
        except:
            pass
        finally:
            os.chdir(self.startdir)

    def _init_repo(self, repo):
        _build_project(self.projdir)
        try:
            repo.init_repo()
        except RuntimeError as err:
            if 'no username supplied' in str(err) or 'Unable to determine your name' in str(err):
                raise SkipTest("skipping test for %s. username not configured" % repo.__class__.__name__)
            raise

    def _commit_repo(self, repo, comment=''):
        repo.commit(comment=comment)

    def _revert_repo(self, repo, commit_id=None):
        repo.revert(commit_id)

    def _do_commit_revert(self, repo):
        fpath = os.path.join(self.projdir, 'myclass.py')
        with open(fpath, 'r') as f:
            myclass_contents = f.read()

        new_contents = myclass_contents + "\n#BLAH!"

        with open(fpath, 'w') as f:
            f.write(new_contents)

        self._revert_repo(repo)

        with open(fpath, 'r') as f:
            contents = f.read()

        self.assertEqual(contents, myclass_contents)

        with open(fpath, 'w') as f:
            f.write(new_contents)

        self._commit_repo(repo)
        os.remove(fpath)
        self._revert_repo(repo)

        with open(fpath, 'r') as f:
            contents = f.read()

        self.assertEqual(contents, new_contents)

    def test_dumb_repo(self):
        repo = DumbRepo(self.projdir)
        self._init_repo(repo)
        self.assertTrue(os.path.isfile(os.path.join(self.projdir, '.projrepo', 'myproject'+PROJ_FILE_EXT)))
        self._do_commit_revert(repo)

    def test_git_repo(self):
        if not GitRepo.is_present():
            raise SkipTest("git wasn't found")

        repo = GitRepo(self.projdir)
        self._init_repo(repo)
        self.assertTrue(os.path.isdir(os.path.join(self.projdir, '.git')))
        self._do_commit_revert(repo)

    def test_hg_repo(self):
        if not HgRepo.is_present():
            raise SkipTest("mercurial wasn't found")

        repo = HgRepo(self.projdir)
        self._init_repo(repo)
        self.assertTrue(os.path.isdir(os.path.join(self.projdir, '.hg')))
        self._do_commit_revert(repo)

    def test_bzr_repo(self):
        if not BzrRepo.is_present():
            raise SkipTest("bazaar wasn't found")

        repo = BzrRepo(self.projdir)
        self._init_repo(repo)
        self.assertTrue(os.path.isdir(os.path.join(self.projdir, '.bzr')))
        self._do_commit_revert(repo)


if __name__ == "__main__":
    unittest.main()
