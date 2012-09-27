import os
import sys
import unittest
import tempfile
import shutil

from openmdao.main.project import DumbRepo, PROJ_FILE_EXT
from openmdao.util.fileutil import build_directory

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
            shutil.rmtree(self.tdir)
        except:
            pass
        finally:
            os.chdir(self.startdir)

    def _init_repo(self, vcs):
        _build_project(self.projdir)
        vcs.init_repo()
        
    def _commit_repo(self, vcs, comment=''):
        vcs.commit(comment=comment)
        
    def _revert_repo(self, vcs, commit_id=None):
        vcs.revert(commit_id)
        
    def test_dumb_repo(self):
        repo = DumbRepo(self.projdir)
        self._init_repo(repo)
        self.assertTrue(os.path.isfile(os.path.join(self.projdir, '.projrepo', 'myproject'+PROJ_FILE_EXT)))
        
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
    
if __name__ == "__main__":
    unittest.main()


