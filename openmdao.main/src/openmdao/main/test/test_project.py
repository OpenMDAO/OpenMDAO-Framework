import unittest
import tempfile
import os
import shutil

from openmdao.main.project import new_project
from openmdao.util.fileutil import find_files

from openmdao.main.api import Component, Assembly
from openmdao.main.project import Project

class ProjectTestCase(unittest.TestCase):
    def setUp(self):
        self.tdir = tempfile.mkdtemp()
        
    def tearDown(self):
        shutil.rmtree(self.tdir)
        
    def test_load_project(self):
        projdir = os.path.join(os.path.dirname(__file__),'project','proj1')
        proj = Project.load(projdir)
        print proj.files
        

    

if __name__ == "__main__":
    unittest.main()


