import unittest
import tempfile
import os
import shutil

from openmdao.util.fileutil import find_files

from openmdao.main.project import Project, project_from_archive

class ProjectTestCase(unittest.TestCase):
    def setUp(self):
        self.tdir = tempfile.mkdtemp()
        
    def tearDown(self):
        shutil.rmtree(self.tdir)
        
    def _create_project(self, name):
        projdir = os.path.join(os.path.dirname(os.path.abspath(__file__)),'project')
        proj = Project(os.path.join(projdir, name))
        top = proj.top
        
        from multiplier import Multiplier
        
        comp1 = top.add('comp1', Multiplier())
        comp2 = top.add('comp2', Multiplier())
        
        top.driver.workflow.add(['comp1', 'comp2'])
        
        top.comp1.mult = 2.0
        top.comp2.mult = 4.0
        top.connect('comp1.rval_out', 'comp2.rval_in')
        top.comp1.rval_in = 5.0
        
        return proj
        
    def test_proj1(self):
        proj = self._create_project('proj1')
        proj.save()
        proj.export(self.tdir)
        proj.deactivate()
        
        newproj = project_from_archive(os.path.join(self.tdir,'proj1.proj'), self.tdir)
    

if __name__ == "__main__":
    unittest.main()


