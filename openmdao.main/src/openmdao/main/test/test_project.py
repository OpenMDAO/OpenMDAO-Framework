import unittest
import tempfile
import os
import shutil

from openmdao.util.fileutil import find_files
from openmdao.main.component import Component
from openmdao.main.project import Project, project_from_archive, \
     _is_valid_project_dir, PROJ_FILE_EXT
from openmdao.lib.datatypes.api import Float

class Multiplier(Component):
    rval_in = Float(iotype='in')
    rval_out = Float(iotype='out')
    mult = Float(iotype='in')
    
    def __init__(self):
        super(Multiplier, self).__init__()
        self.rval_in = 4.
        self.rval_out = 6.
        self.mult = 1.5

    def execute(self):
        self.rval_out = self.rval_in * self.mult

class ProjectTestCase(unittest.TestCase):
    def setUp(self):
        self.startdir = os.getcwd()
        self.tdir = tempfile.mkdtemp()
        os.chdir(self.tdir)
        
    def tearDown(self):
        try:
            shutil.rmtree(self.tdir)
        finally:
            os.chdir(self.startdir)

    def _fill_project(self, top):
        comp1 = top.add('comp1', Multiplier())
        comp2 = top.add('comp2', Multiplier())
        
        top.driver.workflow.add(['comp1', 'comp2'])
        
        top.comp1.mult = 2.0
        top.comp2.mult = 4.0
        top.connect('comp1.rval_out', 'comp2.rval_in')
        top.comp1.rval_in = 5.0
        
    def test_new_project_is_valid(self):
        proj = Project(os.path.join(self.tdir, 'proj1'))
        self._fill_project(proj.top)
        
        self.assertEqual(proj.path, os.path.join(self.tdir, 'proj1'))
        self.assertTrue(_is_valid_project_dir(proj.path))

    def test_project_export_import(self):
        proj = Project(os.path.join(self.tdir, 'proj1'))
        self._fill_project(proj.top)
        
        proj.export(destdir=self.tdir)
        proj.deactivate()
        
        newproj = project_from_archive(os.path.join(self.tdir,
                                                    'proj1%s' % PROJ_FILE_EXT), 
                                       proj_name='proj2',
                                       dest_dir=self.tdir)

        self.assertEqual(newproj.path, os.path.join(self.tdir, 'proj2'))
        self.assertTrue(_is_valid_project_dir(proj.path))
    
        try:
            newproj = project_from_archive(os.path.join(self.tdir,
                                                        'proj1%s' % PROJ_FILE_EXT), 
                                           dest_dir=self.tdir)
        except Exception, err:
            self.assertTrue(str(err).endswith(' already exists'))
        else:
            self.fail("Exception expected")
            
    def test_using(self):
        proj = Project('a_proj')
        self._fill_project(proj.top)
        proj.top.run()
        self.assertEqual(proj.top.comp1.rval_out, 10.)
        self.assertEqual(proj.top.comp2.rval_out, 40.)
        proj.top.comp1.rval_in = 0.5
        os.chdir(self.tdir)
        proj.export(projname='fooproj')
        
        fooproj = project_from_archive('fooproj.proj')
        self.assertEqual(fooproj.top.comp1.rval_in, proj.top.comp1.rval_in)
        fooproj.top.run()
        self.assertEqual(fooproj.top.comp1.rval_out, 1.)
        self.assertEqual(fooproj.top.comp2.rval_out, 4.)
            

if __name__ == "__main__":
    unittest.main()


