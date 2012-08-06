import unittest
import tempfile
import os
import shutil

from openmdao.util.fileutil import find_files
from openmdao.main.component import Component
from openmdao.main.project import Project, project_from_archive, PROJ_FILE_EXT, \
                                  filter_macro
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
        except:
            pass
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
        
    def test_project_export_import(self):
        proj = Project(os.path.join(self.tdir, 'proj1'))
        self._fill_project(proj.get('top'))
        
        proj.export(destdir=self.tdir)
        proj.deactivate()
        
        newproj = project_from_archive(os.path.join(self.tdir,
                                                    'proj1%s' % PROJ_FILE_EXT), 
                                       proj_name='proj2',
                                       dest_dir=self.tdir)

        self.assertEqual(newproj.path, os.path.join(self.tdir, 'proj2'))
    
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
        top = proj.get('top')
        self._fill_project(top)
        top.run()
        self.assertEqual(top.comp1.rval_out, 10.)
        self.assertEqual(top.comp2.rval_out, 40.)
        top.comp1.rval_in = 0.5
        os.chdir(self.tdir)
        proj.export(projname='fooproj')
        
        fooproj = project_from_archive('fooproj.proj')
        footop = fooproj.get('top')
        self.assertEqual(footop.comp1.rval_in, top.comp1.rval_in)
        footop.run()
        self.assertEqual(footop.comp1.rval_out, 1.)
        self.assertEqual(footop.comp2.rval_out, 4.)
            
    def test_localfile_factory(self):
        proj = Project(os.path.join(self.tdir, 'proj2'))
        self._fill_project(proj.get('top'))
        
    def test_filter_macro(self):
        lines = [
            "abc.xyz = 123.45",
            "execfile('foo.py')",
            "top.add('foo', create('MyClass'))",
            "top.foo.x = 8.9",
            "top.foo.execute()",
            "abc.xyz = 99.9",
            "some_unknown_funct(a,b,c)",
            "execfile('foo.py')",
            "top.add('foo', create('SomeClass'))",
            "top.foo.gg = 53",
            "top.blah.xx = 44",
            "top.remove('blah')",
            "abc.run()",
            ]
        expected = [
            "abc.xyz = 99.9",
            "some_unknown_funct(a,b,c)",
            "execfile('foo.py')",
            "top.add('foo', create('SomeClass'))",
            "top.foo.gg = 53",
            ]
        filtered = filter_macro(lines)
        self.assertEqual(filtered, expected)
        

if __name__ == "__main__":
    unittest.main()


