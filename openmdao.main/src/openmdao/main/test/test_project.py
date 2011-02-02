import unittest
import tempfile
import os
import shutil

from openmdao.main.project import new_project
from openmdao.util.fileutil import find_files

fcontents = """
from openmdao.main.api import Component, Assembly

class Multiplier(Component):
    rval_in = Float(iotype='in')
    rval_out = Float(iotype='out')
    mult = Float(iotype='in')
    
    def __init__(self):
        super(Multiplier, self).__init__()
        self.rval_in = 4.
        self.rval_out = 7.
        self.mult = 1.5

    def execute(self):
        self.rval_out = self.rval_in * self.mult
        
top = set_as_top(Assembly())
comp1 = top.add('comp1', Multiplier())
comp2 = top.add('comp2', Multiplier())

top.driver.workflow.add(['comp1', 'comp2'])

top.comp1.mult = 2.0
top.comp2.mult = 4.0
top.connect('comp1.rval_out', 'comp2.rval_in')
top.comp1.rval_in = 5.0
        
"""

class ProjectTestCase(unittest.TestCase):
    def setUp(self):
        self.tdir = tempfile.mkdtemp()
        
    def tearDown(self):
        shutil.rmtree(self.tdir)

    def test_new_project(self):
        expected = set([
            os.path.join(self.tdir, 'foo', 'foo', '__init__.py'),
            os.path.join(self.tdir, 'foo', 'setup.py'),
            os.path.join(self.tdir, 'foo', 'foo', 'foo.py'),
            ])

        new_project(self.tdir, 'Foo')
        found = set(find_files("*.py", self.tdir))
        self.assertEqual(expected, found)
        
    def test_update_project(self):
        new_project(self.tdir, 'Foo')
    
    

if __name__ == "__main__":
    unittest.main()


