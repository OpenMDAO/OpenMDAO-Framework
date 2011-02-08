import unittest

import sys
import os


from openmdao.main.api import Assembly, set_as_top
from openmdao.main.project import Project

projdir = os.path.dirname(os.path.dirname(__file__))
sys.path.append(projdir)
from multiplier import Multiplier
from simple import Simple

class Proj1TestCase(unittest.TestCase):

    def test_lazy_eval(self):
        top = set_as_top(Assembly())
        comp1 = top.add('comp1', Multiplier())
        comp2 = top.add('comp2', Multiplier())
        
        top.driver.workflow.add(['comp1', 'comp2'])
        
        top.comp1.mult = 2.0
        top.comp2.mult = 4.0
        top.connect('comp1.rval_out', 'comp2.rval_in')
        top.comp1.rval_in = 5.0
        
        proj = Project(projdir)
        proj.top = top
        proj.add_file(os.path.join(projdir, 'multiplier.py'))
        proj.add_file(os.path.join(projdir, 'simple.py'))
        
if __name__ == '__main__':
    unittest.main()

