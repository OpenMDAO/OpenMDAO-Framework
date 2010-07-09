"""
Test the ForLoop driver
"""

import unittest

# pylint: disable-msg=F0401,E0611
from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.lib.api import Float, CONMINdriver, Iterate
from openmdao.lib.drivers.forloop import ForLoop
from openmdao.test.execcomp import ExecComp

class MyForLoop(ForLoop):
    def continue_iteration(self):
        return self.parent.comp1.y < 10
    
    def pre_iteration(self):
        self.parent.comp1.a += 1
    

class ForLoopTestCase(unittest.TestCase):
    """test Iterate component"""

    def setUp(self):
        self.top = top = set_as_top(Assembly())
        forloop = top.add('driver', MyForLoop())
        top.add('comp1', ExecComp(exprs=['y=x+a','z=a']))
        top.add('starter', ExecComp(exprs=['y=1']))
        top.connect('starter.y', 'comp1.x')
        forloop.init_workflow.add(top.starter)
        forloop.workflow.add(top.comp1)
        
    def tearDown(self):
        self.top = None

    def test_basic(self):
        self.top.run()
        self.assertEqual(self.top.starter.runcount, 1)
        self.assertEqual(self.top.comp1.runcount, 9)
            

if __name__ == "__main__":
    unittest.main()
  


