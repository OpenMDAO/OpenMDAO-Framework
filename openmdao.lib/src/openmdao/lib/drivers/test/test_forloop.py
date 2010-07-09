"""
Test the ForLoop driver
"""

import unittest

# pylint: disable-msg=F0401,E0611
from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.lib.api import Float, CONMINdriver, Iterate
from openmdao.lib.drivers.forloop import ForLoop
from openmdao.test.execcomp import ExecComp

class ForLoopTestCase(unittest.TestCase):
    """test Iterate component"""

    def setUp(self):
        self.top = top = set_as_top(Assembly())
        forloop = top.add('driver', ForLoop())
        top.add('comp1', ExecComp(exprs=['y=x+1']))
        top.add('starter', ExecComp(exprs=['y=1']))
        top.connect('starter.y', 'comp1.x')
        forloop.init_workflow.add(top.starter)
        forloop.workflow.add(top.comp1)
        forloop.continuation_funct = \
"""
def cont(obj):
    return obj.parent.comp1.y < 4
"""
        
    def tearDown(self):
        self.top = None

    def test_string_funct(self):
        self.top.run()
            

if __name__ == "__main__":
    unittest.main()
  


