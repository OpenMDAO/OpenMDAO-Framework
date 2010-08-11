# pylint: disable-msg=C0111,C0103

import unittest

from openmdao.main.api import Assembly, Component, Driver, set_as_top
from openmdao.lib.api import Float, Str, Instance
from openmdao.util.decorators import add_delegate
from openmdao.main.hasconstraints import HasConstraints
from openmdao.test.execcomp import ExecComp

@add_delegate(HasConstraints)
class MyDriver(Driver):
    pass

class HasConstraintsTestCase(unittest.TestCase):

    def setUp(self):
        self.asm = set_as_top(Assembly())
        self.asm.add('driver', MyDriver())
        self.asm.add('comp1', ExecComp(exprs=['c=a+b', 'd=a-b']))
        self.asm.driver.workflow.add(self.asm.comp1)
        
    def test_add_constraint(self):
        self.asm.driver.add_constraint('comp1.a > comp1.b')
        try:
            self.asm.driver.add_constraint('comp1.qq < comp1.b')
        except ValueError as err:
            self.assertEqual(str(err), '')
        else:
            self.fail('expected ValueError')
        

if __name__ == "__main__":
    unittest.main()


