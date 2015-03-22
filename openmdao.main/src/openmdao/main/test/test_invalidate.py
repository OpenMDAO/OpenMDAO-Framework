import unittest

from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.main.datatypes.api import Float

class MyComp(Component):

    x = Float(1.0, iotype='in')
    y = Float(iotype='out')

    def execute(self):
        self.y = 2.0*self.x

class Sub(Assembly):

    def configure(self):

        self.add('c', MyComp())
        self.driver.workflow.add(['c'])

        self.create_passthrough('c.x')
        self.create_passthrough('c.y')

class Top(Assembly):

    def configure(self):

        self.add('sub', Sub())
        self.add('comp', MyComp())

        self.driver.workflow.add(['sub', 'comp'])

        self.connect('sub.x', 'comp.x')


class InvalidationTestCase(unittest.TestCase):

    def test_assembly_input_invalidation(self):
        top = set_as_top(Top())

        top.set('sub.x', 3.0)
        top.run()
        self.assertEqual(top.sub.x, 3.0)
        self.assertEqual(top.comp.x, 3.0)
        self.assertEqual(top.comp.y, 6.0)
        self.assertEqual(top.sub.y, 6.0)

        top.set('sub.x', 4.0)
        top.run()
        self.assertEqual(top.sub.x, 4.0)
        self.assertEqual(top.comp.x, 4.0)
        self.assertEqual(top.comp.y, 8.0)
        self.assertEqual(top.sub.y, 8.0)

