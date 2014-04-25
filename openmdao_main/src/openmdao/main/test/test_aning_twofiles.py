import unittest

from openmdao.main.api import Component, Assembly, set_as_top
from openmdao.main.datatypes.api import Float, Slot


class OneBase(Component):

    x = Float(iotype='out')

class C(Component):

    x = Float(iotype='in')
    y = Float(iotype='out')

    def execute(self):
        self.y = 2*self.x


class Example(Assembly):

    base = Slot(OneBase)

    def configure(self):
        self.add('base', OneBase())
        self.add('c', C())
        self.driver.workflow.add(['base', 'c'])
        self.connect('base.x', 'c.x')
        self.create_passthrough('c.y')

class TestCase(unittest.TestCase):
    def test_twofiles(self):
        from sub_aning_twofiles import One
        ex = set_as_top(Example())
        ex.replace('base', One())
    
        ex.run()
    
        self.assertEqual(ex.y, 6.0)
        
if __name__ == '__main__':
    unittest.main()