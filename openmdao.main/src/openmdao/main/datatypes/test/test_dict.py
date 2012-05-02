import unittest

from openmdao.main.datatypes.api import Int, Float, Dict
from openmdao.main.api import Component, Assembly, set_as_top

class MyComponent(Component):

    arr = Dict(key_trait=Int, value_trait=Float(units='MW'), iotype='in')

    def execute(self):
        pass

class IntTestCase(unittest.TestCase):
    def test_missing_key_set(self):
        comp = MyComponent()
        comp.set('arr', 2.0, [(0, 1)])
        self.assertEqual(2.0, comp.arr[1])

if __name__ == '__main__':
    unittest.main()

