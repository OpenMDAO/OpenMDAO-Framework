import unittest

from openmdao.main.datatypes.api import Int, Float, Dict
from openmdao.main.api import Component

class MyComponent(Component):

    arr = Dict(key_trait=Int, value_trait=Float(units='MW'), iotype='in')

    def execute(self):
        pass

class DictTestCase(unittest.TestCase):
    def test_missing_key_set(self):
        comp = MyComponent()
        comp.set('arr[1]', 2.0)
        self.assertEqual(2.0, comp.arr[1])

if __name__ == '__main__':
    unittest.main()

