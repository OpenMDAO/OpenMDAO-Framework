import unittest

from openmdao.main.datatypes.api import Int, Float, Dict
from openmdao.main.api import Component, Assembly, set_as_top

class MyComponent(Component):

    arr = Dict(key_trait=Int, value_trait=Float(units='MW'), iotype='in')

    def execute(self):
        pass

class DictTestCase(unittest.TestCase):
    def test_missing_key_set(self):
        comp = MyComponent()
        comp.set('arr', 2.0, [(0, 1)])
        self.assertEqual(2.0, comp.arr[1])
        
        # while here, let's test the get_attributes
        attrs = comp.get_attributes(True)
        attr = attrs['Inputs']
        self.assertTrue( { 'key_type' : 'Int',
                           'name' : 'arr',
                           'id' : 'arr',
                           'indent' : 0,
                           'value' : {1:2.0},
                           'value_type' : 'Float',
                           'valid' : True,
                           'connected' : '',
			   'connection_types' : 0,
                           'type' : 'dict',
                           'implicit' : ''} in attr)
                       
        
        
        

if __name__ == '__main__':
    unittest.main()

