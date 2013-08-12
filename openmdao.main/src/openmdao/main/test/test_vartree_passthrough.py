import unittest

from openmdao.main.api import Component, VariableTree, Assembly, set_as_top
from openmdao.main.datatypes.api import Float, VarTree

class TstContainer(VariableTree):
    dummy1 = Float(1.0) 

class TstComponent(Component):
    dummy_data = VarTree(TstContainer(), iotype='in')
    dummy_data_out = VarTree(TstContainer(), iotype='out')
    dummyin = Float(iotype='in')

    def execute(self):
        self.dummy_data_out = self.dummy_data.copy()

class TstAssembly(Assembly):

    def configure(self):
        self.add('comp', TstComponent())
        self.create_passthrough('comp.dummy_data.dummy1')
        self.create_passthrough('comp.dummy_data_out.dummy1', 'dummy1_out')
        self.driver.workflow.add('comp')

class TstAssembly2(Assembly):

    def configure(self):
        self.add('comp', TstComponent())
        self.create_passthrough('comp.dummy_data')
        self.create_passthrough('comp.dummy_data_out', 'dummy1_out')
        self.driver.workflow.add('comp')

class VarTreePassthroughTestCase(unittest.TestCase):

    def test_vartree_passthrough(self):
        
        # Tests that we can create a passthrough of one variable in a tree
        blah = set_as_top(TstAssembly())
        blah.dummy1 = 5.0
        self.assertEqual(blah.dummy1_out, 1.0)
        blah.run()
        self.assertEqual(blah.comp.dummy_data.dummy1, 5.0)
        self.assertEqual(blah.dummy1, 5.0)
        self.assertEqual(blah.dummy1_out, 5.0)
    
    def test_vartree_passthrough(self):
        
        # Tests that we can create a passthrough of an entire variable tree
        blah = set_as_top(TstAssembly2())
        blah.dummy_data.dummy1 = 5.0
        self.assertEqual(blah.dummy1_out.dummy1, 1.0)
        blah.run()
        self.assertEqual(blah.comp.dummy_data.dummy1, 5.0)
        self.assertEqual(blah.dummy_data.dummy1, 5.0)
        self.assertEqual(blah.dummy1_out.dummy1, 5.0)
        
    def test_get_attributes(self):
        
        # Tests the attributres dictionary for passthrough trees
        
        blah = set_as_top(TstAssembly2())
        attrs = blah.get_attributes(True)
        self.assertTrue({'indent': 0, 
                         'name': 'dummy_data', 
                         'vt': 'vt', 
                         'implicit': '', 
                         'valid': True, 
                         'connected': '',
                         'connection_types' : 0,
                         'ttype': 'vartree',
                         'type': 'TstContainer', 
                         'id': 'dummy_data',
                         'target': 'comp.dummy_data'} in attrs['Inputs'])
        self.assertTrue({'indent': 1, 
                         'name': 'dummy1', 
                         'parent': 'dummy_data', 
                         'value': 1.0, 
                         'high': None, 
                         'valid': True, 
                         'connected': '', 
                         'low': None, 
                         'type': 'float', 
                         'id': 'dummy_data.dummy1'} in attrs['Inputs'])
        self.assertTrue({'indent': 0, 
                         'name': 'dummy1_out', 
                         'vt': 'vt', 
                         'implicit': '', 
                         'valid': False, 
                         'connected': '', 
                         'connection_types' : 0,
                         'ttype': 'vartree',
                         'type': 'TstContainer', 
                         'id': 'dummy1_out',
                         'target': 'comp.dummy_data_out'} in attrs['Outputs'])
        self.assertTrue({'indent': 1, 
                         'name': 'dummy1', 
                         'parent': 'dummy1_out', 
                         'value': 1.0, 
                         'high': None, 
                         'valid': False, 
                         'connected': '', 
                         'low': None, 
                         'type': 'float', 
                         'id': 'dummy1_out.dummy1'} in attrs['Outputs'])

        blah = set_as_top(TstAssembly())
        attrs = blah.get_attributes(True)
        self.assertTrue({'indent': 0, 
                         'name': 'dummy1', 
                         'value': 1.0, 
                         'high': None, 
                         'valid': True, 
                         'implicit': '', 
                         'connected': '', 
                         'connection_types' : 0,
                         'target': 'comp.dummy_data.dummy1', 
                         'low': None, 
                         'type': 'float', 
                         'id': 'dummy1'} in attrs['Inputs'])
        self.assertTrue({'indent': 0, 
                         'name': 'dummy1_out', 
                         'value': 1.0, 
                         'high': None, 
                         'valid': False, 
                         'implicit': '', 
                         'connected': '', 
                         'connection_types' : 0,
                         'target': 'comp.dummy_data_out.dummy1', 
                         'low': None, 
                         'type': 'float', 
                         'id': 'dummy1_out'} in attrs['Outputs'])
        
if __name__ == "__main__":
    unittest.main()

