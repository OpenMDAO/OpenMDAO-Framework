import unittest

from openmdao.main.api import Component, Assembly, Case, CaseSet, set_as_top
from openmdao.lib.datatypes.api import Int, List

class Simple(Component):
    a = Int(iotype='in')
    b = Int(iotype='in')
    c = Int(iotype='out')
    d = Int(iotype='out')
    a_lst = List(Int, iotype='in')
    c_lst = List(Int, iotype='out')
    
    def __init__(self):
        super(Simple, self).__init__()
        self.a = 1
        self.b = 2
        self.c = 3
        self.d = -1
        self.a_lst = [1,2,3]

    def execute(self):
        self.c = self.a + self.b
        self.d = self.a - self.b
        self.c_lst = [x*2 for x in self.a_lst]

class CaseSetTestCase(unittest.TestCase):

    def setUp(self):
        self.top = set_as_top(Assembly())
        self.top.add('comp1', Simple())
        self.top.add('comp2', Simple())
        self.top.connect('comp1.c', 'comp2.a')
        self.top.connect('comp1.d', 'comp2.b')
        self.top.connect('comp1.c_lst', 'comp2.a_lst')
        self.top.driver.workflow.add(['comp1','comp2'])
        
        self.inputs = [('comp1.a',2),('comp1.b',4),('comp1.a_lst', [4,5,6])]
        self.outputs = ['comp2.c+comp2.d', 'comp2.c_lst[2]']
        self.case = case = Case(inputs=self.inputs, outputs=self.outputs)
        case.apply_inputs(self.top)
        self.top.run()
        case.update_outputs(self.top)
    
    def test_caseset_from_dict(self):
        self.fail()
        
    def test_caseset_from_case(self):
        self.fail()
        
    def test_caseset_start_empty(self):
        self.fail()
        
    def test_caseset_unique(self):
        self.fail()
        
    def test_caseset_getitem_str(self):
        self.fail()
        
    def test_caseset_getitem_int(self):
        self.fail()
        
    def test_caseset_iteration(self):
        self.fail()
        

if __name__ == "__main__":
    unittest.main()


