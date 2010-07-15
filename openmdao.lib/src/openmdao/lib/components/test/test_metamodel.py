# pylint: disable-msg=C0111,C0103

import unittest

from enthought.traits.api import TraitError
from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.lib.api import Float

from openmdao.lib.components.metamodel import MetaModel
from openmdao.lib.components.kriging_surrogate import KrigingSurrogate # FIXME: in wrong place

class Simple(Component):
    
    a = Float(iotype='in')
    b = Float(iotype='in')
    c = Float(iotype='out')
    d = Float(iotype='out')
    
    def __init__(self):
        super(Simple, self).__init__()
        self.a = 4.
        self.b = 5.
        self.c = 7.
        self.d = 1.5

    def execute(self):
        self.c = self.a + self.b
        self.d = self.a - self.b
        
class Simple2(Component):
    
    w = Float(iotype='in')
    x = Float(iotype='in')
    y = Float(iotype='out')
    z = Float(iotype='out')
    
    def __init__(self):
        super(Simple2, self).__init__()
        self.w = 4.
        self.x = 5.
        self.y = 7.
        self.z = 1.5

    def execute(self):
        self.y = self.w * 1.1
        self.z = self.x * 0.9

class MyMetaModel(MetaModel):
    my_x = Float(1., iotype='in')

class MetaModelTestCase(unittest.TestCase):
        
    def test_model_change(self):
        metamodel = MetaModel()
        mmins = set(metamodel.list_inputs())
        mmouts = set(metamodel.list_outputs())
        metamodel.surrogate = KrigingSurrogate()
        metamodel.model = Simple()
        inputs = set(metamodel.list_inputs())
        outputs = set(metamodel.list_outputs())
        self.assertEquals(inputs-mmins, set(['a','b']))
        self.assertEquals(outputs-mmouts, set(['c','d']))
        
        # now put a different model in
        metamodel.model = Simple2()
        inputs = set(metamodel.list_inputs())
        outputs = set(metamodel.list_outputs())
        self.assertEquals(inputs-mmins, set(['w','x']))
        self.assertEquals(outputs-mmouts, set(['y','z']))
        
    def test_in_assembly(self):
        asm = set_as_top(Assembly())
        asm.add('metamodel', MetaModel())
        asm.add('comp1', Simple())
        asm.add('comp2', Simple())
        asm.metamodel.surrogate = KrigingSurrogate()
        asm.metamodel.model = Simple()
        asm.driver.workflow.add([asm.metamodel,asm.comp1,asm.comp2])
        
        asm.connect('comp1.c','metamodel.a')
        asm.connect('comp1.d','metamodel.b')
        asm.connect('metamodel.c','comp2.a')
        asm.connect('metamodel.d','comp2.b')
        self.assertEqual(set(asm.list_connections()), 
                         set([('metamodel.d', 'comp2.b'), ('metamodel.c', 'comp2.a'), 
                              ('comp1.c', 'metamodel.a'), ('comp1.d', 'metamodel.b')]))
        asm.comp1.a = 1.
        asm.comp1.b = 2.
        asm.run()
        self.assertEqual(asm.comp2.c, 6.)
        self.assertEqual(asm.comp2.d, -2.)
        
        # set new model and verify disconnect
        asm.metamodel.model = Simple2()
        self.assertEqual(asm.list_connections(), [])
        
        
    def test_default_execute(self):
        metamodel = MetaModel()
        metamodel.surrogate = KrigingSurrogate()
        metamodel.model = Simple()
        simple = Simple()
        
        metamodel.a = simple.a = 1.
        metamodel.b = simple.b = 2.
        
        simple.run()
        metamodel.run()
        
        self.assertEqual(metamodel.c, 3.)
        self.assertEqual(metamodel.d, -1.)
        self.assertEqual(metamodel.c, simple.c)
        self.assertEqual(metamodel.d, simple.d)
        
    def test_includes(self):
        metamodel = MyMetaModel()
        metamodel.includes = ['a','d']
        metamodel.surrogate = KrigingSurrogate()
        metamodel.model = Simple()
        self.assertEqual(metamodel.list_inputs_to_model(), ['a'])
        self.assertEqual(metamodel.list_outputs_from_model(), ['d'])
        
        # now try changing the includes
        metamodel.includes = ['b', 'c']
        self.assertEqual(metamodel.list_inputs_to_model(), ['b'])
        self.assertEqual(metamodel.list_outputs_from_model(), ['c'])

    def test_excludes(self):
        metamodel = MyMetaModel()
        metamodel.excludes = ['a','d']
        metamodel.surrogate = KrigingSurrogate()
        metamodel.model = Simple()
        self.assertEqual(metamodel.list_inputs_to_model(), ['b'])
        self.assertEqual(metamodel.list_outputs_from_model(), ['c'])
        
        # now try changing the excludes
        metamodel.excludes = ['b', 'c']
        self.assertEqual(metamodel.list_inputs_to_model(), ['a'])
        self.assertEqual(metamodel.list_outputs_from_model(), ['d'])
        
    def test_include_exclude(self):
        metamodel = MyMetaModel()
        metamodel.includes = ['a','d']
        try:
            metamodel.excludes = ['b','c']
        except RuntimeError as err:
            self.assertEqual(str(err), 
                             ': includes and excludes are mutually exclusive')
        else:
            self.fail('Expected RuntimeError')
        self.assertEqual(metamodel.excludes, [])
            
        metamodel.includes = []
        metamodel.excludes = ['b','c']
        try:
            metamodel.includes = ['a','d']
        except Exception as err:
            self.assertEqual(str(err), 
                             ': includes and excludes are mutually exclusive')
        else:
            self.fail('Expected Exception')
        self.assertEqual(metamodel.includes, [])
        
if __name__ == "__main__":
    unittest.main()


