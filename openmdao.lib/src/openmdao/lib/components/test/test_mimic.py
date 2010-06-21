# pylint: disable-msg=C0111,C0103

import unittest

from enthought.traits.api import TraitError
from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.lib.api import Float

from openmdao.lib.components.mimic import Mimic

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

class MyMimic(Mimic):
    my_x = Float(1., iotype='in')

class MimicTestCase(unittest.TestCase):
        
    def test_model_change(self):
        mimic = Mimic()
        mimins = set(mimic.list_inputs())
        mimouts = set(mimic.list_outputs())
        mimic.model = Simple()
        inputs = set(mimic.list_inputs())
        outputs = set(mimic.list_outputs())
        self.assertEquals(inputs-mimins, set(['a','b']))
        self.assertEquals(outputs-mimouts, set(['c','d']))
        
        # now put a different model in
        mimic.model = Simple2()
        inputs = set(mimic.list_inputs())
        outputs = set(mimic.list_outputs())
        self.assertEquals(inputs-mimins, set(['w','x']))
        self.assertEquals(outputs-mimouts, set(['y','z']))
        
    def test_in_assembly(self):
        asm = set_as_top(Assembly())
        asm.add('mimic', Mimic())
        asm.add('comp1', Simple())
        asm.add('comp2', Simple())
        asm.mimic.model = Simple()
        asm.connect('comp1.c','mimic.a')
        asm.connect('comp1.d','mimic.b')
        asm.connect('mimic.c','comp2.a')
        asm.connect('mimic.d','comp2.b')
        self.assertEqual(set(asm.list_connections()), 
                         set([('mimic.d', 'comp2.b'), ('mimic.c', 'comp2.a'), 
                              ('comp1.c', 'mimic.a'), ('comp1.d', 'mimic.b')]))
        asm.comp1.a = 1.
        asm.comp1.b = 2.
        asm.run()
        self.assertEqual(asm.comp2.c, 6.)
        self.assertEqual(asm.comp2.d, -2.)
        
        # set new model and verify disconnect
        asm.mimic.model = Simple2()
        self.assertEqual(asm.list_connections(), [])
        
        
    def test_default_execute(self):
        mimic = Mimic()
        mimic.model = Simple()
        simple = Simple()
        
        mimic.a = simple.a = 1.
        mimic.b = simple.b = 2.
        
        simple.run()
        mimic.run()
        
        self.assertEqual(mimic.c, 3.)
        self.assertEqual(mimic.d, -1.)
        self.assertEqual(mimic.c, simple.c)
        self.assertEqual(mimic.d, simple.d)
        
    def test_includes(self):
        mimic = MyMimic()
        mimic.mimic_includes = ['a','d']
        mimic.model = Simple()
        self.assertEqual(mimic.list_inputs_to_model(), ['a'])
        self.assertEqual(mimic.list_outputs_from_model(), ['d'])
        
        # now try changing the includes
        mimic.mimic_includes = ['b', 'c']
        self.assertEqual(mimic.list_inputs_to_model(), ['b'])
        self.assertEqual(mimic.list_outputs_from_model(), ['c'])

    def test_excludes(self):
        mimic = MyMimic()
        mimic.mimic_excludes = ['a','d']
        mimic.model = Simple()
        self.assertEqual(mimic.list_inputs_to_model(), ['b'])
        self.assertEqual(mimic.list_outputs_from_model(), ['c'])
        
        # now try changing the excludes
        mimic.mimic_excludes = ['b', 'c']
        self.assertEqual(mimic.list_inputs_to_model(), ['a'])
        self.assertEqual(mimic.list_outputs_from_model(), ['d'])
        
    def test_include_exclude(self):
        mimic = MyMimic()
        mimic.mimic_includes = ['a','d']
        try:
            mimic.mimic_excludes = ['b','c']
        except RuntimeError as err:
            self.assertEqual(str(err), 
                             ': mimic_includes and mimic_excludes are mutually exclusive')
        else:
            self.fail('Expected RuntimeError')
        self.assertEqual(mimic.mimic_excludes, [])
            
        mimic.mimic_includes = []
        mimic.mimic_excludes = ['b','c']
        try:
            mimic.mimic_includes = ['a','d']
        except Exception as err:
            self.assertEqual(str(err), 
                             ': mimic_includes and mimic_excludes are mutually exclusive')
        else:
            self.fail('Expected Exception')
        self.assertEqual(mimic.mimic_includes, [])
        
if __name__ == "__main__":
    unittest.main()


