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

class MimicTestCase(unittest.TestCase):
        
    def test_simple(self):
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
        self.fail("includes")
        
    def test_excludes(self):
        self.fail("excludes")
        
if __name__ == "__main__":
    unittest.main()


