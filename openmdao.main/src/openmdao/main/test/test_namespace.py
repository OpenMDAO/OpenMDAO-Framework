
import unittest

from enthought.traits.api import Float, Instance
from openmdao.main.api import Container, Component, Assembly, set_as_top

class DumbContainer2(Container):
    x = Float(1., iostatus='in')
    y = Float(2., iostatus='in')
    z = Float(3., iostatus='in')
    
class DumbContainer(Container):
    v1 = Float(1., iostatus='in')
    v2 = Float(2., iostatus='in')
    v3 = Float(3., iostatus='in')
    def __init__(self, *args, **kwargs):
        super(DumbContainer, self).__init__(*args, **kwargs)
        self.add_container('cont', DumbContainer2())
    
    
class SimpleComp(Component):
    cont_in = Instance(DumbContainer, iostatus='in')
    cont_out = Instance(DumbContainer, iostatus='out')
    mult = Float(1., iostatus='in')
    
    def __init__(self, *args, **kwargs):
        super(SimpleComp, self).__init__(*args, **kwargs)
        self.add_container('cont_in', DumbContainer())
        self.add_container('cont_out', DumbContainer())
    
    def execute(self):
        for name in ['v1', 'v2', 'v3']:
            setattr(self.cont_out, name, 
                    self.mult*getattr(self.cont_in, name))
        for name in ['x', 'y', 'z']:
            setattr(self.cont_out.cont, name, 
                    self.mult*getattr(self.cont_in.cont, name))


class NamespaceTestCase(unittest.TestCase):

    def setUp(self):
        self.asm = set_as_top(Assembly())
        self.asm.add_container('scomp1', SimpleComp())
        self.asm.add_container('scomp2', SimpleComp())
    
    def test_pass_container(self):
        #scomp1                   scomp2
            #cont_in         /------->cont_in
                #v1          |           v1
                #v2          |           v2
                #v3          |           v3
                #cont        |           cont
                    #x       |               x
                    #y       |               y
                    #z       |               z
            #cont_out--------/        cont_out
                #v1                      v1
                #v2                      v2
                #v3                      v3
                #cont                    cont
                    #x                       x
                    #y                       y
                    #z                       z
        self.asm.connect('scomp1.cont_out', 'scomp2.cont_in')
        self.asm.scomp1.mult = 2.0
        self.asm.run()
        cont_out = self.asm.scomp1.cont_out
        cont_in = self.asm.scomp2.cont_in
        #self.assertFalse(cont_in is cont_out)
        self.assertEqual(cont_out.v1, cont_in.v1)
        
    def test_connect_namespace(self):
        self.asm.connect('scomp1.cont_out.v1', 'scomp2.cont_in.v3')
        self.asm.connect('scomp1.cont_out.v3', 'scomp2.cont_in.v1')
        self.asm.scomp1.mult = 2.0
        self.asm.run()
        self.assertEqual(self.asm.scomp1.cont_out.v1, self.asm.scomp2.cont_in.v3)
        self.assertEqual(self.asm.scomp1.cont_out.v2, 2.0*self.asm.scomp2.cont_in.v2)
        self.assertEqual(self.asm.scomp1.cont_out.v3, self.asm.scomp2.cont_in.v1)

    def test_connect_nested(self):
        self.asm.connect('scomp1.cont_out.cont.x', 'scomp2.cont_in.v3')
        self.asm.connect('scomp1.cont_out.v3', 'scomp2.cont_in.cont.z')
        self.asm.scomp1.mult = 2.0
        self.asm.run()
        self.assertEqual(self.asm.scomp1.cont_out.cont.x, self.asm.scomp2.cont_in.v3)
        self.assertEqual(self.asm.scomp1.cont_out.v2, 2.0*self.asm.scomp2.cont_in.v2)
        self.assertEqual(self.asm.scomp1.cont_out.v3, self.asm.scomp2.cont_in.cont.z)

if __name__ == "__main__":
    unittest.main()

