
import unittest

from enthought.traits.api import Float, Instance
from openmdao.main.api import Container, Component, Assembly, set_as_top


class DumbContainer(Container):
    v1 = Float(1.)
    v2 = Float(2.)
    v3 = Float(3.)
    
    
class SimpleComp(Component):
    cont_in = Instance(DumbContainer, iostatus='in')
    cont_out = Instance(DumbContainer, iostatus='out')
    f_in = Float(-1., iostatus='in')
    f_out = Float(0., iostatus='out')
    
    def __init__(self, *args, **kwargs):
        super(SimpleComp, self).__init__(*args, **kwargs)
        self.cont_in = DumbContainer()
        self.cont_out = DumbContainer()
    
    def execute(self):
        for name in ['v1', 'v2', 'v3']:
            setattr(self.cont_out, name, 
                    getattr(self.cont_in, name))


class NamespaceTestCase(unittest.TestCase):

    def setUp(self):
        self.asm = set_as_top(Assembly())
        self.asm.add_container('scomp1', SimpleComp())
        self.asm.add_container('scomp2', SimpleComp())
    
    def test_pass_container(self):
        pass
    
    def test_connect_inner_trait(self):
        self.asm.connect('scomp1.cont_out.v1', 'scomp2.cont_in.v3')
            

if __name__ == "__main__":
    unittest.main()

