# pylint: disable-msg=C0111,C0103

import unittest

from enthought.traits.api import TraitError
from openmdao.main.api import Assembly, Component, Driver, set_as_top
from openmdao.lib.datatypes.api import Float, Str, Instance, List
from openmdao.util.decorators import add_delegate
from openmdao.main.hasobjective import HasObjective
import StringIO

class Multiplier(Component):
    rval_in = Float(iotype='in')
    rval_out = Float(iotype='out')
    mult = Float(iotype='in')
    
    def __init__(self):
        super(Multiplier, self).__init__()
        self.rval_in = 4.
        self.rval_out = 7.
        self.mult = 1.5

    def execute(self):
        self.rval_out = self.rval_in * self.mult
        
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


class SaveAsClassTestCase(unittest.TestCase):

    def setUp(self):
        pass
    
    def test_save_as_class(self):
        top = set_as_top(Assembly())
        comp1 = top.add('comp1', Multiplier())
        comp2 = top.add('comp2', Multiplier())
        
        top.driver.workflow.add(['comp1', 'comp2'])
        
        top.comp1.mult = 2.0
        top.comp2.mult = 4.0
        top.connect('comp1.rval_out', 'comp2.rval_in')
        top.comp1.rval_in = 5.0
        
        stream = StringIO.StringIO()
        save_as_class(top, stream)

        self.assertEqual(stream.getvalue(), '')

        
if __name__ == "__main__":
    unittest.main()


