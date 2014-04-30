import unittest

from openmdao.main.api import Component, Assembly, Driver, set_as_top
from openmdao.main.datatypes.api import Float

class BaseComp(Component):
    # inputs: None
    # outputs: myout
    
    myout = Float(3.14, iotype='out')
        
    def execute(self):
        self.myout *= 0.5
    
class ExtendedComp(BaseComp):
    # inputs: myin
    # outputs: myout, myout2

    myout2 = Float(iotype='out')
    myin = Float(iotype='in')

    def execute(self):
        self.myout = self.myin / 2.0
        self.myout2 = self.myin * 2.0

class TestComp(ExtendedComp):
  
    def execute(self):
        self.myout = self.myin + 10.0
        self.myout2 = self.myin - 10.0

class BaseAsym(Assembly):
    # inputs: None
    # outputs: myout
    def configure(self): 
        self.add('bc', BaseComp())
        self.driver.workflow.add(['bc'])
        self.create_passthrough('bc.myout')

class ExtendedAsym(BaseAsym):
    # inputs: myin
    # outputs: myout, myout2

    def configure(self):
        super(ExtendedAsym, self).configure()
        self.replace('bc', ExtendedComp())
        self.create_passthrough('bc.myin')
        self.create_passthrough('bc.myout2')

class TestAsym(ExtendedAsym):
    # inputs: myin
    # outputs: myout, myout2

    def configure(self):
        super(TestAsym, self).configure()
        self.replace('bc', TestComp())


class LargeAsym(Assembly):
    # inputs: myin
    # outputs: myout, myout2

    def configure(self):  
        self.add('ea', ExtendedAsym())
        self.driver.workflow.add(['ea'])
        self.create_passthrough('ea.myin')
        self.create_passthrough('ea.myout')
        self.create_passthrough('ea.myout2')

class TestLargeAsym(LargeAsym):

    def __init__(self):
        super(TestLargeAsym, self).__init__()

    def configure(self):
        super(TestLargeAsym, self).configure()
        self.replace('ea', TestAsym())


class Replace3TestCase(unittest.TestCase):
    def test_TestAsym(self):
        mytest = set_as_top(TestAsym())
        mytest.myin = 2
        self.assertEqual(mytest.myout, 3.14)
        self.assertEqual(mytest.myout2, 0)
        
        mytest.run()
        
        self.assertEqual(mytest.myin, 2)
        self.assertEqual(mytest.myout, 12)
        self.assertEqual(mytest.myout2, -8)
        
        
    def test_LargeAsym(self):
        largetest = set_as_top(TestLargeAsym())
        largetest.myin = 2
        largetest.run()
        
        self.assertEqual(largetest.myin, 2)
        self.assertEqual(largetest.ea.myin, 2)
        self.assertEqual(largetest.ea.myout, 12)
        self.assertEqual(largetest.ea.myout2, -8)
        self.assertEqual(largetest.myout, 12)
        self.assertEqual(largetest.myout2,-8)
        
        
if __name__=="__main__":
    unittest.main()
    
