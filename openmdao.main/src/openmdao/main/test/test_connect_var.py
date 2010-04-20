#test_connect_var.py
"""
    Testing variables in connect(..,..) statement
       global variables betwwen 2 components.
"""


import unittest
from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.lib.api import Float, Int, Str, Bool
from enthought.traits.api import TraitError


class Oneout(Component):
    """ A simple output component    """
    
    ratio1 = Float(3.54, iotype='out', 
                   desc='Float Variable')
    ratio2 = Int(9, iotype='out', 
                   desc='Integer variable')
    #ratio3 = Bool(False, iotype='out', 
    ratio3 = Bool(True, iotype='out', 
                   desc='Boolean Variable')
    ratio4 = Float(1.03, iotype='out', 
                   desc='Float variable ',units='cm')
    ratio5 = Str('05678', iotype='out', 
                   desc='string variable')

    def __init__(self, doc=None, directory=''):
        
        super(Oneout, self).__init__(doc, directory)        
        
    def execute(self):
        """                                                                    
           execute
        """
        # print '%s.execute()' % self.get_pathname()


class Oneinp(Component):
    """ A simple input component    """
    
    ratio1 = Float(2., iotype='in', 
                   desc='Float   Variable')
    ratio2 = Int(2, iotype='in', 
                   desc='Int Variable')
    ratio3 = Bool(False, iotype='in', 
                   desc='Float Variable')
    ratio4 = Float(1.03, iotype='in', 
                   desc='Float variable ',units='ft')
    ratio5 = Str('01234', iotype='in', 
                   desc='string variable')

    def __init__(self, doc=None, directory=''):
        
        super(Oneinp, self).__init__(doc, directory)        
        


    def execute(self):
        """                                                                    
           execute
        """
        # print '%s.execute()' % self.get_pathname()
        

class VariableTestCase(unittest.TestCase):

    def setUp(self):
        """ this function is used to test each type..."""
        self.top = set_as_top(Assembly())
        self.top.add_container('oneinp', Oneinp())
        self.top.add_container('oneout', Oneout())

    def test_var1(self):
        #  connect to same type variables....
        self.top.connect('oneout.ratio1','oneinp.ratio1')      # float to float
        self.top.connect('oneout.ratio2','oneinp.ratio2')      # int   to int
        self.top.connect('oneout.ratio3','oneinp.ratio3')      # Bool  to Bool
        self.top.connect('oneout.ratio4','oneinp.ratio4')      # float with units to float with unit
        self.top.connect('oneout.ratio5','oneinp.ratio5')      # Str   to Str
        self.top.run()
        self.assertEqual(3.54,self.top.oneinp.ratio1)
        self.assertEqual(9 ,self.top.oneinp.ratio2)
        self.assertEqual(True,self.top.oneinp.ratio3)
        self.assertAlmostEqual(0.033792,self.top.oneinp.ratio4,5)
        self.assertEqual('05678',self.top.oneinp.ratio5)
    
        #print  'top dict =',self.top.__dict__


    def test_var2(self):
        self.top.oneout.ratio2 = 11
        self.top.connect('oneout.ratio2','oneinp.ratio1')      # int  to  Float 
        self.top.oneout.ratio3 = True
        self.top.connect('oneout.ratio3','oneinp.ratio2')      # Bool to  int    
        self.top.run()
        self.assertEqual(11.0,self.top.oneinp.ratio1)
        self.assertEqual(True,self.top.oneinp.ratio2)

    def test_var3(self):
        self.top.oneout.ratio3 = False
        self.top.connect('oneout.ratio3','oneinp.ratio1')      # Bool to  Float 
        self.top.connect('oneout.ratio3','oneinp.ratio2')      # Bool to  int    
        self.top.run()
        self.assertEqual(0.0,self.top.oneinp.ratio1)
        self.assertEqual(False,self.top.oneinp.ratio2)

    def test_var4(self):
        self.top.oneout.ratio1 = 12.0   
        try:
            self.top.connect('oneout.ratio1','oneinp.ratio2')  # float to int   
            #self.top.run( )                                       
        except TraitError, err:
            msg = "a value of 12.0 <type 'float'> was specified"
            self.assertTrue( msg in str(err))
        else:
            self.fail('TraitError Expected')

    def test_var5(self):
        self.top.oneout.ratio1 = 12.0   
        try:
            self.top.connect('oneout.ratio1','oneinp.ratio3')  # float to Bool  
            #self.top.run( )                                       
        except TraitError, err:
            msg = "The 'ratio3' trait of an Oneinp instance must be a"\
                  " boolean, but a value of 12.0 <type 'float'> was specified."
            self.assertEqual(str(err), msg)
        else:
            self.fail('TraitError Expected')

    def test_var6(self):
        self.top.oneout.ratio1 = 12.0   
        try:
            self.top.connect('oneout.ratio1','oneinp.ratio5')  # float to Str   
            #self.top.run( )                                       
        except TraitError, err:
            msg = "The 'ratio5' trait of an Oneinp instance must be a"\
                  " string, but a value of 12.0 <type 'float'> was specified."
            self.assertEqual(str(err), msg)
        else:
            self.fail('TraitError Expected')

    def test_var7(self):
        self.top.oneout.ratio2 = 20     
        try:
            self.top.connect('oneout.ratio2','oneinp.ratio3')  # int to Bool    
            #self.top.run( )                                       
        except TraitError, err:
            msg = "The 'ratio3' trait of an Oneinp instance must be a"\
                  " boolean, but a value of 20 <type 'int'> was specified."
            self.assertEqual(str(err), msg)
        else:
            self.fail('TraitError Expected')

    def test_var8(self):
        self.top.oneout.ratio2 = 20     
        try:
            self.top.connect('oneout.ratio2','oneinp.ratio5')  # int to Str     
            #self.top.run( )                                       
        except TraitError, err:
            msg = "The 'ratio5' trait of an Oneinp instance must be a"\
                  " string, but a value of 20 <type 'int'> was specified."
            self.assertEqual(str(err), msg)
        else:
            self.fail('TraitError Expected')

    def test_var9(self):
        self.top.oneout.ratio3 = True   
        try:
            self.top.connect('oneout.ratio3','oneinp.ratio5')  # Bool to Str    
            #self.top.run( )                                       
        except TraitError, err:
            msg = "The 'ratio5' trait of an Oneinp instance must be a"\
                  " string, but a value of True <type 'bool'> was specified."
            self.assertEqual(str(err), msg)
        else:
            self.fail('TraitError Expected')


    def test_var10(self):
        self.top.oneout.ratio5 = '55555'
        try:
            self.top.connect('oneout.ratio5','oneinp.ratio2')  # Str to int
            #self.top.run( ) 
        except TraitError, err:
            msg = "a value of 55555 <type 'str'> was specified"
            self.assertTrue( msg in str(err))
        else:
            self.fail('TraitError Expected')

    def test_var11(self):
        self.top.oneout.ratio5 = '55555'
        try:
            self.top.connect('oneout.ratio5','oneinp.ratio1')  # Str to Float
            #self.top.run( ) 
        except TraitError, err:
            msg = "oneinp: Trait 'ratio1' must be a float, but a value of 55555" + \
                      " <type 'str'> was specified."
            self.assertEqual(str(err), msg)
        else:
            self.fail('TraitError Expected')

    def test_var12(self):
        self.top.oneout.ratio5 = '55555'
        try:
            self.top.connect('oneout.ratio5','oneinp.ratio3')  # Str to Bool
            #self.top.run( ) 
        except TraitError, err:
            msg = "The 'ratio3' trait of an Oneinp instance must be a boolean, but a"\
                  " value of '55555' <type 'str'> was specified."
            self.assertEqual(str(err), msg)
        else:
            self.fail('TraitError Expected')


if __name__ == "__main__":
    unittest.main()

