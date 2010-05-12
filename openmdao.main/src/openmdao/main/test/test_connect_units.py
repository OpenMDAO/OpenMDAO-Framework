#test_connect_units.py
"""
    Testing units in connect(..,..) statement
       global variables betwwen 2 components.
    note:  check the units symbol in appendix
"""


import unittest
from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.lib.api import Float, Int, Str, Bool
from enthought.traits.api import TraitError


class Oneout(Component):
    """ A simple output component    """

    # pressure units
    ratio1 = Float(1.00, iotype='out',
                   desc='Float Variable',units='Pa')         #   pressure units
    ratio2 = Float(1, iotype='out',
                   desc='Float variable', units= 'mi')       #   Mile
    ratio3 = Float(1, iotype='out',
                   desc='Float variable', units= 'degF')     #   temperature in F
    ratio4 = Float(8.314472, iotype='out',
                   desc='Float variable ',units='J/(mol*degK)') # R, gas const
    ratio5 = Float(1, iotype='out',
                   desc='Float variable', units= 'degC')      #   temperature in C
    ratio6 = Float(1, iotype='out',
                   desc='Float variable', units= 'N')         #   force in N
    ratio7 = Float(1, iotype='out',
                   desc='Float variable', units= 'lbf')       #   force in lbf
    ratio8 = Float(1, iotype='out',
                   desc='Float variable', units= 'kg')        #   mass  in kg 
    ratio9 = Float(1, iotype='out',
                   desc='Float variable')                     #   no units defined 
    ratio10 = Float(1, iotype='out',
                   desc='Float variable', units= 'rad')       #   angle in rad 

    def __init__(self, doc=None, directory=''):

        super(Oneout, self).__init__(doc, directory)

    def execute(self):
        """                                                                    
           execute
        """
        # priny '%s.execute()' % self.get_pathname()


class Oneinp(Component):
    """ A simple input component    """

    ratio1 = Float(1.00, iotype='in',
                   desc='Float Variable',units='mm')    # not a pressure unit
    ratio2 = Float(1.00, iotype='in',
                   desc='Float Variable',units='atm')   #  pressure in atm
    ratio3 = Float(1.00, iotype='in',
                   desc='Float Variable',units='torr')  #  pressure in torr 
    #ratio4 = Float(1.00, iotype='in',
    #               desc='Float Variable',units='N')    # newton
    ratio4 = Float(1.00, iotype='in',
                   desc='Float Variable',units='psi')    # pressure in psi 
    ratio5 = Float(1.00, iotype='in',
                   desc='Float Variable',units='bar')    # pressure in bar
    ratio6 = Float(1.00, iotype='in',
                   desc='Float variable ',units='cal/(mol*degK)' )  #R, Gas constant
    ratio7 = Float(1.00, iotype='in',
                   desc='Float variable ',units='m' )    # Meter
    ratio8 = Float(1.00, iotype='in',
                   desc='Float variable ',units='mm' )   # millimeter
    ratio9 = Float(1, iotype='in',
                   desc='Float variable', units= 'degR') # temp in R
    ratio10 = Float(1, iotype='in',
                   desc='Float variable', units= 'degK') #  temp in K
    ratio11 = Float(1, iotype='in',
                   desc='Float variable', units= 'dyn')  #   force in dyn
    ratio12 = Float(1, iotype='in',
                   desc='Float variable', units= 'N')    #   force in N
    ratio13 = Float(1, iotype='in',
                   desc='Float variable', units= 'dyn')  #   force in dyn
    ratio14 = Float(1, iotype='in',
                   desc='Float variable', units= 'lb')    #   mass in lb   
    ratio15 = Float(2, iotype='in',
                   desc='Float variable' )                 #  no units defined 
    ratio16 = Float(2, iotype='in',
                   desc='Float variable', units= 'dyn')   #   force in dyn  
    ratio17 = Float(1, iotype='in',
                   desc='Float variable', units= 'deg')   #   angle in deg 

    def __init__(self, doc=None, directory=''):

        super(Oneinp, self).__init__(doc, directory)


    def execute(self):
        """                                                                    
           execute
        """
        # print '%s.execute(degK' % self.get_pathname()


class VariableTestCase(unittest.TestCase):

    def setUp(self):
        """ this function is used to test each type..."""
        self.top = set_as_top(Assembly())
        self.top.add_container('oneinp', Oneinp())
        self.top.add_container('oneout', Oneout())

    def test_var1(self):
        #  connect to same type variables....
        #self.top.connect('oneout.ratio1','oneinp.ratio1')      # float to float
        self.top.connect('oneout.ratio1','oneinp.ratio2')       # Pa  to atm
        self.top.connect('oneout.ratio1','oneinp.ratio3')       # Pa  to torr
        self.top.connect('oneout.ratio1','oneinp.ratio5')       # Pa  to bar  
        self.top.connect('oneout.ratio4','oneinp.ratio6')       # gas constant, R
        self.top.connect('oneout.ratio2','oneinp.ratio7')       # Mile to meter  
        self.top.connect('oneout.ratio2','oneinp.ratio8')       # Mile to millimeter  
        self.top.connect('oneout.ratio3','oneinp.ratio9')       # temp F to R         
        self.top.connect('oneout.ratio5','oneinp.ratio10')      # temp C to rK        
        self.top.connect('oneout.ratio6','oneinp.ratio11')      # force  N to dyn     
        self.top.connect('oneout.ratio7','oneinp.ratio12')      # force  lbf to N     
        self.top.connect('oneout.ratio7','oneinp.ratio13')      # force  lbf to dyn     
        self.top.connect('oneout.ratio8','oneinp.ratio14')      # mass kg  to lb        
        self.top.connect('oneout.ratio7','oneinp.ratio15')      # force  lbf to no units
        self.top.connect('oneout.ratio9','oneinp.ratio16')      # no units to dyn       
        self.top.connect('oneout.ratio10','oneinp.ratio17')     # rad to deg


        self.top.run()

        #print  ' oneinp_ratio1= ',self.top.oneinp.ratio1
        #print  ' oneinp_ratio2( Pa/ atm)= ',self.top.oneinp.ratio2
        #print  ' oneinp_ratio3( Pa/ torr)= ',self.top.oneinp.ratio3
        #print  ' oneinp_ratio4( Pa/ psi) = ',self.top.oneinp.ratio4
        #print  ' oneinp_ratio5( Pa/ bar) = ',self.top.oneinp.ratio5
        #print  ' oneinp_ratio6( J / cal) = ',self.top.oneinp.ratio6
        #print  ' oneinp_ratio7(mile/meter)= ',self.top.oneinp.ratio7
        #print  ' oneinp_ratio8(mile/mm)= ',self.top.oneinp.ratio8
        #print  ' oneinp_ratio9(F / R  )=  ',self.top.oneinp.ratio9
        #print  ' oneinp_ratio10(C / K  )= ',self.top.oneinp.ratio10
        #print  ' oneinp_ratio11(N / dyn)= ',self.top.oneinp.ratio11
        #print  ' oneinp_ratio12(lbf / N)= ',self.top.oneinp.ratio12
        #print  ' oneinp_ratio13(lbf / dyn)= ',self.top.oneinp.ratio13
        #print  ' oneinp_ratio14(kg  / lb )= ',self.top.oneinp.ratio14
        #print  ' oneinp_ratio15(lbf / no units   )= ',self.top.oneinp.ratio15
        #print  ' oneinp_ratio16(no units / dyn )= ',self.top.oneinp.ratio16
        #print  ' oneinp_ratio17(rad / deg        )= ',self.top.oneinp.ratio17

        #self.assertEqual(3.54,self.top.oneinp.ratio1)
        self.assertAlmostEqual(9.86923266716e-06,self.top.oneinp.ratio2,6)
        self.assertAlmostEqual(0.00750061682704,self.top.oneinp.ratio3,6)
        self.assertEqual(1e-05,self.top.oneinp.ratio5)
        self.assertAlmostEqual(1.98720650096,self.top.oneinp.ratio6, 8)
        self.assertEqual(1609.344,self.top.oneinp.ratio7)
        self.assertEqual(1609344.0,self.top.oneinp.ratio8)
        self.assertEqual( 460.67 ,self.top.oneinp.ratio9)
        self.assertEqual( 274.15 ,self.top.oneinp.ratio10)
        self.assertAlmostEqual( 100000.0 ,self.top.oneinp.ratio11 , 8)
        self.assertEqual(4.44822162  ,self.top.oneinp.ratio12)
        self.assertAlmostEqual( 444822.162 ,self.top.oneinp.ratio13,6)
        self.assertAlmostEqual(2.20462262185 ,self.top.oneinp.ratio14,6)
        self.assertEqual(1.0 ,self.top.oneinp.ratio15)
        self.assertEqual(1.0 ,self.top.oneinp.ratio16)
        self.assertAlmostEqual(57.295779578,self.top.oneinp.ratio17,6)

        #print  'top dict =',self.top.__dict__



    def test_unit2(self):
        self.top.oneout.ratio1 = 20
        try:
            self.top.connect('oneout.ratio1','oneinp.ratio1')      # Pa  to mm       
            self.top.run( )
        except TraitError, err:
            #msg = "oneinp: Cannot locate trait named 'ratio1'"
            msg = ": cannot set 'oneinp.ratio1' from 'oneout.ratio1': ratio1: units 'Pa' are "\
                  "incompatible with assigning units of 'mm'"
            self.assertEqual(str(err), msg)
        else:
            self.fail('TraitError Expected')

#   def test_unit3(self):
#       self.top.oneout.ratio9 = 20
#       try:
#           self.top.connect('oneout.ratio9','oneinp.ratio16')      # no units to dyn     
#           print  ' oneinp_ratio16(no units/ dyn   )= ',self.top.oneinp.ratio16
#           #self.top.run( )
#       # except TraitError, err:
#       except TypeError, err:
#           msg =": cannot set 'oneinp.ratio16' from 'oneout.ratio9': None is not a unit"
#           self.assertEqual(str(err), msg)
#       else:
#           self.fail('TraitError Expected')


if __name__ == "__main__":
    unittest.main()


