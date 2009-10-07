import units
import unittest
import numpy

from pkg_resources import resource_string, resource_stream

#---------------------------------------------------------------------
#---------------------------------------------------------------------
#---------------------------------------------------------------------
#---------------------------------------------------------------------
#---------------------------------------------------------------------
#---------------------------------------------------------------------

# the following class test each method in the NumberDict class

class test_moduleLevelFunctions(unittest.TestCase):
    def test_importlib(self):
        
        #check to make sure the import fucntion errors if not all required base_units are there
        unitLib_test_bad = resource_stream(units.__name__, 'test//unitLib_test_badbaseunits.ini')
        try:
            units.importLibrary(unitLib_test_bad)
        except ValueError,err:
            self.assertEqual(str(err),"Not all required base type were present in the config file. missing: ['mass', 'time'], at least ['length', 'mass', 'time', 'temperature', 'angle'] required")
        except:
            self.fail("ValueError expected")
        #check to make sure that bad units in the units list cause an error    
        unitLib_test_bad = resource_stream(units.__name__, 'test//unitLib_test_badunit.ini')
        try:
            units.importLibrary(unitLib_test_bad)
        except ValueError,err:
            self.assertEqual(str(err),"The following units were not defined because they could not be resolved as a function of any other defined units:['foo']")
        except:
            self.fail("ValueError expected")
            
        
class test_NumberDict(unittest.TestCase):

    def test__UknownKeyGives0(self):
        """a NumberDict instance should initilize using integer and non-integer indices
        a NumberDict instance should initilize all entries with an initial value of 0"""
        x=units.NumberDict()
        #integer test
        self.assertEqual(x[0],0)
        #string test
        self.assertEqual(x['t'],0)
        
    def test__add__KnownValues(self):
        """__add__ should give known result with known input
        for non-string data types, addition must be commutative"""
        x=units.NumberDict()
        y=units.NumberDict()
        x['t1'],x['t2']= 1,2
        y['t1'],y['t2']=2,1
        result1,result2=x+y,y+x
        self.assertEqual((3,3), (result1['t1'],result1['t2']))
        self.assertEqual((3,3), (result2['t1'],result2['t2']))

    def test__sub__KnownValues(self):
        """__sub__ should give known result with known input
        commuting the input should result in equal magnitude, opposite sign"""

        x=units.NumberDict()
        y=units.NumberDict()
        x['t1'],x['t2']= 1,2
        y['t1'],y['t2']= 2,1
        result1,result2=x-y,y-x
        self.assertEqual((-1,1), (result1['t1'],result1['t2']))
        self.assertEqual((1,-1), (result2['t1'],result2['t2']))

    def test__mul__KnownValues(self):
        """__mul__ should give known result with known input"""
        x=units.NumberDict([('t1',1),('t2',2)])
        y=10
        result1,result2=x*y,y*x
        self.assertEqual((10,20), (result1['t1'],result1['t2']))
        self.assertEqual((10,20), (result2['t1'],result2['t2']))




    def test__div__KnownValues(self):
        """__div__ should give known result with known input"""
        x=units.NumberDict()
        x=units.NumberDict([('t1',1),('t2',2)])
        y=10.0
        result1=x/y
        self.assertEqual((.1,.20), (result1['t1'],result1['t2']))
        
       

#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------

class test__PhysicalQuantity(unittest.TestCase):
    

    def test_init(self):
        """__init__ should have the same result regardless of the constructor calling pattern"""
    
 
        x=units.PhysicalQuantity('1m')
        y=units.PhysicalQuantity(1,'m')
        self.assertEqual(x.value,y.value)
        self.assertEqual(x.unit,y.unit)
        
        z=units.PhysicalQuantity('1dam') #check for two letter prefixes
        
        #error for improper init argument
        try:
            x=units.PhysicalQuantity('m')
        except TypeError,err:
            self.assertEqual(str(err),"No number found in input argument: 'm'")
        else:
            self.fail("Expecting TypeError")
            
        try:
            x=units.PhysicalQuantity('1in')
        except ValueError,err:
            self.assertEqual(str(err),"no unit named 'in' is defined")
        else:
            self.fail("Expecting ValueError")
            
        try:
            x=units.PhysicalQuantity(1,None)
        except TypeError,err:
            self.assertEqual(str(err),"None is not a unit")
        else:
            self.fail("Expecting TypeError")            

    def test_str(self):
        """__str__ """
        self.assertEqual(str(units.PhysicalQuantity('1 d')),"1.0 d")

    def test_repr(self):
        """__repr__ """
        self.assertEqual(repr(units.PhysicalQuantity('1 d')),"PhysicalQuantity(1.0,'d')")

    def test_cmp(self):
        """__cmp__ """
        x=units.PhysicalQuantity('1 d')
        y = units.PhysicalQuantity('2 d')
        self.assertEqual(cmp(x,y),-1)
        self.assertEqual(cmp(y,x),1)
        self.assertEqual(cmp(x,x),0)

        try:
            cmp(x,2)
        except TypeError,err:
            self.assertEqual("Incompatible types",str(err))
        else:
            self.fail('Expecting TypeError')

    def test_integers_in_unit_string(self):
        x = units.PhysicalQuantity('1 1/min')
        self.assertAlmostEqual(x.unit.factor,0.0166666,places=5)
        self.assertEqual(x.unit.names,{'1': 1, 'min': -1})
        self.assertEqual(x.unit.powers,[0, 0, 0, 0, 0, 0, 0, 0, -1, 0])


    def test_add_known_Values(self):
        """addition should give known result with known input. 
        The resulting unit should be the same as the unit of the calling instance
        The units of the results should be the same as the units of the calling instance"""
        
        #test addition function for allowed addition values
        known_add_values=(('1m','5m',6),('1cm','1cm',2),('1cm','1ft',31.48))
        for q1,q2,result in known_add_values:
            x=units.PhysicalQuantity(q1)
            y=units.PhysicalQuantity(q2)
            sum = x+y
            self.assertEqual(sum.value,result)
            self.assertEqual(sum.unit,x.unit)

        #test for error if incompatible units
        q1 = units.PhysicalQuantity('1cm')
        q2 = units.PhysicalQuantity('1kg')
        
        try: 
            q1 + q2
        except TypeError,err:
            self.assertEqual(str(err),"Incompatible units")
        else: 
            self.fail("expecting TypeError")
            
        #test offset units
        q1 = units.PhysicalQuantity('1degK')
        q2 = units.PhysicalQuantity('1degR')
        q3 = units.PhysicalQuantity('1degC')
        
        result = q1 + q2
        self.assertAlmostEqual(result.value,1.556,3)
        self.assertEqual(result.unit, q1.unit)
        
        try:
            q3 + q2
        except TypeError, err: 
            self.assertEqual(str(err),"Unit conversion (degR to degC) cannot be expressed as a simple multiplicative factor")
        else: 
            self.fail('expecting TypeError')
        
            

    def test_sub_known_Values(self):
        """subtraction should give known result with known input
        __rsub__ should give the negative of __sub__
        the units of the results should be the same as the units of the calling instance"""
        
        known_sub_Values=(('1m','5m',-4),('1cm','1cm',0),('1cm','5m',-499.0),('7km','1m',6.999))
        for q1,q2, sum in known_sub_Values:
            x=units.PhysicalQuantity(q1)
            y=units.PhysicalQuantity(q2)
            self.assertEqual((x-y).value,sum)
            self.assertEqual((x-y).unit,x.unit)
            self.assertEqual(x.__rsub__(y).value,-sum)
            self.assertEqual(x.__rsub__(y).unit,x.unit)

        #test for error if incompatible units
        q1 = units.PhysicalQuantity('1cm')
        q2 = units.PhysicalQuantity('1kg')
        
        try: 
            q1 - q2
        except TypeError,err:
            self.assertEqual(str(err),"Incompatible units")
        else: 
            self.fail("expecting TypeError")
            
        #test offset units
        q1 = units.PhysicalQuantity('1degK')
        q2 = units.PhysicalQuantity('1degR')
        q3 = units.PhysicalQuantity('1degC')
        
        result = q1 - q2
        self.assertAlmostEqual(result.value,.444,3)
        self.assertEqual(result.unit, q1.unit)
        
        try:
            q3 - q2
        except TypeError, err: 
            self.assertEqual(str(err),"Unit conversion (degR to degC) cannot be expressed as a simple multiplicative factor")
        else: 
            self.fail('expecting TypeError')            
            

    def test_mul_known_Values(self):
        """multiplication should give known result with known input
        the unit of the product should be the product of the units"""

        #PhysicalQuanity * scalar
        x=units.PhysicalQuantity('1cm')
        y = 12.3
        self.assertEqual(x*y,units.PhysicalQuantity('12.3cm'))
        self.assertEqual(y*x,units.PhysicalQuantity('12.3cm'))
        
        #PhysicalQuantity * PhysicalQuantity
        x=units.PhysicalQuantity('1cm')
        y=units.PhysicalQuantity('1cm')
        z=units.PhysicalQuantity('1cm**-1')
        self.assertEqual((x*y).value,1)
        self.assertEqual((x*y).unit,x.unit*y.unit)
        self.assertEqual(str(x*y),'1.0 cm**2')
        
        #multiplication where the result is dimensionless
        self.assertEqual((x*z),1.0)
        self.assertEqual(type(x*z),float)
        self.assertEqual(str(x*z),'1.0')
        
        x=units.PhysicalQuantity('7kg')
        y=units.PhysicalQuantity('10.5m')
        self.assertEqual((x*y).value,73.5)
        self.assertEqual((x*y).unit,x.unit*y.unit)
        self.assertEqual(str(x*y),'73.5 m*kg')
        
        #test for error from offset units
        z = units.PhysicalQuantity('1degC')
        try: 
            x*z
        except TypeError,err: 
            self.assertEqual(str(err),"cannot multiply units with non-zero offset")
        else: 
            self.fail("TypeError expected")
    
    

    def test_div_known_Values(self):
        """__div__ should give known result with known input"""
        """the unit of the product should be the product of the units"""
        
        #scalar division
        x=units.PhysicalQuantity('1cm')
        y = 12.3
        z = 1/12.3
        self.assertAlmostEqual((x/y).value,units.PhysicalQuantity('%f cm'%z).value,4)
        self.assertEqual((x/y).unit,units.PhysicalQuantity('%f cm'%z).unit)
        self.assertEqual(y/x,units.PhysicalQuantity('12.3cm**-1'))
        
        #unitless result
        x=units.PhysicalQuantity('1.0m')
        y=units.PhysicalQuantity('5m')
        quo = 1.0/5
        # if quotient is unit-less (that is, x and y are additively compatible)
        # re-arranges x & y in terms of the known quotient and __rdiv__ and checks for consistency
        self.assertEqual((x/y),quo)
        self.assertEqual(x.__rdiv__(y),1/quo)
        self.assertEqual(type(x/y),float)
        
        x=units.PhysicalQuantity('3cm')
        y=units.PhysicalQuantity('5s')
        quo = 3.0/5
        # if quotient has a unit (x and y are additively incompatible)
        # re-arranges x & y in terms of the known quotient and __rdiv__ and checks for consistency
        self.assertEqual((x/y).value,quo)
        self.assertEqual(x.__rdiv__(y).value,1/quo)

        self.assertEqual((x/y).unit,x.unit/y.unit)
        self.assertEqual(x.__rdiv__(y).unit,y.unit/x.unit)
        self.assertEqual(str(x/y),'0.6 cm/s')
        
    def test_pow_known_Values(self):
        """__pow__ should give known result with known input
        the unit of the power should be the power of the input units"""
        
        #test integer exponent
        x=units.PhysicalQuantity('5V')
        self.assertEqual((x**2).value,5**2)
        self.assertEqual((x**2).unit,x.unit**2)
        
        #test for inverse integer exponent
        x = units.PhysicalQuantity('1m**2')
        y = units.PhysicalQuantity('1m')
        self.assertEqual(x**(1.0/2.0),y)
        self.assertEqual(x/y,y)
        
        #test for error from non integer exponent
        try: 
            x**2.5
        except TypeError,err: 
            self.assertEqual(str(err),"Only integer and inverse integer exponents allowed")
        else: 
            self.fail("Expecting TypeError")
            
        #test for error on offset units
        x=units.PhysicalQuantity('1degC')
        try: 
            x**2
        except TypeError, err: 
            self.assertEqual(str(err),'cannot exponentiate units with non-zero offset')
        else: 
            self.fail("expected TypeError")
            
        #test for error if exponent is a PhysicalQuantity
        try: 
            x**x
        except TypeError, err: 
            self.assertEqual(str(err),'Exponents must be dimensionless')
        else: 
            self.fail("expected TypeError")
        try: #__rpow__
            2**x
        except TypeError, err: 
            self.assertEqual(str(err),'Exponents must be dimensionless')
        else: 
            self.fail("expected TypeError")            
        
    def test_abs_known_Values(self):
        """__abs__ should give known result with known input"""

        x=units.PhysicalQuantity('-5V')
        self.assertEqual(abs(x).unit,x.unit)
        self.assertEqual(abs(x).value,5)
        
        x=units.PhysicalQuantity('5V')
        self.assertEqual(abs(x).unit,x.unit)
        self.assertEqual(abs(x).value,5)

    def test_pos_known_Values(self):
        """should retain sign for value of physical quantity"""

        x=units.PhysicalQuantity('5V')
        self.assertEqual((+x).value,5)
        x=units.PhysicalQuantity('-9.8m')
        self.assertEqual((+x).value,-9.8)

    def test_neg_known_Values(self):
        """__neg__ should flip sign of value for physical quantity"""

        x=units.PhysicalQuantity('5V')
        self.assertEqual((-x).value,-5)
        x=units.PhysicalQuantity('-9.8m')
        self.assertEqual((-x).value,9.8)

    def test_sqrt_known_Values(self):
        """__sqrt__ should give known result with known input"""
        
        x=units.PhysicalQuantity('5V')
        self.assertEqual((x*x).sqrt(),x)

    
    def test_sin_cos_tan_known_Values(self):
        """__sin__ should give known result with known input"""
        
        x=units.PhysicalQuantity('0 rad')
        x.sin()
        self.assertEqual(x.sin(),numpy.sin(x.value))
        self.assertEqual(x.cos(),numpy.cos(x.value))
        self.assertEqual(x.tan(),numpy.tan(x.value))
        
        x=units.PhysicalQuantity('1m')
        try:
            x.sin()  
        except TypeError,err: 
            self.assertEqual(str(err),"Argument of sin must be an angle")
        else:
            self.fail("TypeError expected")
            
        try:
            x.cos()  
        except TypeError,err: 
            self.assertEqual(str(err),"Argument of cos must be an angle")
        else:
            self.fail("TypeError expected")

        try:
            x.tan()  
        except TypeError,err: 
            self.assertEqual(str(err),"Argument of tan must be an angle")
        else:
            self.fail("TypeError expected")
        

    def test_nonzero(self):
        """__nonzero__ should return true in a boolean test"""
        
        x=units.PhysicalQuantity('1degK')
        self.assertTrue(x)

    def test_convertToUnit(self):
        """convertToUnit should change the unit of the calling instance to the requested new unit"""

        x=units.PhysicalQuantity('5cm')
        x.convertToUnit('m')
        self.assertEqual(x,units.PhysicalQuantity('0.05m'))
        
        #Test for no compatible units
        x=units.PhysicalQuantity('5cm')
        try:
            x.convertToUnit('kg')
        except TypeError,err:
            self.assertEqual(str(err),'Incompatible units')
        else: 
            self.fail("TypeError expected")

    def test_inUnitsOf(self):
        """inUnitsOf should return a new PhysicalQuantity with the requested unit, leaving the old unit as it was"""

        x=units.PhysicalQuantity('5cm')
        y = x.inUnitsOf('m')
        self.assertEqual(y,units.PhysicalQuantity('0.05m'))
        self.assertEqual(x,units.PhysicalQuantity('5cm'))
        
        x=units.PhysicalQuantity('5cm')
        try:
            y = x.inUnitsOf('degC')
        except TypeError,err:
            self.assertEqual(str(err),'Incompatible units')
        else: 
            self.fail("TypeError expected")    
        

    def test_inBaseUnits(self):
        """inBaseUnits() should return a new PhysicalQuantity instance
        using the base units, leaving the original instance intact"""

        x = units.PhysicalQuantity(1,'1/h')
        y = x.inBaseUnits()
        
        self.assertEqual(y,units.PhysicalQuantity(1/3600.0,'1/s'))
        self.assertEqual(x,units.PhysicalQuantity(1,'1/h'))   
        
        x = units.PhysicalQuantity(1,'ft**-3')
        y = x.inBaseUnits()
        self.assertEqual(y,units.PhysicalQuantity(35.314666721488585,'1/m**3'))         
        
        x = units.PhysicalQuantity(1,'ft**3')
        y = x.inBaseUnits()
        self.assertEqual(y,units.PhysicalQuantity(0.028316846592000004,'m**3'))            
        
        x=units.PhysicalQuantity('5cm')
        y = x.inBaseUnits()
        self.assertEqual(y,units.PhysicalQuantity('0.05m'))
        self.assertEqual(x,units.PhysicalQuantity('5cm'))   

    def test__isCompatible__known__Values(self):
        """isCompatible should return True for compatible units and False for incompatible ones"""
        
        testvals=(('5m','cm',True),('1s','ms',True),('1m','ms',False))
        for q1, q2, bool in testvals:
            x=units.PhysicalQuantity(q1)
            self.assertEqual(x.isCompatible(q2),bool)
    
    def test_integers_in_unit_definition(self):
        x=units.PhysicalQuantity('10 1/min')
        self.assertEqual(x.unit.factor,1/60.0)
        self.assertEqual(x.unit.powers,[0, 0, 0, 0, 0, 0, 0, 0, -1, 0])
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------

class test__PhysicalUnit(unittest.TestCase):

    def test_repr_str(self):
        """__repr__should return a string which could be used to contruct the unit instance, __str__ should return a string with just the unit name for str"""
        u = units.PhysicalQuantity('1 d')
        self.assertEqual(repr(u.unit),"PhysicalUnit({'d': 1},86400,[0, 0, 0, 0, 0, 0, 0, 0, 1, 0],0)")
        self.assertEqual(str(u.unit),"<PhysicalUnit d>")

    def test_cmp(self):
        """should error for incompatible units, if they are compatible then it should cmp on their factors"""
        x=units.PhysicalQuantity('1 d')
        y=units.PhysicalQuantity('1 s')
        z=units.PhysicalQuantity('1 ft')
        
        self.assertEqual(cmp(x,y),1)
        self.assertEqual(cmp(x,x),0)
        self.assertEqual(cmp(y,x),-1)
        
        try:
            cmp(x,z)
        except TypeError,err:
            self.assertEqual(str(err),"Incompatible units")
        else:
            self.fail("Expecting TypeError")

    known__mul__Values=(('1m','5m',5),('1cm','1cm',1),('1cm','5m',5),('7km','1m',7))
    def test_multiply(self):
        """multiplication should error for units with offsets"""

        x = units.PhysicalQuantity('1g')
        y = units.PhysicalQuantity('2s')
        z = units.PhysicalQuantity('1 degC')
        
        self.assertEqual(x.unit*y.unit,units.PhysicalUnit({'s': 1, 'kg': 1},.001,[0, 0, 0, 0, 0, 0, 0, 1, 1, 0],0))
        self.assertEqual(y.unit*x.unit,units.PhysicalUnit({'s': 1, 'kg': 1},.001,[0, 0, 0, 0, 0, 0, 0, 1, 1, 0],0))
        

        try:
            x.unit * z.unit
        except TypeError,err:
            self.assertEqual(str(err),"cannot multiply units with non-zero offset")
        else: 
            self.fail("Expecting TypeError")
        
    def test_division(self):
        """division should error when working with offset units"""
        
        w = units.PhysicalQuantity('2kg')
        x = units.PhysicalQuantity('1g')
        y = units.PhysicalQuantity('2s')
        z = units.PhysicalQuantity('1 degC')
        
        quo = w.unit/x.unit
        quo2 = x.unit/y.unit

        self.assertEqual(quo,units.PhysicalUnit({'kg': 1, 'g': -1},1000.0,[0, 0, 0, 0, 0, 0, 0, 0, 0, 0],0))
        self.assertEqual(quo2,units.PhysicalUnit({'s': -1, 'g': 1},0.001,[0, 0, 0, 0, 0, 0, 0, 1, -1, 0],0))
        
        quo = y.unit/2.0
        self.assertEqual(quo,units.PhysicalUnit({'s': 1, "2.0":-1},.5,[0, 0, 0, 0, 0, 0, 0, 0, 1, 0],0))
        
        quo = 2.0/y.unit
        self.assertEqual(quo,units.PhysicalUnit({'s': -1,"2.0":1},2,[0, 0, 0, 0, 0, 0, 0, 0, -1, 0],0))        
        
        try:
            x.unit / z.unit
        except TypeError,err:
            self.assertEqual(str(err),"cannot divide units with non-zero offset")
        else: 
            self.fail("Expecting TypeError")


    known__pow__Values=(('1V',3),('1m',2),('1.1m',2))
    def test_pow(self):
        """power should error for offest units and for non-integer powers"""

        x = units.PhysicalQuantity('1m')
        y = units.PhysicalQuantity('1degF')
        
        z = x**3
        self.assertEqual(z.unit,units.PhysicalQuantity('1m**3').unit)
        x = z**(1.0/3.0) #checks inverse integer units
        self.assertEqual(x.unit,units.PhysicalQuantity('1m').unit)
        
        #test offset units: 
        try: 
            y**17
        except TypeError,err:
            self.assertEqual(str(err),'cannot exponentiate units with non-zero offset')
        else:
            self.fail('Expecting TypeError')
        
        #test non-integer powers   
        try: 
            x**1.2
        except TypeError,err:
            self.assertEqual(str(err),'Only integer and inverse integer exponents allowed')
        else:
            self.fail('Expecting TypeError')      
        try: 
            x**(5.0/2.0)
        except TypeError,err:
            self.assertEqual(str(err),'Only integer and inverse integer exponents allowed')
        else:
            self.fail('Expecting TypeError')                
            
        

    known__conversionFactorTo__Values=(('1m','1cm',100),('1s','1ms',1000),('1ms','1s',0.001))

    def test_conversionFactorTo(self):
        """conversionFactorTo should errror for units with different base power, should error for units with incompativle offset"""
        
        w = units.PhysicalQuantity('1cm')
        x = units.PhysicalQuantity('1m')
        y = units.PhysicalQuantity('1degF')
        z1 = units.PhysicalQuantity('1degC')
        z2 = units.PhysicalQuantity('1degK')
        
        self.assertEqual(w.unit.conversionFactorTo(x.unit),1/100.0)
        try: #incompatible units
            w.unit.conversionFactorTo(y.unit)
        except TypeError,err:
            self.assertEqual(str(err),"Incompatible units")
        else:
            self.fail("Expecting TypeError")
        #compatible offset units    
        self.assertEqual(z1.unit.conversionFactorTo(z2.unit),1.0)  
        try: #incompatible offset units
            y.unit.conversionFactorTo(z2.unit)
        except TypeError,err:
            self.assertEqual(str(err),"Unit conversion (degF to degK) cannot be expressed as a simple multiplicative factor")
        else:
            self.fail("Expecting TypeError")    

    known__conversionTupleTo__Values=(('1m','1s'),('1s','1degK'),('1ms','1rad'))
    
    def test_conversionTupleTo(self):
        """test_conversionTupleTo shoudl error when units have different power lists"""

        w = units.PhysicalQuantity('1cm')
        x = units.PhysicalQuantity('1m')
        y = units.PhysicalQuantity('1degF')
        z1 = units.PhysicalQuantity('1degC')
        z2 = units.PhysicalQuantity('1degK')
        
        #check for non offset units
        self.assertEqual(w.unit.conversionTupleTo(x.unit),(1/100.0,0))
        
        #check for offset units
        result = y.unit.conversionTupleTo(z1.unit)
        self.assertAlmostEqual(result[0],0.556,3)
        self.assertAlmostEqual(result[1],-32.0,3)
        
        #check for incompatible units
        try:
            x.unit.conversionTupleTo(z1.unit)
        except TypeError,err: 
            self.assertEqual(str(err),"Incompatible units")
        else:
            self.fail("Expecting TypeError")
            
    def test_name(self):
        """name should return a mathematically correct representation of the unit"""
        x1=units.PhysicalQuantity('1m')
        x2 = units.PhysicalQuantity('1kg')
        y=1/x1
        self.assertEqual(y.unit.name(),'1/m')
        y=1/x1/x1
        self.assertEqual(y.unit.name(),'1/m**2')
        y=x1**2
        self.assertEqual(y.unit.name(),'m**2')
        y=x2/(x1**2)
        self.assertEqual(y.unit.name(),'kg/m**2')


class test__moduleFunctions(unittest.TestCase):        
    def test_addUnit(self):
        try:
            units.addUnit('ft','20*m')
        except KeyError,err: 
            self.assertEqual(str(err),"Unit ft already defined with different factor or powers")
        else:
            self.fail("Expecting Key Error")
            
        try:
            units.addOffsetUnit('degR','degK',20,10)
        except KeyError,err: 
            self.assertEqual(str(err),"Unit degR already defined with different factor or powers")
        else:
            self.fail("Expecting Key Error")            

if __name__ == "__main__":
    unittest.main()