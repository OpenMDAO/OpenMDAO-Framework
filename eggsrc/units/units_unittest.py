
import units
import unittest
import math
#---------------------------------------------------------------------
#---------------------------------------------------------------------
#---------------------------------------------------------------------
#---------------------------------------------------------------------
#---------------------------------------------------------------------
#---------------------------------------------------------------------

# the following class test each method in the NumberDict class


class test__NumberDict(unittest.TestCase):

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
    

    def test__known__same__init__Values(self):
        """__init__ should have the same result regardless of the constructor calling pattern"""
    
        known__arg__Values=(('1m',1,'m'),('3s',3,'s'),('1V',1,'V'))
        for input_str,input_val, input_unit in self.known__arg__Values:
            x=units.PhysicalQuantity(input_str)
            y=units.PhysicalQuantity(input_val,input_unit)
            self.assertEqual(x.value,y.value)
            self.assertEqual(x.unit,y.unit)

    def test__str(self):
        """__str__ """

        self.assertEqual(str(units.PhysicalQuantity('1 d')),"1.0 d")

    def test__repr(self):
        """__repr__ """

        self.assertEqual(repr(units.PhysicalQuantity('1 d')),"PhysicalQuantity(1.0,'d')")

    def test__cmp(self):
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
            pass




    def test__add__known__Values(self):
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
        q2 = units.PhysicalQunatity('1kg')
        
        try: 
            q1 + q2
        except TypeError,err:
            self.assertEqual(str(err),"Incompatible units")
        else: 
            self.fail("expecting TypeError")
            
        #test for error if using offset units
            

    def test__sub__known__Values(self):
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

    def test__mul__known__Values(self):
        """multiplication should give known result with known input
        the unit of the product should be the product of the units"""

        known_mul_Values=(('1m','5m',5),('1cm','1cm',1),('1cm','5m',5),('7km','1m',7))
        for q1,q2, prod in known_mul_Values:
            x=units.PhysicalQuantity(q1)
            y=units.PhysicalQuantity(q2)
            self.assertEqual((x*y).value,prod)
            self.assertEqual((x*y).unit,x.unit*y.unit)

    
    

    def test__div__known__Values(self):
        """__div__ should give known result with known input"""
        """the unit of the product should be the product of the units"""

        known_div_Values=(('3cm','5s',3.0/5),('1V','5.0m',1.0/5),('7.0km','1s',7.0))
        
        x=units.PhysicalQuantity('1.0m')
        y=units.PhysicalQuantity('5m')
        quo = 1.0/5
        # if quotient is unit-less (that is, x and y are additively compatible)
        # re-arranges x & y in terms of the known quotient and __rdiv__ and checks for consistency
        self.assertEqual((x/y),quo)
        self.assertEqual(x.__rdiv__(y),1/quo)
        
        x=units.PhysicalQuantity('3cm')
        y=units.PhysicalQuantity('5s')
        quo = 3.0/5
        # if quotient has a unit (x and y are additively incompatible)
        # re-arranges x & y in terms of the known quotient and __rdiv__ and checks for consistency
        self.assertEqual((x/y).value,quo)
        self.assertEqual(x.__rdiv__(y).value,1/quo)

        self.assertEqual((x/y).unit,x.unit/y.unit)
        self.assertEqual(x.__rdiv__(y).unit,y.unit/x.unit)



    known__pow__Values=(('5V',2,25),('9.8m',2,9.8**2))

    def test__pow__known__Values(self):
        """__pow__ should give known result with known input"""
        """the unit of the power should be the power of the input units"""

        for input_str, pwr,val in self.known__pow__Values:
            x=units.PhysicalQuantity(input_str)
            self.assertEqual(x.__pow__(pwr).value,val)
            self.assertEqual(x.__pow__(pwr).unit,x.unit**pwr)
            self.assertRaises(TypeError, x.__pow__,x)
            self.assertRaises(TypeError, x.__rpow__,x)

    known__abs__Values=(('-5V',5),('9.8m',9.8))
    def test__abs__known__Values(self):
        """__abs__ should give known result with known input"""

        for input_str,val in self.known__abs__Values:
            x=units.PhysicalQuantity(input_str)
            self.assertEqual(x.__abs__().value,abs(val))

    known__pos__Values=(('5V',5),('9.8m',9.8))
    def test__pos__known__Values(self):
        """__pos__ should give known result with known input"""

        for input_str,val in self.known__pos__Values:
            x=units.PhysicalQuantity(input_str)
            self.assertEqual(x.__pos__().value,val)

    known__neg__Values=(('5V',-5),('9.8m',-9.8))
    def test__neg__known__Values(self):
        """__neg__ should give known result with known input"""

        for input_str,val in self.known__neg__Values:
            x=units.PhysicalQuantity(input_str)
            self.assertEqual(x.__neg__().value,val)

    known__sqrt__Values=(('5V',-5),('9.8m',-9.8))

    def test__sqrt__known__Values(self):
        """__sqrt__ should give known result with known input"""

        for input_str,val in self.known__pos__Values:
            x=units.PhysicalQuantity(input_str)
            self.assertEqual((x*x).sqrt(),x)

    known__ang__Values=(('0 rad','10 rad'))
    def test__sin__known__Values(self):
        """__sin__ should give known result with known input"""

        for input_str in self.known__ang__Values:
            x=units.PhysicalQuantity(input_str)
            self.assertEqual(x.sin(),math.sin(x.value))
        for input_str in self.known__pos__Values:
            x=units.PhysicalQuantity(input_str[0])
            self.assertRaises(TypeError,x.sin)
    def test__cos__known__Values(self):
        """__cos__ should give known result with known input"""

        for input_str in self.known__ang__Values:
            x=units.PhysicalQuantity(input_str)
            self.assertEqual(x.cos(),math.cos(x.value))
        for input_str in self.known__pos__Values:
            x=units.PhysicalQuantity(input_str[0])
            self.assertRaises(TypeError,x.cos)
    def test__tan__known__Values(self):
        """__tan__ should give known result with known input"""

        for input_str in self.known__ang__Values:
            x=units.PhysicalQuantity(input_str)
            self.assertEqual(x.tan(),math.tan(x.value))
        for input_str in self.known__pos__Values:
            x=units.PhysicalQuantity(input_str[0])
            self.assertRaises(TypeError,x.tan)

    def test__nonzero__known__Values(self):
        """__nonzero__ should give known result with known input"""

        for input_str in self.known__pos__Values:
            x=units.PhysicalQuantity(input_str[0])
            self.assertEqual(x.__nonzero__(),True)

    def test__round__known__Values(self):
        """_round__ should give known result with known input"""
        self.assertEqual(units._round(2.3),2)
        self.assertEqual(units._round(-2.3),-2)

    known__convertToUnit__Values=(('5cm','m',0.05),('1s','ms',1000))
    def test__convertToUnit__known__Values(self):
        """__convertToUnit_ should give known result with known input"""

        for input_str, new_unit,result in self.known__convertToUnit__Values:
            x=units.PhysicalQuantity(input_str)
            x.convertToUnit(new_unit)
            self.assertEqual(x.value,result)

    known__inUnitsOf__Values=(('5cm','m',0.05),('1s','ms',1000))
    def test__inUnitsOf__known__Values(self):
        """__inUnitsOf__ should give known result with known input"""

        for input_str, new_unit,result in self.known__inUnitsOf__Values:
            x=units.PhysicalQuantity(input_str)
            self.assertEqual(result,x.inUnitsOf(new_unit).value)
            self.assertEqual(x.inUnitsOf(new_unit,new_unit),x.inUnitsOf(new_unit,new_unit))

    def test__inBaseUnits__known__Values(self):
        """__inBaseUnits__ should give known result with known input"""

        for input_str, new_unit,result in self.known__inUnitsOf__Values:
            u=units.PhysicalQuantity('1m')
            v=units.PhysicalQuantity('1s')
            x=units.PhysicalQuantity(input_str)
            self.assertEqual(x.inBaseUnits(),x.inBaseUnits())
            self.assertRaises(KeyError,(1/u).inBaseUnits)
            self.assertRaises(KeyError,(1/u/u).inBaseUnits)
            self.assertRaises(KeyError,(u**2).inBaseUnits)

    known__isCompatible__Values=(('5m','cm',True),('1s','ms',True),('1m','ms',False))

    def test__isCompatible__known__Values(self):
        """__isCompatible_ should give known result with known input"""

        for input_str, chk_unit,result in self.known__isCompatible__Values:
            x=units.PhysicalQuantity(input_str)
            self.assertEqual(x.isCompatible(chk_unit),result)

#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------

class test__PhysicalUnit(unittest.TestCase):

    def test__repr(self):
        """__repr__ """

        self.assertEqual(type(units.PhysicalQuantity('1 d').unit.__repr__()),type(units.PhysicalQuantity('1 d').unit.__repr__()))

    def test__str(self):
        """__str__ """

        self.assertEqual(type(units.PhysicalQuantity('1 d').unit.__str__()),type('test'))

    def test__cmp(self):
        """__cmp__ """
        x=units.PhysicalQuantity('1 d')
        y=x*x
        self.assertEqual(x.unit.__cmp__(x.unit),x.unit.__cmp__(x.unit))
        self.assertRaises(TypeError,x.unit.__cmp__,y.unit)

    known__mul__Values=(('1m','5m',5),('1cm','1cm',1),('1cm','5m',5),('7km','1m',7))
    def test__mul__knownerror__Values(self):
        """__mul__ should give known result with known input"""

        for input_str1,input_str2, prod in self.known__mul__Values:
            x=units.PhysicalQuantity(input_str1).unit
            x.offset=1
            self.assertRaises(TypeError, x.__mul__,5)

    def test__div__knownerror__Values(self):
        """__div__ should give known result with known input"""

        for input_str1,input_str2, prod in self.known__mul__Values:
            x=units.PhysicalQuantity(input_str1).unit
            x.offset=1
            self.assertRaises(TypeError, x.__div__,5)
            self.assertRaises(TypeError, x.__rdiv__,5)
        for input_str1,input_str2, prod in self.known__mul__Values:
            x=units.PhysicalQuantity(input_str1).unit
            x.offset=0
            self.assertEqual(x.__div__(8),x.__div__(8))
            self.assertEqual(x.__rdiv__(x),x.__rdiv__(x))


    known__pow__Values=(('1V',3),('1m',2),('1.1m',2))
    def test__pow__known__Values(self):
        """__pow__ should give known result with known input"""

        for input_str,pwr in self.known__pow__Values:
            x=units.PhysicalQuantity(input_str).unit
            x.offset=0
            self.assertEqual(x.__pow__(pwr),x.__pow__(pwr))
            self.assertEqual((x*x).__pow__(pwr),(x*x).__pow__(pwr))
            self.assertRaises(TypeError, x.__pow__,x)
            self.assertRaises(TypeError, (x*x).__pow__,0.1)
            x.offset=1
            self.assertRaises(TypeError, x.__pow__,5)

    known__conversionFactorTo__Values=(('1m','1cm',100),('1s','1ms',1000),('1ms','1s',0.001))

    def test__conversionFactorTo__known__Values(self):
        """__conversionFactorTo_ should give known result with known input"""

        for input_str1,input_str2,result in self.known__conversionFactorTo__Values:
            x=units.PhysicalQuantity(input_str1)
            y=units.PhysicalQuantity(input_str2)
            self.assertEqual(x.unit.conversionFactorTo(y.unit),result)
        self.assertRaises(TypeError,units.PhysicalQuantity('1s').unit.conversionFactorTo,units.PhysicalQuantity('1m').unit)
        x=units.PhysicalUnit('unit1',1,[1,0,0,0,0,0,0,0,0])
        y=units.PhysicalUnit('unit2',2,[1,0,0,0,0,0,0,0,0])
        y.offset=1
        self.assertRaises(TypeError,x.conversionFactorTo,y)

    known__conversionTupleTo__Values=(('1m','1s'),('1s','1K'),('1ms','1rad'))
    def test__conversionTupleTo__known__Values(self):
        """__conversionTupleTo_ should give known result with known input"""

        for input_str1,input_str2 in self.known__conversionTupleTo__Values:
            x=units.PhysicalQuantity(input_str1)
            y=units.PhysicalQuantity(input_str2)
            self.assertRaises(TypeError,x.unit.conversionTupleTo,y.unit)

    def test__Name__known__Values(self):
        """__Name_ should give known result with known input"""
        x=units.PhysicalQuantity('1m')
        y=1/x
        self.assertEqual(y.unit.name(),'1/m')
        y=1/x/x
        self.assertEqual(y.unit.name(),'1/m**2')
        y=x**2
        self.assertEqual(y.unit.name(),'m**2')


    def test__findUnit__known__Values(self):
        """__findUnit_ should give known result with known input"""
        x=units.PhysicalQuantity('1m')
        y=units.PhysicalQuantity('1dam')
        self.assertEqual(y.unit,units._findUnit('dam'))
        self.assertEqual(x.unit,units._findUnit('m'))
        self.assertRaises(KeyError,units._findUnit,'hello')
        self.assertRaises(TypeError,units._findUnit,5)

    def test__addPrefixed__known__Values(self):
        """__addPrefixed_ should give known result with known input"""
        x=units.PhysicalQuantity('1m')
        units._addPrefixed('m')
        self.assertEqual(units.PhysicalQuantity('1mm'),units.PhysicalQuantity('1mm'))

    def test__addUnit__known__Values(self):
        """__addPrefixed_ should give known result with known input"""

        self.assertRaises(KeyError,units.addUnit,'m',units.PhysicalUnit('1m',2,[0,0,0,0,0,0,0,0,0]))

    def test__importLibrary__known__Values(self):
        """__importLibrary_ should give known result with known input"""


if __name__ == "__main__":
    unittest.main()