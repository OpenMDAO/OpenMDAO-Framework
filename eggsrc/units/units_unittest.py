import nose
import units
import unittest
import coverage
import math
#---------------------------------------------------------------------
#---------------------------------------------------------------------
#---------------------------------------------------------------------
#---------------------------------------------------------------------
#---------------------------------------------------------------------
#---------------------------------------------------------------------

# the following class test each method in the NumberDict class


class test__NumberDict(unittest.TestCase):

    def test__NumberDict__init__and__zero(self):
        """a NumberDict instance should initilize using integer and non-integer indices"""
        """a NumberDict instance should initilize all entries with an initial value of 0"""
        x=units.NumberDict()
        #integer test
        self.assertEqual(x[0],0)
        #float test
        self.assertEqual(x[0.01],0)
        #string test
        self.assertEqual(x['t'],0)
        
    def test__add__KnownValues(self):
        """__add__ should give known result with known input"""
        """for non-string data types, addition must be commutative"""
        x=units.NumberDict()
        y=units.NumberDict()
        x['t'],y['t']=1,2
        result = 1 + 2
        result1,result2=x+y,y+x
        self.assertEqual(result, result1[index])
        self.assertEqual(result2[index], result1[index])


    __add__strings=((1,'ha',1),(2,5,'what'),(3,0.85,'blah'))

    def test__add__string_error(self):
        """__add__ should fail if adding a string and an int"""
        x=units.NumberDict()
        y=units.NumberDict()
        x['t'],y['t']='ha',1
        self.assertRaises(TypeError, x.__add__Na, y)


    known__sub__Values=((1,1,1,0),(2,3,2,1),(3,1,0.25,0.75))

    def test__sub__KnownValues(self):
        """__sub__ should give known result with known input"""
        """commuting the input should result in equal magnitude, opposite sign"""

        x=units.NumberDict()
        y=units.NumberDict()
        for index, val_1,val_2,dif in self.known__sub__Values:

            x[index],y[index]=val_1,val_2
            result1,result2=x.__sub__(y),y.__sub__(x)
            self.assertEqual(dif, result1[index])
            self.assertEqual(-dif, result2[index])


    __sub__strings=((1,'ha',1),(2,5,'what'),(3,0.85,'blah'))

    def test__sub__string_error(self):
        """__sub__ should fail if subtracting a string from anything"""
        x=units.NumberDict()
        y=units.NumberDict()
        for index, val_1,val_2 in self.__sub__strings:
            x[index],y[index]=val_1,val_2
            self.assertRaises(TypeError, x.__sub__,y)


    known__mul__Values=((1,1,1,1),(2,0,2,0),(3,5,5,25))

    def test__mul__KnownValues(self):
        """__mul__ should give known result with known input"""
        x=units.NumberDict()
        for index, val_1,scalar,prod in self.known__mul__Values:

            x[index]=val_1
            result=x.__mul__(scalar)
            self.assertEqual(prod, result[index])


    known__div__Values=((1,1,1,1),(2,4,2,2),(3,3,2,1))

    def test__div__KnownValues(self):
        """__div__ should give known result with known input"""
        x=units.NumberDict()
        for index, val_1,scalar,quo in self.known__div__Values:

            x[index]=val_1
            result=x.__div__(scalar)
            self.assertEqual(quo, result[index])


    __div__strings=((1,'ha',1),(2,5,'what'),(3,0.85,'blah'))

    def test__div__string__error1(self):
        """__div__ should fail if either the dividend or divisor is a string object"""
        x=units.NumberDict()
        for index, val_1,val_2 in self.__div__strings:
            x[index]=val_1
            self.assertRaises(ZeroDivisionError, x.__div__,val_2)


    __div__strings=((1,5,0),(2,7,0),(3,0.85,0))

    def test__div__string__error2(self):
        """__div__ should fail if any divisor is zero"""
        x=units.NumberDict()
        for index, val_1,val_2 in self.__div__strings:
            x[index]=val_1
            self.assertRaises(ZeroDivisionError, x.__div__,val_2)

#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------

class test__PhysicalQuantity(unittest.TestCase):
    known__arg__Values=(('1m',1,'m'),('3s',3,'s'),('1V',1,'V'))

    def test__known__same__init__Values(self):
        """__init__ should have the same result regardless of the constructor calling pattern"""

        for input_str,input_val, input_unit in self.known__arg__Values:
            x=units.PhysicalQuantity(input_str)
            y=units.PhysicalQuantity(input_val,input_unit)
            self.assertEqual(x.value,y.value)
            self.assertEqual(x.unit,y.unit)

    def test__str(self):
        """__str__ """

        self.assertEqual(type(units.PhysicalQuantity('1 d').__str__()),type('test'))

    def test__repr(self):
        """__repr__ """

        self.assertEqual(type(units.PhysicalQuantity('1 d').__repr__()),type(units.PhysicalQuantity('1 d').__repr__()))

    def test__cmp(self):
        """__cmp__ """
        x=units.PhysicalQuantity('1 d')
        self.assertEqual(x.__cmp__(x),x.__cmp__(x))

    known__noNumber__Values=(('m'),('s'),('V'))

    def test__init__noNumber(self):
        """__init__ should return an error if a number cannot be found in the arguments"""

        for input_str in self.known__noNumber__Values:
            self.assertRaises(TypeError,units.PhysicalQuantity,input_str)


    known__add__Values=(('1m','5m',6),('1cm','1cm',2),('1cm','5m',501.0),('7km','1m',7.001))

    def test__add__known__Values(self):
        """__add_ should give known result with known input"""
        """The resulting unit should be the same as the unit of the first"""
        """the units of the results should be the same as the units of the calling instance"""

        for input_str1,input_str2, sum in self.known__add__Values:
            x=units.PhysicalQuantity(input_str1)
            y=units.PhysicalQuantity(input_str2)
            self.assertEqual(x.__add__(y).value,sum)
            self.assertEqual(x.__add__(y).unit,x.unit)


    known__sub__Values=(('1m','5m',-4),('1cm','1cm',0),('1cm','5m',-499.0),('7km','1m',6.999))

    def test__sub__known__Values(self):
        """__sub__ should give known result with known input"""
        """__rsub__ should give the negative of __sub__"""
        """the units of the results should be the same as the units of the calling instance"""

        for input_str1,input_str2, sum in self.known__sub__Values:
            x=units.PhysicalQuantity(input_str1)
            y=units.PhysicalQuantity(input_str2)
            self.assertEqual(x.__sub__(y).value,sum)
            self.assertEqual(x.__sub__(y).unit,x.unit)
            self.assertEqual(x.__rsub__(y).value,-sum)
            self.assertEqual(x.__rsub__(y).unit,x.unit)


    known__mul__Values=(('1m','5m',5),('1cm','1cm',1),('1cm','5m',5),('7km','1m',7))

    def test__mul__known__Values(self):
        """__mul__ should give known result with known input"""
        """the unit of the product should be the product of the units"""

        for input_str1,input_str2, prod in self.known__mul__Values:
            x=units.PhysicalQuantity(input_str1)
            y=units.PhysicalQuantity(input_str2)
            self.assertEqual(x.__mul__(y).value,prod)
            self.assertEqual(x.__mul__(y).unit,x.unit*y.unit)

            self.assertEqual(x.__mul__(1/x),x.__mul__(1/x))

            self.assertEqual(x.__mul__(5),x.__mul__(5))
    known__div__Values=(('1.0m','5m',1.0/5),('3cm','5s',3.0/5),('1V','5.0m',1.0/5),('7.0km','1s',7.0))

    def test__div__known__Values(self):
        """__div__ should give known result with known input"""
        """the unit of the product should be the product of the units"""


        for input_str1,input_str2, quo in self.known__div__Values:
            x=units.PhysicalQuantity(input_str1)
            y=units.PhysicalQuantity(input_str2)
            try:
                # if quotient is unit-less (that is, x and y are additively compatible)
                # re-arranges x & y in terms of the known quotient and __rdiv__ and checks for consistency
                self.assertEqual(x.__div__(y),quo)
                self.assertEqual(x.__rdiv__(y),1/quo)
                self.assertEqual(y.__div__(x),1/quo)
                self.assertEqual(y.__rdiv__(x),quo)
                self.assertEqual(x.__div__(5),x.__div__(5))
            except TypeError:
                # if quotient has a unit (x and y are additively incompatible)
                # re-arranges x & y in terms of the known quotient and __rdiv__ and checks for consistency
                self.assertEqual(x.__div__(y).value,quo)
                self.assertEqual(x.__rdiv__(y).value,1/quo)
                self.assertEqual(y.__div__(x).value,1/quo)
                self.assertEqual(y.__rdiv__(x).value,quo)

                self.assertEqual(x.__div__(y).unit,x.unit/y.unit)
                self.assertEqual(x.__rdiv__(y).unit,y.unit/x.unit)
                self.assertEqual(y.__div__(x).unit,y.unit/x.unit)
                self.assertEqual(y.__rdiv__(x).unit,x.unit/y.unit)


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


