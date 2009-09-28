# Unit Handling
#
#Written by Justin Gray, but based heavily on the PhysicalQuantities
# module in Scientific Python written by Konrad Hinsen

"""This module provides a data type that represents a physical
quantity together with its unit. It is possible to add and
subtract these quantities if the units are compatible, and
a quantity can be converted to another compatible unit.
Multiplication, subtraction, and raising to integer powers
is allowed without restriction, and the result will have
the correct unit. A quantity can be raised to a non-integer
power only if the result can be represented by integer powers
of the base units.

The module provides a basic set of predefined physical quanitites
in its built in library, however it also supports generation of
personal libararies which can be saved and reused"""

import re
import math
import ConfigParser
import pickle

from pkg_resources import resource_string, resource_stream

#Class definitions

class NumberDict(dict):

  """
  Dictionary storing numerical values

  Constructor: NumberDict()

  An instance of this class acts like an array of number with
  generalized (non-integer) indices. A value of zero is assumed
  for undefined entries. NumberDict instances support addition,
  and subtraction with other NumberDict instances, and multiplication
  and division by scalars.
  """

  def __getitem__(self, item):
    try:
        return dict.__getitem__(self, item)
    except KeyError:
        return 0

  def __coerce__(self, other):
    if type(other) == type({}):
        other = NumberDict(other)
    return self, other

  def __add__(self, other):
    sum_dict = NumberDict()
    for (self_k,self_v),(other_k,other_v) in zip(self.iteritems(),other.iteritems()):
        sum_dict[self_k] = self_v + other_v

    return sum_dict

  def __sub__(self, other):
    sum_dict = NumberDict()
    for (self_k,self_v),(other_k,other_v) in zip(self.iteritems(),other.iteritems()):
        sum_dict[self_k] = self_v- other_v

    return sum_dict

  def __mul__(self, other):
    new = NumberDict()
    for key,value in self.iteritems():
        new[key] = other*value
    return new
  __rmul__ = __mul__

  def __div__(self, other):
    new = NumberDict()
    for key,value in self.iteritems():
        new[key] = value/other
    return new

class PhysicalQuantity(object):
  """ Physical quantity with units

  PhysicalQuantity instances allow addition, subtraction,
  multiplication, and division with each other as well as
  multiplication, division, and exponentiation with numbers.
  Addition and subtraction check that the units of the two operands
  are compatible and return the result in the units of the first
  operand.

  """

  #class attributes
  _number = re.compile('[+-]?[0-9]+(\\.[0-9]*)?([eE][+-]?[0-9]+)?')

  def __init__(self, *args):
    """
    There are two constructor calling patterns:

            1. PhysicalQuantity(value, unit), where value is any number
            and unit is a string defining the unit

            2. PhysicalQuantity(value_with_unit), where value_with_unit
            is a string that contains both the value and the unit,
            i.e. '1.5 m/s'. This form is provided for more convenient
            interactive use.

     @param args: either (value, unit) or (value_with_unit,)
     @type args: (number, C{str}) or (C{str},)
     """

    if len(args) == 2:
      self.value = args[0]
      self.unit = _findUnit(args[1])
    else:
      s = args[0].strip()
      match = PhysicalQuantity._number.match(s)
      if match is None:
        raise TypeError("No number found in input argument: '%s'"%s)
      self.value = float(match.group(0))
      self.unit = _findUnit(s[len(match.group(0)):].strip())
      


  def __str__(self):
    return str(self.value) + ' ' + self.unit.name()

  def __repr__(self):
    return (self.__class__.__name__ + '(' + `self.value` + ',' +
            `self.unit.name()` + ')')


  def _sum(self, other, sign1, sign2):
    if not isinstance(other,PhysicalQuantity):
      raise TypeError('Incompatible types')
    new_value = sign1*self.value + sign2*other.value*other.unit.conversionFactorTo(self.unit)
    return self.__class__(new_value, self.unit)
    
  def __add__(self, other):
    return self._sum(other, 1, 1)

  __radd__ = __add__

  def __sub__(self, other):
    return self._sum(other, 1, -1)

  def __rsub__(self, other):
    return self._sum(other, -1, 1)

  def __cmp__(self, other):
    diff = self._sum(other, 1, -1)
    return cmp(diff.value, 0)

  def __mul__(self, other):
    if not isinstance(other,PhysicalQuantity):
      return self.__class__(self.value*other, self.unit)
    value = self.value*other.value
    unit = self.unit*other.unit
    if unit.isDimensionless():
      return value*unit.factor
    else:
      return self.__class__(value, unit)

  __rmul__ = __mul__

  def __div__(self, other):
    if not isinstance(other,PhysicalQuantity):
      return self.__class__(self.value/other, self.unit)
    value = self.value/other.value
    unit = self.unit/other.unit
    if unit.isDimensionless():
      return value*unit.factor
    else:
      return self.__class__(value, unit)

  def __rdiv__(self, other):
    if not isinstance(other,PhysicalQuantity):
      return self.__class__(other/self.value, pow(self.unit, -1))
    value = other.value/self.value
    unit = other.unit/self.unit
    if unit.isDimensionless():
      return value*unit.factor
    else:
      return self.__class__(value, unit)

  def __pow__(self, other):
    if isinstance(other,PhysicalQuantity):
      raise TypeError('Exponents must be dimensionless')
    return self.__class__(pow(self.value, other), pow(self.unit, other))

  def __rpow__(self, other):
    raise TypeError('Exponents must be dimensionless')

  def __abs__(self):
      return self.__class__(abs(self.value), self.unit)

  def __pos__(self):
    return self

  def __neg__(self):
    return self.__class__(-self.value, self.unit)

  def __nonzero__(self):
    return self.value != 0

  def convertValue(self,target_unit):
    (factor, offset) = self.unit.conversionTupleTo(target_unit)
    return (self.value + offset) * factor

  def convertToUnit(self, unit):
    """
    Change the unit and adjust the value such that
    the combination is equivalent to the original one. The new unit
    must be compatible with the previous unit of the object.

    @param unit: a unit
    @type unit: C{str}
    @raise TypeError: if the unit string is not a know unit or a
    unit incompatible with the current one
    """
    unit = _findUnit(unit)

    self.value = self.convertValue(unit)
    self.unit = unit

  def inUnitsOf(self, unit):
    """
    Express the quantity in different units. If one unit is
    specified, a new PhysicalQuantity object is returned that
    expresses the quantity in that unit. If several units
    are specified, the return value is a tuple of
    PhysicalObject instances with with one element per unit such
    that the sum of all quantities in the tuple equals the the
    original quantity and all the values except for the last one
    are integers. This is used to convert to irregular unit
    systems like hour/minute/second.

    @param units: one or several units
    @type units: C{str} or sequence of C{str}
    @returns: one or more physical quantities
    @rtype: L{PhysicalQuantity} or C{tuple} of L{PhysicalQuantity}
    @raises TypeError: if any of the specified units are not compatible
    with the original unit
    """
    unit = _findUnit(unit)
   
    value = self.convertValue(unit)
    return self.__class__(value, unit)


  # Contributed by Berthold Hoellmann
  def inBaseUnits(self):
    """
    @returns: the same quantity converted to base units,
    i.e. SI units in most cases
    @rtype: L{PhysicalQuantity}
    """
    new_value = self.value * self.unit.factor
    num = ''
    denom = ''
    for i in [0,1,2,3,4,5,6,7,8]:
      unit = _unitLib.base_names[i]
      power = self.unit.powers[i]
      if power < 0:
        denom = denom + '/' + unit
        if power < -1:
          denom = denom + '**' + str(-power)
      elif power > 0:
        num = num + '*' + unit
        if power > 1:
          num = num + '**' + str(power)
    if len(num) == 0:
      num = '1'
    else:
      num = num[1:]
    return self.__class__(new_value, num + denom)

  def isCompatible(self, unit):
    """
    @param unit: a unit
    @type unit: C{str}
    @returns: C{True} if the specified unit is compatible with the
    one of the quantity
    @rtype: C{bool}
    """
    unit = _findUnit(unit)
    return self.unit.isCompatible(unit)

  def getValue(self):
      """Return value (float) of physical quantity (no unit)."""
      return self.value

  def getUnitName(self):
      """Return unit (string) of physical quantity."""
      return self.unit.name()  
  
  def sqrt(self):
      return pow(self, 0.5)

  def sin(self):
    if self.unit.isAngle():
      return math.sin(self.value * \
             self.unit.conversionFactorTo(PhysicalQuantity('1rad').unit))
    else:
      raise TypeError('Argument of sin must be an angle')

  def cos(self):
    if self.unit.isAngle():
        return math.cos(self.value * \
            self.unit.conversionFactorTo(PhysicalQuantity('1rad').unit))
    else:
        raise TypeError('Argument of cos must be an angle')

  def tan(self):
    if self.unit.isAngle():
        return math.tan(self.value * \
            self.unit.conversionFactorTo(PhysicalQuantity('1rad').unit))
    else:
        raise TypeError('Argument of tan must be an angle')

class PhysicalUnit(object):
  """
  Physical unit

  A physical unit is defined by a name (possibly composite), a scaling
  factor, and the exponentials of each of the SI base units that enter into
  it. Units can be multiplied, divided, and raised to integer powers.
  """

  def __init__(self,names,factor,powers,offset=0):
    """
    @param names: a dictionary mapping each name component to its
                  associated integer power (e.g. C{{'m': 1, 's': -1}})
                  for M{m/s}). As a shorthand, a string may be passed
                  which is assigned an implicit power 1.
    @type names: C{dict} or C{str}
    @param factor: a scaling factor
    @type factor: C{float}
    @param powers: the integer powers for each of the nine base units
    @type powers: C{list} of C{int}
    @param offset: an additive offset to the base unit (used only for
                   temperatures)
    @type offset: C{float}
    """
    if type(names) == str:
      self.names = NumberDict()
      self.names[names] = 1;

    else:
      self.names = names
    self.factor = factor
    self.offset = offset
    self.powers = powers

  def __repr__(self):
    return 'PhysicalUnit(%s,%s,%s,%s)'%(self.names,self.factor,self.powers,self.offset)

  def __str__(self):
    return '<PhysicalUnit ' + self.name() + '>'

  def __cmp__(self, other):
    if self.powers != other.powers:
      raise TypeError('Incompatible units')
    return cmp(self.factor, other.factor)

  def __mul__(self, other):
    if self.offset != 0 or (isinstance(other,PhysicalUnit) and other.offset != 0):
      raise TypeError("cannot multiply units with non-zero offset")
    if isinstance(other,PhysicalUnit):
      return PhysicalUnit(self.names+other.names,
                          self.factor*other.factor,
                          [a+b for (a,b) in zip(self.powers,other.powers)])
    else:
      return PhysicalUnit(self.names+{str(other): 1},
                          self.factor*other,
                          self.powers,
                          self.offset * other)

  __rmul__ = __mul__

  def __div__(self, other):
    if self.offset != 0 or (isinstance(other,PhysicalUnit) and other.offset != 0):
      raise TypeError("cannot divide units with non-zero offset")
    if isinstance(other,PhysicalUnit):
      return PhysicalUnit(self.names-other.names,
                          self.factor/other.factor,
                          [a-b for (a,b) in zip(self.powers,other.powers)])
    else:
      return PhysicalUnit(self.names+{str(other): -1},
                          self.factor/float(other), self.powers)

  def __rdiv__(self, other):
    if self.offset != 0 or (isinstance(other,PhysicalUnit) and other.offset != 0):
      raise TypeError("cannot divide units with non-zero offset")
    if isinstance(other,PhysicalUnit):
      return PhysicalUnit(other.names-self.names,
                          other.factor/self.factor,
                          [a-b for (a,b) in zip(other.powers,self.powers)])
    else:
      return PhysicalUnit({str(other): 1}-self.names,
                          float(other)/self.factor,
                          [-x for x in self.powers])

  def __pow__(self, other):
    if self.offset != 0:
      raise TypeError("cannot exponentiate units with non-zero offset")
    if isinstance(other, int):
      return PhysicalUnit(other*self.names, pow(self.factor, other),
                          [x*other for x in self.powers])
    if isinstance(other, float):
      inv_exp = 1./other
      rounded = int(math.floor(inv_exp+0.5))
      if abs(inv_exp-rounded) < 1.e-10:
        
        if all([x%rounded==0 for x in self.powers]):
          f = self.factor**other
          p = [x/rounded for x in self.powers]
          if all([x%rounded==0 for x in self.names.values()]):
            names = self.names/rounded
          else:
            names = NumberDict()
            if f != 1.:
              names[str(f)] = 1
            for i in range(len(p)):
              names[_unitLib.base_names[i]] = p[i]
          return PhysicalUnit(names, f, p)
    raise TypeError('Only integer and inverse integer exponents allowed')

  def conversionFactorTo(self, other):
    """
    @param other: another unit
    @type other: L{PhysicalUnit}
    @returns: the conversion factor from this unit to another unit
    @rtype: C{float}
    @raises TypeError: if the units are not compatible
    """
    if self.powers != other.powers:
      raise TypeError('Incompatible units')

    if self.offset != other.offset and self.factor != other.factor:
        raise TypeError(('Unit conversion (%s to %s) cannot be expressed ' +
          'as a simple multiplicative factor') %(self.name(), other.name()))

    return self.factor/other.factor

  def conversionTupleTo(self, other): # added 1998/09/29 GPW
    """
    @param other: another unit
    @type other: L{PhysicalUnit}
    @returns: the conversion factor and offset from this unit to
              another unit
    @rtype: (C{float}, C{float})
    @raises TypeError: if the units are not compatible
    """
    if self.powers != other.powers:
        raise TypeError('Incompatible units')

    # let (s1,d1) be the conversion tuple from 'self' to base units
    #   (ie. (x+d1)*s1 converts a value x from 'self' to base units,
    #   and (x/s1)-d1 converts x from base to 'self' units)
    # and (s2,d2) be the conversion tuple from 'other' to base units
    # then we want to compute the conversion tuple (S,D) from
    #   'self' to 'other' such that (x+D)*S converts x from 'self'
    #   units to 'other' units
    # the formula to convert x from 'self' to 'other' units via the
    #   base units is (by definition of the conversion tuples):
    #     ( ((x+d1)*s1) / s2 ) - d2
    #   = ( (x+d1) * s1/s2) - d2
    #   = ( (x+d1) * s1/s2 ) - (d2*s2/s1) * s1/s2
    #   = ( (x+d1) - (d1*s2/s1) ) * s1/s2
    #   = (x + d1 - d2*s2/s1) * s1/s2
    # thus, D = d1 - d2*s2/s1 and S = s1/s2

    factor = self.factor / other.factor
    offset = self.offset - (other.offset * other.factor / self.factor)
    return (factor, offset)

  def isCompatible(self, other):     # added 1998/10/01 GPW
    """
    @param other: another unit
    @type other: L{PhysicalUnit}
    @returns: C{True} if the units are compatible, i.e. if the powers of
              the base units are the same
    @rtype: C{bool}
    """
    return self.powers == other.powers

  def isDimensionless(self):
    return not any(self.powers)

  def isAngle(self):
    return (self.powers[_unitLib.base_types['angle']] == 1 and sum(self.powers) == 1)

  def setName(self, name):
    self.names = NumberDict()
    self.names[name] = 1

  def name(self):
    num = ''
    denom = ''
    for unit in self.names.keys():
      power = self.names[unit]
      if power < 0:
        denom = denom + '/' + unit
        if power < -1:
          denom = denom + '**' + str(-power)
      elif power > 0:
        num = num + '*' + unit
        if power > 1:
          num = num + '**' + str(power)
    if len(num) == 0:
      num = '1'
    else:
      num = num[1:]
    return num + denom

#Type Checks
def isPhysicalUnit(x):
  """
  @param x: an object
  @type x: any
  @returns: C{True} if x is a L{PhysicalUnit}
  @rtype: C{bool}
  """
  return isinstance(x,PhysicalUnit)

def isPhysicalQuantity(x):
    """
    @param x: an object
    @type x: any
    @returns: C{True} if x is a L{PhysicalQuantity}
    @rtype: C{bool}
    """
    return isinstance(x,PhysicalQuantity)

#Helper Functions
def _findUnit(unit):
    if isinstance(unit,str):
        name = unit.strip()
        if name not in _unitLib.unit_table:
            #check for single letter prefix before unit
            if(name[0] in _unitLib.prefixes and name[1:] in _unitLib.unit_table):
                addUnit(unit,_unitLib.prefixes[name[0]]*_unitLib.unit_table[name[1:]])
            #check for double letter prefix before unit
            elif(name[0:2] in _unitLib.prefixes and name[2:] in _unitLib.unit_table):
                addUnit(unit,_unitLib.prefixes[name[0:2]]*_unitLib.unit_table[name[2:]])
            #no prefixes found, might be function of multiple units
            else:
                try:
                    unit =  eval(name, _unitLib.unit_table) 
                except:
                    raise ValueError, "no unit named '%s' is defined"%name
        unit = eval(name, _unitLib.unit_table)
        for cruft in ['__builtins__', '__args__']:
            try: del _unitLib.unit_table[cruft]
            except: pass
    if not isinstance(unit,PhysicalUnit):
        raise TypeError(str(unit) + ' is not a unit')
    return unit


def _newUnit(name,factor,powers):
  _unitLib.unit_table[name] = PhysicalUnit(name,factor,powers)


def addOffsetUnit(name,baseunit,factor,offset,comment):
    if type(baseunit) == str:
        baseunit = _findUnit(baseunit)
    #else, baseunit should be a instance of PhysicalUnit
    #names,factor,powers,offset=0
    unit = PhysicalUnit(baseunit.names,baseunit.factor*factor,baseunit.powers,offset)
    unit.setName(name)
    if _unitLib.unit_table.has_key(name):
        if (_unitLib.unit_table[name].factor!=unit.factor or _unitLib.unit_table[name].powers!=unit.powers):
          raise KeyError, 'Unit ' + name + ' already defined with different factor or powers'
    _unitLib.unit_table[name] = unit
    _unitLib.set('units',name,unit)   
    if comment: 
        _unitLib.help.append((name,comment,unit))
        
        
def addUnit(name, unit, comment=''):
    if comment:
      _unitLib.help.append((name, comment, unit))
    if type(unit) == str:
      unit = eval(unit, _unitLib.unit_table)
      for cruft in ['__builtins__', '__args__']:
        try: del _unitLib.unit_table[cruft]
        except: pass
    unit.setName(name)
    if _unitLib.unit_table.has_key(name):
      if (_unitLib.unit_table[name].factor!=unit.factor or _unitLib.unit_table[name].powers!=unit.powers):
        raise KeyError, 'Unit ' + name + ' already defined with different factor or powers'
    _unitLib.unit_table[name] = unit
    _unitLib.set('units',name,unit)


_unitLib = ConfigParser.ConfigParser()
def doNothing(string): #makes the ConfigParser case sensetive
  return string
_unitLib.optionxform = doNothing


def importLibrary(libfilepointer):
  global _unitLib 
  _unitLib = ConfigParser.ConfigParser()
  _unitLib.optionxform = doNothing
  _unitLib.readfp(libfilepointer)
  required_base_types = ['length','mass','time','temperature','angle']
  _unitLib.base_names = list()
  _unitLib.base_types = dict() #used to isAngle() and other base type checking
  _unitLib.unit_table = dict()
  _unitLib.prefixes = dict()
  _unitLib.help = list()

  for prefix,factor in _unitLib.items('prefixes'):
    _unitLib.prefixes[prefix] = float(factor)

  base_list = [0 for x in _unitLib.items('base_units')]
  

  for i,(unitType,name) in enumerate(_unitLib.items('base_units')):
      _unitLib.base_types[unitType] = i 
      powers = list(base_list)
      powers[i] = 1
      #print '%20s'%unitType, powers
      _newUnit(name,1,powers) #cant use addUnit because no base units exist yet
      _unitLib.base_names.append(name)

  #test for required base types
  missing = [type for type in required_base_types if not type in _unitLib.base_types]
  if any(missing):
      raise ValueError,"Not all required base type were present in the config file. missing: %s, at least %s required"%(missing,required_base_types)
 
  retry1 = set()
  retry2 = set()
  retryCount = 0
  lastRetryCount = 99999


  for name,unit in _unitLib.items('units'):
    data = unit.split(',')
    if len(data) == 2:
        try:
            comment = data[1]
            unit = data[0]
            addUnit(name,unit,comment)
        except NameError:
            retry1.add((name,unit,comment))
    elif len(data) == 4: 
        try:
            factor,baseunit,offset,comment = tuple(data)
            addOffsetUnit(name,baseunit,float(factor),float(offset),comment)
        except NameError:
            retry1.add((name,baseunit,float(factor),float(offset),comment))

  for cruft in ['__builtins__', '__args__']:
    try: del _unitLib.unit_table[cruft]
    except: pass

  while (lastRetryCount != retryCount and len(retry1)!=0):
    lastRetryCount = retryCount
    retryCount = 0
    retry2 = retry1.copy()
    for data in retry2:
        if len(data) == 3:
            name,unit,comment = data
            try:
                addUnit(name,unit,comment)
                retry1.remove(data)
            except NameError:
                retryCount+=1
        if len(data) == 5:
            try:
                name,factor,baseunit,offset,comment = data
                addOffsetUnit(name,factor,baseunit,offset,comment)
                retry1.remove(data)
            except NameError:
                retryCount+=1        
  if(len(retry1) >0):
    raise ValueError, "The following units were not defined because they could not be resolved as a function of any other defined units:%s"%[x[0] for x in retry1]

try:
    #_unitLib = pickle(resource_stream(__name__, 'unitLib.save'))
    default_lib_string = """(iConfigParser
ConfigParser
p0
(dp1
S'optionxform'
p2
cunits
doNothing
p3
sS'help'
p4
(lp5
(S'V0'
p6
S' volume of ideal gas'
p7
S'2.24136*m**3/(1000*mol)'
p8
tp9
a(S'degR'
p10
S'Rankine'
p11
S'0.555555556*degK'
p12
tp13
a(S'Wb'
p14
S'Weber'
p15
S'V*s'
p16
tp17
a(S'cup'
p18
S' Cup'
p19
S'1e-4*2.3659*m**3'
p20
tp21
a(S'lm'
p22
S'Lumen'
p23
S'cd*sr'
p24
tp25
a(S'tn'
p26
S' Ton (metric)'
p27
S'1e3*kg'
p28
tp29
a(S'degC'
p30
S'centigrade'
p31
ccopy_reg
_reconstructor
p32
(cunits
PhysicalUnit
p33
c__builtin__
object
p34
Ntp35
Rp36
(dp37
S'offset'
p38
F273.14999999999998
sS'powers'
p39
(lp40
I0
aI0
aI1
aI0
aI0
aI0
aI0
aI0
aI0
aI0
asS'names'
p41
g32
(cunits
NumberDict
p42
c__builtin__
dict
p43
(dp44
g30
I1
stp45
Rp46
sS'factor'
p47
F1.0
sbtp48
a(S'Bq'
p49
S'Becquerel'
p50
S'1/s'
p51
tp52
a(S'degF'
p53
S'Fahrenheit'
p54
g32
(g33
g34
Ntp55
Rp56
(dp57
g38
F459.67000000000002
sg39
g40
sg41
g32
(g42
g43
(dp58
g53
I1
stp59
Rp60
sg47
F0.55555555599999995
sbtp61
a(S'm2'
p62
S'Square Meter'
p63
S'm**2'
p64
tp65
a(S'pi'
p66
S'pi'
p67
S'3.141592653589*rad'
p68
tp69
a(S'lx'
p70
S'Lux'
p71
S'lm/m**2'
p72
tp73
a(S'ly'
p74
S' Light Year'
p75
S'9.46055e15*m'
p76
tp77
a(S'lambdan'
p78
S' Compton wavelength of neutron'
p79
S'1.3196217e-15*m'
p80
tp81
a(S'm3'
p82
S'Cubic Meter'
p83
S'm**3'
p84
tp85
a(S'F'
p86
S'Farad'
p87
S'C/V'
p88
tp89
a(S'hr'
p90
S'Hour'
p91
S'3600*s'
p92
tp93
a(S'H'
p94
S'Henry'
p95
S'Wb/A'
p96
tp97
a(S'J'
p98
S'Joule'
p99
S'N*m'
p100
tp101
a(S'phi0'
p102
S' magnetic flux quantum'
p103
S'2.0678538e-15*Wb'
p104
tp105
a(S'Sv'
p106
S'Sievert'
p107
S'J/kg'
p108
tp109
a(S'N'
p110
S'Newton'
p111
S'm*kg/s**2'
p112
tp113
a(S'R'
p114
S' gas constant'
p115
S'8.31424*J/(mol*degK)'
p116
tp117
a(S'T'
p118
S'Tesla'
p119
S'Wb/m**2'
p120
tp121
a(S'V'
p122
S'Volt'
p123
S'W/A'
p124
tp125
a(S'hme'
p126
S' quantum of circulation'
p127
S'7.273894e-4*J*s/kg'
p128
tp129
a(S'lambdap'
p130
S' Compton wavelength of proton'
p131
S'1.3214409e-15*m'
p132
tp133
a(S'me'
p134
S'electron mass'
p135
S'9.1093897e-31*kg'
p136
tp137
a(S'd'
p138
S'Day'
p139
S'86400*s'
p140
tp141
a(S'h'
p142
S' Plank constant'
p143
S'6.626196e-34*J/s'
p144
tp145
a(S'mn'
p146
S' neutron rest mass'
p147
S'1.674920e-27*kg'
p148
tp149
a(S'mi'
p150
S' Mile (U.S. statute)'
p151
S'1.609344e3*m'
p152
tp153
a(S'l'
p154
S' Liter'
p155
S'1e-3*m**3'
p156
tp157
a(S'Gy'
p158
S'Gray'
p159
S'J/kg'
p160
tp161
a(S'mp'
p162
S' proton rest mass'
p163
S'1.672614e-27*kg'
p164
tp165
a(S'galUS'
p166
S' Gallon (U.S.)'
p167
S'3.785411e-3*m**3'
p168
tp169
a(S'eps0'
p170
S'permittivity of vacuum'
p171
S'1/mu0/c**2'
p172
tp173
a(S'deg'
p174
S'Degree'
p175
S'(3.14159265/180)*rad'
p176
tp177
a(S'ohm'
p178
S'Ohm'
p179
S'V/A'
p180
tp181
a(S'Pa'
p182
S'Pascal'
p183
S'N/m**2'
p184
tp185
a(S'Nav'
p186
S'Avagadros number'
p187
S'6.0221367e23/mol'
p188
tp189
a(S'Hz'
p190
S'hertz'
p191
S'1/s'
p192
tp193
a(S'alpha0'
p194
S' Bohr radius'
p195
S'5.2917715e-11*m'
p196
tp197
a(S'mup'
p198
S' proton magnetic moment'
p199
S'1.4106203e-26*J/T'
p200
tp201
a(S'min'
p202
S'Minute'
p203
S'60*s'
p204
tp205
a(S'mue'
p206
S' electron magnetic moment'
p207
S'9.284851e-24*J/T'
p208
tp209
a(S're'
p210
S' classic electron radius'
p211
S'2.817939e-15*m'
p212
tp213
a(S'mub'
p214
S' bohr magneton'
p215
S'9.274096e-24*J/T'
p216
tp217
a(S'inch'
p218
S' Inch'
p219
S'2.54e-2*m'
p220
tp221
a(S'mun'
p222
S' nuclear magneton'
p223
S'5.050951e-27*J/T'
p224
tp225
a(S'C'
p226
S'Coulomb'
p227
S'A*s'
p228
tp229
a(S'Rinfinity'
p230
S' Rydberg constant'
p231
S'1.09737312e7/m'
p232
tp233
a(S'ft'
p234
S' Foot'
p235
S'3.048e-1*m'
p236
tp237
a(S'gammap'
p238
S' gyromagnetic ratio of protons in H2O'
p239
S'2.6751270e8*rad/(s*T)'
p240
tp241
a(S'wk'
p242
S'Week'
p243
S'7*d'
p244
tp245
a(S'Grav'
p246
S'universal gravitational constant'
p247
S'6.67259e-11*m**3/kg/s**2'
p248
tp249
a(S'S'
p250
S'Siemens'
p251
S'A/V'
p252
tp253
a(S'W'
p254
S' Watt'
p255
S'J/s'
p256
tp257
a(S'lambdac'
p258
S' Compton wavelength of electron'
p259
S'2.4263096e-12*m'
p260
tp261
a(S'nmi'
p262
S' Mile (U.S. nautical)'
p263
S'1.852e3*m'
p264
tp265
a(S'c'
p266
S'speed of light'
p267
S'299792458.*m/s'
p268
tp269
a(S'e'
p270
S'elementry charge'
p271
S'1.60217733e-19*C'
p272
tp273
a(S'mu0'
p274
S' permeability of vacuum'
p275
S'4.e-7*pi*N/A**2'
p276
tp277
a(S'g'
p278
S'gram'
p279
S'.001*kg'
p280
tp281
a(S'gammapc'
p282
S' gyromagnetic ratio of protons in H2O corrected for diamagnetism of H2O'
p283
S'2.6751965e8*rad/(s*T)'
p284
tp285
a(S'u'
p286
S' unified atomic mass unit'
p287
S'1.660531e-27*kg'
p288
tp289
a(S'sigma'
p290
S' Stefan-Boltzmann constant'
p291
S'5.66961e-8*W/(m**2*degK**-4)'
p292
tp293
a(g118
g119
g120
tp294
a(g94
g95
g96
tp295
a(g290
g291
g292
tp296
a(g102
g103
g104
tp297
a(g206
g207
g208
tp298
a(g126
g127
g128
tp299
a(g86
g87
g88
tp300
a(g178
g179
g180
tp301
a(g214
g215
g216
tp302
a(g170
g171
g172
tp303
a(g158
g159
g160
tp304
a(g14
g15
g16
tp305
a(g114
g115
g116
tp306
a(g198
g199
g200
tp307
a(g238
g239
g240
tp308
a(g122
g123
g124
tp309
a(g142
g143
g144
tp310
a(g98
g99
g100
tp311
a(g222
g223
g224
tp312
a(g254
g255
g256
tp313
a(g282
g283
g284
tp314
a(g106
g107
g108
tp315
a(g250
g251
g252
tp316
a(g118
g119
g120
tp317
a(g94
g95
g96
tp318
a(g290
g291
g292
tp319
a(g102
g103
g104
tp320
a(g206
g207
g208
tp321
a(g126
g127
g128
tp322
a(g178
g179
g180
tp323
a(g214
g215
g216
tp324
a(g158
g159
g160
tp325
a(g14
g15
g16
tp326
a(g114
g115
g116
tp327
a(g198
g199
g200
tp328
a(g238
g239
g240
tp329
a(g122
g123
g124
tp330
a(g142
g143
g144
tp331
a(g222
g223
g224
tp332
a(g282
g283
g284
tp333
a(g86
g87
g88
tp334
a(g250
g251
g252
tp335
a(g118
g119
g120
tp336
a(g94
g95
g96
tp337
a(g198
g199
g200
tp338
a(g238
g239
g240
tp339
a(g222
g223
g224
tp340
a(g102
g103
g104
tp341
a(g14
g15
g16
tp342
a(g206
g207
g208
tp343
a(g178
g179
g180
tp344
a(g214
g215
g216
tp345
a(g282
g283
g284
tp346
a(g118
g119
g120
tp347
a(g94
g95
g96
tp348
a(g198
g199
g200
tp349
a(g238
g239
g240
tp350
a(g222
g223
g224
tp351
a(g102
g103
g104
tp352
a(g206
g207
g208
tp353
a(g214
g215
g216
tp354
a(g282
g283
g284
tp355
asS'_defaults'
p356
(dp357
sS'unit_table'
p358
(dp359
g178
g32
(g33
g34
Ntp360
Rp361
(dp362
g38
I0
sg39
(lp363
I0
aI0
aI0
aI0
aI2
aI-2
aI0
aI1
aI-3
aI0
asg41
g32
(g42
g43
(dp364
g178
I1
stp365
Rp366
sg47
I1
sbsS'rad'
p367
g32
(g33
g34
Ntp368
Rp369
(dp370
g38
I0
sg39
(lp371
I0
aI1
aI0
aI0
aI0
aI0
aI0
aI0
aI0
aI0
asg41
g32
(g42
g43
(dp372
g367
I1
stp373
Rp374
sg47
I1
sbsg274
g32
(g33
g34
Ntp375
Rp376
(dp377
g38
I0
sg39
(lp378
I0
aI1
aI0
aI0
aI1
aI-2
aI0
aI1
aI-2
aI0
asg41
g32
(g42
g43
(dp379
g274
I1
stp380
Rp381
sg47
F1.2566370614355999e-006
sbsg150
g32
(g33
g34
Ntp382
Rp383
(dp384
g38
F0.0
sg39
(lp385
I0
aI0
aI0
aI0
aI1
aI0
aI0
aI0
aI0
aI0
asg41
g32
(g42
g43
(dp386
g150
I1
stp387
Rp388
sg47
F1609.3440000000001
sbsg122
g32
(g33
g34
Ntp389
Rp390
(dp391
g38
I0
sg39
(lp392
I0
aI0
aI0
aI0
aI2
aI-1
aI0
aI1
aI-3
aI0
asg41
g32
(g42
g43
(dp393
g122
I1
stp394
Rp395
sg47
I1
sbsg226
g32
(g33
g34
Ntp396
Rp397
(dp398
g38
I0
sg39
(lp399
I0
aI0
aI0
aI0
aI0
aI1
aI0
aI0
aI1
aI0
asg41
g32
(g42
g43
(dp400
g226
I1
stp401
Rp402
sg47
I1
sbsS'cd'
p403
g32
(g33
g34
Ntp404
Rp405
(dp406
g38
I0
sg39
(lp407
I0
aI0
aI0
aI0
aI0
aI0
aI0
aI0
aI0
aI1
asg41
g32
(g42
g43
(dp408
g403
I1
stp409
Rp410
sg47
I1
sbsg206
g32
(g33
g34
Ntp411
Rp412
(dp413
g38
I0
sg39
(lp414
I0
aI0
aI0
aI0
aI2
aI1
aI0
aI0
aI0
aI0
asg41
g32
(g42
g43
(dp415
g206
I1
stp416
Rp417
sg47
F9.2848510000000003e-024
sbsg182
g32
(g33
g34
Ntp418
Rp419
(dp420
g38
I0
sg39
(lp421
I0
aI0
aI0
aI0
aI-1
aI0
aI0
aI1
aI-2
aI0
asg41
g32
(g42
g43
(dp422
g182
I1
stp423
Rp424
sg47
I1
sbsg186
g32
(g33
g34
Ntp425
Rp426
(dp427
g38
I0
sg39
(lp428
I0
aI0
aI0
aI0
aI0
aI0
aI-1
aI0
aI0
aI0
asg41
g32
(g42
g43
(dp429
g186
I1
stp430
Rp431
sg47
F6.0221366999999997e+023
sbsg222
g32
(g33
g34
Ntp432
Rp433
(dp434
g38
I0
sg39
(lp435
I0
aI0
aI0
aI0
aI2
aI1
aI0
aI0
aI0
aI0
asg41
g32
(g42
g43
(dp436
g222
I1
stp437
Rp438
sg47
F5.0509510000000003e-027
sbsg106
g32
(g33
g34
Ntp439
Rp440
(dp441
g38
I0
sg39
(lp442
I0
aI0
aI0
aI0
aI2
aI0
aI0
aI0
aI-2
aI0
asg41
g32
(g42
g43
(dp443
g106
I1
stp444
Rp445
sg47
I1
sbsg126
g32
(g33
g34
Ntp446
Rp447
(dp448
g38
I0
sg39
(lp449
I0
aI0
aI0
aI0
aI2
aI0
aI0
aI0
aI-1
aI0
asg41
g32
(g42
g43
(dp450
g126
I1
stp451
Rp452
sg47
F0.00072738939999999997
sbsg214
g32
(g33
g34
Ntp453
Rp454
(dp455
g38
I0
sg39
(lp456
I0
aI0
aI0
aI0
aI2
aI1
aI0
aI0
aI0
aI0
asg41
g32
(g42
g43
(dp457
g214
I1
stp458
Rp459
sg47
F9.2740959999999994e-024
sbsg198
g32
(g33
g34
Ntp460
Rp461
(dp462
g38
I0
sg39
(lp463
I0
aI0
aI0
aI0
aI2
aI1
aI0
aI0
aI0
aI0
asg41
g32
(g42
g43
(dp464
g198
I1
stp465
Rp466
sg47
F1.4106203000000001e-026
sbsg190
g32
(g33
g34
Ntp467
Rp468
(dp469
g38
I0
sg39
(lp470
I0
aI0
aI0
aI0
aI0
aI0
aI0
aI0
aI-1
aI0
asg41
g32
(g42
g43
(dp471
g190
I1
stp472
Rp473
sg47
F1.0
sbsg6
g32
(g33
g34
Ntp474
Rp475
(dp476
g38
I0
sg39
(lp477
I0
aI0
aI0
aI0
aI3
aI0
aI-1
aI0
aI0
aI0
asg41
g32
(g42
g43
(dp478
g6
I1
stp479
Rp480
sg47
F0.00224136
sbsg10
g32
(g33
g34
Ntp481
Rp482
(dp483
g38
F0.0
sg39
g40
sg41
g32
(g42
g43
(dp484
g10
I1
stp485
Rp486
sg47
F0.55555555599999995
sbsg282
g32
(g33
g34
Ntp487
Rp488
(dp489
g38
I0
sg39
(lp490
I0
aI1
aI0
aI0
aI0
aI1
aI0
aI-1
aI1
aI0
asg41
g32
(g42
g43
(dp491
g282
I1
stp492
Rp493
sg47
F267519650.0
sbsS'$'
p494
g32
(g33
g34
Ntp495
Rp496
(dp497
g38
I0
sg39
(lp498
I0
aI0
aI0
aI1
aI0
aI0
aI0
aI0
aI0
aI0
asg41
g32
(g42
g43
(dp499
g494
I1
stp500
Rp501
sg47
I1
sbsg238
g32
(g33
g34
Ntp502
Rp503
(dp504
g38
I0
sg39
(lp505
I0
aI1
aI0
aI0
aI0
aI1
aI0
aI-1
aI1
aI0
asg41
g32
(g42
g43
(dp506
g238
I1
stp507
Rp508
sg47
F267512700.0
sbsg18
g32
(g33
g34
Ntp509
Rp510
(dp511
g38
F0.0
sg39
(lp512
I0
aI0
aI0
aI0
aI3
aI0
aI0
aI0
aI0
aI0
asg41
g32
(g42
g43
(dp513
g18
I1
stp514
Rp515
sg47
F0.00023659000000000001
sbsg22
g32
(g33
g34
Ntp516
Rp517
(dp518
g38
I0
sg39
(lp519
I1
aI0
aI0
aI0
aI0
aI0
aI0
aI0
aI0
aI1
asg41
g32
(g42
g43
(dp520
g22
I1
stp521
Rp522
sg47
I1
sbsg102
g32
(g33
g34
Ntp523
Rp524
(dp525
g38
F0.0
sg39
(lp526
I0
aI0
aI0
aI0
aI2
aI-1
aI0
aI1
aI-2
aI0
asg41
g32
(g42
g43
(dp527
g102
I1
stp528
Rp529
sg47
F2.0678538e-015
sbsg26
g32
(g33
g34
Ntp530
Rp531
(dp532
g38
F0.0
sg39
(lp533
I0
aI0
aI0
aI0
aI0
aI0
aI0
aI1
aI0
aI0
asg41
g32
(g42
g43
(dp534
g26
I1
stp535
Rp536
sg47
F1000.0
sbsg210
g32
(g33
g34
Ntp537
Rp538
(dp539
g38
F0.0
sg39
g385
sg41
g32
(g42
g43
(dp540
g210
I1
stp541
Rp542
sg47
F2.817939e-015
sbsg30
g36
sg49
g32
(g33
g34
Ntp543
Rp544
(dp545
g38
I0
sg39
(lp546
I0
aI0
aI0
aI0
aI0
aI0
aI0
aI0
aI-1
aI0
asg41
g32
(g42
g43
(dp547
g49
I1
stp548
Rp549
sg47
F1.0
sbsg53
g56
sg62
g32
(g33
g34
Ntp550
Rp551
(dp552
g38
I0
sg39
(lp553
I0
aI0
aI0
aI0
aI2
aI0
aI0
aI0
aI0
aI0
asg41
g32
(g42
g43
(dp554
g62
I1
stp555
Rp556
sg47
I1
sbsg218
g32
(g33
g34
Ntp557
Rp558
(dp559
g38
F0.0
sg39
g385
sg41
g32
(g42
g43
(dp560
g218
I1
stp561
Rp562
sg47
F0.025399999999999999
sbsg66
g32
(g33
g34
Ntp563
Rp564
(dp565
g38
F0.0
sg39
g371
sg41
g32
(g42
g43
(dp566
g66
I1
stp567
Rp568
sg47
F3.141592653589
sbsg14
g32
(g33
g34
Ntp569
Rp570
(dp571
g38
I0
sg39
g526
sg41
g32
(g42
g43
(dp572
g14
I1
stp573
Rp574
sg47
I1
sbsg70
g32
(g33
g34
Ntp575
Rp576
(dp577
g38
I0
sg39
(lp578
I1
aI0
aI0
aI0
aI-2
aI0
aI0
aI0
aI0
aI1
asg41
g32
(g42
g43
(dp579
g70
I1
stp580
Rp581
sg47
I1
sbsg74
g32
(g33
g34
Ntp582
Rp583
(dp584
g38
F0.0
sg39
g385
sg41
g32
(g42
g43
(dp585
g74
I1
stp586
Rp587
sg47
F9460550000000000.0
sbsS'A'
p588
g32
(g33
g34
Ntp589
Rp590
(dp591
g38
I0
sg39
(lp592
I0
aI0
aI0
aI0
aI0
aI1
aI0
aI0
aI0
aI0
asg41
g32
(g42
g43
(dp593
g588
I1
stp594
Rp595
sg47
I1
sbsg78
g32
(g33
g34
Ntp596
Rp597
(dp598
g38
F0.0
sg39
g385
sg41
g32
(g42
g43
(dp599
g78
I1
stp600
Rp601
sg47
F1.3196217e-015
sbsg98
g32
(g33
g34
Ntp602
Rp603
(dp604
g38
I0
sg39
(lp605
I0
aI0
aI0
aI0
aI2
aI0
aI0
aI1
aI-2
aI0
asg41
g32
(g42
g43
(dp606
g98
I1
stp607
Rp608
sg47
I1
sbsg234
g32
(g33
g34
Ntp609
Rp610
(dp611
g38
F0.0
sg39
g385
sg41
g32
(g42
g43
(dp612
g234
I1
stp613
Rp614
sg47
F0.30480000000000002
sbsg82
g32
(g33
g34
Ntp615
Rp616
(dp617
g38
I0
sg39
(lp618
I0
aI0
aI0
aI0
aI3
aI0
aI0
aI0
aI0
aI0
asg41
g32
(g42
g43
(dp619
g82
I1
stp620
Rp621
sg47
I1
sbsg86
g32
(g33
g34
Ntp622
Rp623
(dp624
g38
I0
sg39
(lp625
I0
aI0
aI0
aI0
aI-2
aI2
aI0
aI-1
aI4
aI0
asg41
g32
(g42
g43
(dp626
g86
I1
stp627
Rp628
sg47
I1
sbsg90
g32
(g33
g34
Ntp629
Rp630
(dp631
g38
I0
sg39
(lp632
I0
aI0
aI0
aI0
aI0
aI0
aI0
aI0
aI1
aI0
asg41
g32
(g42
g43
(dp633
g90
I1
stp634
Rp635
sg47
I3600
sbsg94
g32
(g33
g34
Ntp636
Rp637
(dp638
g38
I0
sg39
(lp639
I0
aI0
aI0
aI0
aI2
aI-2
aI0
aI1
aI-2
aI0
asg41
g32
(g42
g43
(dp640
g94
I1
stp641
Rp642
sg47
I1
sbsg230
g32
(g33
g34
Ntp643
Rp644
(dp645
g38
I0
sg39
(lp646
I0
aI0
aI0
aI0
aI-1
aI0
aI0
aI0
aI0
aI0
asg41
g32
(g42
g43
(dp647
g230
I1
stp648
Rp649
sg47
F10973731.199999999
sbsg242
g32
(g33
g34
Ntp650
Rp651
(dp652
g38
I0
sg39
g632
sg41
g32
(g42
g43
(dp653
g242
I1
stp654
Rp655
sg47
I604800
sbsg194
g32
(g33
g34
Ntp656
Rp657
(dp658
g38
F0.0
sg39
g385
sg41
g32
(g42
g43
(dp659
g194
I1
stp660
Rp661
sg47
F5.2917715000000002e-011
sbsg246
g32
(g33
g34
Ntp662
Rp663
(dp664
g38
I0
sg39
(lp665
I0
aI0
aI0
aI0
aI3
aI0
aI0
aI-1
aI-2
aI0
asg41
g32
(g42
g43
(dp666
g246
I1
stp667
Rp668
sg47
F6.6725899999999997e-011
sbsg110
g32
(g33
g34
Ntp669
Rp670
(dp671
g38
I0
sg39
(lp672
I0
aI0
aI0
aI0
aI1
aI0
aI0
aI1
aI-2
aI0
asg41
g32
(g42
g43
(dp673
g110
I1
stp674
Rp675
sg47
I1
sbsg250
g32
(g33
g34
Ntp676
Rp677
(dp678
g38
I0
sg39
(lp679
I0
aI0
aI0
aI0
aI-2
aI2
aI0
aI-1
aI3
aI0
asg41
g32
(g42
g43
(dp680
g250
I1
stp681
Rp682
sg47
I1
sbsg114
g32
(g33
g34
Ntp683
Rp684
(dp685
g38
I0
sg39
(lp686
I0
aI0
aI-1
aI0
aI2
aI0
aI-1
aI1
aI-2
aI0
asg41
g32
(g42
g43
(dp687
g114
I1
stp688
Rp689
sg47
F8.3142399999999999
sbsg118
g32
(g33
g34
Ntp690
Rp691
(dp692
g38
I0
sg39
(lp693
I0
aI0
aI0
aI0
aI0
aI-1
aI0
aI1
aI-2
aI0
asg41
g32
(g42
g43
(dp694
g118
I1
stp695
Rp696
sg47
I1
sbsg254
g32
(g33
g34
Ntp697
Rp698
(dp699
g38
I0
sg39
(lp700
I0
aI0
aI0
aI0
aI2
aI0
aI0
aI1
aI-3
aI0
asg41
g32
(g42
g43
(dp701
g254
I1
stp702
Rp703
sg47
I1
sbsS'mol'
p704
g32
(g33
g34
Ntp705
Rp706
(dp707
g38
I0
sg39
(lp708
I0
aI0
aI0
aI0
aI0
aI0
aI1
aI0
aI0
aI0
asg41
g32
(g42
g43
(dp709
g704
I1
stp710
Rp711
sg47
I1
sbsg270
g32
(g33
g34
Ntp712
Rp713
(dp714
g38
F0.0
sg39
g399
sg41
g32
(g42
g43
(dp715
g270
I1
stp716
Rp717
sg47
F1.6021773299999999e-019
sbsg258
g32
(g33
g34
Ntp718
Rp719
(dp720
g38
F0.0
sg39
g385
sg41
g32
(g42
g43
(dp721
g258
I1
stp722
Rp723
sg47
F2.4263096000000002e-012
sbsg130
g32
(g33
g34
Ntp724
Rp725
(dp726
g38
F0.0
sg39
g385
sg41
g32
(g42
g43
(dp727
g130
I1
stp728
Rp729
sg47
F1.3214409000000001e-015
sbsg262
g32
(g33
g34
Ntp730
Rp731
(dp732
g38
F0.0
sg39
g385
sg41
g32
(g42
g43
(dp733
g262
I1
stp734
Rp735
sg47
F1852.0
sbsg134
g32
(g33
g34
Ntp736
Rp737
(dp738
g38
F0.0
sg39
g533
sg41
g32
(g42
g43
(dp739
g134
I1
stp740
Rp741
sg47
F9.1093896999999993e-031
sbsg266
g32
(g33
g34
Ntp742
Rp743
(dp744
g38
I0
sg39
(lp745
I0
aI0
aI0
aI0
aI1
aI0
aI0
aI0
aI-1
aI0
asg41
g32
(g42
g43
(dp746
g266
I1
stp747
Rp748
sg47
F299792458.0
sbsS'degK'
p749
g32
(g33
g34
Ntp750
Rp751
(dp752
g38
I0
sg39
g40
sg41
g32
(g42
g43
(dp753
g749
I1
stp754
Rp755
sg47
I1
sbsS'kg'
p756
g32
(g33
g34
Ntp757
Rp758
(dp759
g38
I0
sg39
g533
sg41
g32
(g42
g43
(dp760
g756
I1
stp761
Rp762
sg47
I1
sbsg138
g32
(g33
g34
Ntp763
Rp764
(dp765
g38
I0
sg39
g632
sg41
g32
(g42
g43
(dp766
g138
I1
stp767
Rp768
sg47
I86400
sbsg278
g32
(g33
g34
Ntp769
Rp770
(dp771
g38
F0.0
sg39
g533
sg41
g32
(g42
g43
(dp772
g278
I1
stp773
Rp774
sg47
F0.001
sbsg202
g32
(g33
g34
Ntp775
Rp776
(dp777
g38
I0
sg39
g632
sg41
g32
(g42
g43
(dp778
g202
I1
stp779
Rp780
sg47
I60
sbsS'sr'
p781
g32
(g33
g34
Ntp782
Rp783
(dp784
g38
I0
sg39
(lp785
I1
aI0
aI0
aI0
aI0
aI0
aI0
aI0
aI0
aI0
asg41
g32
(g42
g43
(dp786
g781
I1
stp787
Rp788
sg47
I1
sbsg146
g32
(g33
g34
Ntp789
Rp790
(dp791
g38
F0.0
sg39
g533
sg41
g32
(g42
g43
(dp792
g146
I1
stp793
Rp794
sg47
F1.67492e-027
sbsS'm'
p795
g32
(g33
g34
Ntp796
Rp797
(dp798
g38
I0
sg39
g385
sg41
g32
(g42
g43
(dp799
g795
I1
stp800
Rp801
sg47
I1
sbsg154
g32
(g33
g34
Ntp802
Rp803
(dp804
g38
F0.0
sg39
(lp805
I0
aI0
aI0
aI0
aI3
aI0
aI0
aI0
aI0
aI0
asg41
g32
(g42
g43
(dp806
g154
I1
stp807
Rp808
sg47
F0.001
sbsg158
g32
(g33
g34
Ntp809
Rp810
(dp811
g38
I0
sg39
(lp812
I0
aI0
aI0
aI0
aI2
aI0
aI0
aI0
aI-2
aI0
asg41
g32
(g42
g43
(dp813
g158
I1
stp814
Rp815
sg47
I1
sbsS's'
p816
g32
(g33
g34
Ntp817
Rp818
(dp819
g38
I0
sg39
g632
sg41
g32
(g42
g43
(dp820
g816
I1
stp821
Rp822
sg47
I1
sbsg286
g32
(g33
g34
Ntp823
Rp824
(dp825
g38
F0.0
sg39
g533
sg41
g32
(g42
g43
(dp826
g286
I1
stp827
Rp828
sg47
F1.6605310000000001e-027
sbsg162
g32
(g33
g34
Ntp829
Rp830
(dp831
g38
F0.0
sg39
g533
sg41
g32
(g42
g43
(dp832
g162
I1
stp833
Rp834
sg47
F1.6726139999999998e-027
sbsg142
g32
(g33
g34
Ntp835
Rp836
(dp837
g38
I0
sg39
(lp838
I0
aI0
aI0
aI0
aI2
aI0
aI0
aI1
aI-3
aI0
asg41
g32
(g42
g43
(dp839
g142
I1
stp840
Rp841
sg47
F6.6261960000000003e-034
sbsg166
g32
(g33
g34
Ntp842
Rp843
(dp844
g38
F0.0
sg39
(lp845
I0
aI0
aI0
aI0
aI3
aI0
aI0
aI0
aI0
aI0
asg41
g32
(g42
g43
(dp846
g166
I1
stp847
Rp848
sg47
F0.0037854109999999998
sbsg290
g32
(g33
g34
Ntp849
Rp850
(dp851
g38
I0
sg39
(lp852
I0
aI0
aI4
aI0
aI0
aI0
aI0
aI1
aI-3
aI0
asg41
g32
(g42
g43
(dp853
g290
I1
stp854
Rp855
sg47
F5.6696099999999999e-008
sbsg170
g32
(g33
g34
Ntp856
Rp857
(dp858
g38
I0
sg39
(lp859
I0
aI-1
aI0
aI0
aI-3
aI2
aI0
aI-1
aI4
aI0
asg41
g32
(g42
g43
(dp860
g170
I1
stp861
Rp862
sg47
F8.8541878176226268e-012
sbsg174
g32
(g33
g34
Ntp863
Rp864
(dp865
g38
F0.0
sg39
g371
sg41
g32
(g42
g43
(dp866
g174
I1
stp867
Rp868
sg47
F0.017453292500000002
sbssS'prefixes'
p869
(dp870
S'a'
p871
F1.0000000000000001e-018
sg266
F0.01
sS'z'
p872
F9.9999999999999991e-022
sS'E'
p873
F1e+018
sg138
F0.10000000000000001
sS'G'
p874
F1000000000.0
sS'f'
p875
F1.0000000000000001e-015
sg142
F100.0
sS'k'
p876
F1000.0
sS'M'
p877
F1000000.0
sg795
F0.001
sS'p'
p878
F9.9999999999999998e-013
sS'da'
p879
F10.0
sS'P'
p880
F1000000000000000.0
sS'n'
p881
F1.0000000000000001e-009
sg286
F9.9999999999999995e-007
sg118
F1000000000000.0
sS'Y'
p882
F9.9999999999999998e+023
sS'Z'
p883
F1e+021
sS'y'
p884
F9.9999999999999992e-025
ssS'base_names'
p885
(lp886
g781
ag367
ag749
ag494
ag795
ag588
ag704
ag756
ag816
ag403
asS'base_types'
p887
(dp888
S'solid_angle'
p889
I0
sS'angle'
p890
I1
sS'temperature'
p891
I2
sS'money'
p892
I3
sS'amount'
p893
I6
sS'current'
p894
I5
sS'length'
p895
I4
sS'mass'
p896
I7
sS'time'
p897
I8
sS'luminous_intesity'
p898
I9
ssS'_sections'
p899
(dp900
S'prefixes'
p901
(dp902
g871
S'1.e-18'
p903
sg266
S'1.e-2'
p904
sg872
S'1.e-21'
p905
sg873
S'1.e18'
p906
sg138
S'1.e-1'
p907
sg874
S'1.e9'
p908
sg878
S'1.e-12'
p909
sg142
S'1.e2'
p910
sg876
S'1.e3'
p911
sg875
S'1.e-15'
p912
sg877
S'1.e6'
p913
sg795
S'1.e-3'
p914
sg879
S'1.e1'
p915
sg880
S'1.e15'
p916
sg881
S'1.e-9'
p917
sg286
S'1.e-6'
p918
sg118
S'1.e12'
p919
sg882
S'1.e24'
p920
sS'__name__'
p921
g901
sg883
S'1.e21'
p922
sg884
S'1.e-24'
p923
ssS'base_units'
p924
(dp925
g889
g781
sg890
g367
sg891
g749
sg892
g494
sg893
g704
sg894
g588
sg895
g795
sg896
g756
sg897
g816
sg921
g924
sg898
g403
ssS'units'
p926
(dp927
g178
g361
sg274
g376
sg78
g597
sg206
g412
sg182
g419
sg186
g426
sg126
g447
sg146
g790
sg214
g454
sg198
g461
sg190
g468
sg6
g475
sg10
g482
sg14
g570
sg238
g503
sg202
g776
sg194
g657
sg22
g517
sg102
g524
sg26
g531
sg210
g538
sg30
g36
sg49
g544
sg53
g56
sg62
g551
sg218
g558
sg66
g564
sg70
g576
sg74
g583
sg226
g397
sg230
g644
sg234
g610
sg82
g616
sg86
g623
sg90
g630
sg94
g637
sg98
g603
sg242
g651
sg106
g440
sg246
g663
sg110
g670
sg250
g677
sg114
g684
sg118
g691
sg254
g698
sg122
g390
sg921
g926
sg258
g719
sg130
g725
sg262
g731
sg134
g737
sg266
g743
sg222
g433
sg270
g713
sg138
g764
sg278
g770
sg282
g488
sg142
g836
sg18
g510
sg150
g383
sg154
g803
sg158
g810
sg286
g824
sg162
g830
sg166
g843
sg290
g850
sg170
g857
sg174
g864
sssb."""
    
    _unitLib = pickle.loads(default_lib_string)
except IOError: 
    defaultLib = resource_stream(__name__, 'unitLibdefault.ini')
    importLibrary(defaultLib)
    f = open('unitLib.save','wb')
    pickle.dump(_unitLib,f,protocol = 0)





