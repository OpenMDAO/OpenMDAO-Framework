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
import ConfigParser
import os.path
import numpy as N

try: 
    from pkg_resources import resource_string, resource_stream
except ImportError: pass

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
    if isinstance(other,dict):
        other = NumberDict(other)
    return self, other

  def __add__(self, other):
    sum_dict = NumberDict()
    for k,v in self.iteritems():
        sum_dict[k] = v
    for k,v in other.iteritems(): 
        sum_dict[k] = sum_dict[k] + v
    return sum_dict

    return sum_dict

  def __sub__(self, other):
    sum_dict = NumberDict()
    for k,v in self.iteritems():
        sum_dict[k] = v
    for k,v in other.iteritems(): 
        sum_dict[k] = sum_dict[k] - v
    return sum_dict

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
    return PhysicalQuantity(new_value, self.unit)
    
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
      return PhysicalQuantity(value, unit)

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
      return N.sin(self.value * \
             self.unit.conversionFactorTo(PhysicalQuantity('1rad').unit))
    else:
      raise TypeError('Argument of sin must be an angle')

  def cos(self):
    if self.unit.isAngle():
        return N.cos(self.value * \
            self.unit.conversionFactorTo(PhysicalQuantity('1rad').unit))
    else:
        raise TypeError('Argument of cos must be an angle')

  def tan(self):
    if self.unit.isAngle():
        return N.tan(self.value * \
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
    if isinstance(names,str):
      self.names = NumberDict(((names,1),))
      #self.names[names] = 1;

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
      rounded = int(N.floor(inv_exp+0.5))
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
            for x,name in zip(p,_unitLib.base_names): 
              names[name] = x
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
    if isinstance(baseunit,str):
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
    if isinstance(unit,str):
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
    defaultLib = resource_stream(__name__, 'unitLibdefault.ini')
except NameError: #pck_resources was not imorted, try __file__
    defaultLib = open(os.path.join(os.path.dirname(__file__),'unitLibDefault.ini'))

importLibrary(defaultLib)






