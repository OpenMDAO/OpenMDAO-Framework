"""This module provides a data type that represents a physical
quantity together with its unit. It is possible to add and
subtract these quantities if the units are compatible and
a quantity can be converted to another compatible unit.
Multiplication, subtraction, and raising to integer powers
are allowed without restriction, and the result will have
the correct unit. A quantity can be raised to a non-integer
power only if the result can be represented by integer powers
of the base units.

The module provides a basic set of predefined physical quanitites
in its built-in library; however, it also supports generation of
personal libararies which can be saved and reused.

This module is based on the PhysicalQuantities module
in Scientific Python, by Konrad Hinsen. Modifications by
Justin Gray."""

import re, ConfigParser
import os.path

from math import sin, cos, tan, floor, pi

# pylint: disable-msg=E0611,F0401, E1101
try:
    from pkg_resources import resource_stream
except ImportError:
    pass

#Class definitions

class NumberDict(dict):
    """
    Dictionary storing numerical values.

    Constructor: NumberDict()

    An instance of this class acts like an array of numbers with
    generalized (non-integer) indices. A value of zero is assumed
    for undefined entries. NumberDict instances support addition
    and subtraction with other NumberDict instances, and multiplication
    and division by scalars.
    """

    def __getitem__(self, item):
        try:
            return dict.__getitem__(self, item)
        except KeyError:
            return 0

    def __coerce__(self, other):
        if isinstance(other, dict):
            other = NumberDict(other)
        return self, other

    def __add__(self, other):
        sum_dict = NumberDict()
        for k, v in self.iteritems():
            sum_dict[k] = v
        for k, v in other.iteritems():
            sum_dict[k] = sum_dict[k] + v
        return sum_dict

    def __sub__(self, other):
        sum_dict = NumberDict()
        for k, v in self.iteritems():
            sum_dict[k] = v
        for k, v in other.iteritems():
            sum_dict[k] = sum_dict[k] - v
        return sum_dict

    def __mul__(self, other):
        new = NumberDict()
        for key, value in self.iteritems():
            new[key] = other*value
        return new

    __rmul__ = __mul__

    def __div__(self, other):
        new = NumberDict()
        for key, value in self.iteritems():
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
                and unit is a string defining the unit.

                2. PhysicalQuantity(value_with_unit), where value_with_unit
                is a string that contains both the value and the unit,
                i.e., '1.5 m/s'. This form is provided for more convenient
                interactive use.

         @param args: either (value, unit) or (value_with_unit,)
         @type args: (number, C{str}) or (C{str},)
         """

        if len(args) == 2:
            self.value = args[0]
            self.unit = _find_unit(args[1])
        else:
            s = args[0].strip()
            match = PhysicalQuantity._number.match(s)
            if match is None:
                raise TypeError("No number found in input argument: '%s'"%s)
            self.value = float(match.group(0))
            self.unit = _find_unit(s[len(match.group(0)):].strip())



    def __str__(self):
        return str(self.value) + ' ' + self.unit.name()

    def __repr__(self):
        return (self.__class__.__name__ + '(' + repr(self.value) + ',' +
                repr(self.unit.name()) + ')')


    def _sum(self, other, sign1, sign2):
        """sums units"""
        if not isinstance(other, PhysicalQuantity):
            raise TypeError('Incompatible types')
        new_value = sign1*self.value + \
                  sign2*other.value*other.unit.conversion_factor_to(self.unit)
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
        if not isinstance(other, PhysicalQuantity):
            return self.__class__(self.value*other, self.unit)
        value = self.value*other.value
        unit = self.unit*other.unit
        if unit.is_dimensionless():
            return value*unit.factor
        else:
            return PhysicalQuantity(value, unit)

    __rmul__ = __mul__

    def __div__(self, other):
        if not isinstance(other, PhysicalQuantity):
            return self.__class__(self.value/other, self.unit)
        value = self.value/other.value
        unit = self.unit/other.unit
        if unit.is_dimensionless():
            return value*unit.factor
        else:
            return self.__class__(value, unit)

    def __rdiv__(self, other):
        if not isinstance(other, PhysicalQuantity):
            return self.__class__(other/self.value, pow(self.unit, -1))
        value = other.value/self.value
        unit = other.unit/self.unit
        if unit.is_dimensionless():
            return value*unit.factor
        else:
            return self.__class__(value, unit)

    def __pow__(self, other):
        if isinstance(other, PhysicalQuantity):
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

    def convert_value(self, target_unit):
        """Converts the values of the PQ to the target_unit."""
        (factor, offset) = self.unit.conversion_tuple_to(target_unit)
        return (self.value + offset) * factor

    def convert_to_unit(self, unit):
        """
        Change the unit and adjust the value so that
        the combination is equivalent to the original one. The new unit
        must be compatible with the previous unit of the object.

        @param unit: a unit
        @type unit: C{str}
        @raise TypeError: if the unit string is not a known unit or a
        unit incompatible with the current one.
        """
        unit = _find_unit(unit)

        self.value = self.convert_value(unit)
        self.unit = unit

    def in_units_of(self, unit):
        """
        Express the quantity in different units. If one unit is
        specified, a new PhysicalQuantity object is returned that
        expresses the quantity in that unit. If several units
        are specified, the return value is a tuple of
        PhysicalObject instances with with one element per unit such
        that the sum of all quantities in the tuple equals the
        original quantity and all the values except for the last one
        are integers. This is used to convert to irregular unit
        systems like hour/minute/second.

        @param units: one or several units
        @type units: C{str} or sequence of C{str}
        @returns: one or more physical quantities
        @rtype: L{PhysicalQuantity} or C{tuple} of L{PhysicalQuantity}
        @raises TypeError: if any of the specified units are not compatible
        with the original unit.
        """
        unit = _find_unit(unit)

        value = self.convert_value(unit)
        return self.__class__(value, unit)


    # Contributed by Berthold Hoellmann
    def in_base_units(self):
        """
        @returns: the same quantity converted to base units,
        i.e., SI units in most cases
        @rtype: L{PhysicalQuantity}
        """
        new_value = self.value * self.unit.factor
        num = ''
        denom = ''
        for unit, power in zip(_UNIT_LIB.base_names, self.unit.powers):
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

    def is_compatible(self, unit):
        """
        @param unit: a unit
        @type unit: C{str}
        @returns: C{True} if the specified unit is compatible with the
        one of the quantity.
        @rtype: C{bool}.
        """
        unit = _find_unit(unit)
        return self.unit.is_compatible(unit)

    def get_value(self):
        """Return value (float) of physical quantity (no unit)."""
        return self.value

    def get_unit_name(self):
        """Return unit (string) of physical quantity."""
        return self.unit.name()

    def sqrt(self):
        """Parsing Square Root"""
        return pow(self, 0.5)

    def sin(self):
        """Parsing Sine."""
        if self.unit.is_angle():
            #return N.sin(self.value * \
            return sin(self.value * \
                self.unit.conversion_factor_to(PhysicalQuantity('1rad').unit))
        else:
            raise TypeError('Argument of sin must be an angle')

    def cos(self):
        """Parsing Cosine."""
        if self.unit.is_angle():
            #return N.cos(self.value * \
            return cos(self.value * \
                self.unit.conversion_factor_to(PhysicalQuantity('1rad').unit))
        else:
            raise TypeError('Argument of cos must be an angle')

    def tan(self):
        """Parsing tangent."""
        if self.unit.is_angle():
            #return N.tan(self.value * \
            return tan(self.value * \
                self.unit.conversion_factor_to(PhysicalQuantity('1rad').unit))
        else:
            raise TypeError('Argument of tan must be an angle')


class UnitsOnlyPQ(PhysicalQuantity):
    """When you want to determine the units of a combined expression without
    worrying about possible problems with the actual values (divide by zero, etc.),
    replace the variables in the expression with instances of this class, evaluate
    the expression, and retrieve the units of the result.

    WARNING: only the units of the resulting UnitsOnlyPQ object will be correct. The value
    may be incorrect, so don't use it.
    """
    def __div__(self, other):
        if not isinstance(other, PhysicalQuantity):
            return self.__class__(self.value, self.unit)
        value = self.value
        unit = self.unit/other.unit
        if unit.is_dimensionless():
            return value*unit.factor
        else:
            return self.__class__(value, unit)

    def __rdiv__(self, other):
        if not isinstance(other, PhysicalQuantity):
            return self.__class__(other, pow(self.unit, -1))
        value = other.value
        unit = other.unit/self.unit
        if unit.is_dimensionless():
            return value*unit.factor
        else:
            return self.__class__(value, unit)

    def tan(self):
        if self.unit.is_angle():
            #return N.tan(self.value * \
            return tan(0. * \
                self.unit.conversion_factor_to(PhysicalQuantity('1rad').unit))
        else:
            raise TypeError('Argument of tan must be an angle')


class PhysicalUnit(object):
    """
    Physical unit.

    A physical unit is defined by a name (possibly composite), a scaling
    factor, and the exponentials of each of the SI base units that enter into
    it. Units can be multiplied, divided, and raised to integer powers.
    """

    def __init__(self, names, factor, powers, offset=0):
        """
        @param names: a dictionary mapping each name component to its
                      associated integer power (e.g., C{{'m': 1, 's': -1}})
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

        if isinstance(names, str):
            self.names = NumberDict(((names, 1),))
            #self.names[names] = 1;

        else:
            self.names = names

        self.factor = float(factor)
        self.offset = float(offset)
        self.powers = powers

    def __repr__(self):
        return 'PhysicalUnit(%s,%s,%s,%s)'% (self.names, self.factor,
                                             self.powers, self.offset)

    def __str__(self):
        return '<PhysicalUnit ' + self.name() + '>'

    def __cmp__(self, other):
        if self.powers != other.powers:
            raise TypeError('Incompatible units')
        return cmp(self.factor, other.factor)

    def __mul__(self, other):
        if self.offset != 0 or (isinstance(other, PhysicalUnit) and \
                                other.offset != 0):
            raise TypeError("cannot multiply units with non-zero offset")
        if isinstance(other, PhysicalUnit):
            return PhysicalUnit(self.names+other.names,
                              self.factor*other.factor,
                        [a+b for (a, b) in zip(self.powers, other.powers)])
        else:
            return PhysicalUnit(self.names+{str(other): 1},
                                self.factor*other,
                                self.powers,
                                self.offset * other)

    __rmul__ = __mul__

    def __div__(self, other):
        if self.offset != 0 or (isinstance(other, PhysicalUnit) and \
                                other.offset != 0):
            raise TypeError("cannot divide units with non-zero offset")
        if isinstance(other, PhysicalUnit):
            return PhysicalUnit(self.names-other.names,
                                self.factor/other.factor,
                            [a-b for (a, b) in zip(self.powers, other.powers)])
        else:
            return PhysicalUnit(self.names+{str(other): -1},
                                self.factor/float(other), self.powers)

    def __rdiv__(self, other):
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
            #rounded = int(N.floor(inv_exp+0.5))
            rounded = int(floor(inv_exp+0.5))
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
                        for x, name in zip(p, _UNIT_LIB.base_names):
                            names[name] = x
                    return PhysicalUnit(names, f, p)

        raise TypeError('Only integer and inverse integer exponents allowed')

    def conversion_factor_to(self, other):
        """
        @param other: another unit
        @type other: L{PhysicalUnit}
        @returns: the conversion factor from this unit to another unit
        @rtype: C{float}
        @raises TypeError: if the units are not compatible.
        """
        if self.powers != other.powers:
            raise TypeError('Incompatible units')

        if self.offset != other.offset and self.factor != other.factor:
            raise TypeError(('Unit conversion (%s to %s) cannot be expressed' +
                             ' as a simple multiplicative factor') % \
                             (self.name(), other.name()))

        return self.factor/other.factor

    # added 1998/09/29 GPW
    def conversion_tuple_to(self, other):
        """
        @param other: another unit
        @type other: L{PhysicalUnit}
        @returns: the conversion factor and offset from this unit to another unit
        @rtype: (C{float}, C{float})
        @raises TypeError: if the units are not compatible.
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

    # added 1998/10/01 GPW
    def is_compatible(self, other):
        """
        @param other: another unit
        @type other: L{PhysicalUnit}
        @returns: C{True} if the units are compatible, i.e., if the powers of the base units are the same
        @rtype: C{bool}.
        """
        return self.powers == other.powers

    def is_dimensionless(self):
        """Dimensionless PQ."""
        return not any(self.powers)

    def is_angle(self):
        """Checks if this PQ is an Angle."""
        return (self.powers[_UNIT_LIB.base_types['angle']] == 1 and \
                             sum(self.powers) == 1)

    def set_name(self, name):
        """Sets the name."""
        self.names = NumberDict()
        self.names[name] = 1

    def name(self):
        """Looks like it's parsing fractions."""
        num = ''
        denom = ''
        for unit, power in self.names.iteritems():
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


#Helper Functions

_UNIT_CACHE = {}

def _find_unit(unit):
    """Find unit helper function."""
    if isinstance(unit, str):
        name = unit.strip()
        try:
            unit = _UNIT_CACHE[name]
        except KeyError:
            try:
                unit = eval(name, {'__builtins__':None}, _UNIT_LIB.unit_table)
            except Exception:

                # This unit might include prefixed units that aren't in the
                # unit_table. We must parse them ALL and add them to the
                # unit_table.

                # First character of a unit is always alphabet or $.
                # Remaining characters may include numbers.
                regex = re.compile('[A-Z,a-z]{1}[A-Z,a-z,0-9]*')

                for item in regex.findall(name):
                    #check if this was a compound unit, so each substring might
                    # be a unit
                    try:
                        eval(item, {'__builtins__':None}, _UNIT_LIB.unit_table)
                    except Exception: #maybe is a prefixed unit then
                        #check for single letter prefix before unit
                        if(item[0] in _UNIT_LIB.prefixes and \
                           item[1:] in _UNIT_LIB.unit_table):
                            add_unit(item, _UNIT_LIB.prefixes[item[0]]* \
                                     _UNIT_LIB.unit_table[item[1:]])

                        #check for double letter prefix before unit
                        elif(item[0:2] in _UNIT_LIB.prefixes and \
                             item[2:] in _UNIT_LIB.unit_table):
                            add_unit(item, _UNIT_LIB.prefixes[item[0:2]]* \
                                      _UNIT_LIB.unit_table[item[2:]])

                        #no prefixes found, unknown unit
                        else:
                            raise ValueError("no unit named '%s' is defined"
                                             % item)

                unit = eval(name, {'__builtins__':None}, _UNIT_LIB.unit_table)

            _UNIT_CACHE[name] = unit

    if not isinstance(unit, PhysicalUnit):
        raise TypeError(str(unit) + ' is not a unit')
    return unit


def _new_unit(name, factor, powers):
    """create new Unit"""
    _UNIT_LIB.unit_table[name] = PhysicalUnit(name, factor, powers)


def add_offset_unit(name, baseunit, factor, offset, comment=''):
    """Adding Offset Unit."""
    if isinstance(baseunit, str):
        baseunit = _find_unit(baseunit)
    #else, baseunit should be a instance of PhysicalUnit
    #names, factor, powers, offset=0
    unit = PhysicalUnit(baseunit.names, baseunit.factor*factor,
                        baseunit.powers, offset)
    unit.set_name(name)
    if name in _UNIT_LIB.unit_table:
        if (_UNIT_LIB.unit_table[name].factor!=unit.factor or \
            _UNIT_LIB.unit_table[name].powers!=unit.powers):
            raise KeyError, "Unit %s already defined with " % name + \
                            "different factor or powers"
    _UNIT_LIB.unit_table[name] = unit
    _UNIT_LIB.set('units', name, unit)
    if comment:
        _UNIT_LIB.help.append((name, comment, unit))


def add_unit(name, unit, comment=''):
    """Adding Unit."""
    if comment:
        _UNIT_LIB.help.append((name, comment, unit))
    if isinstance(unit, str):
        unit = eval(unit, {'__builtins__':None, 'pi':pi},
                           _UNIT_LIB.unit_table)
    unit.set_name(name)
    if name in _UNIT_LIB.unit_table:
        if (_UNIT_LIB.unit_table[name].factor!=unit.factor or \
            _UNIT_LIB.unit_table[name].powers!=unit.powers):
            raise KeyError, "Unit %s already defined with " % name + \
                            "different factor or powers"

    _UNIT_LIB.unit_table[name] = unit
    _UNIT_LIB.set('units', name, unit)


_UNIT_LIB = ConfigParser.ConfigParser()

def _do_nothing(string):
    """Makes the ConfigParser case sensitive."""
    return string

_UNIT_LIB.optionxform = _do_nothing


def import_library(libfilepointer):
    """Imports a units library, replacing any existing definitions."""
    global _UNIT_LIB
    global _UNIT_CACHE
    _UNIT_CACHE = {}
    _UNIT_LIB = ConfigParser.ConfigParser()
    _UNIT_LIB.optionxform = _do_nothing
    _UNIT_LIB.readfp(libfilepointer)
    required_base_types = ['length', 'mass', 'time', 'temperature', 'angle']
    _UNIT_LIB.base_names = list()
    #used to is_angle() and other base type checking
    _UNIT_LIB.base_types = dict()
    _UNIT_LIB.unit_table = dict()
    _UNIT_LIB.prefixes = dict()
    _UNIT_LIB.help = list()

    for prefix, factor in _UNIT_LIB.items('prefixes'):
        factor, comma, comment = factor.partition(',')
        _UNIT_LIB.prefixes[prefix] = float(factor)

    base_list = [0] * len(_UNIT_LIB.items('base_units'))

    for i, (unit_type, name) in enumerate(_UNIT_LIB.items('base_units')):
        _UNIT_LIB.base_types[unit_type] = i
        powers = list(base_list)
        powers[i] = 1
        #print '%20s'%unit_type, powers
        #cant use add_unit because no base units exist yet
        _new_unit(name, 1, powers)
        _UNIT_LIB.base_names.append(name)

    #test for required base types
    missing = [utype for utype in required_base_types
                               if not utype in _UNIT_LIB.base_types]
    if missing:
        raise ValueError('Not all required base type were present in the'
                         ' config file. missing: %s, at least %s required'
                         % (missing, required_base_types))

    # Explicit unitless 'unit'.
    _new_unit('unitless', 1, list(base_list))
    _update_library(_UNIT_LIB)
    return _UNIT_LIB


def update_library(filename):
    """
    Update units in current library from `filename` which must contain a
    ``units`` section.

    filename: string or file
        Source of units configuration data.
    """
    if isinstance(filename, basestring):
        inp = open(filename, 'rU')
    else:
        inp = filename
    try:
        cfg = ConfigParser.ConfigParser()
        cfg.optionxform = _do_nothing
        cfg.readfp(inp)
        _update_library(cfg)
    finally:
        inp.close()

def _update_library(cfg):
    """ Update library from :class:`ConfigParser` `cfg`. """
    retry1 = set()
    for name, unit in cfg.items('units'):
        data = [item.strip() for item in unit.split(',')]
        if len(data) == 2:
            unit, comment = data
            try:
                add_unit(name, unit, comment)
            except NameError:
                retry1.add((name, unit, comment))
        elif len(data) == 4:
            factor, baseunit, offset, comment = data
            try:
                add_offset_unit(name, baseunit, float(factor), float(offset),
                                comment)
            except NameError:
                retry1.add((name, baseunit, float(factor), float(offset),
                            comment))
        else:
            raise ValueError('Unit %r definition %r has invalid format',
                             name, unit)
    retry_count = 0
    last_retry_count = -1
    while last_retry_count != retry_count and retry1:
        last_retry_count = retry_count
        retry_count = 0
        retry2 = retry1.copy()
        for data in retry2:
            if len(data) == 3:
                name, unit, comment = data
                try:
                    add_unit(name, unit, comment)
                    retry1.remove(data)
                except NameError:
                    retry_count += 1
            else:
                try:
                    name, factor, baseunit, offset, comment = data
                    add_offset_unit(name, factor, baseunit, offset, comment)
                    retry1.remove(data)
                except NameError:
                    retry_count += 1
    if retry1:
        raise ValueError('The following units were not defined because they'
                         ' could not be resolved as a function of any other'
                         ' defined units:%s' % [x[0] for x in retry1])


def convert_units(value, units, convunits):
    """Return the given value (given in units) converted
    to convunits.
    """
    pq = PhysicalQuantity(value, units)
    pq.convert_to_unit(convunits)
    return pq.value


try:
    default_lib = resource_stream(__name__, 'unitLibdefault.ini')
except NameError: #pck_resources was not imported, try __file__
    default_lib = open(os.path.join(os.path.dirname(__file__),
                                   'unitLibDefault.ini'))
import_library(default_lib)

