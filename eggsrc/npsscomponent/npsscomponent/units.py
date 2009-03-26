"""
Translation from NPSS units to OpenMDAO units.

    .. parsed-literal::

        TODO: complete and verify translations.

"""

__all__ = (
    'have_translation',
    'get_translation',
    'set_translation',
)

__version__ = '0.1'

from openmdao.main.variable import UNDEFINED

def have_translation(npss_units):
    """ Return True if we can translate npss_units. """
    try:
        units = get_translation(npss_units)
        return units is not UNDEFINED
    except KeyError:
        return False

def get_translation(npss_units):
    """ Return translation for npss_units. """
    return UNITS_MAP[npss_units]

def set_translation(npss_units, mdao_units):
    """ Set translation for npss_units. """
    assert isinstance(npss_units, basestring)
    assert isinstance(mdao_units, basestring)
    UNITS_MAP[npss_units] = mdao_units


# NPSS unit strings taken from /NPSS/dev/Executive/src/Common/UnitNames.ncp
# UNDEFINED => not specified.
# None      => unitless.
UNITS_MAP = {
   '' : UNDEFINED,

   'atm' : 'atm',

   'bar' : 'bar', 
   'Btu' : 'Btu',
   'Btu/hr' : 'Btu/h',
   'Btu/(hr*ft*F)' : 'Btu/(h*ft*degF)',
   'Btu/(hr*ft2)' : 'Btu/(h*ft**2)',
   'Btu/(hr*ft2*F)' : 'Btu/(h*ft**2*degF)',
   'Btu/(hr*ft3)' : 'Btu/(h*ft**3)',
   'Btu/lbm' : 'Btu/lb',
   'Btu/(lbm*R)' : 'Btu/(lb*degR)',
   'Btu/(lbm*R*R)' : 'Btu/(lb*degR**2)',
   'Btu/(lbm*sec)' : 'Btu/(lb*s)',
   'Btu/R' : 'Btu/degR',
   'Btu/(R*in)' : 'Btu/(degR*inch)',
   'Btu/sec' : 'Btu/s',
   'Btu/(sec*ft*R)' : 'Btu/(s*ft*degR)',
   'Btu/(sec*ft2)' : 'Btu/(s*ft**2)',
   'Btu/(sec*in*R)' : 'Btu/(s*inch*degR)',
   'Btu/(sec*in2*R)' : 'Btu/(s*inch**2*degR)', 
   'Btu/(sec*R)' : 'Btu/(s*degR)',
   'Btu*sec2/(lbm*ft2)' : 'Btu*s**2/(lb*ft**2)',

   'C' : 'degC',
   'cm' : 'cm',
   'cm2' : 'cm**2',
   'cm3' : 'cm**3',
   'cm4' : 'cm**4',
   'cm5' : 'cm**5',

   'daN' : 'daN',
   'day' : UNDEFINED,
   'dC' : 'degC',
   'deg' : 'deg',
   'dF' : 'degF',
   'dK' : 'K',
   'dR' : 'regR',

   'F' : 'degF',
   'ft' : 'ft',
   'ft*lbf' : UNDEFINED,
   'ft*lbf*in3/(lbm*rad*rad)' : UNDEFINED,
   'ft*lbf/Btu' : UNDEFINED,
   'ft*lbf/(R*lbm)' : UNDEFINED,
   'ft*lbf/(R*lbmol)' : UNDEFINED,
   'ft*lbf/rad' : UNDEFINED,
   'ft*lbf/(rad*sec)' : UNDEFINED,
   'ft*lbf/sec' : UNDEFINED,
   'ft*lbm/(lbf*sec2)' : UNDEFINED,
   'ft/sec' : 'ft/s',
   'ft/sec2' : 'ft/s**2',
   'ft2' : 'ft**2',
   'ft2*lbm' : 'ft**2*lb',
   'ft2/sec2' : 'ft**2/s**2',
   'ft2/(sec2*R)' : 'ft**2/(s**2*degR)',
   'ft3' : 'ft**3',
   'ft3/lbm' : 'ft**3/lb',

   'gal' : 'galUS',
   'gal*in/(min*SQRT_lbf)' : UNDEFINED,
   'grains' : UNDEFINED,
   'g/sec' : UNDEFINED,

   'hbar' : 'hbar',
   'Hz' : 'Hz',
   'hp' : 'hp',
   'hp/ft2' : 'hp/ft**2',
   'hr' : 'h',
   'hp*hr' : 'hp*h',

   'in' : 'inch',
   'in*lbf*sec2/lbm' : UNDEFINED,
   'in/sec' : 'inch/s',
   'in2' : 'inch**2',
   'in2/lbf' : UNDEFINED,
   'in3' : 'inch**3',
   'in3*sec/rad' : 'inch**3*s/rad',
   'in4' : 'inch**4',
   'in4*sec2/(lbm*ft)' : 'in**4*sec**2/(lb*ft)',
   'in5' : 'inch**5',
   'inH2O' : UNDEFINED,
   'inHg' : UNDEFINED,

   'J/kg' : 'J/kg',
   'J/(kg*K)' : 'J/(kg*K)',
   'J/sec' : 'J/s',
   'J' : 'J',

   'K' : 'K',
   'kg' : 'kg',
   'kg*m2' : 'kg*m**2',
   'kg/J' : 'kg/J',
   'kg/m3' : 'kg/m**3',
   'kg/(N*sec)' : 'kg/(N*s)',
   'kg/sec' : 'kg/s',
   'kJ/kg' : 'kJ/kg',
   'kJ/(kg*K)' : 'kJ/(kg*K)',
   'km' : 'km',
   'kN' : 'kN',
   'knot' : UNDEFINED,
   'kPa' : 'kPa',
   'kW' : 'kW',

   'lbf' : UNDEFINED,
   'lbf*in*sec2/lbm' : UNDEFINED,
   'lbf*in2' : UNDEFINED,
   'lbf*in4*sec2/lbm' : UNDEFINED,
   'lbf/ft2' : UNDEFINED,
   'lbf*sec/(lbm*in2)' : UNDEFINED,
   'lbf*sec2/(lbm*in5)' : UNDEFINED,
   'lbf*sec2/(lbm*in2)' : UNDEFINED,
   'lbm' : 'lb',
   'lbm*in2' : 'lb*inch**2',
   'lbm/(ft*sec)' : 'lb/(ft*s)',
   'lbm/(ft*sec2)' : 'lb/(ft*s**2)',
   'lbm/ft3' : 'lb/ft**3',
   'lbm/(ft3*sec)' : 'lb/(ft**3*s)',
   'lbm/hr' : 'lb/h',
   'lbm/(hr*hp)' : 'lb/(h*hp)',
   'lbm/(hr*lbf)' : UNDEFINED,
   'lbm/(in*sec)' : 'lb/(inch*s)',
   'lbm/in2' : 'lb/inch**2',
   'lbm/in3' : 'lb/inch**3',
   'lbm/(in3*sec)' : 'lb/(inch**3*s)',
   'lbm/lbm' : UNDEFINED,
   'lbm/mol' : 'lb/mol',
   'lbm/sec' : 'lb/s',
   'lbm/(sec*ft2)' : 'lb/(s*ft**2)', 
   'lbm/(sec*in2)' : 'lb/(s*inch**2)', 
   'lbm/sec2' : 'lb/s**2',
   'lbm*R/sec' : 'lb*degR/s',
   'lbm*SQRT_R/psia' : UNDEFINED,
   'lbm*lbm*in2/(lbf*sec2)' : UNDEFINED,
   'lbm*lbm/sec2' : 'lb**2/s**2',
   'lbmol' : UNDEFINED,
   'liter' : 'l',

   'm' : 'm',
   'm/sec' : 'm/s',
   'm/sec2' : 'm/s**2',
   'm2' : 'm**2',
   'm3' : 'm**3',
   'mbar' : 'mbar',
   'MPa' : 'MPa', 
   'mg/J' : 'mg/J',
   'mg/(N*sec)' : 'mg/(N*s)',
   'mil' : UNDEFINED,
   'min' : 'min',
   'mm' : 'mm',
   'mm2' : 'mm**2',
   'mm3' : 'mm**3',
   'mm4' : 'mm**4',
   'mm5' : 'mm**5',
   'mol' : 'mol',

   'N' : 'N',
   'N*m' : 'N*m',
   'N/m2' : 'N/m**2',
   'nmile' : 'nmi',
   'none' : None,
 
   'Pa' : 'Pa',
   '1/rpm' : UNDEFINED,
   '1/sec' : UNDEFINED,
   'psia' : 'psi',
   'psia/sec' : 'psi/s',
   'R/sec' : 'degR/s',
   'rad' : 'rad',
   'rad/sec' : 'rad/s',
   'R' : 'degR',
   'rev' : UNDEFINED,
   'rev/sec' : UNDEFINED,
   'rpm' : '1/min',
   'rpm/sec' : UNDEFINED,
   'rpm/SQRT_R' : UNDEFINED,

   'sec' : 's',
   'sec2' : 's**2',
   'slug' : UNDEFINED,
   'slug*ft2' : UNDEFINED,
   'slug/ft3' : UNDEFINED,
   'SQRT_lbf' : UNDEFINED,
   'SQRT_N' : UNDEFINED,
   'SQRT_R' : UNDEFINED,

   'W' : 'W',
   'W/(m*K)' : 'W/(m*K)',
   'W/m2' : 'W/m**2',
   'W/(m2*K)' : 'W/(m**2*K)',
   'W/m3' : 'W/m**3',

   'yd' : 'yd',
}

