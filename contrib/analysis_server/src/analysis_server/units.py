"""
Translation from AnalysisServer units to OpenMDAO units, used by client
side proxies. The translation table is initialized based on NPSS units.
"""

def have_translation(as_units):
    """
    Return True if we can translate `as_units`.

    as_units: string
        AnalysisServer unit string.
    """
    return as_units in _UNITS_MAP

def get_translation(as_units):
    """
    Return translation for `as_units`.

    as_units: string
        AnalysisServer unit string.
    """
    try:
        return _UNITS_MAP[as_units]
    except KeyError:
        return as_units

def set_translation(as_units, om_units):
    """
    Set translation for `as_units` to `om_units`.

    as_units: string
        AnalysisServer unit string.

    om_units: string
        OpenMDAO unit string.
    """
    assert isinstance(as_units, basestring)
    assert isinstance(om_units, basestring)
    _UNITS_MAP[as_units] = om_units


# Default AS unit strings taken from NPSS.
_UNITS_MAP = {
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
   'cm2' : 'cm**2',
   'cm3' : 'cm**3',
   'cm4' : 'cm**4',
   'cm5' : 'cm**5',

   'dC' : 'degC',
   'dF' : 'degF',
   'dK' : 'K',
   'dR' : 'degR',

   'F' : 'degF',
   'ft/sec' : 'ft/s',
   'ft/sec2' : 'ft/s**2',
   'ft2' : 'ft**2',
   'ft2*lbm' : 'ft**2*lb',
   'ft2/sec2' : 'ft**2/s**2',
   'ft2/(sec2*R)' : 'ft**2/(s**2*degR)',
   'ft3' : 'ft**3',
   'ft3/lbm' : 'ft**3/lb',

   'gal' : 'galUS',

   'hp/ft2' : 'hp/ft**2',
   'hr' : 'h',
   'hp*hr' : 'hp*h',

   'in' : 'inch',
   'in/sec' : 'inch/s',
   'in2' : 'inch**2',
   'in3' : 'inch**3',
   'in3*sec/rad' : 'inch**3*s/rad',
   'in4' : 'inch**4',
   'in4*sec2/(lbm*ft)' : 'in**4*sec**2/(lb*ft)',
   'in5' : 'inch**5',

   'J/sec' : 'J/s',

   'kg*m2' : 'kg*m**2',
   'kg/m3' : 'kg/m**3',
   'kg/(N*sec)' : 'kg/(N*s)',
   'kg/sec' : 'kg/s',

   'lbm' : 'lb',
   'lbm*in2' : 'lb*inch**2',
   'lbm/(ft*sec)' : 'lb/(ft*s)',
   'lbm/(ft*sec2)' : 'lb/(ft*s**2)',
   'lbm/ft3' : 'lb/ft**3',
   'lbm/(ft3*sec)' : 'lb/(ft**3*s)',
   'lbm/hr' : 'lb/h',
   'lbm/(hr*hp)' : 'lb/(h*hp)',
   'lbm/(in*sec)' : 'lb/(inch*s)',
   'lbm/in2' : 'lb/inch**2',
   'lbm/in3' : 'lb/inch**3',
   'lbm/(in3*sec)' : 'lb/(inch**3*s)',
   'lbm/mol' : 'lb/mol',
   'lbm/sec' : 'lb/s',
   'lbm/(sec*ft2)' : 'lb/(s*ft**2)', 
   'lbm/(sec*in2)' : 'lb/(s*inch**2)', 
   'lbm/sec2' : 'lb/s**2',
   'lbm*R/sec' : 'lb*degR/s',
   'lbm*lbm/sec2' : 'lb**2/s**2',
   'liter' : 'l',

   'm/sec' : 'm/s',
   'm/sec2' : 'm/s**2',
   'm2' : 'm**2',
   'm3' : 'm**3',
   'mg/(N*sec)' : 'mg/(N*s)',
   'mm2' : 'mm**2',
   'mm3' : 'mm**3',
   'mm4' : 'mm**4',
   'mm5' : 'mm**5',

   'N/m2' : 'N/m**2',
   'nmile' : 'nmi',
   'none' : '',
 
   'psia' : 'psi',
   'psia/sec' : 'psi/s',
   'R/sec' : 'degR/s',
   'rad/sec' : 'rad/s',
   'R' : 'degR',

   'sec' : 's',
   'sec2' : 's**2',

   'W/m2' : 'W/m**2',
   'W/m3' : 'W/m**3',
}

