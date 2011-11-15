"""
Predefined classes used by :meth:`mesh_probe`.
Some typical fluid flow metric calculation classes are included here.
Others may be added via :meth:`register_metric`. A list of available metrics
is returned by :meth:`list_metrics`. Note that the name a metric is registered
under need not match its class name.

Metrics may be used with 1D, 2D, or 3D Cartesian coordinates. They may also
be used with polar (2D) or cylindrical (3D) coordinates. :meth:`calculate`
should be prepared for this.
"""

from math import sqrt

from openmdao.units.units import PhysicalQuantity

from openmdao.lib.datatypes.domain.zone import CYLINDRICAL

# Dictionary for metric information.
# Populated as metric classes are registered.
_METRICS = {}

# Legal `geometry` values.
_GEOMETRIES = ('volume', 'surface', 'curve', 'any')


def register_metric(name, cls, integrate, geometry='any'):
    """
    Register a metric calculation class.

    name: string
        The name of the metric calculated.

    cls: class
        Class used to calculate metric. It must contain :meth:`calculate`
        and :meth:`dimensionalize`. Constructor arguments are
        `(zone, zone_name, reference_state)`.
        :meth:`calculate` will be called with `(loc, geom)`.
        `loc` contains indices into the zone variable arrays and `geom` is
        either the cell volume, a non-dimensional vector normal
        to the cell face with magnitude equal to its area, or the edge length;
        depending upon the type of region (volume, surface, or curve).
        :meth:`dimensionalize` is called with the accumulated value.
        It should return a :class:`PhysicalQuantity` for the dimensionalized
        value.

    integrate: bool
        If True, then calculated values are integrated, not averaged.

    geometry: string ('volume', 'surface', 'curve', or 'any')
        Specifies what type of geometry this metric is restricted to, if any.
    """
    if geometry not in _GEOMETRIES:
        raise ValueError('geom must be one of %s' % _GEOMETRIES)
    _METRICS[name] = (cls, integrate, geometry)


def get_metric(name):
    """ Return ``(cls, integrate, geometry)``. """
    return _METRICS[name]


def list_metrics():
    """ Return list of valid metric names. """
    return sorted(_METRICS.keys())


def create_scalar_metric(var_name):
    """
    Creates a minimal metric calculation class for `var_name` and registers it.
    This can be used for averaging scalar flow variables without having to
    statically create a class and register it. The created class does not
    support dimensionalization.
    """
    cls_name = var_name.capitalize()
    exec '''
class %(cls_name)s(object):
    """ Computes %(var_name)s. """

    def __init__(self, zone, zone_name, reference_state):
        self.%(var_name)s = zone.flow_solution.%(var_name)s.item

    def calculate(self, loc, length):
        """ Return metric value. """
        return self.%(var_name)s(*loc)

    def dimensionalize(self, value):
        """ Return dimensional `value`. """
        raise NotImplementedError('Dimensional %(var_name)s')

register_metric('%(var_name)s', %(cls_name)s, False, 'any')
''' % {'var_name': var_name, 'cls_name': cls_name}


class Area(object):
    """ Computes area of mesh surface. """

    def __init__(self, zone, zone_name, reference_state):
        if reference_state is None:
            self.aref = 1.
        else:
            try:
                lref = reference_state['length_reference']
            except KeyError:
                raise AttributeError("For area, reference_state is missing"
                                     " 'length_reference'.")
            aref = lref * lref
            self.units = aref.get_unit_name()
            self.aref = aref.value

    def calculate(self, loc, normal):
        """ Return metric value. """
        sc1, sc2, sc3 = normal
        sc1 *= self.aref
        sc2 *= self.aref
        sc3 *= self.aref
        return sqrt(sc1*sc1 + sc2*sc2 + sc3*sc3)

    def dimensionalize(self, value):
        """ Return dimensional `value`. """
        return PhysicalQuantity(value, self.units)

register_metric('area', Area, True, 'surface')


class Length(object):
    """ Computes length of mesh curve. """

    def __init__(self, zone, zone_name, reference_state):
        if reference_state is None:
            self.units = None
        else:
            try:
                lref = reference_state['length_reference']
            except KeyError:
                raise AttributeError("For length, reference_state is missing"
                                     " 'length_reference'.")
            self.units = lref.get_unit_name()
            self.lref = lref.value

    def calculate(self, loc, length):
        """ Return metric value. """
        return length * self.lref

    def dimensionalize(self, value):
        """ Return dimensional `value`. """
        return PhysicalQuantity(value, self.units)

register_metric('length', Length, True, 'curve')


class MassFlow(object):
    """ Computes mass flow across a mesh surface. """

    def __init__(self, zone, zone_name, reference_state):
        flow = zone.flow_solution
        cylindrical = zone.coordinate_system == CYLINDRICAL

        try:
            momentum = flow.momentum
        except AttributeError:
            raise AttributeError("For mass_flow, zone %s is missing 'momentum'."
                                 % zone_name)

        if reference_state is None:
            self.aref = 1.
            self.momref = 1.
            self.wref = None
        else:
            try:
                lref = reference_state['length_reference']
                pref = reference_state['pressure_reference']
                rgas = reference_state['ideal_gas_constant']
                tref = reference_state['temperature_reference']
            except KeyError:
                vals = ('length_reference', 'pressure_reference',
                        'ideal_gas_constant', 'temperature_reference')
                raise AttributeError('For mass_flow, reference_state is missing'
                                     ' one or more of %s.' % (vals,))
            rhoref = pref / rgas / tref
            vref = (rgas * tref).sqrt()
            momref = rhoref * vref
            self.wref = momref * lref * lref
            aref = lref * lref
            self.aref = aref.value
            self.momref = momref.value

        if cylindrical:
            self.mom_c1 = None if momentum.z is None else momentum.z.item
            self.mom_c2 = momentum.r.item
            self.mom_c3 = momentum.t.item
        else:
            self.mom_c1 = momentum.x.item
            self.mom_c2 = None if momentum.y is None else momentum.y.item
            self.mom_c3 = None if momentum.z is None else momentum.z.item

    def calculate(self, loc, normal):
        """ Return metric value. """
        rvu = 0. if self.mom_c1 is None else self.mom_c1(*loc) * self.momref
        rvv = 0. if self.mom_c2 is None else self.mom_c2(*loc) * self.momref
        rvw = 0. if self.mom_c3 is None else self.mom_c3(*loc) * self.momref
        sc1, sc2, sc3 = normal
        sc1 *= self.aref
        sc2 *= self.aref
        sc3 *= self.aref
        return rvu*sc1 + rvv*sc2 + rvw*sc3

    def dimensionalize(self, value):
        """ Dimensionalize `value`. """
        return PhysicalQuantity(value, self.wref.get_unit_name())

register_metric('mass_flow', MassFlow, True, 'surface')


class CorrectedMassFlow(object):
    """ Computes corrected mass flow across a mesh surface. """

    def __init__(self, zone, zone_name, reference_state):
        flow = zone.flow_solution
        cylindrical = zone.coordinate_system == CYLINDRICAL

        # 'pressure' required until we can determine dimensionalized
        # static pressure from 'Q' variables.
        try:
            self.density = flow.density.item
            momentum = flow.momentum
            self.pressure = flow.pressure.item
        except AttributeError:
            vnames = ('density', 'momentum', 'pressure')
            raise AttributeError('For corrected_mass_flow, zone %s is missing'
                                 ' one or more of %s.' % (zone_name, vnames))
        try:
            self.gam = flow.gamma.item
        except AttributeError:
            self.gam = None  # Use passed-in scalar gamma.

        if reference_state is None:
            raise ValueError('corrected_mass_flow must have units specified')

        try:
            lref = reference_state['length_reference']
            pref = reference_state['pressure_reference']
            rgas = reference_state['ideal_gas_constant']
            tref = reference_state['temperature_reference']
            if self.gam is None:
                self.gamma = reference_state['specific_heat_ratio'].value
        except KeyError:
            vals = ('length_reference', 'pressure_reference', 'ideal_gas_constant',
                    'temperature_reference', 'specific_heat_ratio')
            raise AttributeError('For corrected_mass_flow, reference_state is'
                                 ' missing one or more of %s.' % (vals,))

        rhoref = pref / rgas / tref
        vref = (rgas * tref).sqrt()
        momref = rhoref * vref
        self.wref = momref * lref * lref

        pstd = PhysicalQuantity(14.696, 'psi')
        pstd.convert_to_unit(pref.get_unit_name())

        tstd = PhysicalQuantity(518.67, 'degR')
        tstd.convert_to_unit(tref.get_unit_name())

        aref = lref * lref
        self.aref = aref.value
        self.pref = pref.value
        self.rgas = rgas.value
        self.rhoref = rhoref.value
        self.momref = momref.value
        self.pstd = pstd.value
        self.tstd = tstd.value

        if cylindrical:
            self.mom_c1 = None if momentum.z is None else momentum.z.item
            self.mom_c2 = momentum.r.item
            self.mom_c3 = momentum.t.item
        else:
            self.mom_c1 = momentum.x.item
            self.mom_c2 = None if momentum.y is None else momentum.y.item
            self.mom_c3 = None if momentum.z is None else momentum.z.item

    def calculate(self, loc, normal):
        """ Return metric value. """
        rho = self.density(*loc) * self.rhoref
        rvu = 0. if self.mom_c1 is None else self.mom_c1(*loc) * self.momref
        rvv = 0. if self.mom_c2 is None else self.mom_c2(*loc) * self.momref
        rvw = 0. if self.mom_c3 is None else self.mom_c3(*loc) * self.momref
        ps = self.pressure(*loc) * self.pref
        if self.gam is not None:
            gamma = self.gam(*loc)
        else:
            gamma = self.gamma
        sc1, sc2, sc3 = normal
        sc1 *= self.aref
        sc2 *= self.aref
        sc3 *= self.aref
        w = rvu*sc1 + rvv*sc2 + rvw*sc3

        u2 = (rvu*rvu + rvv*rvv + rvw*rvw) / (rho*rho)
        a2 = (gamma * ps) / rho
        mach2 = u2 / a2
        ts = ps / (rho * self.rgas)
        tt = ts * (1. + (gamma-1.)/2. * mach2)

        pt = ps * pow(1. + (gamma-1.)/2. * mach2, gamma/(gamma-1.))

        return w * sqrt(tt/self.tstd) / (pt/self.pstd)

    def dimensionalize(self, value):
        """ Dimensionalize `value`. """
        return PhysicalQuantity(value, self.wref.get_unit_name())

register_metric('corrected_mass_flow', CorrectedMassFlow, True, 'surface')


class StaticPressure(object):
    """ Computes weighted static pressure for a mesh region. """

    def __init__(self, zone, zone_name, reference_state):
        flow = zone.flow_solution
        cylindrical = zone.coordinate_system == CYLINDRICAL

        try:  # Some codes have this directly available.
            self.pressure = flow.pressure.item
        except AttributeError:
            self.pressure = None
            try:  # Look for typical Q variables.
                self.density = flow.density.item
                momentum = flow.momentum
                self.energy = flow.energy_stagnation_density.item
            except AttributeError:
                vnames = ('pressure', 'density', 'momentum',
                          'energy_stagnation_density')
                raise AttributeError('For pressure, zone %s is missing'
                                     ' one or more of %s.' % (zone_name, vnames))
        try:
            self.gam = flow.gamma.item
        except AttributeError:
            self.gam = None  # Use passed-in scalar gamma.

        if reference_state is None:
            self.pref = 1.
            self.rhoref = 1.
            self.momref = 1.
            self.e0ref = 1.
            self.units = None
        else:
            if self.pressure is not None:
                try:
                    pref = reference_state['pressure_reference']
                except KeyError:
                    raise AttributeError("For pressure, reference_state is"
                                         " missing 'pressure_reference'.")
            else:
                raise NotImplementedError('Get dimensional pressure from'
                                          ' Q variables')
#                pref = reference_state['pressure_reference']
#                rgas = reference_state['ideal_gas_constant']
#                tref = reference_state['temperature_reference']
#                if self.gam is None:
#                    self.gamma = reference_state['specific_heat_ratio'].value
#                except AttributeError:
#                    vnames = ('ideal_gas_constant', 'pressure_reference',
#                              'temperature_reference', 'specific_heat_ratio')
#                    raise AttributeError('For static pressure, reference_state is'
#                                         ' missing one or more of %s.' % vnames)
#                rhoref = pref / rgas / tref
#                vref = (rgas * tref).sqrt()
#                momref = rhoref * vref
#                self.rhoref = rhoref.value
#                self.momref = momref.value
#                self.e0ref = ?

            self.units = pref.get_unit_name()
            self.pref = pref.value

        if self.pressure is None:
            if cylindrical:
                self.mom_c1 = None if momentum.z is None else momentum.z.item
                self.mom_c2 = momentum.r.item
                self.mom_c3 = momentum.t.item
            else:
                self.mom_c1 = momentum.x.item
                self.mom_c2 = None if momentum.y is None else momentum.y.item
                self.mom_c3 = None if momentum.z is None else momentum.z.item

    def calculate(self, loc, geom):
        """ Return metric value. """
        if self.pressure is not None:
            return self.pressure(*loc) * self.pref
        else:
            rho = self.density(*loc) * self.rhoref
            vu = 0. if self.mom_c1 is None else self.mom_c1(*loc) * self.momref / rho
            vv = 0. if self.mom_c2 is None else self.mom_c2(*loc) * self.momref / rho
            vw = 0. if self.mom_c3 is None else self.mom_c3(*loc) * self.momref / rho
            e0 = self.energy(*loc) * self.e0ref / rho
            if self.gam is not None:
                gamma = self.gam(*loc)
            else:
                gamma = self.gamma

            return (gamma-1.) * rho * (e0 - 0.5*(vu*vu + vv*vv + vw*vw))

    def dimensionalize(self, value):
        """ Dimensionalize `value`. """
        return PhysicalQuantity(value, self.units)

register_metric('pressure', StaticPressure, False)


class TotalPressure(object):
    """ Computes weighted total pressure for a mesh region. """

    def __init__(self, zone, zone_name, reference_state):
        flow = zone.flow_solution
        cylindrical = zone.coordinate_system == CYLINDRICAL

        try:
            self.density = flow.density.item
            momentum = flow.momentum
        except AttributeError:
            vnames = ('density', 'momentum')
            raise AttributeError('For pressure_stagnation, zone %s is missing'
                             ' one or more of %s.' % (zone_name, vnames))
        try:
            self.pressure = flow.pressure.item
        except AttributeError:
            self.pressure = None
            try:
                self.energy = flow.energy_stagnation_density.item
            except AttributeError:
                vnames = ('pressure', 'energy_stagnation_density')
                raise AttributeError('For pressure_stagnation, zone %s is missing'
                                     ' one or more of %s.' % (zone_name, vnames))
        try:
            self.gam = flow.gamma.item
        except AttributeError:
            self.gam = None  # Use passed-in scalar gamma.

        if reference_state is None:
            self.pref = 1.
            self.rhoref = 1.
            self.momref = 1.
            self.e0ref = 1.
            self.units = None
        else:
            try:
                pref = reference_state['pressure_reference']
                rgas = reference_state['ideal_gas_constant']
                tref = reference_state['temperature_reference']
                if self.gam is None:
                    self.gamma = reference_state['specific_heat_ratio'].value
            except KeyError:
                vals = ('pressure_reference', 'ideal_gas_constant',
                        'temperature_reference', 'specific_heat_ratio')
                raise AttributeError('For pressure_stagnation, reference_state'
                                     ' is missing one or more of %s.' % (vals,))
            if self.pressure is None:
                raise NotImplementedError('Get dimensional pressure_stagnation from'
                                          ' Q variables')
            rhoref = pref / rgas / tref
            vref = (rgas * tref).sqrt()
            momref = rhoref * vref
            self.rhoref = rhoref.value
            self.momref = momref.value
#            self.e0ref = ?
            self.units = pref.get_unit_name()
            self.pref = pref.value

        if cylindrical:
            self.mom_c1 = None if momentum.z is None else momentum.z.item
            self.mom_c2 = momentum.r.item
            self.mom_c3 = momentum.t.item
        else:
            self.mom_c1 = momentum.x.item
            self.mom_c2 = None if momentum.y is None else momentum.y.item
            self.mom_c3 = None if momentum.z is None else momentum.z.item

    def calculate(self, loc, geom):
        """ Return metric value. """
        rho = self.density(*loc) * self.rhoref
        vu = 0. if self.mom_c1 is None else self.mom_c1(*loc) * self.momref / rho
        vv = 0. if self.mom_c2 is None else self.mom_c2(*loc) * self.momref / rho
        vw = 0. if self.mom_c3 is None else self.mom_c3(*loc) * self.momref / rho
        if self.gam is not None:
            gamma = self.gam(*loc)
        else:
            gamma = self.gamma

        u2 = vu*vu + vv*vv + vw*vw
        if self.pressure is not None:
            ps = self.pressure(*loc) * self.pref
        else:
            e0 = self.energy(*loc) * self.e0ref / rho
            ps = (gamma-1.) * rho * (e0 - 0.5*u2)
        a2 = (gamma * ps) / rho
        mach2 = u2 / a2
        return ps * pow(1. + (gamma-1.)/2. * mach2, gamma/(gamma-1.))

    def dimensionalize(self, value):
        """ Dimensionalize `value`. """
        return PhysicalQuantity(value, self.units)

register_metric('pressure_stagnation', TotalPressure, False)


class StaticTemperature(object):
    """ Computes weighted static temperature for a mesh region. """

    def __init__(self, zone, zone_name, reference_state):
        flow = zone.flow_solution
        cylindrical = zone.coordinate_system == CYLINDRICAL

        try:
            self.density = flow.density.item
        except AttributeError:
            raise AttributeError('For temperature, zone %s is missing'
                                 ' density.' % zone_name)
        try:
            self.pressure = flow.pressure.item
        except AttributeError:
            self.pressure = None
            try:  # Look for typical Q variables.
                momentum = flow.momentum
                self.energy = flow.energy_stagnation_density.item
            except AttributeError:
                vnames = ('pressure', 'momentum', 'energy_stagnation_density')
                raise AttributeError('For temperature, zone %s is missing'
                                     ' one or more of %s.' % (zone_name, vnames))
        try:
            self.gam = flow.gamma.item
        except AttributeError:
            self.gam = None  # Use passed-in scalar gamma.

        if reference_state is None:
            self.pref = 1.
            self.rhoref = 1.
            self.momref = 1.
            self.e0ref = 1.
            self.rgas = 1.
            self.gamma = 1.4
            self.tref = None
        else:
            try:
                pref = reference_state['pressure_reference']
                rgas = reference_state['ideal_gas_constant']
                tref = reference_state['temperature_reference']
            except KeyError:
                vals = ('pressure_reference', 'ideal_gas_constant',
                        'temperature_reference')
                raise AttributeError('For temperature, reference_state is missing'
                                     ' one or more of %s.' % (vals,))
            if self.pressure is None:
                if self.gam is None:
                    try:
                        self.gamma = reference_state['specific_heat_ratio'].value
                    except KeyError:
                        raise AttributeError('For temperature, reference_state is'
                                             ' missing specific_heat_ratio')

                raise NotImplementedError('Get dimensional temperature from'
                                          ' Q variables')
            rhoref = pref / rgas / tref
            self.pref = pref.value
            self.rgas = rgas.value
            self.rhoref = rhoref.value
#            self.e0ref = ?
            self.tref = tref

        if self.pressure is None:
            if cylindrical:
                self.mom_c1 = None if momentum.z is None else momentum.z.item
                self.mom_c2 = momentum.r.item
                self.mom_c3 = momentum.t.item
            else:
                self.mom_c1 = momentum.x.item
                self.mom_c2 = None if momentum.y is None else momentum.y.item
                self.mom_c3 = None if momentum.z is None else momentum.z.item

    def calculate(self, loc, geom):
        """ Return metric value. """
        rho = self.density(*loc) * self.rhoref
        if self.pressure is not None:
            ps = self.pressure(*loc) * self.pref
        else:
            vu = 0. if self.mom_c1 is None else self.mom_c1(*loc) * self.momref / rho
            vv = 0. if self.mom_c2 is None else self.mom_c2(*loc) * self.momref / rho
            vw = 0. if self.mom_c3 is None else self.mom_c3(*loc) * self.momref / rho
            e0 = self.energy(*loc) * self.e0ref / rho
            if self.gam is not None:
                gamma = self.gam(*loc)
            else:
                gamma = self.gamma
            ps = (gamma-1.) * rho * (e0 - 0.5*(vu*vu + vv*vv + vw*vw))
        return ps / (rho * self.rgas)

    def dimensionalize(self, value):
        """ Dimensionalize `value`. """
        return PhysicalQuantity(value, self.tref.get_unit_name())

register_metric('temperature', StaticTemperature, False)


class TotalTemperature(object):
    """ Computes weighted total temperature for a mesh region. """

    def __init__(self, zone, zone_name, reference_state):
        flow = zone.flow_solution
        cylindrical = zone.coordinate_system == CYLINDRICAL

        try:
            self.density = flow.density.item
            momentum = flow.momentum
        except AttributeError:
            vnames = ('density', 'momentum')
            raise AttributeError('For temperature_stagnation, zone %s is missing'
                                 ' one or more of %s.' % (zone_name, vnames))
        try:
            self.pressure = flow.pressure.item
        except AttributeError:
            self.pressure = None
            try:
                self.energy = flow.energy_stagnation_density.item
            except AttributeError:
                vnames = ('pressure', 'energy_stagnation_density')
                raise AttributeError('For temperature_stagnation, zone %s is'
                                     ' one or more of %s.' % (zone_name, vnames))
        try:
            self.gam = flow.gamma.item
        except AttributeError:
            self.gam = None  # Use passed-in scalar gamma.

        if reference_state is None:
            self.pref = 1.
            self.rgas = 1.
            self.rhoref = 1.
            self.momref = 1.
            self.e0ref = 1.
        else:
            try:
                pref = reference_state['pressure_reference']
                rgas = reference_state['ideal_gas_constant']
                tref = reference_state['temperature_reference']
                if self.gam is None:
                    self.gamma = reference_state['specific_heat_ratio'].value
            except KeyError:
                vals = ('pressure_reference', 'ideal_gas_constant',
                        'temperature_reference', 'specific_heat_ratio')
                raise AttributeError('For temperature_stagnation, reference_state'
                                     ' is missing one or more of %s.' % (vals,))
            if self.pressure is None:
                raise NotImplementedError('Get dimensional temperature_stagnation'
                                          ' from Q variables')
            rhoref = pref / rgas / tref
            vref = (rgas * tref).sqrt()
            momref = rhoref * vref
            self.pref = pref.value
            self.rgas = rgas.value
            self.rhoref = rhoref.value
            self.momref = momref.value
#            self.e0ref = ?
            self.tref = tref

        if cylindrical:
            self.mom_c1 = None if momentum.z is None else momentum.z.item
            self.mom_c2 = momentum.r.item
            self.mom_c3 = momentum.t.item
        else:
            self.mom_c1 = momentum.x.item
            self.mom_c2 = None if momentum.y is None else momentum.y.item
            self.mom_c3 = None if momentum.z is None else momentum.z.item

    def calculate(self, loc, geom):
        """ Return metric value. """
        rho = self.density(*loc) * self.rhoref
        vu = 0. if self.mom_c1 is None else self.mom_c1(*loc) * self.momref / rho
        vv = 0. if self.mom_c2 is None else self.mom_c2(*loc) * self.momref / rho
        vw = 0. if self.mom_c3 is None else self.mom_c3(*loc) * self.momref / rho
        if self.gam is not None:
            gamma = self.gam(*loc)
        else:
            gamma = self.gamma

        u2 = vu*vu + vv*vv + vw*vw
        if self.pressure is not None:
            ps = self.pressure(*loc) * self.pref
        else:
            e0 = self.energy(*loc) * self.e0ref / rho
            ps = (gamma-1.) * rho * (e0 - 0.5*u2)
        a2 = (gamma * ps) / rho
        mach2 = u2 / a2
        ts = ps / (rho * self.rgas)
        return ts * (1. + (gamma-1.)/2. * mach2)

    def dimensionalize(self, value):
        """ Dimensionalize `value`. """
        return PhysicalQuantity(value, self.tref.get_unit_name())

register_metric('temperature_stagnation', TotalTemperature, False)


class Volume(object):
    """ Computes volume of mesh volume. """

    def __init__(self, zone, zone_name, reference_state):
        if reference_state is None:
            self.units = None
        else:
            try:
                lref = reference_state['length_reference']
            except KeyError:
                raise AttributeError("For volume, reference_state is missing"
                                     " 'length_reference'.")
            volref = lref * lref * lref
            self.units = volref.get_unit_name()
            self.volref = volref.value

    def calculate(self, loc, volume):
        """ Return metric value. """
        return volume * self.volref

    def dimensionalize(self, value):
        """ Return dimensional `value`. """
        return PhysicalQuantity(value, self.units)

register_metric('volume', Volume, True, 'volume')

