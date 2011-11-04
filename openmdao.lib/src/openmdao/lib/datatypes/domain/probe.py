"""
Support for calculating one or more scalar averages across one or more
regions in a domain. Some typical fluid flow metric calculation classes are
included here. Others may be added via :meth:`register_mesh_probe`. A list of
available metrics is returned by :meth:`list_mesh_probes`.
"""

from math import sqrt

from openmdao.units.units import PhysicalQuantity

from openmdao.lib.datatypes.domain.flow import CELL_CENTER
from openmdao.lib.datatypes.domain.zone import CYLINDRICAL

# Dictionary for calculated variable information.
# Holds (integrate_flag, collect_function).
# Populated as collection functions are registered.
_VARIABLES = {}

_SCHEMES = ('area', 'mass')

# TODO: account for ghost cells in index calculations.


def register_mesh_probe(name, cls, integrate):
    """
    Register a mesh probe class.

    name: string
        The name of the metric calculated.

    cls: class
        Class used to calculate metric. It must contain :meth:`calculate_3d`
        and a `result` property. Constructor arguments are
        `(domain, region, reference_state)`. :meth:`calculate_3d` will be called
        with `(i, j, k, get_value, weight, get_normal)`. `i`, `j`, and `k` are
        indices into the zone variable arrays, `get_value` is used to fetch a
        (possibly averaged) value from an array, `weight` is the weighting
        factor to apply during accumulation, and `get_normal` returns a
        vector normal to the face with magnitude equal to its area.

    integrate: bool
        If True, then calculated values are integrated, not averaged.
    """
    _VARIABLES[name] = (integrate, cls)

def list_mesh_probes():
    """ Return list of valid metric names. """
    return sorted(_VARIABLES.keys())


def mesh_probe(domain, regions, variables, weighting_scheme='area'):
    """
    Calculate metrics on mesh regions.
    Currently only supports 3D structured grids.

    domain: DomainObj
        The domain to be processed.

    regions: list
        List of `(zone_name, imin, imax, jmin, jmax, kmin, kmax)`
        mesh region specifications to be used for the calculation.
        Indices start at 0. Negative indices are relative to the end of the
        array.

    variables: list
        List of `(metric_name, units)` tuples. Legal metric names cab be
        obtained from :meth:`list_mesh_probes`. If `units` is None then no
        unit conversion is attempted.

    weighting_scheme: string
        Specifies how individual values are weighted. Legal values are
        'area' for area averaging and 'mass' for mass averaging.

    Returns a list of metric values in the order of the `variables` list.
    """
    metrics = []

    # Check validity of region specifications.
    _regions = []
    for zone_name, imin, imax, jmin, jmax, kmin, kmax in regions:
        try:
            zone = getattr(domain, zone_name)
            zone_imax, zone_jmax, zone_kmax = zone.shape
        except AttributeError:
            raise ValueError('Domain does not contain zone %r' % zone_name)

        # Support end-relative indexing.
        if imin < 0:
            imin = zone_imax + imin
        if imax < 0:
            imax = zone_imax + imax
        if jmin < 0:
            jmin = zone_jmax + jmin
        if jmax < 0:
            jmax = zone_jmax + jmax
        if kmin < 0:
            kmin = zone_kmax + kmin
        if kmax < 0:
            kmax = zone_kmax + kmax

        # Check index validity.
        if imin < 0 or imin >= zone_imax:
            raise ValueError('Zone %s imin %d invalid (max %d)'
                             % (zone_name, imin, zone_imax))
        if imax < imin or imax >= zone_imax:
            raise ValueError('Zone %s imax %d invalid (max %d)'
                             % (zone_name, imax, zone_imax))
        if jmin < 0 or jmin >= zone_jmax:
            raise ValueError('Zone %s jmin %d invalid (max %d)'
                             % (zone_name, jmin, zone_jmax))
        if jmax < jmin or jmax >= zone_jmax:
            raise ValueError('Zone %s jmax %d invalid (max %d)'
                             % (zone_name, jmax, zone_jmax))
        if kmin < 0 or kmin >= zone_kmax:
            raise ValueError('Zone %s kmin %d invalid (max %d)'
                             % (zone_name, kmin, zone_kmax))
        if kmax < kmin or kmax >= zone_kmax:
            raise ValueError('Zone %s kmax %d invalid (max %d)'
                             % (zone_name, kmax, zone_kmax))
        if imin != imax and jmin != jmax and kmin != kmax:
            raise ValueError('Zone %s volume specified: %d,%d %d,%d %d,%d'
                             % (zone_name, imin, imax, jmin, jmax, kmin, kmax))

        _regions.append((zone_name, imin, imax, jmin, jmax, kmin, kmax))

    # Check validity of variables.
    for name, units in variables:
        if name not in _VARIABLES:
            raise ValueError('Unknown/unsupported variable %r' % name)

    # Check validity of weighting scheme.
    if weighting_scheme not in _SCHEMES:
        raise ValueError('Unknown/unsupported weighting scheme %r'
                         % weighting_scheme)

    # Collect weights.
    weights = {}
    weight_total = 0.
    for region in _regions:
        zone_name = region[0]
        zone = getattr(domain, zone_name)
        zone_weights = _weights_3d(weighting_scheme, domain, region)
        zone_weights *= zone.symmetry_instances  # Adjust for symmetry.
# NOTE: this assumes a zone is used only once for all regions.
        weights[zone_name] = zone_weights
        weight_total += sum(zone_weights)

    # For each variable...
    for name, units in variables:
        # Compute total for each region.
        total = None
        for region in _regions:
            zone_name = region[0]
            zone = getattr(domain, zone_name)

            if units is None:
                ref = None
            else:  # Check for a reference_state dictionary.
                ref = zone.reference_state or domain.reference_state
                if not ref:
                    raise ValueError('No zone or domain reference_state'
                                     ' dictionary supplied for %s.' % zone_name)

            value = _calc_3d(name, domain, region, weights, ref)
            value *= zone.symmetry_instances  # Adjust for symmetry.
            if total is None:
                total = value
            else:
                total += value

        # If not integrating adjust for overall weighting.
        if not _VARIABLES[name][0]:
            total /= weight_total

        if units is None:
            metrics.append(total)
        else:
            total.convert_to_unit(units)  # Convert to requested units.
            metrics.append(total.value)

    return metrics


def _calc_3d(name, domain, region, weights, reference_state):
    """ Calculate `name` metric on a 3D (index space) region. """
    integrate, cls = _VARIABLES[name]
    metric = cls(domain, region, reference_state)
    zone_name, imin, imax, jmin, jmax, kmin, kmax = region
    zone = getattr(domain, zone_name)
    grid = zone.grid_coordinates
    flow = zone.flow_solution
    cylindrical = zone.coordinate_system == CYLINDRICAL
    cell_center = flow.grid_location == CELL_CENTER
    weights = weights[zone_name]

    if cylindrical:
        c1 = grid.z.item
        c2 = grid.r.item
        c3 = grid.t.item
    else:
        c1 = grid.x.item
        c2 = grid.y.item
        c3 = grid.z.item

    if imin == imax:
        face = 'i'
        imax += 1  # Ensure range() returns face index.
        get_normal = _iface_normal
        get_value = _iface_cell_value if cell_center else _node_value
    elif jmin == jmax:
        face = 'j'
        jmax += 1
        get_normal = _jface_normal
        get_value = _jface_cell_value if cell_center else _node_value
    else:
        face = 'k'
        kmax += 1
        get_normal = _kface_normal
        get_value = _kface_cell_value if cell_center else _node_value

    normal = None
    weight_index = 0
    total = 0.
    for i in range(imin, imax):
        ip1 = i + 1
        for j in range(jmin, jmax):
            jp1 = j + 1
            for k in range(kmin, kmax):
                kp1 = k + 1

                if integrate:
                    normal = get_normal(c1, c2, c3, i, j, k, cylindrical)

                val = metric.calculate_3d(i, j, k, get_value, normal)
                if not cell_center:
                    if face == 'i':
                        val += metric.calculate_3d(i, jp1, k, get_value, normal)
                        val += metric.calculate_3d(i, jp1, kp1, get_value, normal)
                        val += metric.calculate_3d(i, j, kp1, get_value, normal)
                    elif face == 'j':
                        val += metric.calculate_3d(ip1, j, k, get_value, normal)
                        val += metric.calculate_3d(ip1, j, kp1, get_value, normal)
                        val += metric.calculate_3d(i, j, kp1, get_value, normal)
                    else:
                        val += metric.calculate_3d(ip1, j, k, get_value, normal)
                        val += metric.calculate_3d(ip1, jp1, k, get_value, normal)
                        val += metric.calculate_3d(i, jp1, k, get_value, normal)
                    val *= 0.25

                if integrate:
                    total += val
                else:
                    weight = weights[weight_index]
                    weight_index += 1
                    total += val * weight

    if reference_state is None:
        return total
    else:
        return metric.dimensionalize(total)


def _weights_3d(scheme, domain, region):
    """ Returns 3D (index space) weights for a mesh region. """
    zone_name, imin, imax, jmin, jmax, kmin, kmax = region
    zone = getattr(domain, zone_name)
    grid = zone.grid_coordinates
    flow = zone.flow_solution
    cylindrical = zone.coordinate_system == CYLINDRICAL
    cell_center = flow.grid_location == CELL_CENTER

    if cylindrical:
        c1 = grid.z.item
        c2 = grid.r.item
        c3 = grid.t.item
    else:
        c1 = grid.x.item
        c2 = grid.y.item
        c3 = grid.z.item

    if scheme == 'mass':
        try:
            if cylindrical:
                mom_c1 = flow.momentum.z.item
                mom_c2 = flow.momentum.r.item
                mom_c3 = flow.momentum.t.item
            else:
                mom_c1 = flow.momentum.x.item
                mom_c2 = flow.momentum.y.item
                mom_c3 = flow.momentum.z.item
        except AttributeError:
            raise AttributeError("For mass averaging zone %s is missing"
                                 " 'momentum'." % zone_name)
    if imin == imax:
        imax += 1  # Ensure range() returns face index.
        face_normal = _iface_normal
        face_value = _iface_cell_value if cell_center else _iface_node_value
    elif jmin == jmax:
        jmax += 1
        face_normal = _jface_normal
        face_value = _jface_cell_value if cell_center else _jface_node_value
    else:
        kmax += 1
        face_normal = _kface_normal
        face_value = _kface_cell_value if cell_center else _kface_node_value

    weights = []
    for i in range(imin, imax):
        for j in range(jmin, jmax):
            for k in range(kmin, kmax):
                sc1, sc2, sc3 = face_normal(c1, c2, c3, i, j, k, cylindrical)
                if scheme == 'mass':
                    rvu = face_value(mom_c1, i, j, k)
                    rvv = face_value(mom_c2, i, j, k)
                    rvw = face_value(mom_c3, i, j, k)
                    weights.append(rvu*sc1 + rvv*sc2 + rvw*sc3)
                else:
                    weights.append(sqrt(sc1*sc1 + sc2*sc2 + sc3*sc3))

    return weights


def _iface_normal(c1, c2, c3, i, j, k, cylindrical):
    """
    Return non-dimensional vector normal to I face with magnitude equal to area.
    """
    jp1 = j + 1
    kp1 = k + 1

    # upper-left - lower-right.
    diag_c11 = c1(i, jp1, k) - c1(i, j, kp1)
    diag_c21 = c2(i, jp1, k) - c2(i, j, kp1)
    diag_c31 = c3(i, jp1, k) - c3(i, j, kp1)

    # upper-right - lower-left.
    diag_c12 = c1(i, jp1, kp1) - c1(i, j, k)
    diag_c22 = c2(i, jp1, kp1) - c2(i, j, k)
    diag_c32 = c3(i, jp1, kp1) - c3(i, j, k)

    if cylindrical:
        r1 = (c2(i, j, kp1) + c2(i, jp1, k  )) / 2.
        r2 = (c2(i, j, k  ) + c2(i, jp1, kp1)) / 2.
    else:
        r1 = 1.
        r2 = 1.

    sc1 = -0.5 * ( r2 * diag_c21 * diag_c32 - r1 * diag_c22 * diag_c31)
    sc2 = -0.5 * (-r2 * diag_c11 * diag_c32 + r1 * diag_c12 * diag_c31)
    sc3 = -0.5 * (      diag_c11 * diag_c22 -      diag_c12 * diag_c21)

    return (sc1, sc2, sc3)


def _jface_normal(c1, c2, c3, i, j, k, cylindrical):
    """
    Return non-dimensional vector normal to J face with magnitude equal to area.
    """
    ip1 = i + 1
    kp1 = k + 1

    # upper-left - lower-right.
    diag_c11 = c1(ip1, j, k) - c1(i, j, kp1)
    diag_c21 = c2(ip1, j, k) - c2(i, j, kp1)
    diag_c31 = c3(ip1, j, k) - c3(i, j, kp1)

    # upper-right - lower-left.
    diag_c12 = c1(ip1, j, kp1) - c1(i, j, k)
    diag_c22 = c2(ip1, j, kp1) - c2(i, j, k)
    diag_c32 = c3(ip1, j, kp1) - c3(i, j, k)

    if cylindrical:
        r1 = (c2(i, j, kp1) + c2(ip1, j, k  )) / 2.
        r2 = (c2(i, j, k  ) + c2(ip1, j, kp1)) / 2.
    else:
        r1 = 1.
        r2 = 1.

    sc1 = 0.5 * ( r2 * diag_c21 * diag_c32 - r1 * diag_c22 * diag_c31)
    sc2 = 0.5 * (-r2 * diag_c11 * diag_c32 + r1 * diag_c12 * diag_c31)
    sc3 = 0.5 * (      diag_c11 * diag_c22 -      diag_c12 * diag_c21)

    return (sc1, sc2, sc3)


def _kface_normal(c1, c2, c3, i, j, k, cylindrical):
    """
    Return non-dimensional vector normal to K face with magnitude equal to area.
    """
    ip1 = i + 1
    jp1 = j + 1

    # upper-left - lower-right.
    diag_c11 = c1(i, jp1, k) - c1(ip1, j, k)
    diag_c21 = c2(i, jp1, k) - c2(ip1, j, k)
    diag_c31 = c3(i, jp1, k) - c3(ip1, j, k)

    # upper-right - lower-left.
    diag_c12 = c1(ip1, jp1, k) - c1(i, j, k)
    diag_c22 = c2(ip1, jp1, k) - c2(i, j, k)
    diag_c32 = c3(ip1, jp1, k) - c3(i, j, k)

    if cylindrical:
        r1 = (c2(i, jp1, k) + c2(ip1, j,   k)) / 2.
        r2 = (c2(i, j,   k) + c2(ip1, jp1, k)) / 2.
    else:
        r1 = 1.
        r2 = 1.

    sc1 = 0.5 * ( r2 * diag_c21 * diag_c32 - r1 * diag_c22 * diag_c31)
    sc2 = 0.5 * (-r2 * diag_c11 * diag_c32 + r1 * diag_c12 * diag_c31)
    sc3 = 0.5 * (      diag_c11 * diag_c22 -      diag_c12 * diag_c21)

    return (sc1, sc2, sc3)


def _iface_cell_value(arr, i, j, k):
    """ Returns I face value for cell-centered data. """
    return 0.5 * (arr(i+1, j+1, k+1) + arr(i, j+1, k+1))

def _jface_cell_value(arr, i, j, k):
    """ Returns J face value for cell-centered data. """
    return 0.5 * (arr(i+1, j+1, k+1) + arr(i+1, j, k+1))

def _kface_cell_value(arr, i, j, k):
    """ Returns K face value for cell-centered data. """
    return 0.5 * (arr(i+1, j+1, k+1) + arr(i+1, j+1, k))


def _iface_node_value(arr, i, j, k):
    """ Returns I face value for vertex data. """
    return 0.25 * (arr(i-1, j, k) + arr(i-1, j-1, k) +
                   arr(i-1, j, k-1) + arr(i-1, j-1, k-1))

def _jface_node_value(arr, i, j, k):
    """ Returns J face value for vertex data. """
    return 0.25 * (arr(i, j-1, k) + arr(i-1, j-1, k) +
                   arr(i, j-1, k-1) + arr(i-1, j-1, k-1))

def _kface_node_value(arr, i, j, k):
    """ Returns K face value for vertex data. """
    return 0.25 * (arr(i, j, k-1) + arr(i-1, j, k-1) +
                   arr(i, j-1, k-1) + arr(i-1, j-1, k-1))


def _node_value(arr, i, j, k):
    """ Returns value for vertex data. """
    return arr(i, j, k)


class _Area(object):
    """ Computes area of mesh surface. """

    def __init__(self, domain, region, reference_state):
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

    def calculate_3d(self, i, j, k, get_value, normal):
        """ Return 3D (index space) metric. """
        sc1, sc2, sc3 = normal
        sc1 *= self.aref
        sc2 *= self.aref
        sc3 *= self.aref
        return sqrt(sc1*sc1 + sc2*sc2 + sc3*sc3)

    def dimensionalize(self, value):
        """ Return dimensional `value`. """
        return PhysicalQuantity(value, self.units)

register_mesh_probe('area', _Area, True)


class _MassFlow(object):
    """ Computes mass flow across a mesh surface. """

    def __init__(self, domain, region, reference_state):
        zone_name, imin, imax, jmin, jmax, kmin, kmax = region
        zone = getattr(domain, zone_name)
        flow = zone.flow_solution
        cylindrical = zone.coordinate_system == CYLINDRICAL

        try:
            if cylindrical:
                self.mom_c1 = flow.momentum.z.item
                self.mom_c2 = flow.momentum.r.item
                self.mom_c3 = flow.momentum.t.item
            else:
                self.mom_c1 = flow.momentum.x.item
                self.mom_c2 = flow.momentum.y.item
                self.mom_c3 = flow.momentum.z.item
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

    def calculate_3d(self, i, j, k, get_value, normal):
        """ Return 3D (index space) metric. """
        rvu = get_value(self.mom_c1, i, j, k) * self.momref
        rvv = get_value(self.mom_c2, i, j, k) * self.momref
        rvw = get_value(self.mom_c3, i, j, k) * self.momref
        sc1, sc2, sc3 = normal
        sc1 *= self.aref
        sc2 *= self.aref
        sc3 *= self.aref
        return rvu*sc1 + rvv*sc2 + rvw*sc3

    def dimensionalize(self, value):
        """ Dimensionalize `value`. """
        return PhysicalQuantity(value, self.wref.get_unit_name())

register_mesh_probe('mass_flow', _MassFlow, True)


class _CorrectedMassFlow(object):
    """ Computes corrected mass flow across a mesh surface. """

    def __init__(self, domain, region, reference_state):
        zone_name, imin, imax, jmin, jmax, kmin, kmax = region
        zone = getattr(domain, zone_name)
        flow = zone.flow_solution
        cylindrical = zone.coordinate_system == CYLINDRICAL

        try:
            self.density = flow.density.item
            if cylindrical:
                self.mom_c1 = flow.momentum.z.item
                self.mom_c2 = flow.momentum.r.item
                self.mom_c3 = flow.momentum.t.item
            else:
                self.mom_c1 = flow.momentum.x.item
                self.mom_c2 = flow.momentum.y.item
                self.mom_c3 = flow.momentum.z.item
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

    def calculate_3d(self, i, j, k, get_value, normal):
        """ Return 3D (index space) metric. """
        rho = get_value(self.density, i, j, k) * self.rhoref
        rvu = get_value(self.mom_c1, i, j, k) * self.momref
        rvv = get_value(self.mom_c2, i, j, k) * self.momref
        rvw = get_value(self.mom_c3, i, j, k) * self.momref
        ps = get_value(self.pressure, i, j, k) * self.pref
        if self.gam is not None:
            gamma = get_value(self.gam, i, j, k)
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

register_mesh_probe('corrected_mass_flow', _CorrectedMassFlow, True)


class _StaticPressure(object):
    """ Computes weighted static pressure for a mesh region. """

    def __init__(self, domain, region, reference_state):
        zone_name, imin, imax, jmin, jmax, kmin, kmax = region
        zone = getattr(domain, zone_name)
        flow = zone.flow_solution
        cylindrical = zone.coordinate_system == CYLINDRICAL

        try:  # Some codes have this directly available.
            self.pressure = flow.pressure.item
        except AttributeError:
            self.pressure = None
            try:  # Look for typical Q variabes.
                self.density = flow.density.item
                if cylindrical:
                    self.mom_c1 = flow.momentum.z.item
                    self.mom_c2 = flow.momentum.r.item
                    self.mom_c3 = flow.momentum.t.item
                else:
                    self.mom_c1 = flow.momentum.x.item
                    self.mom_c2 = flow.momentum.y.item
                    self.mom_c3 = flow.momentum.z.item
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

    def calculate_3d(self, i, j, k, get_value, normal):
        """ Return 3D (index space) metric. """
        if self.pressure is not None:
            return get_value(self.pressure, i, j, k) * self.pref
        else:
            rho = get_value(self.density, i, j, k) * self.rhoref
            vu = get_value(self.mom_c1, i, j, k) * self.momref / rho
            vv = get_value(self.mom_c2, i, j, k) * self.momref / rho
            vw = get_value(self.mom_c3, i, j, k) * self.momref / rho
            e0 = get_value(self.energy, i, j, k) * self.e0ref / rho
            if self.gam is not None:
                gamma = get_value(self.gam, i, j, k)
            else:
                gamma = self.gamma

            return (gamma-1.) * rho * (e0 - 0.5*(vu*vu + vv*vv + vw*vw))

    def dimensionalize(self, value):
        """ Dimensionalize `value`. """
        return PhysicalQuantity(value, self.units)

register_mesh_probe('pressure', _StaticPressure, False)


class _TotalPressure(object):
    """ Computes weighted total pressure for a mesh region. """

    def __init__(self, domain, region, reference_state):
        zone_name, imin, imax, jmin, jmax, kmin, kmax = region
        zone = getattr(domain, zone_name)
        flow = zone.flow_solution
        cylindrical = zone.coordinate_system == CYLINDRICAL

        try:
            self.density = flow.density.item
            if cylindrical:
                self.mom_c1 = flow.momentum.z.item
                self.mom_c2 = flow.momentum.r.item
                self.mom_c3 = flow.momentum.t.item
            else:
                self.mom_c1 = flow.momentum.x.item
                self.mom_c2 = flow.momentum.y.item
                self.mom_c3 = flow.momentum.z.item
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

        self.total = 0.

    def calculate_3d(self, i, j, k, get_value, normal):
        """ Return 3D (index space) metric. """
        rho = get_value(self.density, i, j, k) * self.rhoref
        vu = get_value(self.mom_c1, i, j, k) * self.momref / rho
        vv = get_value(self.mom_c2, i, j, k) * self.momref / rho
        vw = get_value(self.mom_c3, i, j, k) * self.momref / rho
        if self.gam is not None:
            gamma = get_value(self.gam, i, j, k)
        else:
            gamma = self.gamma

        u2 = vu*vu + vv*vv + vw*vw
        if self.pressure is not None:
            ps = get_value(self.pressure, i, j, k) * self.pref
        else:
            e0 = get_value(self.energy, i, j, k) * self.e0ref / rho
            ps = (gamma-1.) * rho * (e0 - 0.5*u2)
        a2 = (gamma * ps) / rho
        mach2 = u2 / a2
        return ps * pow(1. + (gamma-1.)/2. * mach2, gamma/(gamma-1.))

    def dimensionalize(self, value):
        """ Dimensionalize `value`. """
        return PhysicalQuantity(value, self.units)

register_mesh_probe('pressure_stagnation', _TotalPressure, False)


class _StaticTemperature(object):
    """ Computes weighted static temperature for a mesh region. """

    def __init__(self, domain, region, reference_state):
        zone_name, imin, imax, jmin, jmax, kmin, kmax = region
        zone = getattr(domain, zone_name)
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
            try:  # Look for typical Q variabes.
                if cylindrical:
                    self.mom_c1 = flow.momentum.z.item
                    self.mom_c2 = flow.momentum.r.item
                    self.mom_c3 = flow.momentum.t.item
                else:
                    self.mom_c1 = flow.momentum.x.item
                    self.mom_c2 = flow.momentum.y.item
                    self.mom_c3 = flow.momentum.z.item
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

    def calculate_3d(self, i, j, k, get_value, normal):
        """ Return 3D (index space) metric. """
        rho = get_value(self.density, i, j, k) * self.rhoref
        if self.pressure is not None:
            ps = get_value(self.pressure, i, j, k) * self.pref
        else:
            vu = get_value(self.mom_c1, i, j, k) * self.momref / rho
            vv = get_value(self.mom_c2, i, j, k) * self.momref / rho
            vw = get_value(self.mom_c3, i, j, k) * self.momref / rho
            e0 = get_value(self.energy, i, j, k) * self.e0ref / rho
            if self.gam is not None:
                gamma = get_value(self.gam, i, j, k)
            else:
                gamma = self.gamma
            ps = (gamma-1.) * rho * (e0 - 0.5*(vu*vu + vv*vv + vw*vw))
        return ps / (rho * self.rgas)

    def dimensionalize(self, value):
        """ Dimensionalize `value`. """
        return PhysicalQuantity(value, self.tref.get_unit_name())

register_mesh_probe('temperature', _StaticTemperature, False)


class _TotalTemperature(object):
    """ Computes weighted total temperature for a mesh region. """

    def __init__(self, domain, region, reference_state):
        zone_name, imin, imax, jmin, jmax, kmin, kmax = region
        zone = getattr(domain, zone_name)
        flow = zone.flow_solution
        cylindrical = zone.coordinate_system == CYLINDRICAL

        try:
            self.density = flow.density.item
            if cylindrical:
                self.mom_c1 = flow.momentum.z.item
                self.mom_c2 = flow.momentum.r.item
                self.mom_c3 = flow.momentum.t.item
            else:
                self.mom_c1 = flow.momentum.x.item
                self.mom_c2 = flow.momentum.y.item
                self.mom_c3 = flow.momentum.z.item
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

    def calculate_3d(self, i, j, k, get_value, normal):
        """ Return 3D (index space) metric. """
        rho = get_value(self.density, i, j, k) * self.rhoref
        vu = get_value(self.mom_c1, i, j, k) * self.momref / rho
        vv = get_value(self.mom_c2, i, j, k) * self.momref / rho
        vw = get_value(self.mom_c3, i, j, k) * self.momref / rho
        if self.gam is not None:
            gamma = get_value(self.gam, i, j, k)
        else:
            gamma = self.gamma

        u2 = vu*vu + vv*vv + vw*vw
        if self.pressure is not None:
            ps = get_value(self.pressure, i, j, k) * self.pref
        else:
            e0 = get_value(self.energy, i, j, k) * self.e0ref / rho
            ps = (gamma-1.) * rho * (e0 - 0.5*u2)
        a2 = (gamma * ps) / rho
        mach2 = u2 / a2
        ts = ps / (rho * self.rgas)
        return ts * (1. + (gamma-1.)/2. * mach2)

    def dimensionalize(self, value):
        """ Dimensionalize `value`. """
        return PhysicalQuantity(value, self.tref.get_unit_name())

register_mesh_probe('temperature_stagnation', _TotalTemperature, False)

