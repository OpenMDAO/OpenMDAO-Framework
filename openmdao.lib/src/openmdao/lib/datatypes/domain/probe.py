from math import sqrt

from openmdao.units.units import PhysicalQuantity

from openmdao.lib.datatypes.domain.flow import CELL_CENTER
from openmdao.lib.datatypes.domain.zone import CYLINDRICAL

# Dictionary for calculated variable information.
# Holds (integrate_flag, collect_function).
# Populated as collection functions are registered.
_VARIABLES = {}

_SCHEMES = ('area', 'mass')

# TODO: support Vertex flow solution grid location.
# TODO: account for ghost cells in index calculations.


def register_surface_probe(name, function, integrate):
    """
    Register a surface probe function.

    name: string
        The name of the metric calculated.

    function: callable
        The function to call.  The passed arguments are
        `(domain, surface, weights, reference_state)`.

    integrate: bool
        If True, then function values are integrated, not averaged.
    """
    _VARIABLES[name] = (integrate, function)


def surface_probe(domain, surfaces, variables, weighting_scheme='area'):
    """
    Calculate metrics on mesh surfaces.
    Currently only supports 3D structured grids.

    domain: DomainObj
        The domain to be processed.

    surfaces: list
        List of `(zone_name, imin, imax, jmin, jmax, kmin, kmax)`
        mesh surface specifications to be used for the calculation.
        Indices start at 0. Negative indices are relative to the end of the
        array.

    variables: list
        List of `(metric_name, units)` tuples. Legal metric names are
        'area', 'mass_flow', 'corrected_mass_flow', 'pressure',
        'pressure_stagnation', 'temperature', and 'temperature_stagnation'.
        If `units` is None then no unit conversion is attempted.

    weighting_scheme: string
        Specifies how individual values are weighted. Legal values are
        'area' for area averaging and 'mass' for mass averaging.

    Returns a list of metric values in the order of the `variables` list.
    """
    metrics = []

    # Check validity of surface specifications.
    _surfaces = []
    for zone_name, imin, imax, jmin, jmax, kmin, kmax in surfaces:
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

        _surfaces.append((zone_name, imin, imax, jmin, jmax, kmin, kmax))

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
    for surface in _surfaces:
        zone_name = surface[0]
        zone = getattr(domain, zone_name)
        zone_weights = _weights(weighting_scheme, domain, surface)
        zone_weights *= zone.symmetry_instances  # Adjust for symmetry.
        weights[zone_name] = zone_weights
        weight_total += sum(zone_weights)

    # For each variable...
    for name, units in variables:
        # Compute total across each surface.
        total = None
        for surface in _surfaces:
            zone_name = surface[0]
            zone = getattr(domain, zone_name)

            if units is None:
                ref = None
            else:  # Check for a reference_state dictionary.
                ref = zone.reference_state or domain.reference_state
                if not ref:
                    raise ValueError('No zone or domain reference_state'
                                     ' dictionary supplied for %s.' % zone_name)

            value = _VARIABLES[name][1](domain, surface, weights, ref)
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


def _weights(scheme, domain, surface):
    """ Returns weights for a mesh surface. """
    zone_name, imin, imax, jmin, jmax, kmin, kmax = surface
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
        ip1 = i + 1
        for j in range(jmin, jmax):
            jp1 = j + 1
            for k in range(kmin, kmax):
                kp1 = k + 1

                sc1, sc2, sc3 = face_normal(c1, c2, c3, i, j, k, cylindrical)
                if scheme == 'mass':
                    kp1 = k + 1
                    rvu = face_value(mom_c1, ip1, jp1, kp1)
                    rvv = face_value(mom_c2, ip1, jp1, kp1)
                    rvw = face_value(mom_c3, ip1, jp1, kp1)
                    weights.append(rvu*sc1 + rvv*sc2 + rvw*sc3)
                else:
                    weights.append(sqrt(sc1*sc1 + sc2*sc2 + sc3*sc3))

    return weights


def _iface_normal(c1, c2, c3, i, j, k, cylindrical, lref=1.):
    """ Return vector normal to I face with magnitude equal to area. """
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

    aref = lref * lref

    sc1 = -0.5 * ( r2 * diag_c21 * diag_c32 - r1 * diag_c22 * diag_c31) * aref
    sc2 = -0.5 * (-r2 * diag_c11 * diag_c32 + r1 * diag_c12 * diag_c31) * aref
    sc3 = -0.5 * (      diag_c11 * diag_c22 -      diag_c12 * diag_c21) * aref

    return (sc1, sc2, sc3)


def _jface_normal(c1, c2, c3, i, j, k, cylindrical, lref=1.):
    """ Return vector normal to J face with magnitude equal to area. """
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

    aref = lref * lref

    sc1 = 0.5 * ( r2 * diag_c21 * diag_c32 - r1 * diag_c22 * diag_c31) * aref
    sc2 = 0.5 * (-r2 * diag_c11 * diag_c32 + r1 * diag_c12 * diag_c31) * aref
    sc3 = 0.5 * (      diag_c11 * diag_c22 -      diag_c12 * diag_c21) * aref

    return (sc1, sc2, sc3)


def _kface_normal(c1, c2, c3, i, j, k, cylindrical, lref=1.):
    """ Return vector normal to K face with magnitude equal to area. """
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

    aref = lref * lref

    sc1 = 0.5 * ( r2 * diag_c21 * diag_c32 - r1 * diag_c22 * diag_c31) * aref
    sc2 = 0.5 * (-r2 * diag_c11 * diag_c32 + r1 * diag_c12 * diag_c31) * aref
    sc3 = 0.5 * (      diag_c11 * diag_c22 -      diag_c12 * diag_c21) * aref

    return (sc1, sc2, sc3)


def _iface_cell_value(arr, i, j, k):
    """ Returns I face value for cell-centered data. """
    return 0.5 * (arr(i, j, k) + arr(i-1, j, k))

def _jface_cell_value(arr, i, j, k):
    """ Returns J face value for cell-centered data. """
    return 0.5 * (arr(i, j, k) + arr(i, j-1, k))

def _kface_cell_value(arr, i, j, k):
    """ Returns K face value for cell-centered data. """
    return 0.5 * (arr(i, j, k) + arr(i, j, k-1))


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


def _area(domain, surface, weights, reference_state):
    """ Returns area of mesh surface as a :class:`PhysicalQuantity`. """
    zone_name, imin, imax, jmin, jmax, kmin, kmax = surface
    zone = getattr(domain, zone_name)
    grid = zone.grid_coordinates
    cylindrical = zone.coordinate_system == CYLINDRICAL

    if cylindrical:
        c1 = grid.z.item
        c2 = grid.r.item
        c3 = grid.t.item
    else:
        c1 = grid.x.item
        c2 = grid.y.item
        c3 = grid.z.item

    if imin == imax:
        imax += 1  # Ensure range() returns face index.
        face_normal = _iface_normal
    elif jmin == jmax:
        jmax += 1
        face_normal = _jface_normal
    else:
        kmax += 1
        face_normal = _kface_normal

    if reference_state is None:
        lref = 1.
    else:
        try:
            lref = reference_state['length_reference']
        except KeyError:
            raise AttributeError("For area, reference_state is missing"
                                 " 'length_reference'.")
        aref = lref * lref
        lref = lref.value

    total = 0.
    for i in range(imin, imax):
        for j in range(jmin, jmax):
            for k in range(kmin, kmax):
                sc1, sc2, sc3 = face_normal(c1, c2, c3, i, j, k, cylindrical,
                                            lref)
                total += sqrt(sc1*sc1 + sc2*sc2 + sc3*sc3)

    if reference_state is None:
        return total
    else:
        return PhysicalQuantity(total, aref.get_unit_name())

register_surface_probe('area', _area, True)


def _massflow(domain, surface, weights, reference_state):
    """ Returns mass flow for a mesh surface as a :class:`PhysicalQuantity`. """
    zone_name, imin, imax, jmin, jmax, kmin, kmax = surface
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
        raise AttributeError("For mass_flow, zone %s is missing 'momentum'."
                             % zone_name)
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

    if reference_state is None:
        lref = 1.
        momref = 1.
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
        wref = momref * lref * lref
        lref = lref.value
        momref = momref.value

    total = 0.
    for i in range(imin, imax):
        ip1 = i + 1
        for j in range(jmin, jmax):
            jp1 = j + 1
            for k in range(kmin, kmax):
                kp1 = k + 1

                rvu = face_value(mom_c1, ip1, jp1, kp1) * momref
                rvv = face_value(mom_c2, ip1, jp1, kp1) * momref
                rvw = face_value(mom_c3, ip1, jp1, kp1) * momref
                sc1, sc2, sc3 = face_normal(c1, c2, c3, i, j, k, cylindrical,
                                            lref)

                w = rvu*sc1 + rvv*sc2 + rvw*sc3
                total += w

    if reference_state is None:
        return total
    else:
        return PhysicalQuantity(total, wref.get_unit_name())

register_surface_probe('mass_flow', _massflow, True)


def _corrected_massflow(domain, surface, weights, reference_state):
    """
    Returns corrected mass flow for a mesh surface as a
    :class:`PhysicalQuantity`.
    """
    zone_name, imin, imax, jmin, jmax, kmin, kmax = surface
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

    try:
        density = flow.density.item
        if cylindrical:
            mom_c1 = flow.momentum.z.item
            mom_c2 = flow.momentum.r.item
            mom_c3 = flow.momentum.t.item
        else:
            mom_c1 = flow.momentum.x.item
            mom_c2 = flow.momentum.y.item
            mom_c3 = flow.momentum.z.item
        pressure = flow.pressure.item
    except AttributeError:
        vnames = ('density', 'momentum', 'pressure')
        raise AttributeError('For corrected_mass_flow, zone %s is missing'
                             ' one or more of %s.' % (zone_name, vnames))
    try:
        gam = flow.gamma.item
    except AttributeError:
        gam = None  # Use passed-in scalar gamma.

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

    if reference_state is None:
        raise ValueError('corrected_mass_flow must have units specified')

    try:
        lref = reference_state['length_reference']
        pref = reference_state['pressure_reference']
        rgas = reference_state['ideal_gas_constant']
        tref = reference_state['temperature_reference']
        if gam is None:
            gamma = reference_state['specific_heat_ratio'].value
    except KeyError:
        vals = ('length_reference', 'pressure_reference', 'ideal_gas_constant',
                'temperature_reference', 'specific_heat_ratio')
        raise AttributeError('For corrected_mass_flow, reference_state is'
                             ' missing one or more of %s.' % (vals,))

    rhoref = pref / rgas / tref
    vref = (rgas * tref).sqrt()
    momref = rhoref * vref
    wref = momref * lref * lref

    pstd = PhysicalQuantity(14.696, 'psi')
    pstd.convert_to_unit(pref.get_unit_name())

    tstd = PhysicalQuantity(518.67, 'degR')
    tstd.convert_to_unit(tref.get_unit_name())

    lref = lref.value
    pref = pref.value
    rgas = rgas.value
    rhoref = rhoref.value
    momref = momref.value
    pstd = pstd.value
    tstd = tstd.value

    total = 0.
    for i in range(imin, imax):
        ip1 = i + 1
        for j in range(jmin, jmax):
            jp1 = j + 1
            for k in range(kmin, kmax):
                kp1 = k + 1

                rho = face_value(density, ip1, jp1, kp1) * rhoref
                rvu = face_value(mom_c1, ip1, jp1, kp1) * momref
                rvv = face_value(mom_c2, ip1, jp1, kp1) * momref
                rvw = face_value(mom_c3, ip1, jp1, kp1) * momref
                ps = face_value(pressure, ip1, jp1, kp1) * pref
                if gam is not None:
                    gamma = face_value(gam, ip1, jp1, kp1)
                sc1, sc2, sc3 = face_normal(c1, c2, c3, i, j, k, cylindrical,
                                            lref)

                w = rvu*sc1 + rvv*sc2 + rvw*sc3

                u2 = (rvu*rvu + rvv*rvv + rvw*rvw) / (rho*rho)
                a2 = (gamma * ps) / rho
                mach2 = u2 / a2
                ts = ps / (rho * rgas)
                tt = ts * (1. + (gamma-1.)/2. * mach2)

                pt = ps * pow(1. + (gamma-1.)/2. * mach2, gamma/(gamma-1.))

                wc = w * sqrt(tt/tstd) / (pt/pstd)
                total += wc

    return PhysicalQuantity(total, wref.get_unit_name())

register_surface_probe('corrected_mass_flow', _corrected_massflow, True)


def _static_pressure(domain, surface, weights, reference_state):
    """
    Returns weighted static pressure for a mesh surface as a
    :class:`PhysicalQuantity`.
    """
    zone_name, imin, imax, jmin, jmax, kmin, kmax = surface
    zone = getattr(domain, zone_name)
    flow = zone.flow_solution
    cylindrical = zone.coordinate_system == CYLINDRICAL
    cell_center = flow.grid_location == CELL_CENTER
    weights = weights[zone_name]

    try:  # Some codes (i.e. ADPAC restart file) have this directly available.
        pressure = flow.pressure.item
    except AttributeError:
        pressure = None
        try:  # Look for typical Q variabes.
            density = flow.density.item
            if cylindrical:
                mom_c1 = flow.momentum.z.item
                mom_c2 = flow.momentum.r.item
                mom_c3 = flow.momentum.t.item
            else:
                mom_c1 = flow.momentum.x.item
                mom_c2 = flow.momentum.y.item
                mom_c3 = flow.momentum.z.item
            energy = flow.energy_stagnation_density.item
        except AttributeError:
            vnames = ('pressure', 'density', 'momentum',
                      'energy_stagnation_density')
            raise AttributeError('For pressure, zone %s is missing'
                                 ' one or more of %s.' % (zone_name, vnames))
    try:
        gam = flow.gamma.item
    except AttributeError:
        gam = None  # Use passed-in scalar gamma.

    if imin == imax:
        imax += 1  # Ensure range() returns face index.
        face_value = _iface_cell_value if cell_center else _iface_node_value
    elif jmin == jmax:
        jmax += 1
        face_value = _jface_cell_value if cell_center else _jface_node_value
    else:
        kmax += 1
        face_value = _kface_cell_value if cell_center else _kface_node_value

    if reference_state is None:
        pref = 1.
        rhoref = 1.
        momref = 1.
        e0ref = 1.
    else:
        if pressure is not None:
            try:
                pref = reference_state['pressure_reference']
            except KeyError:
                raise AttributeError("For pressure, reference_state is"
                                     " missing 'pressure_reference'.")
        else:
            raise NotImplementedError('Get dimensional pressure from'
                                      ' Q variables')
#            pref = reference_state['pressure_reference']
#            rgas = reference_state['ideal_gas_constant']
#            tref = reference_state['temperature_reference']
#            if gam is None:
#                gamma = reference_state['specific_heat_ratio'].value
#            except AttributeError:
#                vnames = ('ideal_gas_constant', 'pressure_reference',
#                          'temperature_reference', 'specific_heat_ratio')
#                raise AttributeError('For static pressure, reference_state is'
#                                     ' missing one or more of %s.' % vnames)
#            rhoref = pref / rgas / tref
#            vref = (rgas * tref).sqrt()
#            momref = rhoref * vref
#            rhoref = rhoref.value
#            momref = momref.value
#            e0ref = ?

        units = pref.get_unit_name()
        pref = pref.value

    total = 0.
    weight_index = 0
    for i in range(imin, imax):
        ip1 = i + 1
        for j in range(jmin, jmax):
            jp1 = j + 1
            for k in range(kmin, kmax):
                kp1 = k + 1

                if pressure is not None:
                    ps = face_value(pressure, ip1, jp1, kp1) * pref
                else:
                    rho = face_value(density, ip1, jp1, kp1) * rhoref
                    vu = face_value(mom_c1, ip1, jp1, kp1) * momref / rho
                    vv = face_value(mom_c2, ip1, jp1, kp1) * momref / rho
                    vw = face_value(mom_c3, ip1, jp1, kp1) * momref / rho
                    e0 = face_value(energy, ip1, jp1, kp1) * e0ref / rho
                    if gam is not None:
                        gamma = face_value(gam, ip1, jp1, kp1)

                    ps = (gamma-1.) * rho * (e0 - 0.5*(vu*vu + vv*vv + vw*vw))

                weight = weights[weight_index]
                weight_index += 1
                total += ps * weight

    if reference_state is None:
        return total
    else:
        return PhysicalQuantity(total, units)

register_surface_probe('pressure', _static_pressure, False)


def _total_pressure(domain, surface, weights, reference_state):
    """
    Returns weighted total pressure for a mesh surface as a
    :class:`PhysicalQuantity`.
    """
    zone_name, imin, imax, jmin, jmax, kmin, kmax = surface
    zone = getattr(domain, zone_name)
    flow = zone.flow_solution
    cylindrical = zone.coordinate_system == CYLINDRICAL
    cell_center = flow.grid_location == CELL_CENTER
    weights = weights[zone_name]

    try:
        density = flow.density.item
        if cylindrical:
            mom_c1 = flow.momentum.z.item
            mom_c2 = flow.momentum.r.item
            mom_c3 = flow.momentum.t.item
        else:
            mom_c1 = flow.momentum.x.item
            mom_c2 = flow.momentum.y.item
            mom_c3 = flow.momentum.z.item
    except AttributeError:
        vnames = ('density', 'momentum')
        raise AttributeError('For pressure_stagnation, zone %s is missing'
                             ' one or more of %s.' % (zone_name, vnames))
    try:
        pressure = flow.pressure.item
    except AttributeError:
        pressure = None
        try:
            energy = flow.energy_stagnation_density.item
        except AttributeError:
            vnames = ('pressure', 'energy_stagnation_density')
            raise AttributeError('For pressure_stagnation, zone %s is missing'
                                 ' one or more of %s.' % (zone_name, vnames))
    try:
        gam = flow.gamma.item
    except AttributeError:
        gam = None  # Use passed-in scalar gamma.

    if imin == imax:
        imax += 1  # Ensure range() returns face index.
        face_value = _iface_cell_value if cell_center else _iface_node_value
    elif jmin == jmax:
        jmax += 1
        face_value = _jface_cell_value if cell_center else _jface_node_value
    else:
        kmax += 1
        face_value = _kface_cell_value if cell_center else _kface_node_value

    if reference_state is None:
        pref = 1.
        rhoref = 1.
        momref = 1.
        e0ref = 1.
    else:
        try:
            pref = reference_state['pressure_reference']
            rgas = reference_state['ideal_gas_constant']
            tref = reference_state['temperature_reference']
            if gam is None:
                gamma = reference_state['specific_heat_ratio'].value
        except KeyError:
            vals = ('pressure_reference', 'ideal_gas_constant',
                    'temperature_reference', 'specific_heat_ratio')
            raise AttributeError('For pressure_stagnation, reference_state'
                                 ' is missing one or more of %s.' % (vals,))
        if pressure is None:
            raise NotImplementedError('Get dimensional pressure_stagnation from'
                                      ' Q variables')
        rhoref = pref / rgas / tref
        vref = (rgas * tref).sqrt()
        momref = rhoref * vref
        rhoref = rhoref.value
        momref = momref.value
#        e0ref = ?
        units = pref.get_unit_name()
        pref = pref.value

    total = 0.
    weight_index = 0
    for i in range(imin, imax):
        ip1 = i + 1
        for j in range(jmin, jmax):
            jp1 = j + 1
            for k in range(kmin, kmax):
                kp1 = k + 1

                rho = face_value(density, ip1, jp1, kp1) * rhoref
                vu = face_value(mom_c1, ip1, jp1, kp1) * momref / rho
                vv = face_value(mom_c2, ip1, jp1, kp1) * momref / rho
                vw = face_value(mom_c3, ip1, jp1, kp1) * momref / rho
                if gam is not None:
                    gamma = face_value(gam, ip1, jp1, kp1)

                u2 = vu*vu + vv*vv + vw*vw
                if pressure is not None:
                    ps = face_value(pressure, ip1, jp1, kp1) * pref
                else:
                    e0 = face_value(energy, ip1, jp1, kp1) * e0ref / rho
                    ps = (gamma-1.) * rho * (e0 - 0.5*u2)
                a2 = (gamma * ps) / rho
                mach2 = u2 / a2
                pt = ps * pow(1. + (gamma-1.)/2. * mach2, gamma/(gamma-1.))

                weight = weights[weight_index]
                weight_index += 1
                total += pt * weight

    if reference_state is None:
        return total
    else:
        return PhysicalQuantity(total, units)

register_surface_probe('pressure_stagnation', _total_pressure, False)


def _static_temperature(domain, surface, weights, reference_state):
    """
    Returns weighted static temperature for a mesh surface as a
    :class:`PhysicalQuantity`.
    """
    zone_name, imin, imax, jmin, jmax, kmin, kmax = surface
    zone = getattr(domain, zone_name)
    flow = zone.flow_solution
    cylindrical = zone.coordinate_system == CYLINDRICAL
    cell_center = flow.grid_location == CELL_CENTER
    weights = weights[zone_name]

    try:
        density = flow.density.item
    except AttributeError:
        raise AttributeError('For temperature, zone %s is missing'
                             ' density.' % zone_name)
    try:
        pressure = flow.pressure.item
    except AttributeError:
        pressure = None
        try:  # Look for typical Q variabes.
            if cylindrical:
                mom_c1 = flow.momentum.z.item
                mom_c2 = flow.momentum.r.item
                mom_c3 = flow.momentum.t.item
            else:
                mom_c1 = flow.momentum.x.item
                mom_c2 = flow.momentum.y.item
                mom_c3 = flow.momentum.z.item
            energy = flow.energy_stagnation_density.item
        except AttributeError:
            vnames = ('pressure', 'momentum', 'energy_stagnation_density')
            raise AttributeError('For temperature, zone %s is missing'
                                 ' one or more of %s.' % (zone_name, vnames))
    try:
        gam = flow.gamma.item
    except AttributeError:
        gam = None  # Use passed-in scalar gamma.

    if imin == imax:
        imax += 1  # Ensure range() returns face index.
        face_value = _iface_cell_value if cell_center else _iface_node_value
    elif jmin == jmax:
        jmax += 1
        face_value = _jface_cell_value if cell_center else _jface_node_value
    else:
        kmax += 1
        face_value = _kface_cell_value if cell_center else _kface_node_value

    if reference_state is None:
        pref = 1.
        rhoref = 1.
        momref = 1.
        e0ref = 1.
        rgas = 1.
        gamma = 1.4
    else:
        try:
            pref = reference_state['pressure_reference']
            rgas = reference_state['ideal_gas_constant']
            tref = reference_state['temperature_reference']
            if pressure is None and gam is None:
                gamma = reference_state['specific_heat_ratio'].value
        except KeyError:
            vals = ('pressure_reference', 'ideal_gas_constant',
                    'temperature_reference')
            raise AttributeError('For temperature, reference_state is missing'
                                 ' one or more of %s.' % (vals,))
        if pressure is None:
            if gam is None:
                try:
                    gamma = reference_state['specific_heat_ratio'].value
                except KeyError:
                    raise AttributeError('For temperature, reference_state is'
                                         ' missing specific_heat_ratio')
            raise NotImplementedError('Get dimensional temperature from'
                                      ' Q variables')
        rhoref = pref / rgas / tref
        pref = pref.value
        rgas = rgas.value
        rhoref = rhoref.value
#         e0ref = ?

    total = 0.
    weight_index = 0
    for i in range(imin, imax):
        ip1 = i + 1
        for j in range(jmin, jmax):
            jp1 = j + 1
            for k in range(kmin, kmax):
                kp1 = k + 1

                rho = face_value(density, ip1, jp1, kp1) * rhoref
                if pressure is not None:
                    ps = face_value(pressure, ip1, jp1, kp1) * pref
                else:
                    vu = face_value(mom_c1, ip1, jp1, kp1) * momref / rho
                    vv = face_value(mom_c2, ip1, jp1, kp1) * momref / rho
                    vw = face_value(mom_c3, ip1, jp1, kp1) * momref / rho
                    e0 = face_value(energy, ip1, jp1, kp1) * e0ref / rho
                    if gam is not None:
                        gamma = face_value(gam, ip1, jp1, kp1)
                    ps = (gamma-1.) * rho * (e0 - 0.5*(vu*vu + vv*vv + vw*vw))
                ts = ps / (rho * rgas)

                weight = weights[weight_index]
                weight_index += 1
                total += ts * weight

    if reference_state is None:
        return total
    else:
        return PhysicalQuantity(total, tref.get_unit_name())

register_surface_probe('temperature', _static_temperature, False)


def _total_temperature(domain, surface, weights, reference_state):
    """
    Returns weighted total temperature for a mesh surface as a
    :class:`PhysicalQuantity`.
    """
    zone_name, imin, imax, jmin, jmax, kmin, kmax = surface
    zone = getattr(domain, zone_name)
    flow = zone.flow_solution
    cylindrical = zone.coordinate_system == CYLINDRICAL
    cell_center = flow.grid_location == CELL_CENTER
    weights = weights[zone_name]

    try:
        density = flow.density.item
        if cylindrical:
            mom_c1 = flow.momentum.z.item
            mom_c2 = flow.momentum.r.item
            mom_c3 = flow.momentum.t.item
        else:
            mom_c1 = flow.momentum.x.item
            mom_c2 = flow.momentum.y.item
            mom_c3 = flow.momentum.z.item
    except AttributeError:
        vnames = ('density', 'momentum')
        raise AttributeError('For temperature_stagnation, zone %s is missing'
                             ' one or more of %s.' % (zone_name, vnames))
    try:
        pressure = flow.pressure.item
    except AttributeError:
        pressure = None
        try:
            energy = flow.energy_stagnation_density.item
        except AttributeError:
            vnames = ('pressure', 'energy_stagnation_density')
            raise AttributeError('For temperature_stagnation, zone %s is'
                                 ' one or more of %s.' % (zone_name, vnames))
    try:
        gam = flow.gamma.item
    except AttributeError:
        gam = None  # Use passed-in scalar gamma.

    if imin == imax:
        imax += 1  # Ensure range() returns face index.
        face_value = _iface_cell_value if cell_center else _iface_node_value
    elif jmin == jmax:
        jmax += 1
        face_value = _jface_cell_value if cell_center else _jface_node_value
    else:
        kmax += 1
        face_value = _kface_cell_value if cell_center else _kface_node_value

    if reference_state is None:
        pref = 1.
        rgas = 1.
        rhoref = 1.
        momref = 1.
        e0ref = 1.
    else:
        try:
            pref = reference_state['pressure_reference']
            rgas = reference_state['ideal_gas_constant']
            tref = reference_state['temperature_reference']
            if gam is None:
                gamma = reference_state['specific_heat_ratio'].value
        except KeyError:
            vals = ('pressure_reference', 'ideal_gas_constant',
                    'temperature_reference', 'specific_heat_ratio')
            raise AttributeError('For temperature_stagnation, reference_state'
                                 ' is missing one or more of %s.' % (vals,))
        if pressure is None:
            raise NotImplementedError('Get dimensional temperature_stagnation'
                                      ' from Q variables')
        rhoref = pref / rgas / tref
        vref = (rgas * tref).sqrt()
        momref = rhoref * vref
        pref = pref.value
        rgas = rgas.value
        rhoref = rhoref.value
        momref = momref.value
#        e0ref = ?

    total = 0.
    weight_index = 0
    for i in range(imin, imax):
        ip1 = i + 1
        for j in range(jmin, jmax):
            jp1 = j + 1
            for k in range(kmin, kmax):
                kp1 = k + 1

                rho = face_value(density, ip1, jp1, kp1) * rhoref
                vu = face_value(mom_c1, ip1, jp1, kp1) * momref / rho
                vv = face_value(mom_c2, ip1, jp1, kp1) * momref / rho
                vw = face_value(mom_c3, ip1, jp1, kp1) * momref / rho
                if gam is not None:
                    gamma = face_value(gam, ip1, jp1, kp1)

                u2 = vu*vu + vv*vv + vw*vw
                if pressure is not None:
                    ps = face_value(pressure, ip1, jp1, kp1) * pref
                else:
                    e0 = face_value(energy, ip1, jp1, kp1) * e0ref / rho
                    ps = (gamma-1.) * rho * (e0 - 0.5*u2)
                a2 = (gamma * ps) / rho
                mach2 = u2 / a2
                ts = ps / (rho * rgas)
                tt = ts * (1. + (gamma-1.)/2. * mach2)

                weight = weights[weight_index]
                weight_index += 1
                total += tt * weight

    if reference_state is None:
        return total
    else:
        return PhysicalQuantity(total, tref.get_unit_name())

register_surface_probe('temperature_stagnation', _total_temperature, False)

