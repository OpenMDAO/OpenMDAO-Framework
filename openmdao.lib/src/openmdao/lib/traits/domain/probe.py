from math import sqrt

from openmdao.units.units import PhysicalQuantity

from openmdao.lib.traits.domain.zone import CYLINDRICAL

# Dictionary for calculated variable information.
# Holds (integrate_flag, collect_function).
# Populated as collection functions are defined.
_VARIABLES = {}

_SCHEMES = ('area', 'mass')

# TODO: support Vertex flow solution grid location.
# TODO: account for ghost cells in index calculations.


def register_surface_probe(name, function, integrate):
    """
    Register a surface probe function.

    - `name` is the name of the metric calculated.
    - `function` is the function to call.  The passed arguments are \
    ``(domain, surface, weights, reference_state)``.
    - If `integrate` then function values are integrated, not averaged.
    """
    _VARIABLES[name] = (integrate, function)


def surface_probe(domain, surfaces, variables, weighting_scheme='area'):
    """
    Calculate metrics on mesh surfaces.
    Currently only supports 3D structured grids with cell-centered data.

    - `domain` is the :class:`Domain` to be processed.
    - `surfaces` is a list of ``(zone_name,imin,imax,jmin,jmax,kmin,kmax)`` \
    mesh surface specifications to be used for the calculation. \
    Indices start at 0. Negative indices are relative to the end of the array.
    - `variables` is a list of ``(metric_name, units)`` tuples. Legal metric \
    names are 'area', 'mass_flow', 'corrected_mass_flow', 'pressure', \
    'pressure_stagnation', 'temperature', and 'temperature_stagnation'.
    - `weighting_scheme` specifies how individual values are weighted. \
    Legal values are 'area' for area averaging and 'mass' for mass averaging.

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
            raise ValueError("Domain does not contain zone '%s'" % zone_name)

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
            raise ValueError("Unknown/unsupported variable '%s'" % name)

    # Check validity of weighting scheme.
    if weighting_scheme not in _SCHEMES:
        raise ValueError("Unknown/unsupported weighting scheme '%s'"
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

            # Check for a reference_state dictionary.
            ref = zone.reference_state or domain.reference_state
            if not ref:
                raise ValueError('No zone or domain reference_state dictionary'
                                 ' supplied for %s.' % zone_name)

            value = _VARIABLES[name][1](domain, surface, weights, ref)
            value *= zone.symmetry_instances  # Adjust for symmetry.
            if total is None:
                total = value
            else:
                total += value

        # If not integrating adjust for overall weighting.
        if not _VARIABLES[name][0]:
            total /= weight_total

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
    cell_center = flow.grid_location == 'CellCenter'

    x = grid.x.item
    y = grid.y.item
    z = grid.z.item

    if scheme == 'mass':
        try:
            mom_x = flow.momentum.x.item
            mom_y = flow.momentum.y.item
            mom_z = flow.momentum.z.item
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

                sx, sy, sz = face_normal(x, y, z, i, j, k, cylindrical)
                if scheme == 'mass':
                    kp1 = k + 1
                    rvu = face_value(mom_x, ip1, jp1, kp1)
                    rvv = face_value(mom_y, ip1, jp1, kp1)
                    rvw = face_value(mom_z, ip1, jp1, kp1)
                    weights.append(rvu*sx + rvv*sy + rvw*sz)
                else:
                    weights.append(sqrt(sx*sx + sy*sy + sz*sz))

    return weights


def _iface_normal(x, y, z, i, j, k, cylindrical, lref=1.):
    """ Return vector normal to I face with magnitude equal to area. """
    jp1 = j + 1
    kp1 = k + 1

    # upper-left - lower-right.
    diag_x1 = x(i, jp1, k) - x(i, j, kp1)
    diag_y1 = y(i, jp1, k) - y(i, j, kp1)
    diag_z1 = z(i, jp1, k) - z(i, j, kp1)

    # upper-right - lower-left.
    diag_x2 = x(i, jp1, kp1) - x(i, j, k)
    diag_y2 = y(i, jp1, kp1) - y(i, j, k)
    diag_z2 = z(i, jp1, kp1) - z(i, j, k)

    if cylindrical:
        r1 = (y(i, j, kp1) + y(i, jp1, k  )) / 2.
        r2 = (y(i, j, k  ) + y(i, jp1, kp1)) / 2.
    else:
        r1 = 1.
        r2 = 1.

    aref = lref * lref

    sx = -0.5 * ( r2 * diag_y1 * diag_z2 - r1 * diag_y2 * diag_z1) * aref
    sy = -0.5 * (-r2 * diag_x1 * diag_z2 + r1 * diag_x2 * diag_z1) * aref
    sz = -0.5 * (      diag_x1 * diag_y2 -      diag_x2 * diag_y1) * aref

    return (sx, sy, sz)


def _jface_normal(x, y, z, i, j, k, cylindrical, lref=1.):
    """ Return vector normal to J face with magnitude equal to area. """
    ip1 = i + 1
    kp1 = k + 1

    # upper-left - lower-right.
    diag_x1 = x(ip1, j, k) - x(i, j, kp1)
    diag_y1 = y(ip1, j, k) - y(i, j, kp1)
    diag_z1 = z(ip1, j, k) - z(i, j, kp1)

    # upper-right - lower-left.
    diag_x2 = x(ip1, j, kp1) - x(i, j, k)
    diag_y2 = y(ip1, j, kp1) - y(i, j, k)
    diag_z2 = z(ip1, j, kp1) - z(i, j, k)

    if cylindrical:
        r1 = (y(i, j, kp1) + y(ip1, j, k  )) / 2.
        r2 = (y(i, j, k  ) + y(ip1, j, kp1)) / 2.
    else:
        r1 = 1.
        r2 = 1.

    aref = lref * lref

    sx = 0.5 * ( r2 * diag_y1 * diag_z2 - r1 * diag_y2 * diag_z1) * aref
    sy = 0.5 * (-r2 * diag_x1 * diag_z2 + r1 * diag_x2 * diag_z1) * aref
    sz = 0.5 * (      diag_x1 * diag_y2 -      diag_x2 * diag_y1) * aref

    return (sx, sy, sz)


def _kface_normal(x, y, z, i, j, k, cylindrical, lref=1.):
    """ Return vector normal to K face with magnitude equal to area. """
    ip1 = i + 1
    jp1 = j + 1

    # upper-left - lower-right.
    diag_x1 = x(i, jp1, k) - x(ip1, j, k)
    diag_y1 = y(i, jp1, k) - y(ip1, j, k)
    diag_z1 = z(i, jp1, k) - z(ip1, j, k)

    # upper-right - lower-left.
    diag_x2 = x(ip1, jp1, k) - x(i, j, k)
    diag_y2 = y(ip1, jp1, k) - y(i, j, k)
    diag_z2 = z(ip1, jp1, k) - z(i, j, k)

    if cylindrical:
        r1 = (y(i, jp1, k) + y(ip1, j,   k)) / 2.
        r2 = (y(i, j,   k) + y(ip1, jp1, k)) / 2.
    else:
        r1 = 1.
        r2 = 1.

    aref = lref * lref

    sx = 0.5 * ( r2 * diag_y1 * diag_z2 - r1 * diag_y2 * diag_z1) * aref
    sy = 0.5 * (-r2 * diag_x1 * diag_z2 + r1 * diag_x2 * diag_z1) * aref
    sz = 0.5 * (      diag_x1 * diag_y2 -      diag_x2 * diag_y1) * aref

    return (sx, sy, sz)


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
    raise NotImplementedError('Zone solution location Vertex not supported')

def _jface_node_value(arr, i, j, k):
    """ Returns J face value for vertex data. """
    raise NotImplementedError('Zone solution location Vertex not supported')

def _kface_node_value(arr, i, j, k):
    """ Returns K face value for vertex data. """
    raise NotImplementedError('Zone solution location Vertex not supported')


def _area(domain, surface, weights, reference_state):
    """ Returns area of mesh surface as a :class:`PhysicalQuantity`. """
    zone_name, imin, imax, jmin, jmax, kmin, kmax = surface
    zone = getattr(domain, zone_name)
    grid = zone.grid_coordinates
    cylindrical = zone.coordinate_system == CYLINDRICAL

    x = grid.x.item
    y = grid.y.item
    z = grid.z.item

    if imin == imax:
        imax += 1  # Ensure range() returns face index.
        normal = _iface_normal
    elif jmin == jmax:
        jmax += 1
        normal = _jface_normal
    else:
        kmax += 1
        normal = _kface_normal

    try:
        lref = reference_state['length_reference']
    except KeyError:
        raise AttributeError("For area, reference_state is missing"
                             " 'length_reference'.")
    aref = lref * lref

    total = 0.
    for i in range(imin, imax):
        for j in range(jmin, jmax):
            for k in range(kmin, kmax):
                sx, sy, sz = normal(x, y, z, i, j, k, cylindrical, lref.value)
                total += sqrt(sx*sx + sy*sy + sz*sz)

    return PhysicalQuantity(total, aref.get_unit_name())

register_surface_probe('area', _area, True)


def _massflow(domain, surface, weights, reference_state):
    """ Returns mass flow for a mesh surface as a :class:`PhysicalQuantity`. """
    zone_name, imin, imax, jmin, jmax, kmin, kmax = surface
    zone = getattr(domain, zone_name)
    grid = zone.grid_coordinates
    flow = zone.flow_solution
    cylindrical = zone.coordinate_system == CYLINDRICAL
    cell_center = flow.grid_location == 'CellCenter'

    try:
        mom_x = flow.momentum.x.item
        mom_y = flow.momentum.y.item
        mom_z = flow.momentum.z.item
    except AttributeError:
        raise AttributeError("For mass flow, zone %s is missing 'momentum'."
                             % zone_name)
    x = grid.x.item
    y = grid.y.item
    z = grid.z.item

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

    try:
        lref = reference_state['length_reference']
        pref = reference_state['pressure_reference']
        rgas = reference_state['ideal_gas_constant']
        tref = reference_state['temperature_reference']
    except KeyError:
        vals = ('length_reference', 'pressure_reference', 'ideal_gas_constant',
                'temperature_reference')
        raise AttributeError('For mass flow, reference_state is missing'
                             ' one or more of %s.' % (vals,))

    rhoref = pref / rgas / tref
    vref = (rgas * tref).sqrt()
    momref = rhoref * vref
    wref = momref * lref * lref

    total = 0.
    for i in range(imin, imax):
        ip1 = i + 1
        for j in range(jmin, jmax):
            jp1 = j + 1
            for k in range(kmin, kmax):
                kp1 = k + 1

                rvu = face_value(mom_x, ip1, jp1, kp1) * momref.value
                rvv = face_value(mom_y, ip1, jp1, kp1) * momref.value
                rvw = face_value(mom_z, ip1, jp1, kp1) * momref.value
                sx, sy, sz = face_normal(x, y, z, i, j, k, cylindrical,
                                         lref.value)

                w = rvu*sx + rvv*sy + rvw*sz
                total += w

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
    cell_center = flow.grid_location == 'CellCenter'

    try:
        density = flow.density.item
        mom_x = flow.momentum.x.item
        mom_y = flow.momentum.y.item
        mom_z = flow.momentum.z.item
        pressure = flow.pressure.item
    except AttributeError:
        vnames = ('density', 'momentum', 'pressure')
        raise AttributeError('For corrected mass flow, zone %s is missing'
                             ' one or more of %s.' % (zone_name, vnames))
    try:
        gam = flow.gamma.item
    except AttributeError:
        gam = None  # Use passed-in scalar gamma.

    x = grid.x.item
    y = grid.y.item
    z = grid.z.item

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
        raise AttributeError('For corrected mass flow, reference_state is'
                             ' missing one or more of %s.' % (vals,))

    rhoref = pref / rgas / tref
    vref = (rgas * tref).sqrt()
    momref = rhoref * vref
    wref = momref * lref * lref

    pstd = PhysicalQuantity(14.696, 'psi')
    pstd.convert_to_unit(pref.get_unit_name())

    tstd = PhysicalQuantity(518.67, 'degR')
    tstd.convert_to_unit(tref.get_unit_name())

    total = 0.
    for i in range(imin, imax):
        ip1 = i + 1
        for j in range(jmin, jmax):
            jp1 = j + 1
            for k in range(kmin, kmax):
                kp1 = k + 1

                rho = face_value(density, ip1, jp1, kp1) * rhoref.value
                rvu = face_value(mom_x, ip1, jp1, kp1) * momref.value
                rvv = face_value(mom_y, ip1, jp1, kp1) * momref.value
                rvw = face_value(mom_z, ip1, jp1, kp1) * momref.value
                ps = face_value(pressure, ip1, jp1, kp1) * pref.value
                if gam is not None:
                    gamma = face_value(gam, ip1, jp1, kp1)
                sx, sy, sz = face_normal(x, y, z, i, j, k, cylindrical,
                                         lref.value)

                w = rvu*sx + rvv*sy + rvw*sz

                u2 = (rvu*rvu + rvv*rvv + rvw*rvw) / (rho*rho)
                a2 = (gamma * ps) / rho
                mach2 = u2 / a2
                ts = ps / (rho * rgas.value)
                tt = ts * (1. + (gamma-1.)/2. * mach2)

                pt = ps * pow(1. + (gamma-1.)/2. * mach2, gamma/(gamma-1.))

                wc = w * sqrt(tt/tstd.value) / (pt/pstd.value)
                total += wc

    return PhysicalQuantity(total, wref.get_unit_name())

register_surface_probe('corrected_mass_flow', _corrected_massflow, True)


def _static_pressure(domain, surface, weights, reference_state):
    """
    Returns weighted static pressure for a mesh surface as a
    :class:`PhysicalQuantity`.
    """
    zone_name, imin, imax, jmin, jmax, kmin, kmax = surface
    flow = getattr(domain, zone_name).flow_solution
    cell_center = flow.grid_location == 'CellCenter'
    weights = weights[zone_name]

    try:
        pressure = flow.pressure.item
    except AttributeError:
        raise AttributeError("For static pressure, zone %s is missing"
                             " 'pressure'." % zone_name)
    if imin == imax:
        imax += 1  # Ensure range() returns face index.
        face_value = _iface_cell_value if cell_center else _iface_node_value
    elif jmin == jmax:
        jmax += 1
        face_value = _jface_cell_value if cell_center else _jface_node_value
    else:
        kmax += 1
        face_value = _kface_cell_value if cell_center else _kface_node_value

    try:
        pref = reference_state['pressure_reference']
    except KeyError:
        raise AttributeError("For static pressure, reference_state is missing"
                             " 'pressure_reference'.")
    total = 0.
    weight_index = 0
    for i in range(imin, imax):
        ip1 = i + 1
        for j in range(jmin, jmax):
            jp1 = j + 1
            for k in range(kmin, kmax):
                kp1 = k + 1

                ps = face_value(pressure, ip1, jp1, kp1) * pref.value
                weight = weights[weight_index]
                weight_index += 1

                total += ps * weight

    return PhysicalQuantity(total, pref.get_unit_name())

register_surface_probe('pressure', _static_pressure, False)


def _total_pressure(domain, surface, weights, reference_state):
    """
    Returns weighted total pressure for a mesh surface as a
    :class:`PhysicalQuantity`.
    """
    zone_name, imin, imax, jmin, jmax, kmin, kmax = surface
    flow = getattr(domain, zone_name).flow_solution
    cell_center = flow.grid_location == 'CellCenter'
    weights = weights[zone_name]

    try:
        density = flow.density.item
        mom_x = flow.momentum.x.item
        mom_y = flow.momentum.y.item
        mom_z = flow.momentum.z.item
        pressure = flow.pressure.item
    except AttributeError:
        vnames = ('density', 'momentum', 'pressure')
        raise AttributeError('For total pressure, zone %s is missing'
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

    try:
        pref = reference_state['pressure_reference']
        rgas = reference_state['ideal_gas_constant']
        tref = reference_state['temperature_reference']
        if gam is None:
            gamma = reference_state['specific_heat_ratio'].value
    except KeyError:
        vals = ('pressure_reference', 'ideal_gas_constant',
                'temperature_reference', 'specific_heat_ratio')
        raise AttributeError('For total pressure, reference_state is missing'
                             ' one or more of %s.' % (vals,))

    rhoref = pref / rgas / tref
    vref = (rgas * tref).sqrt()
    momref = rhoref * vref

    total = 0.
    weight_index = 0
    for i in range(imin, imax):
        ip1 = i + 1
        for j in range(jmin, jmax):
            jp1 = j + 1
            for k in range(kmin, kmax):
                kp1 = k + 1

                rho = face_value(density, ip1, jp1, kp1) * rhoref.value
                vu = face_value(mom_x, ip1, jp1, kp1) * momref.value / rho
                vv = face_value(mom_y, ip1, jp1, kp1) * momref.value / rho
                vw = face_value(mom_z, ip1, jp1, kp1) * momref.value / rho
                ps = face_value(pressure, ip1, jp1, kp1) * pref.value
                if gam is not None:
                    gamma = face_value(gam, ip1, jp1, kp1)
                weight = weights[weight_index]
                weight_index += 1

                u2 = vu*vu + vv*vv + vw*vw
                a2 = (gamma * ps) / rho
                mach2 = u2 / a2
                pt = ps * pow(1. + (gamma-1.)/2. * mach2, gamma/(gamma-1.))
                total += pt * weight

    return PhysicalQuantity(total, pref.get_unit_name())

register_surface_probe('pressure_stagnation', _total_pressure, False)


def _static_temperature(domain, surface, weights, reference_state):
    """
    Returns weighted static temperature for a mesh surface as a
    :class:`PhysicalQuantity`.
    """
    zone_name, imin, imax, jmin, jmax, kmin, kmax = surface
    flow = getattr(domain, zone_name).flow_solution
    cell_center = flow.grid_location == 'CellCenter'
    weights = weights[zone_name]

    try:
        density = flow.density.item
        pressure = flow.pressure.item
    except AttributeError:
        vnames = ('density', 'pressure')
        raise AttributeError('For static temperature, zone %s is missing'
                             ' one or more of %s.' % (zone_name, vnames))
    if imin == imax:
        imax += 1  # Ensure range() returns face index.
        face_value = _iface_cell_value if cell_center else _iface_node_value
    elif jmin == jmax:
        jmax += 1
        face_value = _jface_cell_value if cell_center else _jface_node_value
    else:
        kmax += 1
        face_value = _kface_cell_value if cell_center else _kface_node_value

    try:
        pref = reference_state['pressure_reference']
        rgas = reference_state['ideal_gas_constant']
        tref = reference_state['temperature_reference']
    except KeyError:
        vals = ('pressure_reference', 'ideal_gas_constant',
                'temperature_reference')
        raise AttributeError('For static pressure, reference_state is missing'
                             ' one or more of %s.' % (vals,))

    rhoref = pref / rgas / tref

    total = 0.
    weight_index = 0
    for i in range(imin, imax):
        ip1 = i + 1
        for j in range(jmin, jmax):
            jp1 = j + 1
            for k in range(kmin, kmax):
                kp1 = k + 1

                rho = face_value(density, ip1, jp1, kp1) * rhoref.value
                ps = face_value(pressure, ip1, jp1, kp1) * pref.value
                weight = weights[weight_index]
                weight_index += 1

                ts = ps / (rho * rgas.value)
                total += ts * weight

    return PhysicalQuantity(total, tref.get_unit_name())

register_surface_probe('temperature', _static_temperature, False)


def _total_temperature(domain, surface, weights, reference_state):
    """
    Returns weighted total temperature for a mesh surface as a
    :class:`PhysicalQuantity`.
    """
    zone_name, imin, imax, jmin, jmax, kmin, kmax = surface
    flow = getattr(domain, zone_name).flow_solution
    cell_center = flow.grid_location == 'CellCenter'
    weights = weights[zone_name]

    try:
        density = flow.density.item
        mom_x = flow.momentum.x.item
        mom_y = flow.momentum.y.item
        mom_z = flow.momentum.z.item
        pressure = flow.pressure.item
    except AttributeError:
        vnames = ('density', 'momentum', 'pressure')
        raise AttributeError('For total temperature, zone %s is missing'
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

    try:
        pref = reference_state['pressure_reference']
        rgas = reference_state['ideal_gas_constant']
        tref = reference_state['temperature_reference']
        if gam is None:
            gamma = reference_state['specific_heat_ratio'].value
    except KeyError:
        vals = ('pressure_reference', 'ideal_gas_constant',
                'temperature_reference', 'specific_heat_ratio')
        raise AttributeError('For total pressure, reference_state is missing'
                             ' one or more of %s.' % (vals,))

    rhoref = pref / rgas / tref
    vref = (rgas * tref).sqrt()
    momref = rhoref * vref

    total = 0.
    weight_index = 0
    for i in range(imin, imax):
        ip1 = i + 1
        for j in range(jmin, jmax):
            jp1 = j + 1
            for k in range(kmin, kmax):
                kp1 = k + 1

                rho = face_value(density, ip1, jp1, kp1) * rhoref.value
                vu = face_value(mom_x, ip1, jp1, kp1) * momref.value / rho
                vv = face_value(mom_y, ip1, jp1, kp1) * momref.value / rho
                vw = face_value(mom_z, ip1, jp1, kp1) * momref.value / rho
                ps = face_value(pressure, ip1, jp1, kp1) * pref.value
                if gam is not None:
                    gamma = face_value(gam, ip1, jp1, kp1)
                weight = weights[weight_index]
                weight_index += 1

                u2 = vu*vu + vv*vv + vw*vw
                a2 = (gamma * ps) / rho
                mach2 = u2 / a2
                ts = ps / (rho * rgas.value)
                tt = ts * (1. + (gamma-1.)/2. * mach2)
                total += tt * weight

    return PhysicalQuantity(total, tref.get_unit_name())

register_surface_probe('temperature_stagnation', _total_temperature, False)

