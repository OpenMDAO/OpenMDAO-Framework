"""
Support for calculating one or more scalar averages across one or more
regions in a domain.
"""

from math import sqrt

from openmdao.lib.datatypes.domain.flow import CELL_CENTER
from openmdao.lib.datatypes.domain.zone import CYLINDRICAL
from openmdao.lib.datatypes.domain.metrics import get_metric, list_metrics

_SCHEMES = ('area', 'mass')

# TODO: account for ghost cells in index calculations.


def mesh_probe(domain, regions, variables, weighting_scheme='area'):
    """
    Calculate metrics on mesh regions.
    Currently only supports 3D structured grids.

    domain: DomainObj
        The domain to be processed.

    regions: list
        List of `(zone_name, imin, imax[, jmin, jmax[, kmin, kmax]])`
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

    .. note::

        The per-item averaging scheme is simplistic. For instance, all four
        vertices of a face get equal weight, or the two cells sharing a
        face get equal weight. This will lead to inaccuracies for highly
        irregular grids.

    """
    # Check validity of region specifications.
    _regions = _check_regions(domain, regions)

    # Check validity of variables.
    need_weights = False
    for name, units in variables:
        if name not in list_metrics():
            raise ValueError('Unknown/unsupported variable %r' % name)
        cls, integrate, geometry = get_metric(name)
        if not integrate:
            need_weights = True

    # Check validity of weighting scheme.
    if weighting_scheme not in _SCHEMES:
        raise ValueError('Unknown/unsupported weighting scheme %r'
                         % weighting_scheme)

    # Collect weights.
    if need_weights:
        weights, weight_total = _calc_weights(weighting_scheme, domain, _regions)
    else:
        weights, weight_total = {}, 0.

    # Collect metric values.
    metrics = []
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

            value = _calc_metric(name, domain, region, weights, ref)
            value *= zone.symmetry_instances  # Adjust for symmetry.
            if total is None:
                total = value  # Set initial PhysicalQuantity (or float).
            else:
                total += value

        # If not integrating adjust for overall weighting.
        cls, integrate, geometry = get_metric(name)
        if not integrate:
            total /= weight_total

        if units is None:
            metrics.append(total)
        else:
            total.convert_to_unit(units)  # Convert to requested units.
            metrics.append(total.value)

    return metrics


def _check_regions(domain, regions):
    """ Check validity of region specifications and normalize. """
    _regions = []
    for i, region in enumerate(regions, 1):
        if len(region) == 7:
            zone_name, imin, imax, jmin, jmax, kmin, kmax = region
        elif len(region) == 5:
            zone_name, imin, imax, jmin, jmax = region
            kmin, kmax = None, None
        elif len(region) == 3:
            zone_name, imin, imax = region
            jmin, jmax, kmin, kmax = None, None, None, None
        else:
            raise ValueError('region specification %d invalid: %r'
                             % (i, region))
        try:
            zone = getattr(domain, zone_name)
        except AttributeError:
            raise ValueError('region %d: Domain does not contain zone %r'
                             % (i, zone_name))
        shape = zone.shape
        if len(zone.shape) > 2:
            if len(region) != 7:
                raise ValueError('region %d: index dimensionality mismatch' % i)
            zone_imax, zone_jmax, zone_kmax = shape
        elif len(zone.shape) > 1:
            if len(region) != 5:
                raise ValueError('region %d: index dimensionality mismatch' % i)
            zone_imax, zone_jmax = zone.shape
            zone_kmax = None
        elif len(shape) > 0:
            if len(region) != 3:
                raise ValueError('region %d: index dimensionality mismatch' % i)
            zone_imax, = zone.shape
            zone_jmax, zone_kmax = None, None
        else:
            raise ValueError('region %d: zone is empty' % i)

        # Normalize end-relative indexing.
        if imin < 0:
            imin = zone_imax + imin
        if imax < 0:
            imax = zone_imax + imax
        if imin < 0 or imin >= zone_imax:
            raise ValueError('region %d: imin %d invalid (max %d)'
                             % (i, imin, zone_imax))
        if imax < imin or imax >= zone_imax:
            raise ValueError('region %d: imax %d invalid (max %d)'
                             % (i, imax, zone_imax))

        if zone_jmax is not None:
            if jmin < 0:
                jmin = zone_jmax + jmin
            if jmax < 0:
                jmax = zone_jmax + jmax
            if jmin < 0 or jmin >= zone_jmax:
                raise ValueError('region %d: jmin %d invalid (max %d)'
                                 % (i, jmin, zone_jmax))
            if jmax < jmin or jmax >= zone_jmax:
                raise ValueError('region %d: jmax %d invalid (max %d)'
                                 % (i, jmax, zone_jmax))

        if zone_kmax is not None:
            if kmin < 0:
                kmin = zone_kmax + kmin
            if kmax < 0:
                kmax = zone_kmax + kmax
            if kmin < 0 or kmin >= zone_kmax:
                raise ValueError('region %d: kmin %d invalid (max %d)'
                                 % (i, kmin, zone_kmax))
            if kmax < kmin or kmax >= zone_kmax:
                raise ValueError('region %d: kmax %d invalid (max %d)'
                                 % (i, kmax, zone_kmax))

        if len(region) == 7:
            _regions.append((zone_name, imin, imax, jmin, jmax, kmin, kmax))
        elif len(region) == 5:
            _regions.append((zone_name, imin, imax, jmin, jmax))
        else:
            _regions.append((zone_name, imin, imax))

    return _regions


def _get_dimension(region):
    """ Return dimensionality of `region` (0, 1, 2, or 3). """
    if len(region) == 7:
        zone_name, imin, imax, jmin, jmax, kmin, kmax = region
    elif len(region) == 5:
        zone_name, imin, imax, jmin, jmax = region
        kmin, kmax = 0, 0
    else:
        zone_name, imin, imax = region
        jmin, jmax, kmin, kmax = 0, 0, 0, 0

    dim = 0
    if imin != imax:
        dim += 1
    if jmin != jmax:
        dim += 1
    if kmin != kmax:
        dim += 1

    return dim


def _calc_weights(scheme, domain, regions):
    """
    Calculate averaging weights, updating `weights` and returning total value.
    """
    weights = {}
    weight_total = 0.
    for region in regions:
        dim = _get_dimension(region)

        if dim == 3:
            zone_weights = _volume_weights(scheme, domain, region)
        elif dim == 2:
            if len(region) == 7:
                zone_weights = _surface_weights_3d(scheme, domain, region)
            else:
                zone_weights = _surface_weights_2d(scheme, domain, region)
        elif dim == 1:
            if len(region) == 7:
                zone_weights = _curve_weights_3d(scheme, domain, region)
            elif len(region) == 5:
                zone_weights = _curve_weights_2d(scheme, domain, region)
            else:
                zone_weights = _curve_weights_1d(scheme, domain, region)
        else:
            zone_weights = [1.]

        zone_name = region[0]
        zone = getattr(domain, zone_name)
        zone_weights *= zone.symmetry_instances  # Adjust for symmetry.
        if zone_name in weights:
            raise RuntimeError('Zone %r used more than once' % zone_name)
        else:
            weights[zone_name] = zone_weights
        weight_total += sum(zone_weights)

    return (weights, weight_total)


def _volume_weights(scheme, domain, region):
    """ Returns weights for a mesh volume. """
    raise NotImplementedError('_volume_weights')


def _surface_weights_3d(scheme, domain, region):
    """ Returns 3D (index space) weights for a mesh surface. """
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
        imax += 1
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
                    loc = (i, j, k)
                    rvu = face_value(mom_c1, loc)
                    rvv = face_value(mom_c2, loc)
                    rvw = face_value(mom_c3, loc)
                    weights.append(rvu*sc1 + rvv*sc2 + rvw*sc3)
                else:
                    weights.append(sqrt(sc1*sc1 + sc2*sc2 + sc3*sc3))
    return weights


def _surface_weights_2d(scheme, domain, region):
    """ Returns 2D (index space) weights for a mesh surface. """
    zone_name, imin, imax, jmin, jmax = region
    zone = getattr(domain, zone_name)
    grid = zone.grid_coordinates
    cylindrical = zone.coordinate_system == CYLINDRICAL

    if cylindrical:
        c1 = None if grid.z is None else grid.z.item
        c2 = grid.r.item
        c3 = grid.t.item
    else:
        c1 = grid.x.item
        c2 = grid.y.item
        c3 = None if grid.z is None else grid.z.item

    if scheme == 'mass':
        raise NotImplementedError('2D (index space) mass averaging')

    weights = []
    for i in range(imin, imax):
        for j in range(jmin, jmax):
                sc1, sc2, sc3 = _cell_normal(c1, c2, c3, i, j, cylindrical)
                weights.append(sqrt(sc1*sc1 + sc2*sc2 + sc3*sc3))
    return weights


def _curve_weights_3d(scheme, domain, region):
    """ Returns 3D (index space) weights for a mesh curve. """
    zone_name, imin, imax, jmin, jmax, kmin, kmax = region
    zone = getattr(domain, zone_name)
    grid = zone.grid_coordinates
    cylindrical = zone.coordinate_system == CYLINDRICAL

    if cylindrical:
        raise NotImplementedError('curve weights for cylindrical coordinates')
    else:
        x = grid.x.item
        y = grid.y.item
        z = grid.z.item

    if scheme == 'mass':
        raise NotImplementedError('curve mass averaging')

    along_i, along_j = False, False
    if imin != imax:
        along_i = True
        jmax += 1
        kmax += 1
    elif jmin != jmax:
        along_j = True
        imax += 1
        kmax += 1
    else:
        imax += 1
        jmax += 1

    weights = []
    for i in range(imin, imax):
        for j in range(jmin, jmax):
            for k in range(kmin, kmax):
                if along_i:
                    dx = x(i+1, j, k) - x(i, j, k)
                    dy = y(i+1, j, k) - y(i, j, k)
                    dz = z(i+1, j, k) - z(i, j, k)
                elif along_j:
                    dx = x(i, j+1, k) - x(i, j, k)
                    dy = y(i, j+1, k) - y(i, j, k)
                    dz = z(i, j+1, k) - z(i, j, k)
                else:
                    dx = x(i, j, k+1) - x(i, j, k)
                    dy = y(i, j, k+1) - y(i, j, k)
                    dz = z(i, j, k+1) - z(i, j, k)
                weights.append(sqrt(dx*dx + dy*dy + dz*dz))
    return weights


def _curve_weights_2d(scheme, domain, region):
    """ Returns 2D (index space) weights for a mesh curve. """
    zone_name, imin, imax, jmin, jmax = region
    zone = getattr(domain, zone_name)
    grid = zone.grid_coordinates
    cylindrical = zone.coordinate_system == CYLINDRICAL

    if cylindrical:
        raise NotImplementedError('curve weights for cylindrical coordinates')
    else:
        x = grid.x.item
        y = grid.y.item
        z = None if grid.z is None else grid.z.item

    if scheme == 'mass':
        raise NotImplementedError('curve mass averaging')

    if imin != imax:
        along_i = True
        jmax += 1
    else:
        along_i = False
        imax += 1

    weights = []
    for i in range(imin, imax):
        for j in range(jmin, jmax):
                if along_i:
                    dx = x(i+1, j) - x(i, j)
                    dy = y(i+1, j) - y(i, j)
                    dz = 0. if z is None else z(i+1, j) - z(i, j)
                else:
                    dx = x(i, j+1) - x(i, j)
                    dy = y(i, j+1) - y(i, j)
                    dz = 0. if z is None else z(i, j+1) - z(i, j)
                weights.append(sqrt(dx*dx + dy*dy + dz*dz))
    return weights


def _curve_weights_1d(scheme, domain, region):
    """ Returns 1D (index space) weights for a mesh curve. """
    zone_name, imin, imax = region
    zone = getattr(domain, zone_name)
    grid = zone.grid_coordinates
    cylindrical = zone.coordinate_system == CYLINDRICAL

    if cylindrical:
        raise NotImplementedError('curve weights for cylindrical coordinates')
    else:
        x = grid.x.item
        y = None if grid.y is None else grid.y.item
        z = None if grid.z is None else grid.z.item

    if scheme == 'mass':
        raise NotImplementedError('curve mass averaging')

    weights = []
    for i in range(imin, imax):
        dx = x(i+1) - x(i)
        dy = 0. if y is None else y(i+1) - y(i)
        dz = 0. if z is None else z(i+1) - z(i)
        weights.append(sqrt(dx*dx + dy*dy + dz*dz))
    return weights


def _calc_metric(name, domain, region, weights, reference_state):
    """
    Calculate metric `name` on `region` using `weights` and `reference_state`.
    """
    zone_name = region[0]
    zone = getattr(domain, zone_name)
    cls, integrate, geometry = get_metric(name)
    metric = cls(zone, zone_name, reference_state)
    weights = weights.get(zone_name)

    # Could be volume, surface, curve, or point.
    dim = _get_dimension(region)
    if dim == 3:
        if geometry not in ('volume', 'any'):
            raise RuntimeError('metric %r not applicable to volumes')
        total = _volume(metric, integrate, zone, region, weights)
    elif dim == 2:
        if geometry not in ('surface', 'any'):
            raise RuntimeError('metric %r not applicable to surfaces')
        if len(region) == 7:
            total = _surface_3d(metric, integrate, zone, region, weights)
        else:
            total = _surface_2d(metric, integrate, zone, region, weights)
    elif dim == 1:
        if geometry not in ('curve', 'any'):
            raise RuntimeError('metric %r not applicable to curves')
        if len(region) == 7:
            total = _curve_3d(metric, integrate, zone, region, weights)
        elif len(region) == 5:
            total = _curve_2d(metric, integrate, zone, region, weights)
        else:
            total = _curve_1d(metric, integrate, zone, region, weights)
    else:
        if geometry != 'any':
            raise RuntimeError('metric %r not applicable to points')
        total = _point(metric, zone, region)

    if reference_state is None:
        return total
    else:
        return metric.dimensionalize(total)


def _volume(metric, integrate, zone, region, weights):
    """ Calculate metric on a volume. """
    zone_name, imin, imax, jmin, jmax, kmin, kmax = region
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

    vol = None
    weight_index = 0
    total = 0.
    for i in range(imin, imax):
        for j in range(jmin, jmax):
            for k in range(kmin, kmax):

# TODO: calculate volume for integrating.

                if cell_center:
# FIXME: built-in ghosts
                    # Cell value is value.
                    loc = (i+1, j+1, k+1)
                    val = metric.calculate(loc, vol)
                else:
                    # Average across vertices.
                    loc = (i, j, k)
                    val = metric.calculate(loc, vol)
                    loc = (i, j+1, k)
                    val += metric.calculate(loc, vol)
                    loc = (i, j+1, k+1)
                    val += metric.calculate(loc, vol)
                    loc = (i, j, k+1)
                    val += metric.calculate(loc, vol)
                    loc = (i+1, j, k)
                    val += metric.calculate(loc, vol)
                    loc = (i+1, j+1, k)
                    val += metric.calculate(loc, vol)
                    loc = (i+1, j+1, k+1)
                    val += metric.calculate(loc, vol)
                    loc = (i+1, j, k+1)
                    val += metric.calculate(loc, vol)
                    val *= 0.125

                if integrate:
                    total += val
                else:
                    weight = weights[weight_index]
                    weight_index += 1
                    total += val * weight
    return total


def _surface_3d(metric, integrate, zone, region, weights):
    """ Calculate metric on a 3D (index space) surface. """
    zone_name, imin, imax, jmin, jmax, kmin, kmax = region
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

    if imin == imax: 
        face = 'i'
        imax += 1
        get_normal = _iface_normal
    elif jmin == jmax:
        face = 'j'
        jmax += 1 
        get_normal = _jface_normal
    else:
        face = 'k'
        kmax += 1
        get_normal = _kface_normal

    normal = None
    weight_index = 0
    total = 0.
    for i in range(imin, imax):
        for j in range(jmin, jmax):
            for k in range(kmin, kmax):

                if integrate:
                    normal = get_normal(c1, c2, c3, i, j, k, cylindrical)
    
                if cell_center:
# FIXME: built-in ghosts
                    # Average across cells sharing surface.
                    loc = (i+1, j+1, k+1)
                    val = metric.calculate(loc, normal)
                    if face == 'i':
                        loc = (i, j+1, k+1)
                    elif face == 'j':
                        loc = (i+1, j, k+1)
                    else:
                        loc = (i+1, j+1, k)
                    val += metric.calculate(loc, normal)
                    val *= 0.5
                else:
                    # Average across vertices.
                    loc = (i, j, k)
                    val = metric.calculate(loc, normal)
                    if face == 'i':
                        loc = (i, j+1, k)
                        val += metric.calculate(loc, normal)
                        loc = (i, j+1, k+1)
                        val += metric.calculate(loc, normal)
                        loc = (i, j, k+1)
                        val += metric.calculate(loc, normal)
                    elif face == 'j':
                        loc = (i+1, j, k)
                        val += metric.calculate(loc, normal)
                        loc = (i+1, j, k+1)
                        val += metric.calculate(loc, normal)
                        loc = (i, j, k+1)
                        val += metric.calculate(loc, normal)
                    else:
                        loc = (i+1, j, k)
                        val += metric.calculate(loc, normal)
                        loc = (i+1, j+1, k)
                        val += metric.calculate(loc, normal)
                        loc = (i, j+1, k)
                        val += metric.calculate(loc, normal)
                    val *= 0.25

                if integrate:
                    total += val
                else:
                    weight = weights[weight_index]
                    weight_index += 1
                    total += val * weight
    return total


def _surface_2d(metric, integrate, zone, region, weights):
    """ Calculate metric on a 2D (index space) surface. """
    zone_name, imin, imax, jmin, jmax = region
    grid = zone.grid_coordinates
    flow = zone.flow_solution
    cylindrical = zone.coordinate_system == CYLINDRICAL
    cell_center = flow.grid_location == CELL_CENTER

    if cylindrical: 
        c1 = None if grid.z is None else grid.z.item
        c2 = grid.r.item
        c3 = grid.t.item
    else:
        c1 = grid.x.item
        c2 = grid.y.item
        c3 = None if grid.z is None else grid.z.item

    normal = None
    weight_index = 0
    total = 0.
    for i in range(imin, imax):
        for j in range(jmin, jmax):

            if integrate:
                normal = get_normal(c1, c2, c3, i, j, cylindrical)
    
            if cell_center:
# FIXME: built-in ghosts
                # Cell value is value.
                loc = (i+1, j+1)
                val = metric.calculate(loc, normal)
            else:
                # Average across vertices.
                loc = (i, j)
                val = metric.calculate(loc, normal)
                loc = (i, j+1)
                val += metric.calculate(loc, normal)
                loc = (i+1, j+1)
                val += metric.calculate(loc, normal)
                loc = (i+1, j)
                val += metric.calculate(loc, normal)
                val *= 0.25

            if integrate:
                total += val
            else:
                weight = weights[weight_index]
                weight_index += 1
                total += val * weight
    return total


def _curve_3d(metric, integrate, zone, region, weights):
    """ Calculate metric on a 3D (index space) curve. """
    zone_name, imin, imax, jmin, jmax, kmin, kmax = region
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

    if imin != imax:
        edge = 'i'
        jmax += 1
        kmax += 1
        get_length = _iedge_length
    elif jmin != jmax:
        edge = 'j'
        imax += 1
        kmax += 1
        get_length = _jedge_length
    else:
        edge = 'k'
        imax += 1
        jmax += 1
        get_length = _kedge_length

    length = None
    weight_index = 0
    total = 0.
    for i in range(imin, imax):
        for j in range(jmin, jmax):
            for k in range(kmin, kmax):

                if integrate:
                    loc = (i, j, k)
                    length = get_length(c1, c2, c3, loc, cylindrical)

                if cell_center:
# FIXME: built-in ghosts
                    # Average across cells sharing edge.
                    loc = (i+1, j+1, k+1)
                    val = metric.calculate(loc, length)
                    if edge == 'i':
                        loc = (i+1, j, k+1)
                        val += metric.calculate(loc, length)
                        loc = (i+1, j+1, k)
                        val += metric.calculate(loc, length)
                        loc = (i+1, j, k)
                        val += metric.calculate(loc, length)
                    elif edge == 'j':
                        loc = (i, j+1, k+1)
                        val += metric.calculate(loc, length)
                        loc = (i+1, j+1, k)
                        val += metric.calculate(loc, length)
                        loc = (i, j+1, k)
                        val += metric.calculate(loc, length)
                    else:
                        loc = (i, j+1, k+1)
                        val += metric.calculate(loc, length)
                        loc = (i+1, j, k+1)
                        val += metric.calculate(loc, length)
                        loc = (i, j, k+1)
                        val += metric.calculate(loc, length)
                    val *= 0.25
                else:
                    # Average across vertices.
                    loc = (i, j, k)
                    val = metric.calculate(loc, length)
                    if edge == 'i':
                        loc = (i+1, j, k)
                    elif edge == 'j':
                        loc = (i, j+1, k)
                    else:
                        loc = (i, j, k+1)
                    val += metric.calculate(loc, length)
                    val *= 0.5

                if integrate:
                    total += val
                else:
                    weight = weights[weight_index]
                    weight_index += 1
                    total += val * weight
    return total


def _curve_2d(metric, integrate, zone, region, weights):
    """ Calculate metric on a 2D (index space) curve. """
    zone_name, imin, imax, jmin, jmax = region
    grid = zone.grid_coordinates
    flow = zone.flow_solution
    cylindrical = zone.coordinate_system == CYLINDRICAL
    cell_center = flow.grid_location == CELL_CENTER

    if cylindrical: 
        c1 = None if grid.z is None else grid.z.item
        c2 = grid.r.item
        c3 = grid.t.item
    else:
        c1 = grid.x.item
        c2 = grid.y.item
        c3 = None if grid.z is None else grid.z.item

    if imin != imax:
        edge = 'i'
        jmax += 1
        get_length = _iedge_length
    else:
        edge = 'j'
        imax += 1
        get_length = _jedge_length

    length = None
    weight_index = 0
    total = 0.
    for i in range(imin, imax):
        for j in range(jmin, jmax):

            if integrate:
                loc = (i, j)
                length = get_length(c1, c2, c3, loc, cylindrical)

            if cell_center:
# FIXME: built-in ghosts
                # Average across cells sharing edge.
                loc = (i+1, j+1)
                val = metric.calculate(loc, length)
                if edge == 'i':
                    loc = (i+1, j)
                else:
                    loc = (i, j+1)
                val += metric.calculate(loc, length)
                val *= 0.5
            else:
                # Average across vertices.
                loc = (i, j)
                val = metric.calculate(loc, length)
                if edge == 'i':
                    loc = (i+1, j)
                else:
                    loc = (i, j+1)
                val += metric.calculate(loc, length)
                val *= 0.5

            if integrate:
                total += val
            else:
                weight = weights[weight_index]
                weight_index += 1
                total += val * weight
    return total


def _curve_1d(metric, integrate, zone, region, weights):
    """ Calculate metric on a 1D (index space) curve. """
    zone_name, imin, imax = region
    grid = zone.grid_coordinates
    flow = zone.flow_solution
    cylindrical = zone.coordinate_system == CYLINDRICAL
    cell_center = flow.grid_location == CELL_CENTER

    if cylindrical:
        c1 = None if grid.z is None else grid.z.item
        c2 = grid.r.item
        c3 = grid.t.item
    else:
        c1 = grid.x.item
        c2 = None if grid.y is None else grid.y.item
        c3 = None if grid.z is None else grid.z.item

    get_length = _iedge_length

    length = None
    weight_index = 0
    total = 0.
    for i in range(imin, imax):

        if integrate:
            loc = (i,)
            length = get_length(c1, c2, c3, loc, cylindrical)

        if cell_center:
# FIXME: built-in ghosts
            # Cell value is value.
            loc = (i+1,)
            val += metric.calculate(loc, length)
        else:
            # Average across vertices.
            loc = (i,)
            val = metric.calculate(loc, length)
            loc = (i+1,)
            val += metric.calculate(loc, length)
            val *= 0.5

        if integrate:
            total += val
        else:
            weight = weights[weight_index]
            weight_index += 1
            total += val * weight
    return total


def _point(metric, zone, region):
    """ Calculate metric at a point. """
    zone_name = region[0]
    zone = getattr(domain, zone_name)
    flow = zone.flow_solution
    cell_center = flow.grid_location == CELL_CENTER

    if cell_center:
        # Average across cells sharing point.
# FIXME: built-in ghosts
        if len(region) == 7:
            zone_name, imin, imax, jmin, jmax, kmin, kmax = region
            loc = (imin+1, jmin+1, kmin+1)
            val = metric.calculate(loc, None)
            loc = (imin+1, jmin+1, kmin)
            val += metric.calculate(loc, None)
            loc = (imin+1, jmin, kmin)
            val += metric.calculate(loc, None)
            loc = (imin+1, jmin, kmin+1)
            val += metric.calculate(loc, None)
            loc = (imin, jmin+1, kmin+1)
            val += metric.calculate(loc, None)
            loc = (imin, jmin+1, kmin)
            val += metric.calculate(loc, None)
            loc = (imin, jmin, kmin)
            val += metric.calculate(loc, None)
            loc = (imin, jmin, kmin+1)
            val += metric.calculate(loc, None)
            return 0.125 * val
        elif len(region) == 5:
            zone_name, imin, imax, jmin, jmax = region
            loc = (imin+1, jmin+1)
            val = metric.calculate(loc, None)
            loc = (imin, jmin+1)
            val += metric.calculate(loc, None)
            loc = (imin, jmin)
            val += metric.calculate(loc, None)
            loc = (imin+1, jmin)
            val += metric.calculate(loc, None)
            return 0.25 * val
        else:
            zone_name, imin, imax = region
            loc = (imin+1,)
            val = metric.calculate(loc, None)
            loc = (imin,)
            val += metric.calculate(loc, None)
            return 0.5 * val
    else:
        # Vertex value is value.
        return metric.calculate(loc, None)


def _iface_normal(c1, c2, c3, i, j, k, cylindrical):
    """
    Return non-dimensional vector normal to I face with magnitude equal to area.
    """
# FIXME: built-in ghosts
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
# FIXME: built-in ghosts
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
# FIXME: built-in ghosts
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


def _cell_normal(c1, c2, c3, i, j, cylindrical):
    """
    Return non-dimensional vector normal with magnitude equal to area.
    If there is no 'z' coordinate, `c1` will be None in cylindrical
    coordinates, otherwise `c3` will be None.
    """
# FIXME: built-in ghosts
    ip1 = i + 1
    jp1 = j + 1

    # upper-left - lower-right.
    diag_c11 = 0. if c1 is None else c1(i, jp1) - c1(ip1, j)
    diag_c21 = c2(i, jp1) - c2(ip1, j)
    diag_c31 = 0. if c3 is None else c3(i, jp1) - c3(ip1, j)

    # upper-right - lower-left.
    diag_c12 = 0. if c1 is None else c1(ip1, jp1) - c1(i, j)
    diag_c22 = c2(ip1, jp1) - c2(i, j)
    diag_c32 = 0. if c3 is None else c3(ip1, jp1) - c3(i, j)

    if cylindrical:
        r1 = (c2(i, jp1) + c2(ip1, j)) / 2.
        r2 = (c2(i, j) + c2(ip1, jp1)) / 2.
    else:
        r1 = 1.
        r2 = 1.

    sc1 = 0.5 * ( r2 * diag_c21 * diag_c32 - r1 * diag_c22 * diag_c31)
    sc2 = 0.5 * (-r2 * diag_c11 * diag_c32 + r1 * diag_c12 * diag_c31)
    sc3 = 0.5 * (      diag_c11 * diag_c22 -      diag_c12 * diag_c21)

    return (sc1, sc2, sc3)


def _iedge_length(c1, c2, c3, loc, cylindrical):
    """ Return length of edge along 'i'. """
    if cylindrical:
        raise NotImplementedError('cylindrical _iedge_length')
    else:
        if len(loc) > 2:
            i, j, k = loc
            ip1 = i + 1
            dx = c1(ip1, j, k) - c1(i, j, k)
            dy = c2(ip1, j, k) - c2(i, j, k)
            dz = c3(ip1, j, k) - c3(i, j, k)
        elif len(loc) > 1:
            i, j = loc
            ip1 = i + 1
            dx = c1(ip1, j) - c1(i, j)
            dy = c2(ip1, j) - c2(i, j)
            dz = 0. if c3 is None else c3(ip1, j) - c3(i, j)
        else:
            i, = loc
            ip1 = i + 1
            dx = c1(ip1) - c1(i)
            dy = 0. if c2 is None else c2(ip1) - c2(i)
            dz = 0. if c3 is None else c3(ip1) - c3(i)
        return sqrt(dx*dx + dy*dy + dz*dz)


def _jedge_length(c1, c2, c3, loc, cylindrical):
    """ Return length of edge along 'j'. """
    if cylindrical:
        raise NotImplementedError('cylindrical _jedge_length')
    else:
        if len(loc) > 2:
            i, j, k = loc
            jp1 = j + 1
            dx = c1(i, jp1, k) - c1(i, j, k)
            dy = c2(i, jp1, k) - c2(i, j, k)
            dz = c3(i, jp1, k) - c3(i, j, k)
        else:
            i, j = loc
            jp1 = j + 1
            dx = c1(i, jp1) - c1(i, j)
            dy = c2(i, jp1) - c2(i, j)
            dz = 0. if c3 is None else c3(i, jp1) - c3(i, j)
        return sqrt(dx*dx + dy*dy + dz*dz)


def _kedge_length(c1, c2, c3, loc, cylindrical):
    """ Return length of edge along 'k'. """
    i, j, k = loc
    if cylindrical:
        raise NotImplementedError('cylindrical _kedge_length')
    else:
        kp1 = k + 1
        dx = c1(i, j, kp1) - c1(i, j, k)
        dy = c2(i, j, kp1) - c2(i, j, k)
        dz = c3(i, j, kp1) - c3(i, j, k)
        return sqrt(dx*dx + dy*dy + dz*dz)


def _iface_cell_value(arr, loc):
    """ Returns I face value for cell-centered data. """
    i, j, k = loc
# FIXME: built-in ghosts
    return 0.5 * (arr(i+1, j+1, k+1) + arr(i, j+1, k+1))

def _jface_cell_value(arr, loc):
    """ Returns J face value for cell-centered data. """
    i, j, k = loc
# FIXME: built-in ghosts
    return 0.5 * (arr(i+1, j+1, k+1) + arr(i+1, j, k+1))

def _kface_cell_value(arr, loc):
    """ Returns K face value for cell-centered data. """
    i, j, k = loc
# FIXME: built-in ghosts
    return 0.5 * (arr(i+1, j+1, k+1) + arr(i+1, j+1, k))


def _iface_node_value(arr, loc):
    """ Returns I face value for vertex data. """
    i, j, k = loc
    return 0.25 * (arr(i-1, j, k) + arr(i-1, j-1, k) +
                   arr(i-1, j, k-1) + arr(i-1, j-1, k-1))

def _jface_node_value(arr, loc):
    """ Returns J face value for vertex data. """
    i, j, k = loc
    return 0.25 * (arr(i, j-1, k) + arr(i-1, j-1, k) +
                   arr(i, j-1, k-1) + arr(i-1, j-1, k-1))

def _kface_node_value(arr, loc):
    """ Returns K face value for vertex data. """
    i, j, k = loc
    return 0.25 * (arr(i, j, k-1) + arr(i-1, j, k-1) +
                   arr(i, j-1, k-1) + arr(i-1, j-1, k-1))

