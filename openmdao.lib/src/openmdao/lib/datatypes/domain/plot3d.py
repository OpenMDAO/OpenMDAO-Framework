"""
Functions to read and write a :class:`DomainObj` in Plot3D format.
Many function arguments are common:

multiblock: bool
    If True, then the file is assumed to have a multiblock header.

dim: int
    Specifies the expected dimensionality of the blocks.

blanking: bool
    If True, then blanking data is expected.

planes: bool
    If True, then the data is expected in planar, not whole, format.

binary: bool
    If True, the data is in binary, not text, form.

big_endian: bool
    If True, the data bytes are in 'big-endian' order.
    Only meaningful if `binary`.

single_precision: bool
    If True, floating-point data is 32 bits, not 64.
    Only meaningful if `binary`.

unformatted: bool
    If True, the data is surrounded by Fortran record length markers.
    Only meaningful if `binary`.

logger: Logger or None
    Used to record progress.

Default argument values are set for a typical 3D multiblock single-precision
Fortran unformatted file.  When writing, zones are assumed in Cartesian
coordinates with data located at the vertices.
"""

import numpy

from openmdao.util.log import NullLogger
from openmdao.util.stream import Stream

from openmdao.lib.datatypes.domain.domain import DomainObj
from openmdao.lib.datatypes.domain.zone import Zone
from openmdao.lib.datatypes.domain.vector import Vector


def read_plot3d_q(grid_file, q_file, multiblock=True, dim=3, blanking=False,
                  planes=False, binary=True, big_endian=False,
                  single_precision=True, unformatted=True, logger=None):
    """
    Returns a :class:`DomainObj` initialized from Plot3D `grid_file` and
    `q_file`.  Q variables are assigned to 'density', 'momentum', and
    'energy_stagnation_density'.  Scalars are assigned to 'mach', 'alpha',
    'reynolds', and 'time'.

    grid_file: string
        Grid filename.

    q_file: string
        Q data filename.
    """
    logger = logger or NullLogger()

    domain = read_plot3d_grid(grid_file, multiblock, dim, blanking, planes,
                              binary, big_endian, single_precision,
                              unformatted, logger)

    mode = 'rb' if binary else 'r'
    with open(q_file, mode) as inp:
        logger.info('reading Q file %r', q_file)
        stream = Stream(inp, binary, big_endian, single_precision, False,
                        unformatted, False)
        if multiblock:
            # Read number of zones.
            nblocks = stream.read_int(full_record=True)
        else:
            nblocks = 1
        if nblocks != len(domain.zones):
            raise RuntimeError('Q zones %d != Grid zones %d'
                               % (nblocks, len(domain.zones)))

        # Read zone dimensions.
        if unformatted:
            reclen = stream.read_recordmark()
            expected = stream.reclen_ints(dim * nblocks)
            if reclen != expected:
                logger.warning('unexpected dimensions recordlength'
                               ' %d vs. %d', reclen, expected)
        for zone in domain.zones:
            name = domain.zone_name(zone)
            imax, jmax, kmax = _read_plot3d_dims(stream, dim)
            if dim > 2:
                logger.debug('    %s: %dx%dx%d', name, imax, jmax, kmax)
                zone_i, zone_j, zone_k = zone.shape
                if imax != zone_i or jmax != zone_j or kmax != zone_k:
                    raise RuntimeError('%s: Q %dx%dx%d != Grid %dx%dx%d'
                                       % (name, imax, jmax, kmax,
                                          zone_i, zone_j, zone_k))
            else:
                logger.debug('    %s: %dx%d', name, imax, jmax)
                zone_i, zone_j = zone.shape
                if imax != zone_i or jmax != zone_j:
                    raise RuntimeError('%s: Q %dx%d != Grid %dx%d'
                                       % (name, imax, jmax, zone_i, zone_j))
        if unformatted:
            reclen2 = stream.read_recordmark()
            if reclen2 != reclen:
                logger.warning('mismatched dimensions recordlength'
                               ' %d vs. %d', reclen2, reclen)

        # Read zone scalars and variables.
        for zone in domain.zones:
            name = domain.zone_name(zone)
            logger.debug('reading data for %s', name)
            _read_plot3d_qscalars(zone, stream, logger)
            _read_plot3d_qvars(zone, stream, planes, logger)

    return domain


def read_plot3d_f(grid_file, f_file, varnames=None, multiblock=True, dim=3,
                  blanking=False, planes=False, binary=True, big_endian=False,
                  single_precision=True, unformatted=True, logger=None):
    """
    Returns a :class:`DomainObj` initialized from Plot3D `grid_file` and
    `f_file`.  Variables are assigned to names of the form `f_N`.

    grid_file: string
        Grid filename.

    f_file: string
        Function data filename.
    """
    logger = logger or NullLogger()

    domain = read_plot3d_grid(grid_file, multiblock, dim, blanking, planes,
                              binary, big_endian, single_precision,
                              unformatted, logger)

    mode = 'rb' if binary else 'r'
    with open(f_file, mode) as inp:
        logger.info('reading F file %r', f_file)
        stream = Stream(inp, binary, big_endian, single_precision, False,
                        unformatted, False)
        if multiblock:
            # Read number of zones.
            nblocks = stream.read_int(full_record=True)
        else:
            nblocks = 1
        if nblocks != len(domain.zones):
            raise RuntimeError('F zones %d != Grid zones %d'
                               % (nblocks, len(domain.zones)))

        # Read zone dimensions.
        if unformatted:
            reclen = stream.read_recordmark()
            expected = stream.reclen_ints((dim+1) * nblocks)
            if reclen != expected:
                logger.warning('unexpected dimensions recordlength'
                               ' %d vs. %d', reclen, expected)
        for zone in domain.zones:
            name = domain.zone_name(zone)
            imax, jmax, kmax, nvars = _read_plot3d_dims(stream, dim, True)
            if dim > 2:
                logger.debug('    %s: %dx%dx%d %d',
                             name, imax, jmax, kmax, nvars)
                zone_i, zone_j, zone_k = zone.shape
                if imax != zone_i or jmax != zone_j or kmax != zone_k:
                    raise RuntimeError('%s: F %dx%dx%d != Grid %dx%dx%d'
                                       % (name, imax, jmax, kmax,
                                          zone_i, zone_j, zone_k))
            else:
                logger.debug('    %s: %dx%d %d', name, imax, jmax, nvars)
                zone_i, zone_j = zone.shape
                if imax != zone_i or jmax != zone_j:
                    raise RuntimeError('%s: F %dx%d != Grid %dx%d'
                                       % (name, imax, jmax, zone_i, zone_j))
        if unformatted:
            reclen2 = stream.read_recordmark()
            if reclen2 != reclen:
                logger.warning('mismatched dimensions recordlength'
                               ' %d vs. %d', reclen2, reclen)

        # Read zone variables.
        for zone in domain.zones:
            name = domain.zone_name(zone)
            logger.debug('reading data for %s', name)
            _read_plot3d_fvars(zone, stream, dim, nvars, varnames, planes,
                               logger)
    return domain


def read_plot3d_grid(grid_file, multiblock=True, dim=3, blanking=False,
                     planes=False, binary=True, big_endian=False,
                     single_precision=True, unformatted=True, logger=None):
    """
    Returns a :class:`DomainObj` initialized from Plot3D `grid_file`.

    grid_file: string
        Grid filename.
    """
    logger = logger or NullLogger()
    domain = DomainObj()

    mode = 'rb' if binary else 'r'
    with open(grid_file, mode) as inp:
        logger.info('reading grid file %r', grid_file)
        stream = Stream(inp, binary, big_endian, single_precision, False,
                        unformatted, False)

        # Read zone dimensions.
        shape = _read_plot3d_shape(stream, multiblock, dim, logger)

        # Read zone coordinates.
        for i in range(len(shape)):
            zone = domain.add_zone('', Zone())
            name = domain.zone_name(zone)
            logger.debug('reading coordinates for %s', name)
            _read_plot3d_coords(zone, stream, shape[i], blanking, planes,
                                logger)
    return domain


def read_plot3d_shape(grid_file, multiblock=True, dim=3, binary=True,
                      big_endian=False, unformatted=True, logger=None):
    """
    Returns a list of zone dimensions from Plot3D `grid_file`.

    grid_file: string
        Grid filename.
    """
    logger = logger or NullLogger()

    mode = 'rb' if binary else 'r'
    with open(grid_file, mode) as inp:
        logger.info('reading grid file %r', grid_file)
        stream = Stream(inp, binary, big_endian, True, False,
                        unformatted, False)
        return _read_plot3d_shape(stream, multiblock, dim, logger)


def _read_plot3d_shape(stream, multiblock, dim, logger):
    """ Returns a list of zone dimensions from Plot3D `stream`. """
    if multiblock:
        # Read number of zones.
        nblocks = stream.read_int(full_record=True)
        if nblocks < 1 or nblocks > 1000:
            raise RuntimeError('bad nblocks %d' % nblocks)
    else:
        nblocks = 1
    logger.debug('    nblocks %d', nblocks)

    # Read zone dimensions.
    if stream.unformatted:
        reclen = stream.read_recordmark()
        expected = stream.reclen_ints(dim * nblocks)
        if reclen != expected:
            logger.warning('unexpected dimensions recordlength'
                           ' %d vs. %d', reclen, expected)
    shape = []
    for i in range(nblocks):
        imax, jmax, kmax = _read_plot3d_dims(stream, dim)
        if dim > 2:
            shape.append((imax, jmax, kmax))
            logger.debug('    block %d: %dx%dx%d', i+1, imax, jmax, kmax)
        else:
            shape.append((imax, jmax))
            logger.debug('    block %d: %dx%d', i+1, imax, jmax)

    if stream.unformatted:
        reclen2 = stream.read_recordmark()
        if reclen2 != reclen:
            logger.warning('mismatched dimensions recordlength'
                           ' %d vs. %d', reclen2, reclen)
    return shape


def _read_plot3d_dims(stream, dim, f_file=False):
    """
    Reads dimensions for a zone from given Plot3D stream. `dim` is the expected
    dimensionality.  Returns ``(imax, jmax, kmax)``, kmax == 0 for 2D.
    If `f_file`, returns ``(imax, jmax, kmax, nvars)``.
    """
    if dim == 3:
        if f_file:
            imax, jmax, kmax, nvars = stream.read_ints(4)
        else:
            imax, jmax, kmax = stream.read_ints(3)
        if imax < 1 or jmax < 1 or kmax < 1:
            raise ValueError('invalid dimensions: %dx%dx%d'
                             % (imax, jmax, kmax))
    elif dim == 2:
        if f_file:
            imax, jmax, nvars = stream.read_ints(3)
        else:
            imax, jmax = stream.read_ints(2)
        kmax = 0
        if imax < 1 or jmax < 1:
            raise ValueError("invalid dimensions: %dx%d" % (imax, jmax))
    else:
        raise ValueError('invalid dim parameter: %r' % dim)

    if f_file:
        if nvars < 1:
            raise ValueError("invalid nvars: %d" % nvars)
        return (imax, jmax, kmax, nvars)
    else:
        return (imax, jmax, kmax)


def _read_plot3d_coords(zone, stream, shape, blanking, planes, logger):
    """ Reads coordinates (& blanking) from given Plot3D stream. """
    if blanking:
        raise NotImplementedError('blanking not supported yet')

    if planes:
        raise NotImplementedError('planar format not supported yet')

    dim = len(shape)

    if stream.unformatted:
        if dim > 2:
            total = 3 * shape[0] * shape[1] * shape[2]
        else:
            total = 2 * shape[0] * shape[1]
        reclen = stream.read_recordmark()
        expected = stream.reclen_floats(total)
        if reclen != expected:
            logger.warning('unexpected coords recordlength'
                           ' %d vs. %d', reclen, expected)

    zone.grid_coordinates.x = stream.read_floats(shape, order='Fortran')
    logger.debug('    x min %g, max %g',
                 zone.grid_coordinates.x.min(), zone.grid_coordinates.x.max())

    zone.grid_coordinates.y = stream.read_floats(shape, order='Fortran')
    logger.debug('    y min %g, max %g',
                 zone.grid_coordinates.y.min(), zone.grid_coordinates.y.max())

    if dim > 2:
        zone.grid_coordinates.z = stream.read_floats(shape, order='Fortran')
        logger.debug('    z min %g, max %g',
                     zone.grid_coordinates.z.min(), zone.grid_coordinates.z.max())

    if stream.unformatted:
        reclen2 = stream.read_recordmark()
        if reclen2 != reclen:
            logger.warning('mismatched coords recordlength %d vs. %d',
                           reclen2, reclen)


def _read_plot3d_qscalars(zone, stream, logger):
    """ Reads Mach number, alpha, Reynolds number, and time. """
    mach, alpha, reynolds, time = stream.read_floats(4, full_record=True)
    logger.debug('    mach %g, alpha %g, reynolds %g, time %g',
                 mach, alpha, reynolds, time)
    zone.flow_solution.mach = mach
    zone.flow_solution.alpha = alpha
    zone.flow_solution.reynolds = reynolds
    zone.flow_solution.time = time


def _read_plot3d_qvars(zone, stream, planes, logger):
    """ Reads 'density', 'momentum' and 'energy_stagnation_density'. """
    if planes:
        raise NotImplementedError('planar format not supported yet')

    shape = zone.shape
    dim = len(shape)

    if stream.unformatted: 
        if dim > 2:
            imax, jmax, kmax = shape
            total = 5 * imax * jmax * kmax
        else:
            imax, jmax = shape
            total = 4 * imax * jmax
        reclen = stream.read_recordmark()
        expected = stream.reclen_floats(total)
        if reclen != expected:
            logger.warning('unexpected Q variables recordlength'
                           ' %d vs. %d', reclen, expected)
    name = 'density'
    arr = stream.read_floats(shape, order='Fortran')
    logger.debug('    %s min %g, max %g', name, arr.min(), arr.max())
    zone.flow_solution.add_array(name, arr)

    vec = Vector()

    vec.x = stream.read_floats(shape, order='Fortran')
    logger.debug('    momentum.x min %g, max %g', vec.x.min(), vec.x.max())

    vec.y = stream.read_floats(shape, order='Fortran')
    logger.debug('    momentum.y min %g, max %g', vec.y.min(), vec.y.max())

    if dim > 2:
        vec.z = stream.read_floats(shape, order='Fortran')
        logger.debug('    momentum.z min %g, max %g', vec.z.min(), vec.z.max())

    zone.flow_solution.add_vector('momentum', vec)

    name = 'energy_stagnation_density'
    arr = stream.read_floats(shape, order='Fortran')
    logger.debug('    %s min %g, max %g', name, arr.min(), arr.max())
    zone.flow_solution.add_array(name, arr)

    if stream.unformatted:
        reclen2 = stream.read_recordmark()
        if reclen2 != reclen:
            logger.warning('mismatched Q variables recordlength'
                           ' %d vs. %d', reclen2, reclen)


def _read_plot3d_fvars(zone, stream, dim, nvars, varnames, planes, logger):
    """ Reads 'function' variables. """
    if planes:
        raise NotImplementedError('planar format not supported yet')

    shape = zone.shape
    dim = len(shape)

    if stream.unformatted: 
        if dim > 2:
            imax, jmax, kmax = shape
            total = nvars * imax * jmax * kmax
        else:
            imax, jmax = shape
            total = nvars * imax * jmax
        reclen = stream.read_recordmark()
        expected = stream.reclen_floats(total)
        if reclen != expected:
            logger.warning('unexpected F variables recordlength'
                           ' %d vs. %d', reclen, expected)
    for i in range(nvars):
        if varnames and i < len(varnames):
            name = varnames[i]
        else:
            name = 'f_%d' % (i+1)
        arr = stream.read_floats(shape, order='Fortran')
        zone.flow_solution.add_array(name, arr)
        logger.debug('    %s min %g, max %g', name, arr.min(), arr.max())

    if stream.unformatted:
        reclen2 = stream.read_recordmark()
        if reclen2 != reclen:
            logger.warning('mismatched F variables recordlength'
                           ' %d vs. %d', reclen2, reclen)


def write_plot3d_q(domain, grid_file, q_file, planes=False, binary=True,
                   big_endian=False, single_precision=True, unformatted=True,
                   logger=None):
    """
    Writes `domain` to `grid_file` and `q_file` in Plot3D format.
    Requires 'density', 'momentum', and 'energy_stagnation_density' variables
    as well as 'mach', 'alpha', 'reynolds', and 'time' scalars.
    Ghost data is not written.

    domain: :class:`DomainObj` or :class:`Zone`
        The domain or zone to be written.

    grid_file: string
        Grid filename.

    q_file: string
        Q data filename.
    """
    logger = logger or NullLogger()

    if isinstance(domain, DomainObj):
        writing_domain = True
        zones = domain.zones
    elif isinstance(domain, Zone):
        writing_domain = False
        zones = [domain]
    else:
        raise TypeError("'domain' argument must be a DomainObj or Zone")

    # Verify we have the needed data.
    for zone in zones:
        flow = zone.flow_solution
        missing = []
        for name in ('mach', 'alpha', 'reynolds', 'time',
                     'density', 'momentum', 'energy_stagnation_density'):
            if not hasattr(flow, name):
                missing.append(name)
        if missing:
            if writing_domain:
                name = domain.zone_name(zone)
            else:
                name = ''
            raise AttributeError('zone %s flow_solution is missing %s'
                                 % (name, missing))
    # Write grid file.
    write_plot3d_grid(domain, grid_file, planes, binary, big_endian,
                      single_precision, unformatted, logger)
    # Write Q file.
    mode = 'wb' if binary else 'w'
    with open(q_file, mode) as out:
        logger.info('writing Q file %r', q_file)
        stream = Stream(out, binary, big_endian, single_precision, False,
                        unformatted, False)
        if len(zones) > 1:
            # Write number of zones.
            stream.write_int(len(zones), full_record=True)

        # Write zone dimensions.
        _write_plot3d_dims(domain, stream, logger)

        # Write zone scalars and variables.
        varnames = ('density', 'momentum', 'energy_stagnation_density')
        for zone in zones:
            if writing_domain:
                name = domain.zone_name(zone)
            else:
                name = 'zone'
            logger.debug('writing data for %s', name)
            _write_plot3d_qscalars(zone, stream, logger)
            _write_plot3d_vars(zone, stream, varnames, planes, logger)


def write_plot3d_f(domain, grid_file, f_file, varnames=None, planes=False,
                   binary=True, big_endian=False, single_precision=True,
                   unformatted=True, logger=None):
    """
    Writes `domain` to `grid_file` and `f_file` in Plot3D format.
    If `varnames` is None, then all arrays and then all vectors are written.
    Ghost data is not written.

    domain: :class:`DomainObj` or :class:`Zone`
        The domain or zone to be written.

    grid_file: string
        Grid filename.

    f_file: string
        Function data filename.
    """
    logger = logger or NullLogger()

    if isinstance(domain, DomainObj):
        writing_domain = True
        zones = domain.zones
    elif isinstance(domain, Zone):
        writing_domain = False
        zones = [domain]
    else:
        raise TypeError("'domain' argument must be a DomainObj or Zone")

    if varnames is None:
        flow = zones[0].flow_solution
        varnames = [flow.name_of_obj(obj) for obj in flow.arrays]
        varnames.extend([flow.name_of_obj(obj) for obj in flow.vectors])

    # Verify we have the needed data.
    for zone in zones:
        flow = zone.flow_solution
        missing = []
        for name in varnames:
            if not hasattr(flow, name):
                missing.append(name)
        if missing:
            if writing_domain:
                name = domain.zone_name(zone)
            else:
                name = ''
            raise AttributeError('zone %s flow_solution is missing %s'
                                 % (name, missing))
    # Write grid file.
    write_plot3d_grid(domain, grid_file, planes, binary, big_endian,
                      single_precision, unformatted, logger)
    # Write F file.
    mode = 'wb' if binary else 'w'
    with open(f_file, mode) as out:
        logger.info('writing F file %r', f_file)
        stream = Stream(out, binary, big_endian, single_precision, False,
                        unformatted, False)
        if len(zones) > 1:
            # Write number of zones.
            stream.write_int(len(zones), full_record=True)

        # Write zone dimensions.
        _write_plot3d_dims(domain, stream, logger, varnames)

        # Write zone variables.
        for zone in zones:
            if writing_domain:
                name = domain.zone_name(zone)
            else:
                name = 'zone'
            logger.debug('writing data for %s', name)
            _write_plot3d_vars(zone, stream, varnames, planes, logger)


def write_plot3d_grid(domain, grid_file, planes=False, binary=True,
                      big_endian=False, single_precision=True,
                      unformatted=True, logger=None):
    """
    Writes `domain` to `grid_file` in Plot3D format.
    Ghost data is not written.

    domain: :class:`DomainObj` or :class:`Zone`
        The domain or zone to be written.

    grid_file: string
        Grid filename.
    """
    logger = logger or NullLogger()

    if isinstance(domain, DomainObj):
        writing_domain = True
        zones = domain.zones
    elif isinstance(domain, Zone):
        writing_domain = False
        zones = [domain]
    else:
        raise TypeError("'domain' argument must be a DomainObj or Zone")

    mode = 'wb' if binary else 'w'
    with open(grid_file, mode) as out:
        logger.info('writing grid file %r', grid_file)
        stream = Stream(out, binary, big_endian, single_precision, False,
                        unformatted, False)
        if len(zones) > 1:
            # Write number of zones.
            stream.write_int(len(zones), full_record=True)

        # Write zone dimensions.
        _write_plot3d_dims(domain, stream, logger)

        # Write zone coordinates.
        for zone in zones:
            if writing_domain:
                name = domain.zone_name(zone)
            else:
                name = 'zone'
            logger.debug('writing coords for %s', name)
            _write_plot3d_coords(zone, stream, planes, logger)


def _write_plot3d_dims(domain, stream, logger, varnames=None):
    """ Write dimensions of each zone to Plot3D stream. """
    if isinstance(domain, DomainObj):
        writing_domain = True
        zones = domain.zones
    else:
        writing_domain = False
        zones = [domain]

    # Zones must be all 2D or all 3D.
    dim = 0
    for zone in zones:
        if writing_domain:
            name = domain.zone_name(zone)
        else:
            name = 'zone'
        shape = zone.shape
        if not dim:
            dim = len(shape)
        elif dim != len(shape):
            raise ValueError('zone %r is not %dD' % (name, dim))
        if dim > 2:
            imax, jmax, kmax = shape
            logger.debug('    %s: %dx%dx%d', name, imax, jmax, kmax)
        elif dim > 1:
            imax, jmax = shape
            logger.debug('    %s: %dx%d', name, imax, jmax)
        else:
            raise RuntimeError('domain must be at least 2D')

    nvars = 0
    if varnames:
        flow = zones[0].flow_solution
        for name in varnames:
            obj = getattr(flow, name)
            nvars += dim if isinstance(obj, Vector) else 1

    if stream.unformatted:
        count = len(shape)
        if nvars:
            count += 1
        reclen = len(zones) * stream.reclen_ints(count)
        stream.write_recordmark(reclen)

    for zone in zones:
        shape = list(zone.shape)
        if nvars:
            shape.append(nvars)
        stream.write_ints(numpy.array(shape, dtype=numpy.int32))

    if stream.unformatted:
        stream.write_recordmark(reclen)


def _write_plot3d_coords(zone, stream, planes, logger):
    """ Write coordinates (& blanking) to Plot3D stream. """
    if hasattr(zone.grid_coordinates, 'iblank'):
        raise NotImplementedError('blanking not supported yet')

    if planes:
        raise NotImplementedError('planar format not supported yet')

    shape = zone.shape
    dim = len(shape)

    if stream.unformatted:
        if dim > 2:
            imax, jmax, kmax = shape
            total = 3 * imax * jmax * kmax
        else:
            imax, jmax = shape
            total = 2 * imax * jmax
        reclen = stream.reclen_floats(total)
        stream.write_recordmark(reclen)

    logger.debug('    x min %g, max %g',
                 zone.grid_coordinates.x.min(), zone.grid_coordinates.x.max())
    _write_array(zone.grid_coordinates.x, zone.grid_coordinates.ghosts, stream)

    logger.debug('    y min %g, max %g',
                 zone.grid_coordinates.y.min(), zone.grid_coordinates.y.max())
    _write_array(zone.grid_coordinates.y, zone.grid_coordinates.ghosts, stream)

    if dim > 2:
        logger.debug('    z min %g, max %g',
                     zone.grid_coordinates.z.min(), zone.grid_coordinates.z.max())
        _write_array(zone.grid_coordinates.z, zone.grid_coordinates.ghosts,
                     stream)

    if stream.unformatted:
        stream.write_recordmark(reclen)


def _write_plot3d_qscalars(zone, stream, logger):
    """ Writes Mach number, alpha, Reynolds number, and time. """
    flow = zone.flow_solution
    logger.debug('    mach %g, alpha %g, reynolds %g, time %g',
                 flow.mach, flow.alpha, flow.reynolds, flow.time)
    scalars = (flow.mach, flow.alpha, flow.reynolds, flow.time)
    stream.write_floats(numpy.array(scalars, dtype=numpy.float32),
                        full_record=True)


def _write_plot3d_vars(zone, stream, varnames, planes, logger):
    """ Writes 'function' variables. """
    if planes:
        raise NotImplementedError('planar format not supported yet')

    shape = zone.shape
    dim = len(shape)
    flow = zone.flow_solution
    nvars = 0
    for name in varnames:
        obj = getattr(flow, name)
        nvars += dim if isinstance(obj, Vector) else 1
    logger.debug('    nvars %d', nvars)

    if stream.unformatted:
        if dim > 2:
            imax, jmax, kmax = shape
            total = nvars * imax * jmax * kmax
        else:
            imax, jmax = shape
            total = nvars * imax * jmax
        reclen = stream.reclen_floats(total)
        stream.write_recordmark(reclen)

    for name in varnames:
        obj = getattr(flow, name)
        if isinstance(obj, Vector):
            arr = obj.x
            logger.debug('    %s.x min %g, max %g', name, arr.min(), arr.max())
            _write_array(arr, flow.ghosts, stream)

            arr = obj.y
            logger.debug('    %s.y min %g, max %g', name, arr.min(), arr.max())
            _write_array(arr, flow.ghosts, stream)

            if dim > 2:
                arr = obj.z
                logger.debug('    %s.z min %g, max %g', name, arr.min(), arr.max())
                _write_array(arr, flow.ghosts, stream)
        else:
            arr = obj
            logger.debug('    %s min %g, max %g', name, arr.min(), arr.max())
            _write_array(arr, flow.ghosts, stream)

    if stream.unformatted:
        stream.write_recordmark(reclen)


def _write_array(arr, ghosts, stream):
    """ Write `arr` to `stream`, adjusting for `ghosts`. """
    shape = arr.shape
    if len(shape) > 2:
        imin = ghosts[0]
        jmin = ghosts[2]
        kmin = ghosts[4]
        imax, jmax, kmax = shape
        imax -= ghosts[1]
        jmax -= ghosts[3]
        kmax -= ghosts[5]
        stream.write_floats(arr[imin:imax, jmin:jmax, kmin:kmax], order='Fortran')
    else:
        imin = ghosts[0]
        jmin = ghosts[2]
        imax, jmax = shape
        imax -= ghosts[1]
        jmax -= ghosts[3]
        stream.write_floats(arr[imin:imax, jmin:jmax], order='Fortran')

