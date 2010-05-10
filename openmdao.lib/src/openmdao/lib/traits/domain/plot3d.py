"""
Functions to read and write a :class:`DomainObj` in Plot3D format.

- If `multiblock`, then the file is assumed to have a multiblock header.
- `dim` specifies the expected dimensionality of the blocks.
- If `blanking`, then blanking data is expected.
- If `planes`, then the data is expected in planar, not whole, format.
- If `binary`, the data is in binary, not text, form.
- If `big_endian`, the data bytes are in 'big-endian' order. \
  Only meaningful if `binary`.
- If `single_precision`, floating-point data is 32 bits, not 64. \
  Only meaningful if `binary`.
- If `unformatted`, the data is surrounded by Fortran record length \
  markers.  Only meaningful if `binary`.

Default argument values are set for a typical 3D multiblock single-precision
Fortran unformatted file.
"""

import os.path
import numpy

from openmdao.util.log import NullLogger
from openmdao.util.stream import Stream

from openmdao.lib.traits.domain.domain import DomainObj
from openmdao.lib.traits.domain.zone import Zone
from openmdao.lib.traits.domain.vector import Vector


def read_plot3d_q(grid_file, q_file, multiblock=True, dim=3, blanking=False,
                  planes=False, binary=True, big_endian=False,
                  single_precision=True, unformatted=True, logger=None):
    """
    Returns a :class:`DomainObj` initialized from Plot3D `grid_file` and
    `q_file`.
    """
    logger = logger or NullLogger()

    domain = read_plot3d_grid(grid_file, multiblock, dim, blanking, planes,
                              binary, big_endian, single_precision,
                              unformatted, logger)

    mode = 'rb' if binary else 'r'
    with open(q_file, mode) as inp:
        logger.info("reading Q file '%s'", q_file)
        stream = Stream(inp, binary, big_endian, single_precision, False,
                        unformatted, False)
        if multiblock:
            # Read number of zones.
            nblocks = stream.read_int(full_record=True)
        else:
            nblocks = 1
        if nblocks != len(domain.zones):
            raise RuntimeError('Q zones %d != Grid zones %d' \
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
            imax, jmax, kmax = _read_plot3d_dims(zone, stream, dim)
            if dim > 2:
                logger.debug('    %s: %dx%dx%d', name, imax, jmax, kmax)
                zone_i, zone_j, zone_k = zone.shape
                if imax != zone_i or jmax != zone_j or kmax != zone_k:
                    raise RuntimeError('%s: Q %dx%dx%d != Grid %dx%dx%d' \
                                       % (name, imax, jmax, kmax,
                                          zone_i, zone_j, zone_k))
            else:
                logger.debug('    %s: %dx%d', name, imax, jmax)
                zone_i, zone_j = zone.shape
                if imax != zone_i or jmax != zone_j:
                    raise RuntimeError('%s: Q %dx%d != Grid %dx%d' \
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


def read_plot3d_grid(grid_file, multiblock=True, dim=3, blanking=False,
                     planes=False, binary=True, big_endian=False,
                     single_precision=True, unformatted=True, logger=None):
    """
    Returns a :class:`DomainObj` initialized from Plot3D `grid_file`.
    """
    logger = logger or NullLogger()
    domain = DomainObj()

    mode = 'rb' if binary else 'r'
    with open(grid_file, mode) as inp:
        logger.info("reading grid file '%s'", grid_file)
        stream = Stream(inp, binary, big_endian, single_precision, False,
                        unformatted, False)
        if multiblock:
            # Read number of zones.
            nblocks = stream.read_int(full_record=True)
            if nblocks < 1 or nblocks > 1000:
                raise RuntimeError('bad nblocks %d' % nblocks)
        else:
            nblocks = 1
        logger.debug('    nblocks %d', nblocks)

        # Read zone dimensions.
        if unformatted:
            reclen = stream.read_recordmark()
            expected = stream.reclen_ints(dim * nblocks)
            if reclen != expected:
                logger.warning('unexpected dimensions recordlength'
                               ' %d vs. %d', reclen, expected)
        for i in range(nblocks):
            zone = domain.add_zone('', Zone())
            name = domain.zone_name(zone)
            imax, jmax, kmax = _read_plot3d_dims(zone, stream, dim)
            if dim > 2:
                logger.debug('    %s: %dx%dx%d', name, imax, jmax, kmax)
            else:
                logger.debug('    %s: %dx%d', name, imax, jmax)

        if unformatted:
            reclen2 = stream.read_recordmark()
            if reclen2 != reclen:
                logger.warning('mismatched dimensions recordlength'
                               ' %d vs. %d', reclen2, reclen)
        # Read zone coordinates.
        for zone in domain.zones:
            name = domain.zone_name(zone)
            logger.debug('reading coordinates for %s', name)
            _read_plot3d_coords(zone, stream, dim, blanking, planes, logger)

    return domain


def _read_plot3d_dims(zone, stream, dim):
    """
    Reads dimensions from given Plot3D stream. `dim` is the expected
    dimensionality.  Returns ``(imax, jmax, kmax)``, kmax == 0 for 2D.
    """
    if dim == 3:
        imax, jmax, kmax = stream.read_ints(3)
        if imax < 1 or jmax < 1 or kmax < 1:
            raise ValueError("invalid dimensions: %dx%dx%d" \
                             % (imax, jmax, kmax))
    elif dim == 2:
        imax, jmax = stream.read_ints(2)
        kmax = 0
        if imax < 1 or jmax < 1:
            raise ValueError("invalid dimensions: %dx%d" % (imax, jmax))
    else:
        raise ValueError("invalid dim parameter: '%s'" % dim)

    zone._imax, zone._jmax, zone._kmax = imax, jmax, kmax
    return (imax, jmax, kmax)


def _read_plot3d_coords(zone, stream, dim, blanking, planes, logger):
    """ Reads coordinates (& blanking) from given Plot3D stream. """
    if blanking:
        raise NotImplementedError('blanking not supported yet')

    if planes:
        raise NotImplementedError('planar format not supported yet')

    if dim > 2:
        shape = (zone._imax, zone._jmax, zone._kmax)
    else:
        shape = (zone._imax, zone._jmax)

    if stream.unformatted:
        if dim > 2:
            total = 3 * zone._imax * zone._jmax * zone._kmax
        else:
            total = 2 * zone._imax * zone._jmax
        reclen = stream.read_recordmark()
        expected = stream.reclen_floats(total)
        if reclen != expected:
            logger.warning('unexpected coords recordlength'
                           ' %d vs. %d', reclen, expected)

    zone.coords.x = stream.read_floats(shape, order='Fortran')
    logger.debug('    x min %g, max %g',
                 zone.coords.x.min(), zone.coords.x.max())

    zone.coords.y = stream.read_floats(shape, order='Fortran')
    logger.debug('    y min %g, max %g',
                 zone.coords.y.min(), zone.coords.y.max())

    if dim > 2:
        zone.coords.z = stream.read_floats(shape, order='Fortran')
        logger.debug('    z min %g, max %g',
                     zone.coords.z.min(), zone.coords.z.max())

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
    zone.mach = mach
    zone.alpha = alpha
    zone.reynolds = reynolds
    zone.time = time


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

    zone.density = stream.read_floats(shape, order='Fortran')
    logger.debug('    density min %g, max %g',
                 zone.density.min(), zone.density.max())

    momentum = zone.add_vector('momentum', Vector())

    momentum.x = stream.read_floats(shape, order='Fortran')
    logger.debug('    momentum.x min %g, max %g',
                 momentum.x.min(), momentum.x.max())

    momentum.y = stream.read_floats(shape, order='Fortran')
    logger.debug('    momentum.y min %g, max %g',
                 momentum.y.min(), momentum.y.max())
    if dim > 2:
        momentum.z = stream.read_floats(shape, order='Fortran')
        logger.debug('    momentum.z min %g, max %g',
                     momentum.z.min(), momentum.z.max())

    zone.energy_stagnation_density = stream.read_floats(shape, order='Fortran')
    logger.debug('    energy_stagnation_density min %g, max %g',
                 zone.energy_stagnation_density.min(),
                 zone.energy_stagnation_density.max())

    if stream.unformatted:
        reclen2 = stream.read_recordmark()
        if reclen2 != reclen:
            logger.warning('mismatched Q variables recordlength'
                           ' %d vs. %d', reclen2, reclen)


def write_plot3d_q(domain, grid_file, q_file, planes=False, binary=True,
                   big_endian=False, single_precision=True, unformatted=True,
                   logger=None):
    """ Writes `domain` to `grid_file` and `q_file` in Plot3D format. """
    logger = logger or NullLogger()

    # Verify we have the needed data.
    for zone in domain.zones:
        missing = []
        for attr in ('mach', 'alpha', 'reynolds', 'time',
                     'density', 'momentum', 'energy_stagnation_density'):
            if not hasattr(zone, attr):
                missing.append(attr)
        if missing:
            name = domain.zone_name(zone)
            raise AttributeError("zone '%s' is missing %s", name, missing)

    # Write grid file.
    write_plot3d_grid(domain, grid_file, planes, binary, big_endian,
                      single_precision, unformatted, logger)

    # Write Q file.
    mode = 'wb' if binary else 'w'
    with open(q_file, mode) as out:
        logger.info("writing Q file '%s'", q_file)
        stream = Stream(out, binary, big_endian, single_precision, False,
                        unformatted, False)
        if len(domain.zones) > 1:
            # Write number of zones.
            stream.write_int(len(domain.zones), full_record=True)

        # Write zone dimensions.
        _write_plot3d_dims(domain, stream, logger)

        # Write zone scalars and variables.
        for zone in domain.zones:
            name = domain.zone_name(zone)
            logger.debug('writing data for %s', name)
            _write_plot3d_qscalars(zone, stream, logger)
            _write_plot3d_qvars(zone, stream, planes, logger)


def write_plot3d_grid(domain, grid_file, planes=False, binary=True,
                      big_endian=False, single_precision=True,
                      unformatted=True, logger=None):
    """ Writes `domain` to `grid_file` in Plot3D format. """
    logger = logger or NullLogger()

    mode = 'wb' if binary else 'w'
    with open(grid_file, mode) as out:
        logger.info("writing grid file '%s'", grid_file)
        stream = Stream(out, binary, big_endian, single_precision, False,
                        unformatted, False)
        if len(domain.zones) > 1:
            # Write number of zones.
            stream.write_int(len(domain.zones), full_record=True)

        # Write zone dimensions.
        _write_plot3d_dims(domain, stream, logger)

        # Write zone coordinates.
        for zone in domain.zones:
            name = domain.zone_name(zone)
            logger.debug('writing coords for %s', name)
            _write_plot3d_coords(zone, stream, planes, logger)


def _write_plot3d_dims(domain, stream, logger):
    """ Write dimensions of each zone to Plot3D stream. """
    # Zones must be all 2D or all 3D.
    dim = 0
    for zone in domain.zones:
        name = domain.zone_name(zone)
        shape = zone.shape
        if not dim:
            dim = len(shape)
        elif dim != len(shape):
            raise ValueError("zone '%s' is not %dD" % (name, dim))
        if dim > 2:
            imax, jmax, kmax = shape
            logger.debug('    %s: %dx%dx%d', name, imax, jmax, kmax)
        elif dim > 1:
            imax, jmax = shape
            logger.debug('    %s: %dx%d', name, imax, jmax)
        else:
            raise RuntimeError('domain must be at least 2D')

    if stream.unformatted:
        reclen = len(domain.zones) * stream.reclen_ints(dim)
        stream.write_recordmark(reclen)

    for zone in domain.zones:
        shape = zone.coords.x.shape
        stream.write_ints(numpy.array(shape, dtype=numpy.int32))

    if stream.unformatted:
        stream.write_recordmark(reclen)


def _write_plot3d_coords(zone, stream, planes, logger):
    """ Write coordinates (& blanking) to Plot3D stream. """
    if hasattr(zone.coords, 'iblank'):
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
                 zone.coords.x.min(), zone.coords.x.max())
    stream.write_floats(zone.coords.x, order='Fortran')

    logger.debug('    y min %g, max %g',
                 zone.coords.y.min(), zone.coords.y.max())
    stream.write_floats(zone.coords.y, order='Fortran')

    if dim > 2:
        logger.debug('    z min %g, max %g',
                     zone.coords.z.min(), zone.coords.z.max())
        stream.write_floats(zone.coords.z, order='Fortran')

    if stream.unformatted:
        stream.write_recordmark(reclen)


def _write_plot3d_qscalars(zone, stream, logger):
    """ Writes Mach number, alpha, Reynolds number, and time. """
    logger.debug('    mach %g, alpha %g, reynolds %g, time %g',
                 zone.mach, zone.alpha, zone.reynolds, zone.time)
    scalars = (zone.mach, zone.alpha, zone.reynolds, zone.time)
    stream.write_floats(numpy.array(scalars, dtype=numpy.float32),
                        full_record=True)


def _write_plot3d_qvars(zone, stream, planes, logger):
    """ Writes 'density', 'momentum' and 'energy_stagnation_density'. """
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
        reclen = stream.reclen_floats(total)
        stream.write_recordmark(reclen)

    logger.debug('    density min %g, max %g',
                 zone.density.min(), zone.density.max())
    stream.write_floats(zone.density, order='Fortran')

    logger.debug('    momentum.x min %g, max %g',
                 zone.momentum.x.min(), zone.momentum.x.max())
    stream.write_floats(zone.momentum.x, order='Fortran')

    logger.debug('    momentum.y min %g, max %g',
                 zone.momentum.y.min(), zone.momentum.y.max())
    stream.write_floats(zone.momentum.y, order='Fortran')

    if dim > 2:
        logger.debug('    momentum.z min %g, max %g',
                     zone.momentum.z.min(), zone.momentum.z.max())
        stream.write_floats(zone.momentum.z, order='Fortran')

    logger.debug('    energy_stagnation_density min %g, max %g',
                 zone.energy_stagnation_density.min(),
                 zone.energy_stagnation_density.max())
    stream.write_floats(zone.energy_stagnation_density, order='Fortran')

    if stream.unformatted:
        stream.write_recordmark(reclen)

