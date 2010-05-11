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


def read_plot3d_f(grid_file, f_file, varnames=None, multiblock=True, dim=3,
                  blanking=False, planes=False, binary=True, big_endian=False,
                  single_precision=True, unformatted=True, logger=None):
    """
    Returns a :class:`DomainObj` initialized from Plot3D `grid_file` and
    `f_file`.
    """
    logger = logger or NullLogger()

    domain = read_plot3d_grid(grid_file, multiblock, dim, blanking, planes,
                              binary, big_endian, single_precision,
                              unformatted, logger)

    mode = 'rb' if binary else 'r'
    with open(f_file, mode) as inp:
        logger.info("reading F file '%s'", f_file)
        stream = Stream(inp, binary, big_endian, single_precision, False,
                        unformatted, False)
        if multiblock:
            # Read number of zones.
            nblocks = stream.read_int(full_record=True)
        else:
            nblocks = 1
        if nblocks != len(domain.zones):
            raise RuntimeError('F zones %d != Grid zones %d' \
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
            imax, jmax, kmax, nvars = _read_plot3d_dims(zone, stream, dim, True)
            if dim > 2:
                logger.debug('    %s: %dx%dx%d %d',
                             name, imax, jmax, kmax, nvars)
                zone_i, zone_j, zone_k = zone.shape
                if imax != zone_i or jmax != zone_j or kmax != zone_k:
                    raise RuntimeError('%s: F %dx%dx%d != Grid %dx%dx%d' \
                                       % (name, imax, jmax, kmax,
                                          zone_i, zone_j, zone_k))
            else:
                logger.debug('    %s: %dx%d %d', name, imax, jmax, nvars)
                zone_i, zone_j = zone.shape
                if imax != zone_i or jmax != zone_j:
                    raise RuntimeError('%s: F %dx%d != Grid %dx%d' \
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


def _read_plot3d_dims(zone, stream, dim, f_file=False):
    """
    Reads dimensions from given Plot3D stream. `dim` is the expected
    dimensionality.  Returns ``(imax, jmax, kmax)``, kmax == 0 for 2D.
    If `f_file`, returns ``(imax, jmax, kmax, nvars)``.
    """
    if dim == 3:
        if f_file:
            imax, jmax, kmax, nvars = stream.read_ints(4)
        else:
            imax, jmax, kmax = stream.read_ints(3)
        if imax < 1 or jmax < 1 or kmax < 1:
            raise ValueError("invalid dimensions: %dx%dx%d" \
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
        raise ValueError("invalid dim parameter: '%s'" % dim)

    zone._imax, zone._jmax, zone._kmax = imax, jmax, kmax

    if f_file:
        if nvars < 1:
            raise ValueError("invalid nvars: %d" % nvars)
        return (imax, jmax, kmax, nvars)
    else:
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
    name = 'density'
    arr = stream.read_floats(shape, order='Fortran')
    logger.debug('    %s min %g, max %g', name, arr.min(), arr.max())
    zone.add_array(name, arr)

    vec = Vector()

    vec.x = stream.read_floats(shape, order='Fortran')
    logger.debug('    momentum.x min %g, max %g', vec.x.min(), vec.x.max())

    vec.y = stream.read_floats(shape, order='Fortran')
    logger.debug('    momentum.y min %g, max %g', vec.y.min(), vec.y.max())

    if dim > 2:
        vec.z = stream.read_floats(shape, order='Fortran')
        logger.debug('    momentum.z min %g, max %g', vec.z.min(), vec.z.max())

    zone.add_vector('momentum', vec)

    name = 'energy_stagnation_density'
    arr = stream.read_floats(shape, order='Fortran')
    logger.debug('    %s min %g, max %g', name, arr.min(), arr.max())
    zone.add_array(name, arr)

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
        zone.add_array(name, arr)
        logger.debug('    %s min %g, max %g', name, arr.min(), arr.max())

    if stream.unformatted:
        reclen2 = stream.read_recordmark()
        if reclen2 != reclen:
            logger.warning('mismatched F variables recordlength'
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
            raise AttributeError('zone %s is missing %s' \
                                 % (domain.zone_name(zone), missing))
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
        varnames = ('density', 'momentum', 'energy_stagnation_density')
        for zone in domain.zones:
            name = domain.zone_name(zone)
            logger.debug('writing data for %s', name)
            _write_plot3d_qscalars(zone, stream, logger)
            _write_plot3d_vars(zone, stream, varnames, planes, logger)


def write_plot3d_f(domain, grid_file, f_file, varnames=None, planes=False,
                   binary=True, big_endian=False, single_precision=True,
                   unformatted=True, logger=None):
    """ Writes `domain` to `grid_file` and `f_file` in Plot3D format. """
    logger = logger or NullLogger()

    # Verify we have the needed data.
    if varnames is None:
        zone = domain.zones[0]
        varnames = [zone.name_of_obj(obj) for obj in zone.arrays]
        varnames.extend([zone.name_of_obj(obj) for obj in zone.vectors])
    for zone in domain.zones:
        missing = []
        for name in varnames:
            if not hasattr(zone, name):
                missing.append(name)
        if missing:
            raise AttributeError('zone %s is missing %s' \
                                 % (domain.zone_name(zone), missing))
    # Write grid file.
    write_plot3d_grid(domain, grid_file, planes, binary, big_endian,
                      single_precision, unformatted, logger)
    # Write F file.
    mode = 'wb' if binary else 'w'
    with open(f_file, mode) as out:
        logger.info("writing F file '%s'", f_file)
        stream = Stream(out, binary, big_endian, single_precision, False,
                        unformatted, False)
        if len(domain.zones) > 1:
            # Write number of zones.
            stream.write_int(len(domain.zones), full_record=True)

        # Write zone dimensions.
        _write_plot3d_dims(domain, stream, logger, varnames)

        # Write zone variables.
        for zone in domain.zones:
            name = domain.zone_name(zone)
            logger.debug('writing data for %s', name)
            _write_plot3d_vars(zone, stream, varnames, planes, logger)


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


def _write_plot3d_dims(domain, stream, logger, varnames=None):
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

    nvars = 0
    if varnames:
        zone = domain.zones[0]
        for name in varnames:
            obj = getattr(zone, name)
            nvars += dim if isinstance(obj, Vector) else 1

    if stream.unformatted:
        count = len(shape)
        if nvars:
            count += 1
        reclen = len(domain.zones) * stream.reclen_ints(count)
        stream.write_recordmark(reclen)

    for zone in domain.zones:
        shape = list(zone.coords.x.shape)
        if nvars:
            shape.append(nvars)
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


def _write_plot3d_vars(zone, stream, varnames, planes, logger):
    """ Writes 'function' variables. """
    if planes:
        raise NotImplementedError('planar format not supported yet')

    shape = zone.shape
    dim = len(shape)
    nvars = 0
    for name in varnames:
        obj = getattr(zone, name)
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
        obj = getattr(zone, name)
        if isinstance(obj, Vector):
            arr = obj.x
            logger.debug('    %s.x min %g, max %g', name, arr.min(), arr.max())
            stream.write_floats(arr, order='Fortran')

            arr = obj.y
            logger.debug('    %s.y min %g, max %g', name, arr.min(), arr.max())
            stream.write_floats(arr, order='Fortran')

            if dim > 2:
                arr = obj.z
                logger.debug('    %s.z min %g, max %g', name, arr.min(), arr.max())
                stream.write_floats(arr, order='Fortran')
        else:
            arr = obj
            logger.debug('    %s min %g, max %g', name, arr.min(), arr.max())
            stream.write_floats(arr, order='Fortran')

    if stream.unformatted:
        stream.write_recordmark(reclen)

