"""
Reader for OVERFLOW test data.
"""

from openmdao.lib.datatypes.domain import Vector, read_plot3d_grid
from openmdao.util.log import NullLogger
from openmdao.util.stream import Stream


def read_q(grid_file, q_file, multiblock=True, blanking=False, logger=None):
    """
    Read grid and solution files.
    Returns a :class:`DomainObj` initialized from `grid_file` and `q_file`.

    grid_file: string
        Grid filename.

    q_file: string
        Q data filename.
    """
    logger = logger or NullLogger()

    domain = read_plot3d_grid(grid_file, multiblock, dim=3, blanking=blanking,
                              planes=False, binary=True, big_endian=False,
                              single_precision=False, unformatted=True,
                              logger=logger)

    with open(q_file, 'rb') as inp:
        logger.info("reading Q file '%s'", q_file)
        stream = Stream(inp, binary=True, big_endian=False,
                        single_precision=False, integer_8=False,
                        unformatted=True, recordmark_8=False)
        if multiblock:
            # Read number of zones.
            nblocks = stream.read_int(full_record=True)
        else:
            nblocks = 1
        if nblocks != len(domain.zones):
            raise RuntimeError('Q zones %d != Grid zones %d' \
                               % (nblocks, len(domain.zones)))

        # Read zone dimensions, nq, nqc.
        reclen = stream.read_recordmark()
        expected = stream.reclen_ints(3*nblocks + 2)
        if reclen != expected:
            logger.warning('unexpected dimensions recordlength'
                           ' %d vs. %d', reclen, expected)

        for zone in domain.zones:
            name = domain.zone_name(zone)
            imax, jmax, kmax = stream.read_ints(3)
            if imax < 1 or jmax < 1 or kmax < 1:
                raise ValueError("invalid dimensions: %dx%dx%d" \
                                 % (imax, jmax, kmax))
            logger.debug('    %s: %dx%dx%d', name, imax, jmax, kmax)
            zone_i, zone_j, zone_k = zone.shape
            if imax != zone_i or jmax != zone_j or kmax != zone_k:
                raise RuntimeError('%s: Q %dx%dx%d != Grid %dx%dx%d' \
                                   % (name, imax, jmax, kmax,
                                      zone_i, zone_j, zone_k))

        nq, nqc = stream.read_ints(2)
        logger.debug('    nq %d, nqc %d', nq, nqc)

        reclen2 = stream.read_recordmark()
        if reclen2 != reclen:
            logger.warning('mismatched dimensions recordlength'
                           ' %d vs. %d', reclen2, reclen)

        # Read zone scalars and variables.
        for zone in domain.zones:
            name = domain.zone_name(zone)
            logger.debug('reading data for %s', name)
            _read_scalars(zone, nqc, stream, logger)
            _read_vars(zone, nq, nqc, stream, logger)

    return domain


def _read_scalars(zone, nqc, stream, logger):
    """ Reads scalars for `zone`. """
    reclen = stream.read_recordmark()
    expected = stream.reclen_floats(7) \
             + stream.reclen_ints(1) \
             + stream.reclen_floats(3) \
             + stream.reclen_floats(max(2, nqc)) \
             + stream.reclen_floats(3)
    if reclen != expected:
        logger.warning('unexpected scalars recordlength'
                       ' %d vs. %d', reclen, expected)

    refmach, alpha, rey, time, gaminf, beta, tinf = stream.read_floats(7)
    igam = stream.read_int()
    htinf, ht1, ht2 = stream.read_floats(3)
    rgas = stream.read_floats(max(2, nqc)),
    fsmach, tvref, dtvref = stream.read_floats(3)

    logger.debug('    refmach %g, fsmach %r, alpha %g, beta %g, rey %g, time %g',
                 refmach, fsmach, alpha, beta, rey, time)
    logger.debug('    gaminf %g, igam %d, tinf %g, htinf %g, ht1 %g, ht2 %g',
                 gaminf, igam, tinf, htinf, ht1, ht2)
    logger.debug('    rgas %s', rgas)
    logger.debug('    tvref %g, dtvref %d', tvref, dtvref)

    flow = zone.flow_solution
    flow.refmach = refmach
    flow.fsmach = fsmach
    flow.alpha = alpha
    flow.beta = beta
    flow.reynolds = rey
    flow.time = time
    flow.gaminf = gaminf
    flow.igam = igam
    flow.tinf = tinf
    flow.htinf = htinf
    flow.ht1 = ht1
    flow.ht2 = ht2
    flow.rgas = rgas
    flow.tvref = tvref
    flow.dtvref = dtvref

    reclen2 = stream.read_recordmark()
    if reclen2 != reclen:
        logger.warning('mismatched dimensions recordlength'
                       ' %d vs. %d', reclen2, reclen)


def _read_vars(zone, nq, nqc, stream, logger):
    """ Reads field variables for `zone` """
    shape = zone.shape
    imax, jmax, kmax = shape
    reclen = stream.read_recordmark()
    expected = stream.reclen_floats(nq * imax * jmax * kmax)
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

    vec.z = stream.read_floats(shape, order='Fortran')
    logger.debug('    momentum.z min %g, max %g', vec.z.min(), vec.z.max())

    zone.flow_solution.add_vector('momentum', vec)

    name = 'energy_stagnation_density'
    arr = stream.read_floats(shape, order='Fortran')
    logger.debug('    %s min %g, max %g', name, arr.min(), arr.max())
    zone.flow_solution.add_array(name, arr)

    name = 'gamma'
    arr = stream.read_floats(shape, order='Fortran')
    logger.debug('    %s min %g, max %g', name, arr.min(), arr.max())
    zone.flow_solution.add_array(name, arr)

    for i in range(nqc):
        name = 'species_%d_density' % (i+1)
        arr = stream.read_floats(shape, order='Fortran')
        logger.debug('    %s min %g, max %g', name, arr.min(), arr.max())
        zone.flow_solution.add_array(name, arr)

    for i in range(nq - (6 + nqc)):
        name = 'turbulence_%d' % (i+1)
        arr = stream.read_floats(shape, order='Fortran')
        logger.debug('    %s min %g, max %g', name, arr.min(), arr.max())
        zone.flow_solution.add_array(name, arr)

    if stream.unformatted:
        reclen2 = stream.read_recordmark()
        if reclen2 != reclen:
            logger.warning('mismatched Q variables recordlength'
                           ' %d vs. %d', reclen2, reclen)

