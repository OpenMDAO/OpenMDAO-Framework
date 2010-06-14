"""
Modified ADPAC component wrapper restart.py for surface_probe() testing.
"""

from openmdao.units.units import PhysicalQuantity, add_unit
from openmdao.util.stream import Stream

from openmdao.lib.traits.domain import Vector, read_plot3d_grid


def read(casename, logger):
    """ Return domain read from ADPAC .input, .mesh, and .restart files. """
    rgas  = 1716.3507
    diam  = 0.083333
    pref  = 759.0528
    gamma = 1.4
    tref  = 444.3192
    nbld  = 64

    try:
        PhysicalQuantity(0., 'slug')
    except ValueError:
        add_unit('slug', '14.5939*kg', 'Slug')

    # Read mesh.
    domain = read_plot3d_grid(casename+'.mesh', big_endian=True,
                              unformatted=False, logger=logger)

    # Set global reference state.
    domain.reference_state = {
        'ideal_gas_constant': PhysicalQuantity(rgas, 'ft*lbf/(slug*degR)'),
        'length_reference': PhysicalQuantity(diam, 'ft'),
        'pressure_reference': PhysicalQuantity(pref, 'lbf/ft**2'),
        'specific_heat_ratio': PhysicalQuantity(gamma, 'unitless'),
        'temperature_reference': PhysicalQuantity(tref, 'degR'),
    }

    # Set zone handedness and symmetry.  Also make cylindrical if necessary.
    for i, zone in enumerate(domain.zones):
        zone.right_handed = False
        zone.symmetry = 'rotational'
        zone.symmetry_axis = 'x'
        zone.symmetry_instances = nbld
        zone.make_cylindrical()

    # Read restart.
    restart = casename+'.restart.new'
    with open(restart, 'rb') as inp:
        logger.info("reading restart file '%s'", restart)
        stream = Stream(inp, binary=True, big_endian=True,
                        single_precision=True, integer_8=False,
                        unformatted=False, recordmark_8=False)

        # Read number of zones.
        nblocks = stream.read_int()
        if nblocks != len(domain.zones):
            raise RuntimeError("nblocks (%d) in '%s' != #Mesh zones (%d)"
                               % (nblocks, restart, len(domain.zones)))

        # Read zone dimensions.
        for zone in domain.zones:
            name = domain.zone_name(zone)
            imax, jmax, kmax = stream.read_ints(3)
            logger.debug('    %s: %dx%dx%d', name, imax, jmax, kmax)
            zone_i, zone_j, zone_k = zone.shape
            if imax != zone_i+1 or jmax != zone_j+1 or kmax != zone_k+1:
                raise RuntimeError('%s: Restart %dx%dx%d != Mesh %dx%dx%d' \
                                   % (name, imax, jmax, kmax,
                                      zone_i, zone_j, zone_k))
        # Read zone variables.
        for zone in domain.zones:
            name = domain.zone_name(zone)
            zone_i, zone_j, zone_k = zone.shape
            shape = (zone_i+1, zone_j+1, zone_k+1)
            logger.debug('reading data for %s', name)

            zone.flow_solution.grid_location = 'CellCenter'
            zone.flow_solution.ghosts = [1, 1, 1, 1, 1, 1]

            name = 'density'
            arr = stream.read_floats(shape, order='Fortran')
            logger.debug('    %s min %g, max %g', name, arr.min(), arr.max())
            zone.flow_solution.add_array(name, arr)

            vec = Vector()
            vec.x = stream.read_floats(shape, order='Fortran')
            logger.debug('    momentum.x min %g, max %g',
                         vec.x.min(), vec.x.max())
            vec.y = stream.read_floats(shape, order='Fortran')
            logger.debug('    momentum.y min %g, max %g',
                         vec.y.min(), vec.y.max())
            vec.z = stream.read_floats(shape, order='Fortran')
            logger.debug('    momentum.z min %g, max %g',
                         vec.z.min(), vec.z.max())
            zone.flow_solution.add_vector('momentum', vec)

            name = 'energy_stagnation_density'
            arr = stream.read_floats(shape, order='Fortran')
            logger.debug('    %s min %g, max %g', name, arr.min(), arr.max())
            zone.flow_solution.add_array(name, arr)

            name = 'pressure'
            arr = stream.read_floats(shape, order='Fortran')
            logger.debug('    %s min %g, max %g', name, arr.min(), arr.max())
            zone.flow_solution.add_array(name, arr)

    return domain

