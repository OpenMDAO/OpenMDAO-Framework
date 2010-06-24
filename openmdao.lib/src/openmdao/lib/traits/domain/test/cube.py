import numpy

from openmdao.units import PhysicalQuantity
from openmdao.lib.traits.domain import DomainObj, Vector, Zone, write_plot3d_q


def create_cube(shape, width, height, depth):
    """ Creates a cube-shaped single-zone structured domain. """
    imax, jmax, kmax = shape

    delta_x = float(width)  / (imax - 1)
    delta_y = float(height) / (jmax - 1)
    delta_z = float(depth)  / (kmax - 1)

    dtype = numpy.float32  # Default single-precision.

    x = numpy.zeros(shape, dtype=dtype)
    y = numpy.zeros(shape, dtype=dtype)
    z = numpy.zeros(shape, dtype=dtype)

    q1 = numpy.zeros(shape, dtype=dtype)
    q2 = numpy.zeros(shape, dtype=dtype)
    q3 = numpy.zeros(shape, dtype=dtype)
    q4 = numpy.zeros(shape, dtype=dtype)
    q5 = numpy.zeros(shape, dtype=dtype)

    for i in range(imax):
        for j in range(jmax):
            for k in range(kmax):
                x.itemset(i, j, k, delta_x * i)
                y.itemset(i, j, k, delta_y * j)
                z.itemset(i, j, k, delta_z * k)

                q1.itemset(i, j, k, delta_x * i)

                q2.itemset(i, j, k, delta_x * i)
                q3.itemset(i, j, k, delta_y * j)
                q4.itemset(i, j, k, delta_z * k)

                q5.itemset(i, j, k, delta_z * k)

    momentum = Vector()
    momentum.x = q2
    momentum.y = q3
    momentum.z = q4
    
    zone = Zone()
    zone.grid_coordinates.x = x
    zone.grid_coordinates.y = y
    zone.grid_coordinates.z = z

    zone.flow_solution.mach = 0.5
    zone.flow_solution.alpha = 0.
    zone.flow_solution.reynolds = 100000.
    zone.flow_solution.time = 42.

    zone.flow_solution.add_array('density', q1)
    zone.flow_solution.add_vector('momentum', momentum)
    zone.flow_solution.add_array('energy_stagnation_density', q5)

    domain = DomainObj()
    domain.reference_state = dict(length_reference=PhysicalQuantity(1., 'ft'))
    domain.add_zone('xyzzy', zone)

    return domain


if __name__ == '__main__':
    write_plot3d_q(create_cube((30, 20, 10), 5., 4., 3.), 'cube.xyz', 'cube.q')

