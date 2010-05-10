import math

import numpy

from openmdao.lib.traits.domain import DomainObj, Vector, Zone, write_plot3d_q

_DEG2RAD = math.asin(1.) / 90.


def create_wedge_3d(shape, length, inner, outer, angle):
    """ Creates a 3D wedge-shaped single-zone structured domain. """
    imax, jmax, kmax = shape

    delta_x      = float(length) / (imax - 1)
    delta_radius = float(outer - inner) / (jmax - 1)
    delta_theta  = float(angle * _DEG2RAD) / (kmax - 1)

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
        axial = delta_x * i
        for j in range(jmax):
            radial = inner + delta_radius * j
            for k in range(kmax):
                tangential = delta_theta * k

                x.itemset(i, j, k, axial)
                y.itemset(i, j, k, radial * math.cos(tangential))
                z.itemset(i, j, k, radial * math.sin(tangential))

                q1.itemset(i, j, k, axial)

                q2.itemset(i, j, k, axial)
                q3.itemset(i, j, k, 0.)
                q4.itemset(i, j, k, length - axial)

                q5.itemset(i, j, k, radial)

    momentum = Vector()
    momentum.x = q2
    momentum.y = q3
    momentum.z = q4
    
    zone = Zone()
    zone.coords.x = x
    zone.coords.y = y
    zone.coords.z = z

    zone.mach = 0.5
    zone.alpha = 0.
    zone.reynolds = 100000.
    zone.time = 42.

    zone.density = q1
    zone.add_vector('momentum', momentum)
    zone.energy_stagnation_density = q5

    domain = DomainObj()
    domain.add_zone('xyzzy', zone)

    return domain


def create_wedge_2d(shape, inner, outer, angle):
    """ Creates a 2D wedge-shaped single-zone structured domain. """
    imax, jmax = shape

    delta_radius = float(outer - inner) / (imax - 1)
    delta_theta  = float(angle * _DEG2RAD) / (jmax - 1)

    dtype = numpy.float32  # Default single-precision.

    x = numpy.zeros(shape, dtype=dtype)
    y = numpy.zeros(shape, dtype=dtype)

    q1 = numpy.zeros(shape, dtype=dtype)
    q2 = numpy.zeros(shape, dtype=dtype)
    q3 = numpy.zeros(shape, dtype=dtype)
    q4 = numpy.zeros(shape, dtype=dtype)

    for i in range(imax):
        radial = inner + delta_radius * i
        for j in range(jmax):
            tangential = delta_theta * j

            x.itemset(i, j, radial * math.cos(tangential))
            y.itemset(i, j, radial * math.sin(tangential))

            q1.itemset(i, j, radial)

            q2.itemset(i, j, radial)
            q3.itemset(i, j, tangential)

            q4.itemset(i, j, tangential)

    momentum = Vector()
    momentum.x = q2
    momentum.y = q3
    
    zone = Zone()
    zone.coords.x = x
    zone.coords.y = y

    zone.mach = 0.5
    zone.alpha = 0.
    zone.reynolds = 100000.
    zone.time = 42.

    zone.density = q1
    zone.add_vector('momentum', momentum)
    zone.energy_stagnation_density = q4

    domain = DomainObj()
    domain.add_zone('xyzzy', zone)

    return domain


if __name__ == '__main__':
    write_plot3d_q(create_wedge_3d((30, 20, 10), 5., 0.5, 2., 30.),
                   'wedge3d.xyz', 'wedge3d.q')

    write_plot3d_q(create_wedge_2d((20, 10), 0.5, 2., 30.),
                   'wedge2d.xyz', 'wedge2d.q')

