from math import asin, cos, sin

import numpy

from openmdao.units import PhysicalQuantity
from openmdao.lib.datatypes.domain import DomainObj, Vector, Zone, write_plot3d_q

_DEG2RAD = asin(1.) / 90.


def create_wedge_3d(shape, length, inner, outer, angle, axis='z'):
    """ Creates a 3D wedge-shaped single-zone structured domain. """
    imax, jmax, kmax = shape

    delta_axis   = float(length) / (imax - 1) if imax > 1 else 1.
    delta_radius = float(outer - inner) / (jmax - 1) if jmax > 1 else 1.
    delta_theta  = float(angle * _DEG2RAD) / (kmax - 1) if kmax > 1 else 1.

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
        axial = delta_axis * i
        for j in range(jmax):
            radial = inner + delta_radius * j
            for k in range(kmax):
                tangential = delta_theta * k

                if axis == 'z':
                    x.itemset(i, j, k, radial * cos(tangential))
                    y.itemset(i, j, k, radial * sin(tangential))
                    z.itemset(i, j, k, axial)
                else:
                    x.itemset(i, j, k, axial)
                    y.itemset(i, j, k, radial * cos(tangential))
                    z.itemset(i, j, k, radial * sin(tangential))

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


def create_wedge_2d(shape, inner, outer, angle):
    """ Creates a 2D wedge-shaped single-zone structured domain. """
    imax, jmax = shape

    delta_radius = float(outer - inner) / (imax - 1) if imax > 1 else 1.
    delta_theta  = float(angle * _DEG2RAD) / (jmax - 1) if jmax > 1 else 1.

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

            x.itemset(i, j, radial * cos(tangential))
            y.itemset(i, j, radial * sin(tangential))

            q1.itemset(i, j, radial)

            q2.itemset(i, j, radial)
            q3.itemset(i, j, tangential)

            q4.itemset(i, j, tangential)

    momentum = Vector()
    momentum.x = q2
    momentum.y = q3
    
    zone = Zone()
    zone.grid_coordinates.x = x
    zone.grid_coordinates.y = y

    zone.flow_solution.mach = 0.5
    zone.flow_solution.alpha = 0.
    zone.flow_solution.reynolds = 100000.
    zone.flow_solution.time = 42.

    zone.flow_solution.add_array('density', q1)
    zone.flow_solution.add_vector('momentum', momentum)
    zone.flow_solution.add_array('energy_stagnation_density', q4)

    domain = DomainObj()
    domain.reference_state = dict(length_reference=PhysicalQuantity(1., 'ft'))
    domain.add_zone('xyzzy', zone)

    return domain


if __name__ == '__main__': # pragma no cover
    write_plot3d_q(create_wedge_3d((30, 20, 10), 5., 0.5, 2., 30.),
                   'wedge3d.xyz', 'wedge3d.q')

    write_plot3d_q(create_wedge_2d((20, 10), 0.5, 2., 30.),
                   'wedge2d.xyz', 'wedge2d.q')

