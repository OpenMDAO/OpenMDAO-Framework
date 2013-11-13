"""
    bar3_wrap_f.py - Bar3 (Fortran implementation) for the a three bar truss
    example structures problem. This openMDAO component contains a three bar 
    truss example referenced in CometBoards
"""

from numpy import zeros

# pylint: disable-msg=E0611,F0401
from openmdao.main.api import Component
from openmdao.main.datatypes.api import Float

from openmdao.examples.bar3simulation.bar3 import runbar3truss, forces


class Bar3Truss(Component):
    """ Model of a three bar truss - Fortran Implementation."""

    # set up interface to the framework
    # pylint: disable-msg=E1101
    
    load_x_dir = Float(50.0, iotype='in', units='lb',
                        desc='Load in X direction')

    load_y_dir = Float(100.0, iotype='in', units='lb',
                        desc='Load in Y direction')

    bar1_area  = Float(1.0, low=0.0009, high=10000.,
                        iotype='in', units='inch*inch',
                        desc='Cross-sectional area for bar 1')
    bar2_area  = Float(1.0, low=0.0009, high=10000.,
                        iotype='in', units='inch*inch',
                        desc='Cross-sectional area for bar 2')
    bar3_area  = Float(1.0, low=0.0009, high=10000.,
                        iotype='in', units='inch*inch',
                        desc='Cross-sectional area for bar 3')

    Youngs_Modulus = Float(30000.0, iotype='in',
                        units='1000.0*lb/(inch*inch)',
                        desc='Youngs Modulus')

    bar2_length = Float(100.0, iotype='in', units='inch',
                        desc='Length of bar 2 and horizontal distance between \
                        nodes')

    weight_density = Float(0.284, iotype='in', units='lb/(inch**3)',
                        desc='weight density of all bars')

    lumped_mass = Float(0.68005, iotype='in', units='lb*s*s/inch',
                        desc='Lumped Mass at the free node')

    bar1_force = Float(0., iotype='out', units='lb',
                        desc='Force in bar 1')
    bar2_force = Float(0., iotype='out', units='lb',
                        desc='Force in bar 2')
    bar3_force = Float(0., iotype='out', units='lb',
                        desc='Force in bar 3')
    bar1_stress = Float(0., iotype='out', units='lb/(inch*inch)',
                        desc='Stress in bar 1')
    bar2_stress = Float(0., iotype='out', units='lb/(inch*inch)',
                        desc='Stress in bar 2')
    bar3_stress = Float(0., iotype='out', units='lb/(inch*inch)',
                        desc='Stress in bar 3')
    displacement_x_dir = Float(0., iotype='out', units='inch',
                        desc='Displacement in x-direction')
    displacement_y_dir = Float(0., iotype='out', units='inch',
                        desc='Displacement in y-direction')
    frequency = Float(0., iotype='out', units='Hz',
                        desc='Frequency in Hertz')
    weight = Float(0., iotype='out', units='lb',
                        desc='Weight of the structure')

    def execute(self):
        """ Simulates the analysis of a three bar truss structure.
            Force, Stress, Displacement,Frequency and Weight are returned at
            the Bar3Truss output.
            """

        load = zeros(2,'d')
        load[0] = self.load_x_dir
        load[1] = self.load_y_dir
        
        lumped_mass = self.lumped_mass
        bar1_area = self.bar1_area
        bar2_area = self.bar2_area
        bar3_area = self.bar3_area
        Youngs_Modulus = self.Youngs_Modulus
        bar2_length = self.bar2_length
        weight_density = self.weight_density

        # Call the Fortran model and pass it what it needs.

        (self.bar1_stress, self.bar2_stress, self.bar3_stress, 
         self.displacement_x_dir, self.displacement_y_dir, 
         self.frequency, self.weight) = runbar3truss(load, lumped_mass, 
                                                     bar1_area,bar2_area,bar3_area,
                                                     Youngs_Modulus, bar2_length, 
                                                     weight_density)

        # Pull value of Forces from the COMMON block Forces.
        self.bar1_force = float(forces.force1)
        self.bar2_force = float(forces.force2)
        self.bar3_force = float(forces.force3)
        
        # The FORTRAN code returns stresses with the wrong sign, so we
        # flip the sign here (tension = positive).
        self.bar1_stress = -self.bar1_stress
        self.bar2_stress = -self.bar2_stress
        self.bar3_stress = -self.bar3_stress

#end Bar3Truss
    
if __name__ == "__main__": # pragma: no cover

    truss = Bar3Truss()

    import time
    time1 = time.time()

    truss.run()

    print " "
    print "Weight = %8.4f" % (truss.weight)
    print " "
    print "Bar Forces = %8.4f" % (truss.bar1_force), \
                       "%8.4f" % (truss.bar2_force), \
                       "%8.4f" % (truss.bar3_force)
    print " "
    print "Bar Stresses = %8.4f" % (truss.bar1_stress), \
                         "%8.4f" % (truss.bar2_stress), \
                         "%8.4f" % (truss.bar3_stress) 
    print " "
    print "Displacement in x & y directions = %8.4f" \
                   % (truss.displacement_x_dir),"%8.4f" \
                   % (truss.displacement_y_dir)
    print " "
    print "Frequency = %8.4f" % (truss.frequency)
    print " "
    print "Elapsed time: ", time.time()-time1
