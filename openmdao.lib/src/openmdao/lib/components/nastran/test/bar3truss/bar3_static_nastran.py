import operator
import math
from numpy import zeros

# pylint: disable-msg=E0611,F0401
from openmdao.main.api import Component
from openmdao.lib.datatypes.api import Float

from openmdao.lib.components.nastran.nastran import NastranComponent

class Bar3Static(NastranComponent):
    """ Model of a three bar truss - Nastran Implementation."""

    # set up interface to the framework
    # pylint: disable-msg=E1101

    bar1_area  = Float(1., nastran_card="PROD",
                       nastran_id="11", nastran_fieldnum=3,
                       low=0.0009, high=10000.,
                       iotype='in', units='inch*inch',
                       desc='Cross-sectional area for bar 1')

    bar2_area  = Float(1., nastran_card="PROD",
                       nastran_id="12", nastran_fieldnum=3,
                       low=0.0009, high=10000.,
                       iotype='in', units='inch*inch',
                       desc='Cross-sectional area for bar 2')

    bar3_area  = Float(1., nastran_card='PROD',
                       nastran_id="13", nastran_fieldnum=3,
                       low=0.0009, high=10000.,
                        iotype='in', units='inch*inch',
                        desc='Cross-sectional area for bar 3')


    load_x_dir = Float(50000.0, nastran_card="FORCE",
                       nastran_id="8", nastran_fieldnum=5,
                       iotype='in', units='lb*inch',
                              desc='Load in X direction')

    load_y_dir = Float(100000.0, nastran_card="FORCE",
                       nastran_id="8", nastran_fieldnum=6,
                       iotype='in', units='lb*inch',
                        desc='Load in Y direction')

    loadmag = Float(1.0, nastran_card="FORCE",
                    nastran_id="8", nastran_fieldnum=4,
                    iotype="in", units="lb",
                    desc="magnitude of the force")

    Youngs_Modulus = Float(30000000.0,
                           nastran_card="MAT1",
                           nastran_id="5",
                           nastran_fieldnum=2,
                           iotype='in',
                           units='lb/(inch*inch)',
                           desc='Youngs Modulus')

    weight_density = Float(0.284,
                           nastran_card="MAT1",
                           nastran_id="5",
                           nastran_fieldnum=5,
                           iotype='in', units='lb/(inch**3)',
                           desc='weight density of all bars')

    #lumped_mass = Float(0.68005, iotype='in',
    #                           units='lb*s*s/inch',
    #                           desc='Lumped Mass at the free node')

    #bar1_force = Float(0., iotype='out', units='lb',
    #                    desc='Force in bar 1')
    #bar2_force = Float(0., iotype='out', units='lb',
    #                    desc='Force in bar 2')
    #bar3_force = Float(0., iotype='out', units='lb',
    #                    desc='Force in bar 3')

    bar1_stress = Float(0., #nastran_func=stress1,
                        iotype='out',
                        units='lb/(inch*inch)',
                        desc='Stress in bar 1')
    bar2_stress = Float(0., #nastran_func=stress2,
                        iotype='out',
                        units='lb/(inch*inch)',
                        desc='Stress in bar 2')
    bar3_stress = Float(0., #nastran_func=stress3,
                        iotype='out',
                        units='lb/(inch*inch)',
                        desc='Stress in bar 3')

    displacement_x_dir = Float(0., iotype='out',
                               units='inch',
                               desc='Displacement in x-direction',
                               #nastran_func=xdisp)
                               nastran_header="displacement vector",
                               nastran_subcase=1,
                               nastran_constraints={"POINT ID." : "1"},
                               nastran_columns=["T1"])

    displacement_y_dir = Float(0., iotype='out',
                               units='inch',
                               desc='Displacement in y-direction',
                               #nastran_func=ydisp)
                               nastran_header="displacement vector",
                               nastran_subcase=1,
                               nastran_constraints={"POINT ID." : "1"},
                               nastran_columns=["T2"])

    #frequency = Float(0.1, iotype='out', units='Hz',
    #                    desc='Frequency in Hertz')

    def mass(filep):
        filep.reset_anchor()
        filep.mark_anchor("MASS AXIS SYSTEM (S)")
        return filep.transfer_var(1, 2)

    weight = Float(0., nastran_func=mass, iotype='out', units='lb',
                        desc='Weight of the structure')

    def execute(self):
        """ Simulates the analysis of a three bar truss structure.
            Force, Stress, Displacement,Frequency and Weight are returned at
            the Bar3Truss output.
        """

        # process all the input variables and convert them
        # to something nastran will enjoy

        # run, nastran, run
        super(Bar3Static, self).execute()

        # By now, all the variables with a nastran instruction
        # on how to parse them will be all set.
        # If you need to generate more output variables (like
        # the sum of two previously generated variables), now
        # is the time!


        stresses = []
        header = "S T R E S S E S   I N   R O D   E L E M E N T S      ( C R O D )"
        for i in range(1,4):
            constraints = {"ELEMENT ID." : str(i)}

            columns = ["AXIAL STRESS", "TORSIONAL STRESS"]
            [[axial, torsion]] = self.parser.get(header, None, \
                                             constraints, columns)
            axial, torsion = map(float, [axial, torsion])
            stresses.append((axial, torsion))

        [self.bar1_stress, self.bar2_stress, self.bar3_stress] = \
                          map(calculate_stress, stresses)




#end Bar3Truss

def calculate_stress((ax, tors)):
    sigma = 2 * ax * ax
    tau = 3 * tors * tors
    val = math.sqrt(.5 * (sigma + tau))
    return val


if __name__ == "__main__": # pragma: no cover

    truss = Bar3Truss()
    truss.nastran_command = "/msc/nastran/bin/nastran"
    truss.nastran_filename = "vared_bar3.bdf"

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
