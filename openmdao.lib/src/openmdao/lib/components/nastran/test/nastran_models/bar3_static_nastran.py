"""
    bar3_wrap_f.py - Bar3 (Fortran implementation) for the a three bar truss
    example structures problem. This openMDAO component contains a three bar
    truss example referenced in CometBoards
"""
import math

from openmdao.lib.datatypes.api import Float

from openmdao.lib.components.nastran.nastran import NastranComponent

class Bar3Static(NastranComponent):
    """ Model of a three bar truss - Fortran Implementation."""

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

    displacement_x_dir = Float(0.20, iotype='out',
                               units='inch',
                               desc='Displacement in x-direction',
                               #nastran_func=xdisp)
                               nastran_header="displacement vector",
                               nastran_subcase=1,
                               nastran_constraints={"POINT ID." : "1"},
                               nastran_columns=["T1"])

    displacement_y_dir = Float(0.05, iotype='out',
                               units='inch',
                               desc='Displacement in y-direction',
                               #nastran_func=ydisp)
                               nastran_header="displacement vector",
                               nastran_subcase=1,
                               nastran_constraints={"POINT ID." : "1"},
                               nastran_columns=["T2"])
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
        super(Bar3Static, self).execute()

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


def calculate_stress((ax, tors)):
    sigma = 2 * ax * ax
    tau = 3 * tors * tors
    val = math.sqrt(.5 * (sigma + tau))
    return val
