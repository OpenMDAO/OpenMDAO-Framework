"""
    blade_2dv_static_nastran.py - Blade implementation for a quad
    example structures problem. 
"""
import math

from openmdao.lib.datatypes.api import Float

from openmdao.lib.components.nastran.nastran import NastranComponent

class BladeStatic(NastranComponent):
    """ Model of a Blade quad elements  - Nastran Implementation."""

    group1_thickness  = Float(0.5, nastran_card="PSHELL",
                       nastran_id="1", nastran_fieldnum=3,
                       iotype='in', units='inch',
                       desc='Thickness for group 1')

    group2_thickness  = Float(0.03, nastran_card="PSHELL",
                       nastran_id="2", nastran_fieldnum=3,
                       iotype='in', units='inch',
                       desc='Thickness for group 2')

    # these are actually groups of stresses that will be
    # constrained

    group1_stress = Float(0., #nastran_func=stress1,
                        iotype='out',
                        units='lb/(inch*inch)',
                        desc='Stress in group 1')
    group2_stress = Float(0., #nastran_func=stress2,
                        iotype='out',
                        units='lb/(inch*inch)',
                        desc='Stress in group 2')

    displacement_z_dir = Float(0.1632, iotype='out',
                               units='inch',
                               desc='Displacement in z-direction',
                               #nastran_func=x1disp)
                               nastran_header="displacement vector",
                               nastran_subcase=1,
                               nastran_constraints={"POINT ID." : "28"},
                               nastran_columns=["T3"])

    def mass(filep):
        filep.reset_anchor()
        filep.mark_anchor("MASS AXIS SYSTEM (S)")
        return filep.transfer_var(1, 2)

    weight = Float(0., nastran_func=mass, iotype='out', units='lb',
                        desc='Weight of the structure')

    def execute(self):
        """ Simulates the analysis of a blade with quad elements.
            Force, Stress, Displacement,Frequency and Weight are returned at
            the Blade output.
        """

        super(BladeStatic, self).execute()

        stresses = []
        header = "S T R E S S E S   I N   Q U A D R I L A T E R A L   E L E M E N T S   ( Q U A D 4 )        OPTION = BILIN"


        columns = ["VON MISES"]
        data = self.parser.get(header, None, \
                               {}, columns, row_width=15)
        von_mises =[]
        for element in data:
            values = map(lambda x: x[0],element)
            biggest = -1.0E+10
            for value in values: 
                if value != '':
                   biggest = max(float(value),biggest)  
            von_mises.append(biggest)

        groups = [range(25601,25945+1), range(1, 25600+1)]

        [self.group1_stress, self.group2_stress] = group_von_mises(groups, von_mises) 

def group_von_mises(groups, von_mises):
    final = []
    for group in groups:
        final.append([])
        for element in group:
            final[-1].append(abs(von_mises[element-1])) # stresses is zero indexed
        # we actually just wanted the maximum
        final[-1] = max(final[-1])
    return final

def calculate_stress((ax, tors)):
    sigma = 2 * ax * ax
    tau = 3 * tors * tors
    val = math.sqrt(.5 * (sigma + tau))
    return val

