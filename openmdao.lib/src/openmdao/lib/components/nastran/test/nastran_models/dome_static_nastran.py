"""
    dome_static_nastran.py - implementation for the geodesic dome
    example structures problem referenced in CometBoards
"""
import math

from openmdao.lib.datatypes.api import Float

from openmdao.lib.components.nastran.nastran import NastranComponent

class DomeStatic(NastranComponent):

    for i in range(1,157):
        cmd = "bar%d_init_area = 1.00" %i
        exec (cmd)

    for i in range(157,253):
        cmd = "tria%d_init_thickness = 1.00" %i
        exec (cmd)

    for i in range(1,157):
        cmd = 'bar%d_area  = Float(bar%d_init_area, nastran_card="PROD",\
                       nastran_id=%d, nastran_fieldnum=3,\
                       iotype="in", units="inch*inch",\
                       desc="Cross-sectional area for bar %d")' %(i,i,i,i)
        exec(cmd)

    for i in range(157,253):
        cmd = 'tria%d_thickness  = Float(tria%d_init_thickness, nastran_card="PSHELL",\
                       nastran_id=%d, nastran_fieldnum=3,\
                       iotype="in", units="inch*inch",\
                       desc="Membrane thickness for tria %d")' %(i,i,i,i)
        exec(cmd)

    # these are stresses that will be  constrained

    for i in range(1,157):
        cmd = "bar%d_stress = Float(0., iotype='out', units='lb/(inch*inch)', desc='Axial stress in element %d')" %(i,i)
        exec(cmd)
    for i in range(157,253):
        cmd = "tria%d_stress = Float(0., iotype='out', units='lb/(inch*inch)', desc='Von Mises stress in element %d')" %(i,i)
        exec(cmd)

    def mass(filep):
        filep.reset_anchor()
        filep.mark_anchor("MASS AXIS SYSTEM (S)")
        return filep.transfer_var(1, 2)


    weight = Float(0., nastran_func=mass, iotype='out', units='lb',
                        desc='Weight of the structure')


    def execute(self):
        """ Simulates the analysis of a ten bar truss structure.
            Force, Stress, Displacement,Frequency and Weight are returned at
            the Ring output.
        """

        # process all the input variables and convert them
        # to something nastran will enjoy

        # run, nastran, run
        super(DomeStatic, self).execute()

        stresses_bars = []
        header = "S T R E S S E S   I N   R O D   E L E M E N T S      ( C R O D )"

        columns = ["AXIAL STRESS", "TORSIONAL STRESS"]
        data = self.parser.get(header, None, \
                               {}, columns)

        for i, stresses_bars in enumerate(data):
            stress = calculate_stress((float(stresses_bars[0]), float(stresses_bars[1])))
            cmd = "self.bar%d_stress = stress" % (i+1)
            exec(cmd)

        stresses_tria = []
        header = "S T R E S S E S   I N   T R I A N G U L A R   E L E M E N T S   ( T R I A 3 )"

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

def calculate_stress((ax, tors)):
    sigma = 2 * ax * ax
    tau = 3 * tors * tors
    val = math.sqrt(.5 * (sigma + tau))
    return val

def group_von_mises(groups, von_mises):
    final = []
    for group in groups:
        final.append([])
        for element in group:
            final[-1].append(abs(von_mises[element-1])) # stresses is zero indexed
        # we actually just wanted the maximum
        final[-1] = max(final[-1])
    return final

