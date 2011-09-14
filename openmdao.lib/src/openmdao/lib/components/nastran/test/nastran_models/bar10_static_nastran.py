"""
    bar10_static_nastran.py - Ten Bar truss example structures problem. 
    This openMDAO component contains a ten bar truss example referenced in CometBoards
"""
import math

from openmdao.lib.datatypes.api import Float

from openmdao.lib.components.nastran.nastran import NastranComponent

class Bar10Static(NastranComponent):

    for i in range(1,11):
        cmd = "bar%d_init_area = 10.0" %i
        exec (cmd)

    for i in range(1,11):
        cmd = 'bar%d_area  = Float(bar%d_init_area, nastran_card="PROD",\
                       nastran_id=%d, nastran_fieldnum=3,\
                       iotype="in", units="inch*inch",\
                       desc="Cross-sectional area for bar %d")' %(i,i,i,i)
        exec(cmd)

    # these are stresses that will be  constrained

    for i in range(1,11):
        cmd = "bar%d_stress = Float(0., iotype='out', units='lb/(inch*inch)', desc='Axial stress in element %d')" %(i,i)
        exec(cmd)

    # these are displacements that will be  constrained

    displacement1_y_dir = Float(2.0, iotype='out',
                               units='inch',
                               desc='Displacement in y-direction',
                               #nastran_func=ydisp)
                               nastran_header="displacement vector",
                               nastran_subcase=1,
                               nastran_constraints={"POINT ID." : "3"},
                               nastran_columns=["T2"])

    displacement2_y_dir = Float(2.0, iotype='out',
                               units='inch',
                               desc='Displacement in y-direction',
                               #nastran_func=ydisp)
                               nastran_header="displacement vector",
                               nastran_subcase=1,
                               nastran_constraints={"POINT ID." : "4"},
                               nastran_columns=["T2"])

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

        super(Bar10Static, self).execute()

        stresses = []
        header = "S T R E S S E S   I N   R O D   E L E M E N T S      ( C R O D )"

        columns = ["AXIAL STRESS", "TORSIONAL STRESS"]
        data = self.parser.get(header, None, \
                               {}, columns)

        for i, stresses in enumerate(data):
            stress = calculate_stress((float(stresses[0]), float(stresses[1])))
            cmd = "self.bar%d_stress = stress" % (i+1)
            exec(cmd)

def calculate_stress((ax, tors)):
    sigma = 2 * ax * ax
    tau = 3 * tors * tors
    val = math.sqrt(.5 * (sigma + tau))
    return val


if __name__ == "__main__": # pragma: no cover

    truss = Bar10Static()
    truss.nastran_command = "/msc/nastran/bin/nastran"
    truss.nastran_filename = "bdf_files/bar10.bdf"
    truss.delete_tmp_files = True

    truss.run()

    print " "
    print "Weight = %8.4f" % (truss.weight)
    print " "
    print "Design: "
    for i in range(1,11):
         cmdprint = "print 'Design variable %d = ', truss.bar%d_area" %(i,i)
         exec(cmdprint)
    print " "
    print "Constraints:"

    max_stress = 0.0
    for i in range(1,11):
         cmdprint = "print 'Axial Stress in Element %d = ', truss.bar%d_stress" %(i,i)
         exec(cmdprint)
         max_cmd = "max_stress = max(max_stress, truss.bar%d_stress)" %i
         exec(max_cmd)

    print " "
    print "Maximum Stress = ", max_stress

    print " "
    print "Displacement in y-dir at node 3  =  %8.4f" % (truss.displacement1_y_dir)
    print "Displacement in y-dir at node 4  =  %8.4f" % (truss.displacement2_y_dir)
    print " "
