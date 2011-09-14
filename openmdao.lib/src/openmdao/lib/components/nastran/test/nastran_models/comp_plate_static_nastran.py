"""
    comp_plate_static_nastran.py - Composite plate implementation example.
"""
import math

from openmdao.lib.datatypes.api import Float
from openmdao.util.filewrap import FileParser

from openmdao.lib.components.nastran.nastran import NastranComponent

class Comp_Plate(NastranComponent):
    """ Model of a composite plate """

    #initial thickness of each ply(sum of all the layers)

    thick1_init = 2.6562
    thick2_init = 2.6479
    thick3_init = 1.3309

    # design variables
    thick1 = Float(thick1_init, iotype="in", units="inch", desc="Thickness of pcomp 801")
    thick2 = Float(thick2_init, iotype="in", units="inch", desc="Thickness of pcomp 802")
    thick3 = Float(thick3_init, iotype="in", units="inch", desc="Thickness of pcomp 803")

    # outputs
    property1_max_major_strain = Float(0.0, iotype="out", desc="max major strain for pcomp 801")
    property2_max_major_strain = Float(0.0, iotype="out", desc="max major strain for pcomp 802")
    property3_max_major_strain = Float(0.0, iotype="out", desc="max major strain for pcomp 803")
    property1_max_minor_strain = Float(0.0, iotype="out", desc="max minor strain for pcomp 801")
    property2_max_minor_strain = Float(0.0, iotype="out", desc="max minor strain for pcomp 802")
    property3_max_minor_strain = Float(0.0, iotype="out", desc="max minor strain for pcomp 803")

    property1_max_major_minor_strain = Float(0.0, iotype="out", desc="max major minor strain for pcomp 801")
    property2_max_major_minor_strain = Float(0.0, iotype="out", desc="max major minor strain for pcomp 802")
    property3_max_major_minor_strain = Float(0.0, iotype="out", desc="max major minor strain for pcomp 803")

    displacement_18_z_dir = Float(0.0, iotype='out',
                               units='inch',
                               desc='Displacement in z-direction',
                               #nastran_func=disp4x)
                               nastran_header="displacement vector",
                               nastran_subcase=1,
                               nastran_constraints={"POINT ID." : "18"},
                               nastran_columns=["T3"])

    def mass(filep):
        filep.reset_anchor()
        filep.mark_anchor("MASS AXIS SYSTEM (S)")
        return filep.transfer_var(1, 2)

    weight = Float(0., nastran_func=mass, iotype='out', units='lb',
                        desc='Weight of the structure')

    def execute(self):

        super(Comp_Plate, self).execute()

        parser = FileParser()
        parser.set_file(self.nastran_filename)

        self.comp_elm_dict = {}
        parser.reset_anchor()
        for cquad4 in range(1,26):
            parser.mark_anchor("CQUAD4")
            elmtype = parser.transfer_var(0, 1)
            elmid = parser.transfer_var(0, 2)
            pid = parser.transfer_var(0, 3)

            if pid not in self.comp_elm_dict:
                self.comp_elm_dict[ pid ] = []

            self.comp_elm_dict[pid].append( elmid )

        max_minor_strain_by_pid, max_major_strain_by_pid = self.calculate_max_strains()

        self.property1_max_major_strain = max_major_strain_by_pid[ 801 ]
        self.property2_max_major_strain = max_major_strain_by_pid[ 802 ]
        self.property3_max_major_strain = max_major_strain_by_pid[ 803 ]

        self.property1_max_minor_strain = max_minor_strain_by_pid[ 801 ]
        self.property2_max_minor_strain = max_minor_strain_by_pid[ 802 ]
        self.property3_max_minor_strain = max_minor_strain_by_pid[ 803 ]

        # Calculate the maximum strain (max(major,minor)) for each property 
        self.property1_max_major_minor_strain = max( self.property1_max_major_strain, self.property1_max_minor_strain )
        self.property2_max_major_minor_strain = max( self.property2_max_major_strain, self.property2_max_minor_strain )
        self.property3_max_major_minor_strain = max( self.property3_max_major_strain, self.property3_max_minor_strain )

    def calculate_max_strains( self ):
        '''Using the data from the input.out file,
            calculate the max major and minor strains
            for each of the three properties'''

        # Get the element ID, minor strain and major strain all at once
        elm_id_minor_majors = self.parser.get("strains in layered composite elements (quad4)",
                                              1,
                                              {},
                                              ["ELEMENT ID",
                                               "PRINCIPAL  STRAINS (ZERO SHEAR) MINOR",
                                              "PRINCIPAL  STRAINS (ZERO SHEAR) MAJOR"])

        # find the max major and minor strains for each element ID
        max_major_strain_by_elmid = {}
        max_minor_strain_by_elmid = {}

        for elm_id_minor_major in elm_id_minor_majors:
            elmid, minor, major = ( int(elm_id_minor_major[0]),
                                    abs(float(elm_id_minor_major[1])),
                                    abs(float(elm_id_minor_major[2]))
                                    )
            if elmid in max_major_strain_by_elmid:
                max_major_strain_by_elmid[ elmid ] = max( max_major_strain_by_elmid[ elmid ], major )
            else:
                max_major_strain_by_elmid[ elmid ] = major
                
            if elmid in max_minor_strain_by_elmid:
                max_minor_strain_by_elmid[ elmid ] = max( max_minor_strain_by_elmid[ elmid ], minor )
            else:
                max_minor_strain_by_elmid[ elmid ] = minor
                

        # Find the max minor and major strains for each property
        max_major_strain_by_pid = {}
        max_minor_strain_by_pid = {}
        # Go through the dictionary of self.comp_elm_dict
        for pid in self.comp_elm_dict:
            element_ids_in_pid = self.comp_elm_dict[ pid ]
            # For each element in the pid, update the maximums
            for elmid in element_ids_in_pid:
                if pid in max_major_strain_by_pid:
                    max_major_strain_by_pid[ pid ] = max(
                        max_major_strain_by_pid[ pid ],
                        max_major_strain_by_elmid[ elmid ]
                        )
                else:
                    max_major_strain_by_pid[ pid ] = max_major_strain_by_elmid[ elmid ]
                    
                if pid in max_minor_strain_by_pid:
                    max_minor_strain_by_pid[ pid ] = max(
                        max_minor_strain_by_pid[ pid ],
                        max_minor_strain_by_elmid[ elmid ]
                        )
                else:
                    max_minor_strain_by_pid[ pid ] = max_minor_strain_by_elmid[ elmid ]
                
        
        for pid in max_minor_strain_by_pid:
            print "pid, max minor, max major", pid, max_minor_strain_by_pid[ pid ], max_major_strain_by_pid[ pid ]

        return max_minor_strain_by_pid, max_major_strain_by_pid
        

    def nastran_maker_hook(self, maker):

        # We want to keep the ratios of ply thickness
        # of each ply, but we want to change them in relation
        # to the overall thickness (t1, t2, and t3)

        # We'll use NastranMaker to set the individual ply's
        # thicknesses.

        super(Comp_Plate, self).nastran_maker_hook(maker)

        value_distribution = {"801" : [.25,.25,.25,.25],
                              "802" : [.25,.25,.25,.25],
                              "803" : [.25,.25,.25,.25]}

        
        # for each pcomp, we have to set all four plys
        for pcomp in range(1,4): # there are three pcomps
            id = str(800 + pcomp)
            values = []
            for x in value_distribution[id]:
                values.append(x * self.__getattribute__("thick%d" % pcomp) )

            for ply in range(4): # there are four plys
                plynum = 12 + 4 * ply + 2 * (ply/2)
                maker.set("PCOMP", id,
                          plynum, values[ply])
