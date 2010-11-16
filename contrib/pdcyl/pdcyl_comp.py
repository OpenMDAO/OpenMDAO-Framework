"""
OpenMDAO Wrapper for PDCYL.

PDCYL is a structural esimation code that was pulled from ACSYNT.
"""

# pylint: disable-msg=E0611,F0401
from numpy import int32 as numpy_int32
from numpy import float32 as numpy_float32
from numpy import array, zeros

from openmdao.lib.datatypes.api import Array, Enum, Float, Int, Str
from openmdao.lib.components.api import ExternalCode
from openmdao.main.api import FileMetadata

class PdcylComp(ExternalCode):
    """ OpenMDAO component wrapper for PDCYL. """

    icalc   = Int(0, iotype='in', desc='print switch')
    
    # Wing geometry
    # --------------------
    wsweep  = Float(iotype='in', units='deg',
                 desc='Wing sweep referenced to the leading edge')
    war     = Float(iotype='in', desc='Wing Aspect Ratio')
    wtaper  = Float(iotype='in', desc=' Wing taper ratio')
    wtcroot = Float(iotype='in', desc=' Wing thickness-to-cord at root')
    wtctip  = Float(iotype='in', desc=' Wing thickness-to-cord at tip') 
    warea   = Float(iotype='in', units='ft**2', desc ='Wing planform area')

    # Material properties
    # --------------------
    ps     = Float(iotype='in', desc='Plasticity factor') 
    tmgw   = Float(iotype='in', units='inch', desc='Min. gage thickness for the wing')  
    effw   = Float(iotype='in', desc='Buckling efficiency of the web')
    effc   = Float(iotype='in', desc='Buckling efficiency of the covers')
    esw    = Float(iotype='in', units='psi', desc="Young's Modulus for wing material")
    fcsw   = Float(iotype='in', units='psi', desc='Ult. compressive strength of wing')
    dsw    = Float(iotype='in', units='lb/inch**3', desc=' Density of the wing material')
    kdew   = Float(iotype='in', desc="Knock-down factor for Young's Modulus")
    kdfw   = Float(iotype='in', desc='Knock-down factor for Ultimate strength')

    # Geometric parameters
    # --------------------
    istama   = Enum([1, 2], iotype='in', desc=' 1 - Position of wing is unknown; 2 - position is known')
    cs1      = Float(iotype='in', desc='Position of structural wing box from leading edge as percent of root chord')
    cs2      = Float(iotype='in', desc=' Position of structural wing box from trailing edge as percent of root chord')
    uwwg     = Float(iotype='in', units='lb/ft**2', desc=' Wing Weight / Wing Area of baseline aircraft  ')
    xwloc1   = Float(iotype='in', desc=' Location of wing as a percentage of body length')

    # Structural Concept
    # --------------------
    claqr   = Float(iotype='in', desc='Ratio of body lift to wing lift')
    ifuel   = Enum([1, 2], iotype='in', desc=' 1 - No fuel is stored in the wing; 2 - Fuel is stored in the wing')
    cwman   = Float(iotype='in', desc='Design maneuver load factor')
    cf      = Float(iotype='in', desc="Shanley's const. for frame bending")

    # Tails
    # --------------------
    itail   = Enum([1, 2], iotype='in', desc=' 1 - Control surfaces mounted on tail; 2 - Control surfaces mounted on wing')
    uwt     = Float(iotype='in', units='lb/ft**2', desc='(Htail Weight + Vtail Weight) / Htail Area of baseline aircraft')
    clrt    = Float(iotype='in', desc=' Location of tail as a percentage of body length')
    harea   = Float(iotype='in', units='ft**2', desc=' Location of tail as a percentage of body length')

    # Fuselage geometry
    # --------------------
    frn     = Float(iotype='in', desc='Fineness ratio of the nose section      (length/diameter)')
    frab    = Float(iotype='in', desc='Fineness ratio of the after-body section   (length/diameter)')
    bodl    = Float(iotype='in', units='ft', desc='Length of the fuselage  ')
    bdmax   = Float(iotype='in', units='ft', desc='Maximum diameter of fuselage')
    vbod    = Float(iotype='in', units='ft**3', desc='Fuselage total volume ')
    volnose = Float(iotype='in', units='ft**3', desc='Nose Volume')                
    voltail = Float(iotype='in', units='ft**3', desc='Tail volume ')
    
    # Structural Concept
    # --------------------
    ckf     = Float(iotype='in', desc='Frame stiffness coefficient')
    ec      = Float(iotype='in', desc='Power in approximation equation for buckling stability')
    kgc     = Float(iotype='in', desc='Buckling coefficient for component general buckling of stiffener web panel')
    kgw     = Float(iotype='in', desc='Buckling coefficient for component local buckling of web panel')
    #     KCONT(12)   ! Structural Geometry Concept Top/Bottom
    #     KCONB(12)   ! 2 - Simply stiffened shell, frames, sized for minimum weight in buckling
    #                 ! 3 - Z-stiffened shell, frames, best buckling
    #                 ! 4 - Z-stiffened shell, frames, buckling-minimum gage compromise
    #                 ! 5 - Z-stiffened shell, frames, buckling-pressure compromise
    #                 ! 6 - Truss-core sandwich, frames, best buckling
    #                 ! 8 - Truss-core sandwich, no frames, best buckling
    #                 ! 9 - Truss-core sandwich, no frames, buckling-min. gage-pressure compromise
    kcont   = Array(zeros([11]), iotype='in', dtype=numpy_int32, shape = (12,), desc='Structural Geometry Concept Top')
    kconb   = Array(zeros([12]), iotype='in', dtype=numpy_int32, shape = (12,), desc='Structural Geometry Concept Bottom')

    # Material properties
    # -------------------
#   ftst    = Array(_ZEROS12,dtype=float,shape=(12,),iotype='in')
#   ftsb    = Array(_ZEROS12,dtype=float,shape=(12,),iotype='in')
#   fcst    = Array(_ZEROS12,dtype=float,shape=(12,),iotype='in')
#   fcsb    = Array(_ZEROS12,dtype=float,shape=(12,),iotype='in')
#   est     = Array(_ZEROS12,dtype=float,shape=(12,),iotype='in')
#   esb     = Array(_ZEROS12,dtype=float,shape=(12,),iotype='in')
#   eft     = Array(_ZEROS12,dtype=float,shape=(12,),iotype='in')
#   efb     = Array(_ZEROS12,dtype=float,shape=(12,),iotype='in')
#   dst     = Array(_ZEROS12,dtype=float,shape=(12,),iotype='in')
#   dsb     = Array(_ZEROS12,dtype=float,shape=(12,),iotype='in')
#   dft     = Array(_ZEROS12,dtype=float,shape=(12,),iotype='in')
#   dfb     = Array(_ZEROS12,dtype=float,shape=(12,),iotype='in')
#   tmgt    = Array(_ZEROS12,dtype=float,shape=(12,),iotype='in')
#   tmgb    = Array(_ZEROS12,dtype=float,shape=(12,),iotype='in')
    ftst    = Float(iotype='in', desc='input as ftst(1) ')
    ftsb    = Float(iotype='in', desc='input as ftsb(1) ') 
    fcst    = Float(iotype='in', desc='input as fcst(1) ') 
    fcsb    = Float(iotype='in', desc='input as fcsb(1) ') 
    est     = Float(iotype='in', desc='input as est(1) ')
    esb     = Float(iotype='in', desc='input as esb(1) ') 
    eft     = Float(iotype='in', desc='input as eft(1) ')
    efb     = Float(iotype='in', desc='input as efb(1) ') 
    dst     = Float(iotype='in', desc='input as dst(1) ') 
    dsb     = Float(iotype='in', desc='input as dsb(1) ') 
    dft     = Float(iotype='in', desc='input as dft(1) ') 
    dfb     = Float(iotype='in', desc='input as dfb(1) ') 
    tmgt    = Float(iotype='in', desc='input as tmgt(1) ') 
    tmgb    = Float(iotype='in', desc='input as tmgb(1) ')
    kde     = Float(iotype='in', desc='Knock-down factor for modulus')
    kdf     = Float(iotype='in', desc='Knock-down factor for strength')
    
    # Geometric parameters
    # --------------------
    clbr1   = Float(iotype='in', desc='Fuselage break point as a fraction of total fuselage length')
    icyl    = Enum([1, 0], iotype='in', desc=' 1 - modeled with a mid-body cylinder, 0 - use two power-law bodies back to back')

    # Engines
    # --------------------
    neng    = Int(iotype='in', desc=' Total number of engines')
    nengwing= Int(iotype='in', desc=' Number of engines on wing')
    wfp     = Float(iotype='in', desc='(Engine Weight * NENG) / WGTO')
    clrw1   = Float(iotype='in', desc=' Location of first engine pair.  Input 0 for centerline engine.') 
    clrw2   = Float(iotype='in', desc=' Location of second engine pair.  measured from body centerline') 
    clrw3   = Float(iotype='in', desc=' Location of third engine pair.  measured from body centerline') 

    # Loads
    # --------------------
    deslf   = Float(iotype='in', desc='Design load factor')
    ultlf   = Float(iotype='in', desc='Ultimate load factor (usually 1.5*DESLF)')
    axac    = Float(iotype='in', desc='Axial acceleration')
    cman    = Float(iotype='in', desc=' Weight fraction at maneuver')
    iload   = Int(iotype='in', desc='1 - Analyze maneuver only,2 - Analyze maneuver and landing only')
    #pgt     = Array(_ZEROS12,dtype=float,shape=(12,),iotype='in')
    #pgb     = Array(_ZEROS12,dtype=float,shape=(12,),iotype='in')
    pgt     = Float(iotype='in', desc=' input as pgt(1)') 
    pgb     = Float(iotype='in', desc=' input as pgb(1)')
    wfbump  = Float(iotype='in', desc=' Weight fraction at bump')
    wfland  = Float(iotype='in', desc=' Weight fraction at landing')
    
    # Landing Gear
    # -------------------
    vsink   = Float(iotype='in', units='ft/s', desc='Design sink velocity at landing ')
    stroke  = Float(iotype='in', units='ft', desc=' Stroke of landing gear  ')
    clrg1   = Float(iotype='in', desc='Length fraction of nose landing gear measured as a fraction of total fuselage length') 
    clrg2   = Float(iotype='in', desc='Length fraction of main landing gear measured as a fraction of total fuselage length') 
    wfgr1   = Float(iotype='in', desc='Weight fraction of nose landing gear')
    wfgr2   = Float(iotype='in', desc='Weight fraction of main landing gear')
    igear   = Enum([1, 2], iotype='in', desc='1 - Main landing gear located on fuselage,2 - Main landing gear located on wing')
    gfrl    = Float(iotype='in', desc='Ratio of force taken by nose landing gear to force taken by main gear at landing')
    clrgw1  = Float(iotype='in', desc='Position of wing gear as a fraction of structural semispan')
    clrgw2  = Float(iotype='in', desc='Position of second pair wing gear as a fraction of structural semispan')

    # Weights
    # -------------------
    wgto    = Float(iotype='in', units='lb', desc=' Gross takeoff weight')
    wtff    = Float(iotype='in', desc='Weight fraction of fuel')
    cbum    = Float(iotype='in', desc='Weight fraction at bump')
    clan    = Float(iotype='in', desc='Weight fraction at landing')

    # Factors
    # --------------------
    ischrenk= Int(iotype='in', desc='1 - use Schrenk load distribution on wing,Else - use trapezoidal distribution')
    icomnd  = Enum([1, 2], iotype='in', desc='1 - print gross shell dimensions envelope,2 - print detailed shell geometry')
    wgno    = Float(iotype='in', desc='Nonoptimal factor for wing (including the secondary structure)')
    slfmb   = Float(iotype='in', desc='Static load factor for bumps')
    wmis    = Float(iotype='in', desc='Volume component of secondary structure')
    wsur    = Float(iotype='in', desc='Surface area component of secondary structure')
    wcw     = Float(iotype='in', desc='Factor in weight equation for nonoptimal weights')
    wca     = Float(iotype='in', desc='Factor in weight equation for nonoptimal weights')
    nwing   = Int(iotype='in', desc='Number of wing segments for analysis')

    
    def __init__(self, directory=''):
        """Constructor for the PdcylComp component"""

        super(PdcylComp, self).__init__(directory)

        # External Code public variables
        self.stdin = 'PDCYL.in'
        self.stdout = 'PDCYL.out'
        self.stderr = 'PDCYL.err'
        self.command = 'PDCYL'
        
        self.external_files = [
            FileMetadata(path=self.stdin, input=True),
            FileMetadata(path=self.stdout),
            FileMetadata(path=self.stderr),
        ]
        
    def execute(self):
        """Run PDCYL."""
        
        #Prepare the input file for PDCYL
        self.generate_input()
        
        #Run PDCYL via ExternalCode's execute function
        super(PdcylComp, self).execute()

        #Parse the outut file from PDCYL
        self.parse_output()
        
    def generate_input(self):
        """Creates the PDCYL custom input file."""
        
        pass
        
    def parse_output(self):
        """Parses the PCYL output file and extracts data."""
        
        pass
        

if __name__ == "__main__": # pragma: no cover         

    from openmdao.main.api import set_as_top
    
    my_comp = set_as_top(PdcylComp())
    my_comp.run()
