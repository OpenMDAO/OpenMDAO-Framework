"""
OpenMDAO Wrapper for PDCYL.

PDCYL is a structural esimation code that was pulled from ACSYNT.
"""

# pylint: disable-msg=E0611,F0401
from openmdao.main.datatypes.api import Bool, Enum, Float, Int, Str
from openmdao.lib.components.api import ExternalCode
from openmdao.main.api import FileMetadata
from openmdao.util.filewrap import FileParser

class PdcylComp(ExternalCode):
    """ OpenMDAO component wrapper for PDCYL. """

    icalc   = Bool(False, iotype='in', desc='Print switch. Set to True for verbose output.')
    title   = Str("PDCYl Component", iotype='in', desc='Title of the analysis')
    
    # Wing geometry
    # --------------------
    wsweep  = Float(iotype='in', units='deg', desc='Wing sweep referenced to the leading edge')
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
    # These vars are listed in the pdcyl code, but they are never read in. Not sure 
    # what that's all about.
    #vbod    = Float(iotype='in', units='ft**3', desc='Fuselage total volume ')
    #volnose = Float(iotype='in', units='ft**3', desc='Nose Volume')                
    #voltail = Float(iotype='in', units='ft**3', desc='Tail volume ')
    
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
    kcont   = Enum([2, 3, 4, 5, 6, 8, 9], iotype='in', desc='Structural Geometry Concept Top')
    kconb   = Enum([2, 3, 4, 5, 6, 8, 9], iotype='in', desc='Structural Geometry Concept Bottom')

    # Material properties
    # -------------------
    ftst   = Float(iotype='in', desc="Tensile Strength on Top")
    ftsb   = Float(iotype='in', desc="Tensile Strength on Bottom")
    fcst   = Float(iotype='in', desc="Compressive Strength on Top")
    fcsb   = Float(iotype='in', desc="Compressive Strength on Bottom")
    est    = Float(iotype='in', desc="Young's Modulus for the shells Top")
    esb    = Float(iotype='in', desc="Young's Modulus for the shells Bottom")
    eft    = Float(iotype='in', desc="Young's Modulus for the frames Top")
    efb    = Float(iotype='in', desc="Young's Modulus for the frames Bottom")
    dst    = Float(iotype='in', desc="Density of shell material on Top")
    dsb    = Float(iotype='in', desc="Density of shell material on Bottom")
    dft    = Float(iotype='in', desc="Density of frame material on Top")
    dfb    = Float(iotype='in', desc="Density of frame material on Bottom")
    tmgt   = Float(iotype='in', desc="Minimum gage thickness Top")
    tmgb   = Float(iotype='in', desc="Minimum gage thickness Bottom")
    kde    = Float(iotype='in', desc="Knock-down factor for modulus")
    kdf    = Float(iotype='in', desc="Knock-down factor for strength")
    
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
    iload   = Enum([1, 2, 3], iotype='in', desc='1 - Analyze maneuver only; 2 - Analyze maneuver and landing only; 3 - Analyze bump, landing and maneuver')
    pgt     = Float(iotype='in', desc="Fuselage gage pressure on top")
    pgb     = Float(iotype='in', desc="Fuselage gage pressure on bottom")
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
    
    # Outputs
    # --------------------
    wfuselaget = Float(iotype='out', units='lb', desc='Total fuselage weight')
    wwingt     = Float(iotype='out', units='lb', desc='Total wing weight')
    

    
    def __init__(self):
        """Constructor for the PdcylComp component"""

        super(PdcylComp, self).__init__()

        # External Code public variables
        self.stdin = 'PDCYL.in'
        self.stdout = 'PDCYL.out'
        self.stderr = 'PDCYL.err'
        self.command = ['PDCYL']
        
        self.external_files = [
            FileMetadata(path=self.stdin, input=True),
            FileMetadata(path=self.stdout),
            FileMetadata(path=self.stderr),
        ]
        
        # Dictionary contains location of every numeric scalar variable
        fields = {}
        fields[8]   = 'wsweep'
        fields[9]   = 'war'
        fields[10]  = 'wtaper'
        fields[11]  = 'wtcroot'
        fields[12]  = 'wtctip'
        fields[13]  = 'warea'
        fields[15]  = 'ps'
        fields[16]  = 'tmgw'
        fields[17]  = 'effw'
        fields[18]  = 'effc'
        fields[19]  = 'esw'
        fields[20]  = 'fcsw'
        fields[21]  = 'dsw'
        fields[22]  = 'kdew'
        fields[23]  = 'kdfw'
        fields[25]  = 'istama'
        fields[27]  = 'cs1'
        fields[28]  = 'cs2'
        fields[29]  = 'uwwg'
        fields[30]  = 'xwloc1'
        fields[32]  = 'claqr'
        fields[33]  = 'ifuel'
        fields[35]  = 'cwman'
        fields[36]  = 'cf'
        fields[40]  = 'itail'
        fields[42]  = 'uwt'
        fields[43]  = 'clrt'
        fields[44]  = 'harea'
        fields[49]  = 'frn'
        fields[50]  = 'frab'
        fields[51]  = 'bodl'
        fields[52]  = 'bdmax'
        fields[54]  = 'ckf'
        fields[55]  = 'ec'
        fields[56]  = 'kgc'
        fields[57]  = 'kgw'
        fields[58]  = 'kcont'
        fields[59]  = 'kconb'
        fields[67]  = 'ftst'
        fields[68]  = 'ftsb'
        fields[69]  = 'fcst'
        fields[70]  = 'fcsb'
        fields[71]  = 'est'
        fields[72]  = 'esb'
        fields[73]  = 'eft'
        fields[74]  = 'efb'
        fields[75]  = 'dst'
        fields[76]  = 'dsb'
        fields[77]  = 'dft'
        fields[78]  = 'dfb'
        fields[79]  = 'tmgt'
        fields[80]  = 'tmgb'
        fields[81]  = 'kde'
        fields[82]  = 'kdf'
        fields[84]  = 'clbr1'
        fields[85]  = 'icyl'
        fields[90]  = 'neng'
        fields[91]  = 'nengwing'
        fields[92]  = 'wfp'
        fields[93]  = 'clrw1'
        fields[95]  = 'clrw2'
        fields[96]  = 'clrw3'
        fields[100] = 'deslf'
        fields[101] = 'ultlf'
        fields[102] = 'axac'
        fields[103] = 'cman'
        fields[104] = 'iload'
        fields[107] = 'pgt'
        fields[108] = 'pgb'
        fields[109] = 'wfbump'
        fields[110] = 'wfland'
        fields[114] = 'vsink'
        fields[115] = 'stroke'
        fields[116] = 'clrg1'
        fields[117] = 'clrg2'
        fields[118] = 'wfgr1'
        fields[119] = 'wfgr2'
        fields[120] = 'igear'
        fields[122] = 'gfrl'
        fields[123] = 'clrgw1'
        fields[124] = 'clrgw2'
        fields[129] = 'wgto'
        fields[130] = 'wtff'
        fields[131] = 'cbum'
        fields[132] = 'clan'
        fields[136] = 'ischrenk'
        fields[138] = 'icomnd'
        fields[140] = 'wgno'
        fields[141] = 'slfmb'
        fields[142] = 'wmis'
        fields[143] = 'wsur'
        fields[144] = 'wcw'
        fields[145] = 'wca'
        fields[146] = 'nwing'

        self._fields = fields
        
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
        
        data = []
        form = "%.15g %s\n"
        
        # It turns out to be simple and quick to generate a new input file each
        # time, rather than poking values into a template.
        
        data.append("\n\n")
        data.append(self.title)
        data.append("\n\n")
        
        if self.icalc == True:
            icalc = 3
        else:
            icalc = 0
            
        data.append("%d icalc print switch" % icalc)
        data.append("\n\n\n")

        data.append("Wing geometry:\n")
        for nline in range(8, 14):
            name = self._fields[nline]
            data.append(form % (self.get(name), name))
        
        data.append("Material properties:\n")
        for nline in range(15, 24):
            name = self._fields[nline]
            data.append(form % (self.get(name), name))
            
        data.append("Geometric properties:\n")
        name = self._fields[25]
        data.append(form % (self.get(name), name))
        data.append("\n")
        for nline in range(27, 31):
            name = self._fields[nline]
            data.append(form % (self.get(name), name))
            
        data.append("Structural concept:\n")
        for nline in range(32, 34):
            name = self._fields[nline]
            data.append(form % (self.get(name), name))
        data.append("\n")
        for nline in range(35, 37):
            name = self._fields[nline]
            data.append(form % (self.get(name), name))
        
        data.append("\n\n")
        data.append("Tails:\n")
        name = self._fields[40]
        data.append(form % (self.get(name), name))
        data.append("\n")
        for nline in range(42, 45):
            name = self._fields[nline]
            data.append(form % (self.get(name), name))
        
        data.append("\n\n\n")
        data.append("Fuselage geometry:\n")
        for nline in range(49, 53):
            name = self._fields[nline]
            data.append(form % (self.get(name), name))
            
        data.append("Structural concept:\n")
        for nline in range(54, 60):
            name = self._fields[nline]
            data.append(form % (self.get(name), name))
            
        data.append("\n\n\n\n\n\n")
        data.append("Material properties:\n")
        for nline in range(67, 83):
            name = self._fields[nline]
            data.append(form % (self.get(name), name))        
        
        data.append("Geometric parameters:\n")
        for nline in range(84, 86):
            name = self._fields[nline]
            data.append(form % (self.get(name), name))
            
        data.append("\n\n\n")
        data.append("Engines:\n")
        for nline in range(90, 94):
            name = self._fields[nline]
            data.append(form % (self.get(name), name))
        data.append("\n")
        for nline in range(95, 97):
            name = self._fields[nline]
            data.append(form % (self.get(name), name))
            
        data.append("\n\n")
        data.append("Loads:\n")
        for nline in range(100, 105):
            name = self._fields[nline]
            data.append(form % (self.get(name), name))
        data.append("\n\n")
        for nline in range(107, 111):
            name = self._fields[nline]
            data.append(form % (self.get(name), name))
            
        data.append("\n\n")
        data.append("Landing gear:\n")
        for nline in range(114, 121):
            name = self._fields[nline]
            data.append(form % (self.get(name), name))
        data.append("\n\n")
        for nline in range(122, 125):
            name = self._fields[nline]
            data.append(form % (self.get(name), name))

        data.append("\n\n\n")
        data.append("Weights:\n")
        for nline in range(129, 133):
            name = self._fields[nline]
            data.append(form % (self.get(name), name))
        
        data.append("\n\n")
        data.append("Factors:\n")
        name = self._fields[136]
        data.append(form % (self.get(name), name))
        data.append("\n")
        name = self._fields[138]
        data.append(form % (self.get(name), name))
        data.append("\n")
        for nline in range(140, 147):
            name = self._fields[nline]
            data.append(form % (self.get(name), name))
        
        outfile = open(self.stdin, 'w')
        outfile.writelines(data)
        outfile.close()

        
    def parse_output(self):
        """Parses the PCYL output file and extracts data."""
        
        infile = FileParser()
        infile.set_file(self.stdout)
        
        self.wwingt = infile.transfer_keyvar("Total Wing Structural Weight", 1)
        self.wfuselaget = infile.transfer_keyvar("Fuselage Total Structural Weight", 1)
        
    def load_model(self, filename):
        """Reads in an existing PDCYL input file and populates the variable
        tree with its values."""
        
        infile = FileParser()
        infile.set_file(filename)
        
        # Title is a string
        self.title = infile.transfer_line(2)

        # Print flag becomes a Bool
        if infile.transfer_var(4, 1) == 3:
            self.icalc = True
        else:
            self.icalc = False
        
        # Named variables in dictionary
        for key, val in self._fields.iteritems():
            self.set(val, infile.transfer_var(key, 1))

if __name__ == "__main__": # pragma: no cover         

    my_comp = PdcylComp()
    my_comp.run()
