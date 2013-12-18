"""
    DREA.py - Executes the Fortran code DREA
"""

import os

from numpy import array
from numpy import float as numpy_float

from openmdao.main.datatypes.api import Int, Float, Array, Enum, VarTree
from openmdao.lib.components.api import ExternalCode

from openmdao.util.namelist_util import Namelist

# The following will be used for file wrapping, see the next sections
from openmdao.main.api import FileMetadata
from openmdao.util.filewrap import FileParser
from geometry import Geometry
from MEflows import MEflows

class DREA(ExternalCode):
    """OpenMDAO component wrapper for DREA."""

    # Variables from MEflows and Geometry variable trees
    # -------------------------
    flow_in = VarTree(MEflows(), iotype='in')
    flow_out = VarTree(MEflows(), iotype='out')    
    geo_in = VarTree(Geometry(), iotype='in')
    geo_out = VarTree(Geometry(), iotype='out')
    
    #mode parameter used to replace ist and ifab in control.in
    # -------------------------
    mode = Enum("Auto", ["Auto", "Fabri", "Subsonic"], iotype='in', desc='Auto mode: try Fabri choke solution, if failed, try subsonic solution.'
    'Fabri mode: Fabri choke solution only, Subsonic mode: Subsonic solution only')

    # Variables for control.in
    # -------------------------
    icnvl = Enum(0, [0, 1], iotype='in', desc='Control variable; 0=inviscid and viscous mixing solutions, 1=inviscid (control volume) only')
    ieject = Enum(1, [0, 1], iotype='in', desc='Control variable; 0=mixer solution, 1=ejector solution')
    #ist = Enum(1, [0, 1], iotype='in', desc='Control variable; 0=subsonic solution, 1=supersonic solution')
    #ifab = Enum(1, [0, 1], iotype='in', desc='Control variable; 0=back pressure constrained solution, 1=Fabri choke solution')
    ispm = Enum(0, [0, 1], iotype='in', desc='Control variable; 0=direct solution, 1=iterative closure inlet static pressure matching')
    iprnt = Int(2, iotype='in', desc='Number of streamwise (x) station printer control, 2=print every station, etc.')
    ipw = Int(1, iotype='in', desc='Number of cross-stream (y) station printer control, 1=print every variable, etc.')
    nmax = Int(6, iotype='in', desc='Maximum number of summations used in analytical (Greens function) expansion for marching analytical/numerical decomposition')
    
    # Variables for flocond.in
    # -------------------------
    #p01d = Float(8467.2, iotype='in', units='lbf/ft**2', desc='Primary stream total pressure')
    #p02d = Float(2116.8, iotype='in', units='lbf/ft**2', desc='Secondary stream total pressure')
    #t01d = Float(1037.38, iotype='in', units='degR', desc='Primary stream total temperature')
    #t02d = Float(518.69, iotype='in', units='degR', desc='Secondary stream total temperature')
    #rm1 = Float(1.50, iotype='in', desc='Primary stream Mach number')
    #rm2 = Float(0.4, iotype='in', desc='Secondary stream Mach number')
    a1d = Float(6.00, units='ft**2', desc='Primary inlet stream cross-sectional area')
    a2d = Float(8.40, units='ft**2', desc='Secondary inlet stream cross-sectional area')
    a3d = Float(13.68, units='ft**2', desc='Exit plane cross-sectional area')
    rg = Float(1718., iotype='in', units='ft*lb/slug/degR', desc='Real gas constant (air) ((ft lb)/(slug deg R))')
    #gam = Float(1.4, iotype='in', desc='Specific heat ratio')
    #pinf = Float(2116.8, iotype='in', units='lbf/ft**2', desc='Ambient static pressure')
    rec1 = Float(1.0, iotype='in', desc='Primary stream nozzle pressure recovery')
    rec2 = Float(0.98, iotype='in', desc='Secondary stream nozzle pressure recovery')
    
    # Variables for expnd.in
    # -------------------------
    rm1s = Float(1.8, iotype='in', desc='Expanded primary stream Mach number')
    rm2s = Float(0.8, iotype='in', desc='Expanded secondary stream Mach number')
    dxe = Float(0.1, iotype='in', desc='Jacobian permutation for Broyden solver, approximately 0.1')
    relx = Float(1., iotype='in', desc='Relaxation constant (normally not used, set equal to 1.0)')
    errm = Float(1.E-6, iotype='in', desc='Maximum error in expand routines')
    nmx = Int(500, iotype='in', desc='Maximum number of iterations in Broyden solver')
    intt = Int(100, iotype='in', desc='Number of intervals chosen to search for static pressure constrained expansion problem')
    
    # Variables for zrdmix.in
    # -------------------------
    BWID = Float(6.0, units='ft', desc='Width of 2-d ejector mixing section')
    #RLD = Float(10.0, iotype='in', units='ft', desc='Length of mixing section')
    RLPRNT = Float(0.3333, iotype='in', units='ft', desc='Streamwise (x) location for cross-stream profile print out to file yprmw.out')
    PR = Float(1.E0, iotype='in', desc='Turbulent Prandtl Number')
    CGR = Float(1., iotype='in', desc='Streamwise (x) variable grid control parameter: CGR > 1 cluster points in near field, CGR < 1 cluster points in far field, and CGR= 1 constant grid spacing')
    REVRT = Float(2.0E0, iotype='in', desc='Circulation Reynolds Number')
    H0LM = Float(2.88, iotype='in', desc='Lobe height to wavelength ratio')
    H0HY = Float(0.90, iotype='in', desc='Lobe height to mixing section height (from centerline) ratio (chute penetration)')
    #ALP1 = Float(-10.0, iotype='in', desc='Primary flow angle off of mixing chutes')
    #ALP2 = Float(-10.0, iotype='in', desc='Secondary flow angle off of mixing chutes')
    IMAX = Int(5, iotype='in', desc='Number of streamwise (x) grid points')
    JMAX = Int(20, iotype='in', desc='Number of cross-stream (y) grid points, maximum=30')
    
    # Variables for hwall.in
    # -------------------------
    geom = Array(array([[0.0,2.4],[10.0,2.28]]), dtype=numpy_float, iotype='in', desc='x,y nozzle geometry coordinates')
    
    # Output variables
    # -------------------------
    GrossThrust = Float(iotype='out', units='lbf', desc='Overall gross thrust')
    ExitMassFlow = Float(iotype='out', units='lbm/s', desc='Exit mass flow')
    ExitVelocity = Float(iotype='out', units='ft/s', desc='Exit plane ideally mixed velocity')
    ExitMach = Float(iotype='out', desc='Exit plane ideally mixed Mach number')
    ExitStaticTemp = Float(iotype='out', units='degR', desc='Exit plane ideally mixed static temperature')
    ExitTotalTemp = Float(iotype='out', units='degR', desc='Exit plane ideally mixed total temperature')
    PumpingRatio = Float(iotype='out', desc='Entrainment ratio w2/w1')
    CFG = Float(iotype='out', desc='Gross thrust coefficient')
    PrimaryVelocity = Float(iotype='out', units='ft/s', desc='Primary nozzle velocity')
    SecondaryVelocity = Float(iotype='out', units='ft/s', desc='Secondary nozzle velocity')
    PrimaryMassFlow = Float(iotype='out', units='slug/s', desc='Primary nozzle mass flow')
    SecondaryMassFlow = Float(iotype='out', units='slug/s', desc='Secondary nozzle mass flow')
    SecondaryMach = Float(iotype='out', desc='Secondary nozzle Mach number')
    DegreeOfMixing = Float(iotype='out', desc='Degree of mixing in pressure constraint')
    NPR = Float(iotype='out', desc='Nozzle pressure ratio')

    def __init__(self):
        super(DREA,self).__init__()
        self.command = ['drea']
        
        self.add('geo_in', Geometry())
        self.add('geo_out', Geometry())
        self.add('flow_in', MEflows())
        self.add('flow_out', MEflows())
        
        self.ist = None
        self.ifab = None

        self.external_files = [
            FileMetadata(path='control.in', input=True),
            FileMetadata(path='flocond.in', input=True),
            FileMetadata(path='expnd.in', input=True),
            FileMetadata(path='zrdmix.in', input=True),
            FileMetadata(path='hwall.in', input=True),
            FileMetadata(path='ejectd.out'),
            FileMetadata(path=self.stderr),
        ]
    
    def _runDREA(self, FabriOrSub):
        self.generate_input(FabriOrSub)
        
        #Remove existing primary output file before execution
        if os.path.exists("ejectd.out"):
            os.remove("ejectd.out")
            
        #Execute the component
        #super(DREA, self).execute()

        #Parse output file
        self.parse_output(FabriOrSub)   
    
    def execute(self):
        """ Executes our file-wrapped component. """

        # Copy Flow parameters from flow_in to flow_out
        self.flow_out = self.flow_in.copy()
        self.geo_out = self.geo_in.copy()
        
        # Perform area calculations based on input AsAp, AeAt and AR
        # Note that DREA only uses half the area as it assumes a plane of symmetry
        self.a1d = self.geo_in.Apri/2 
        self.a2d = self.geo_in.AsAp*self.geo_in.Apri/2 
        self.a3d = self.geo_in.AeAt*(self.geo_in.Apri+self.geo_in.Asec)/2 
        self.BWID = (self.geo_in.AR*(self.geo_in.Apri+self.geo_in.Asec))**0.5
        
        #Prepare the input files for DREA
        if self.mode != 'Auto':
            self._runDREA(self.mode)
        
        else:
            #Try Fabri choke solution
            try: 
                self._runDREA('Fabri')
            except RuntimeError, err: 
               
                if "EJECTOR SOLUTION" in str(err): 
                    self._runDREA('Subsonic')
                else:
                    raise(err)
                
                
    def generate_input(self, FabriOrSub):
        """Creates the DREA input files."""
        
        # Determine ist, ifab and geometry parameters
        # -------------------------
        if FabriOrSub == 'Fabri':
            self.ist = 1
            self.ifab = 1
        if FabriOrSub == 'Subsonic':
            self.ist = 0
            self.ifab = 0
        self.geom[-1,0] = self.geo_in.length
        self.geom[0,1] = (self.geo_in.Apri/2+self.geo_in.Asec/2)/self.geo_in.width
        self.geom[-1,1] = self.geo_in.Aexit/2/self.geo_in.width
        
        # Create control.in
        # -------------------------
        crt = Namelist(self)
        crt.set_filename("control.in")
        crt.add_group('cntrl')
        crt.add_var("icnvl")
        crt.add_var("ieject")
        crt.add_var("ist")
        crt.add_var("ifab")
        crt.add_var("ispm")
        crt.add_var("iprnt")
        crt.add_var("ipw")
        crt.add_var("nmax")
        crt.generate()
        
        # Create flocond.in
        # -------------------------
        flw = Namelist(self)
        flw.set_filename("flocond.in")
        flw.add_group('floc')
        flw.add_newvar("p01d", self.flow_in.pri.Pt)
        flw.add_newvar("p02d", self.flow_in.sec.Pt)
        flw.add_newvar("t01d", self.flow_in.pri.Tt)
        flw.add_newvar("t02d", self.flow_in.sec.Tt)
        flw.add_newvar("rm1", self.flow_in.pri.Mach)
        flw.add_newvar("rm2", self.flow_in.sec.Mach)
        flw.add_var("a1d") 
        flw.add_var("a2d")
        flw.add_var("a3d")
        flw.add_var("rg")
        flw.add_newvar("gam", self.flow_in.gamma)
        flw.add_newvar("pinf", self.flow_in.Pstatic)
        flw.add_var("rec1")
        flw.add_var("rec2")
        flw.generate()
        
        # Create expnd.in
        # -------------------------
        exd = Namelist(self)
        exd.set_filename("expnd.in")
        exd.add_group('exd')
        exd.add_var("rm1s")
        exd.add_var("rm2s")
        exd.add_var("dxe")
        exd.add_var("relx"),
        exd.add_var("errm")
        exd.add_var("nmx")
        exd.add_var("intt")
        exd.generate()
        
        # Create zrdmix.in
        # -------------------------
        zrd = Namelist(self)
        zrd.set_filename("zrdmix.in")
        zrd.add_group('zrd')
        zrd.add_var("BWID")
        zrd.add_newvar("RLD", self.geo_in.length)
        zrd.add_var("RLPRNT")
        zrd.add_var("PR")
        zrd.add_var("CGR")
        zrd.add_var("REVRT")
        zrd.add_newvar("H0LM", self.geo_in.LhWave)
        zrd.add_newvar("H0HY", self.geo_in.LhMh)
        zrd.add_newvar("ALP1", self.geo_in.ChuteAngles)
        zrd.add_newvar("ALP2", self.geo_in.ChuteAngles)
        zrd.add_var("IMAX")
        zrd.add_var("JMAX")
        zrd.generate()
        
        # Create hwall.in
        # -------------------------
        geom_data = []
        form = "%.15f, %.15f, \n"
        
        geom_data.append("%d\n" % self.geom.shape[0])
        
        for element in self.geom:
            geom_data.append(form % (element[0], element[1]))
        
        outfile = open("hwall.in", 'w')
        outfile.writelines(geom_data)
        outfile.close()

    def parse_output(self,FabriOrSub):
        """Parses the DREA output file and extracts data."""
        
        infile = FileParser()
        infile.set_file('ejectd.out')
        infile.mark_anchor('EJECTOR SOLUTION')

        if FabriOrSub == 'Fabri':
            self.GrossThrust = infile.transfer_keyvar("SUPERSONIC GROSS THRUST (OVERALL)=", 1)
            self.ExitMassFlow = infile.transfer_keyvar("SUPERSONIC EXIT MASS FLOW RATE=", 1)
            self.ExitVelocity = infile.transfer_keyvar("SUP VELOCITY=", 1)
            self.ExitMach = infile.transfer_keyvar("SUPERSONIC MACH=", 1)
            self.ExitStaticTemp = infile.transfer_keyvar("SUP TEMPERATURE=", 1)
            self.ExitTotalTemp = infile.transfer_keyvar("SUPERSONIC TOTAL TEMPERATURE=", 1)
            self.CFG = infile.transfer_keyvar("SUPERSONIC CFG=", 1)
        if FabriOrSub == 'Subsonic':
            self.GrossThrust = infile.transfer_keyvar("SUBSONIC GROSS THRUST (OVERALL)=", 1)
            self.ExitMassFlow = infile.transfer_keyvar("SUBSONIC EXIT MASS FLOW RATE=", 1)
            self.ExitVelocity = infile.transfer_keyvar("SUB VELOCITY=", 1)
            self.ExitMach = infile.transfer_keyvar("SUBSONIC MACH=", 1)
            self.ExitStaticTemp = infile.transfer_keyvar("SUB TEMPERATURE=", 1)
            self.ExitTotalTemp = infile.transfer_keyvar("SUBSONIC TOTAL TEMPERATURE=", 1)
            self.CFG = infile.transfer_keyvar("SUBSONIC CFG=", 1)
            
        self.flow_out.pri.Vel = infile.transfer_keyvar("U1D=",1)
        self.flow_out.sec.Vel = infile.transfer_keyvar("U2D=",1)
        self.flow_out.pri.W = infile.transfer_keyvar("RMD1D=",1)*2 #Doubled since half of the area is used
        self.flow_out.sec.W = infile.transfer_keyvar("RMD2D=",1)*2 #Doubled since half of the area is used
        self.PumpingRatio = infile.transfer_keyvar("PUMPING RATIO W2/W1=",1)
        self.flow_out.sec.Mach = infile.transfer_keyvar("RM2=", 1)
        self.DegreeOfMixing = infile.transfer_keyvar("DEGREE OF MIXING IN PRESSURE CONSTRAINT",0,rowoffset=1)
        self.NPR = infile.transfer_keyvar("NPR=",1)
   
    def load_model(self):
        ct = Namelist(self)
        ct.set_filename('control.in')
        ct.parse_file()
        ct.load_model()

        fc = Namelist(self)
        fc.set_filename('flocond.in')
        fc.parse_file()
        fc.load_model()
        
        ep = Namelist(self)
        ep.set_filename('expnd.in')
        ep.parse_file()
        ep.load_model()
        
        zr = Namelist(self)
        zr.set_filename('zrdmix.in')
        zr.parse_file()
        zr.load_model()
        
if __name__ == "__main__":
    MyComp = DREA()
    MyComp.mode = 'Subsonic'
    MyComp.flow_in.pri.Pt = 6350.4
    MyComp.flow_in.sec.Pt = 2116.8
    MyComp.flow_in.pri.Tt = 648.36
    MyComp.flow_in.sec.Tt = 518.69
    MyComp.flow_in.pri.Mach = 1.3
    MyComp.flow_in.sec.Mach = .55

    MyComp.geo_in.Apri = 6.0*2
    MyComp.geo_in.Asec = 8.4*2
    MyComp.geo_in.Aexit = 13.68*2
    MyComp.geo_in.ChugeAngles = 5.0
    MyComp.run()
    
    print MyComp.GrossThrust
    print MyComp.ExitMassFlow
    print MyComp.ExitVelocity
    print MyComp.ExitMach
    print MyComp.ExitStaticTemp
    print MyComp.ExitTotalTemp
    print MyComp.CFG
    print MyComp.PumpingRatio
    print MyComp.geo_in.length
    
    print MyComp.flow_out.pri.Pt
    print MyComp.flow_out.sec.Pt
    print MyComp.flow_out.pri.Tt
    print MyComp.flow_out.sec.Tt
    print MyComp.flow_out.pri.Mach
    print MyComp.flow_out.sec.Mach
    print MyComp.flow_out.pri.Vel
    print MyComp.flow_out.sec.Vel
    print MyComp.flow_out.pri.W
    print MyComp.flow_out.sec.W
