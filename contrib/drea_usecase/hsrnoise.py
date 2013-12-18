"""
    hsrnoise.py - Executes the Fortran code HSRNOISE
"""

import os
from glob import glob

from math import pi
from numpy import zeros, sqrt, cos, sin, radians, log10
from numpy import float as numpy_float

from openmdao.main.datatypes.api import Int, Float, Array, Enum, VarTree
from openmdao.lib.components.api import ExternalCode

from openmdao.units import add_unit
#from openmdao.util.namelist_util import Namelist

# The following will be used for file wrapping, see the next sections
from openmdao.util.filewrap import InputFileGenerator, FileParser
from geometry import Geometry
from MEflows import MEflows

class HSRNOISE(ExternalCode):
    """OpenMDAO component wrapper for HSRNOISE."""
        
    add_unit('dB', 'kg**0', 'decibel')

    # Variables from MEflows and Geometry variable trees
    # -------------------------
    flow_in = VarTree(MEflows(), iotype='in')
    geo_in = VarTree(Geometry(), iotype='in')
    
    LinFrac = Float(0.9, iotype='in', desc='Fraction of ejector length covered by acoustic liner')
    phi = Float(0.0, iotype='in', units='deg', desc='Roll angle at which noise is estimated')
    
    # Variables GEOM namelist
    # -----------------------------
    HMIC = Float(4.0, iotype='in', units='ft', desc='Height of microphone above ground level')
    SL = Float(0.0, iotype='in', units='ft', desc='Sideline distance')
    
    # Variables FLIPATH namelist
    # -----------------------------
    ALTEVO = Float(700.0, iotype='in', units='ft', desc='Aircraft altitude overhead')
    FPA = Float(0.0, iotype='in', units='deg', desc='Flight path angle, positive for climb')
    PAE = Float(0.0, iotype='in', units='deg', desc='Angle between engine axis and horizontal direction, positive for inlet up')
    VAIR = Float(0.0, iotype='in', units='ft/s', desc='Aircraft velocity')
    
    # Variables SOURCE namelist
    # -----------------------------
    JETMETHOD = Enum(1, [1, 2], iotype='in', desc='Jet method flag; 1=Boeing JN8C4 Method, 2=Stone Method')
    NENG = Int(1, iotype='in', desc='Number of engines')
    
    # Variables for JET1IN namelist
    # -----------------------------
    #APRI = Float(8.65620, iotype='in', units='ft**2', desc='Primary nozzle exit area')
    #ASEC = Float(16.832, iotype='in', units='ft**2', desc='Secondary nozzle exit area')
    ATHP = Float(8.5466, iotype='in', units='ft**2', desc='Throat area of the primary nozzle')
    CFG = Float(0.95, iotype='in', desc='Mixer-ejector thrust coefficient')
    DELMIX = Float(0, iotype='in', units='dB', desc='Mixed jet suppression')
    DELPE = Float(0, iotype='in', units='dB', desc='Primary external jet suppression')
    DELPI = Float(0, iotype='in', units='dB', desc='Primary internal jet suppression')
    #DIVANG = Float(0, iotype='in', units='deg', desc='Spoke divergence angle. Outer tangent line relative to ejector flow surface at nozzle downstream of mixer exit')
    #EJASP = Float(1.5, iotype='in', desc='Mixer-ejector aspect ratio major/minor axis')
    EJD = Float(5.274, iotype='in', units='ft', desc='Equivalent inner diameter of the ejector at the mixer exit')
    #EJL = Float(13.3, iotype='in', units='ft', desc='Ejector length')
    #EJLIN = Float(9.0, iotype='in', units='ft', desc='Length of ejector lining')
    FLIN = Float(2000, iotype='in', units='Hz', desc='Liner design center frequency')
    #PEN = Float(0.925, iotype='in', desc='Mixer spoke penetration to nozzle full height ratio')
    PLUGD = Float(0, iotype='in', units='ft', desc='Plug diameter at nozzle exit station')
    PSI0 = Float(0, iotype='in', units='deg', desc='Rectangular exit plane major axis roll angle relative to wing plane')
    SPOKE = Float(18.0, iotype='in', desc='Number of spokes or lobes of the mixer')
    #TPRI = Float(1553.0, iotype='in', units='degR', desc='Primary jet total temperature')
    #TSEC = Float(529.0, iotype='in', units='degR', desc='Secondary jet total temperature')
    #VPRI = Float(2378.0, iotype='in', units='ft/s', desc='Primary jet fully expanded velocity')
    #VSEC = Float(449.0, iotype='in', units='ft/s', desc='Secondary jet velocity')
    #WPRI = Float(811.3, iotype='in', units='lbm/s', desc='Primary jet mass flow rate')
    #WSEC = Float(566.9, iotype='in', units='lbm/s', desc='Secondary jet mass flow rate')
    #XMAR = Float(0.97, iotype='in', desc='Ejector exit area to mixing plane area ratio')
    
    # Variables for JET2IN namelist
    # -----------------------------
    APT = Float(8.5466, iotype='in', units='ft**2', desc='Throat area of the primary nozzle')
    AS = Float(16.832, iotype='in', units='ft**2', desc='Secondary nozzle flow area')
    CER = Float(iotype='in', desc='Core expansion ratio')
    DHP = Float(0.5, iotype='in', units='ft', desc='Hydrolic diameter of primary flow(DHP = 4*Area/Perimeter)')
    DL = Float(0.7, iotype='dB/ft', desc='Peak perforate suppression at FPK')
    DM = Float(0, iotype='in', desc='Degree of mixing (calculated if not supplied)')
    FPK = Float(1995, iotype='in', units='Hz', desc='Peak frequency of broad-band suppression')
    GAMMAC = Float(0., iotype='in', desc='Specific heat ratio of primary stream. Internally calculated if set to 0')
    HEX = Float(4.0, iotype='in', units='ft', desc='Ejector height at nozzle')
    IEX = Enum(0, [0, 1], iotype='in', desc='Mixed jet parameter flag for TEX and VEX; 0=calculate internally, 1=use input values')
    ISUPPR = Enum(0, [0, 1], iotype='in', desc='Suppression flag; 0=negate suppression of internal noise sources, 1=suppress internal noise sources')
    LBE = Float(10.0, iotype='in', units='ft', desc='Length to end of bulk treatment from primary exit')
    LBS = Float(0.0, iotype='in', units='ft', desc='Length to start of bulk treatment from primary exit')
    LE = Float(10.0, iotype='in', units='ft', desc='Ejector length')
    LPE = Float(0.0, iotype='in', units='ft', desc='Length to end of perforate treatment from primary exit. For perforate over bultk treatment, add bulk length to perforate')
    LPS = Float(0.0, iotype='in', units='ft', desc='Length to start of perforate treatment from primary exit')
    MMC = Float(0.0, iotype='in', desc='Molecular mass (weight) of primary stream')
    MPD = Float(1.2, iotype='in', desc='Design Mach number of primary nozzle')
    PC = Float(iotype='in', units='lbf/ft**2', desc='Total pressure of primary stream upstream of nozzle')
    PEN = Float(0.925, iotype='in', desc='Mixer spoke penetration to nozzle full height ratio')
    SAR = Float(iotype='in', desc='Ratio of total mixing area to primary nozzle throat')
    SUPPK = Float(1.6628, iotype='in', units='dB/ft', desc='Peak bulk suppression')
    TC = Float(iotype='in', units='degR', desc='Total temperature of primary stream upstream of nozzle')
    TEX = Float(iotype='in', units='degR', desc='Mixed exit total temperature')
    VEX = Float(iotype='in', units='ft/s', desc='Mixed exit velocity')
    WEX = Float(6.0, units='ft', desc='Ejector width at nozzle exit')
    WSWP = Float(iotype='in', desc='Pumping ratio')
    
    # Output variables
    # -----------------------------
    TotalEPNL = Float(iotype='out', units='dB', desc='EPNL for the total aircraft')
    TotalMaxPNLT = Float(iotype='out', units='dB', desc='Maximum PNLT for the total aircraft')
    JetEPNL = Float(iotype='out', units='dB', desc='EPNL from the jet only')
    OASPL =  Array(zeros((0,17)),iotype='out', desc='OASPL values for total jet noise')
    OASPL30 = Float(iotype='out', units='dB', desc='OASPL from the jet only at emission angle 30')
    OASPL60 = Float(iotype='out', units='dB', desc='OASPL from the jet only at emission angle 60')
    OASPL90 = Float(iotype='out', units='dB', desc='OASPL from the jet only at emission angle 90')
    OASPL120 = Float(iotype='out', units='dB', desc='OASPL from the jet only at emission angle 120')
    OASPL150 = Float(iotype='out', units='dB', desc='OASPL from the jet only at emission angle 150')
    thetas = Array(zeros((0,17)), dtype=numpy_float, iotype='out', desc='Yaw angles')
    corr = Array(zeros((0,17)), dtype=numpy_float, iotype='out', desc='Correction for spherical spreading')
    SPL = Array(zeros((24,17)), dtype=numpy_float, iotype='out', desc='SPL values produced by HSRNoise')
    SPL_corr = Array(zeros((24,17)), dtype=numpy_float, iotype='out', desc='SPL values after being corrected back to a 1 ft lossless arc')
    Freq = Array(zeros((24,0)), dtype=numpy_float, iotype='out', desc='Frequencies')
    
    def __init__(self):
        super(HSRNOISE,self).__init__()
        self.command = ['hsrnoise', 'test.input', 'test.output']

        self.add('geo_in', Geometry())
        self.add('flow_in', MEflows())

    def execute(self):
        """ Executes our file-wrapped component. """

        self.ATHP = self.geo_in.Apri/(((self.flow_in.gamma+1)/2)**((-self.flow_in.gamma-1)/(2*(self.flow_in.gamma-1)))*(1+(self.flow_in.gamma-1)/2*self.flow_in.pri.Mach**2)**((self.flow_in.gamma+1)/(2*(self.flow_in.gamma-1)))/self.flow_in.pri.Mach)
        self.EJD = 2*(self.geo_in.Aexit/pi)**0.5
        self.SPOKE = self.geo_in.Num_Lobes
        self.HMIC = self.ALTEVO-10*cos(radians(self.phi))
        self.SL = 10*sin(radians(self.phi))
        
        #Prepare the input file for HSRNOISE
        self.generate_input()
        
        #Execute the component
        #super(HSRNOISE, self).execute()

        #Parse HSRNOISE output file
        self.parse_output()
        
        #Delete .plt files produced by HSRnoise
        for filename in glob('*.plt'):
            os.remove(filename)
        
        #Post process the data produced by HSRNoise for use in ANOPP
        self.dist = sqrt((self.ALTEVO-self.HMIC)**2+self.SL**2)/sin(radians(self.thetas))
        self.corr = 20*log10(self.dist)
        self.SPL_corr = self.SPL + self.corr
        
    def generate_input(self):
        """Creates the HSRNOISE input file."""
        
        self.EJLIN = self.LinFrac*self.geo_in.length
        
        parser = InputFileGenerator()
        parser.set_template_file('test_temp.input')
        parser.set_generated_file('test.input')
        
        # Set Geometry and Flight Conditions
        # --------------------------------------
        parser.mark_anchor("$GEOM")
        parser.transfer_var(self.HMIC, 1, 3)
        parser.transfer_var(self.SL, 2, 3)
        parser.mark_anchor("$FLIPATH")
        parser.transfer_var(self.ALTEVO, 1, 3)
        
        
        parser.mark_anchor("JETMETHOD")
        parser.transfer_var(self.JETMETHOD, 0,3)
                
        # Replace JET1IN variables in test.input
        # --------------------------------------
        parser.mark_anchor("$JET1IN")
        parser.transfer_var(self.geo_in.Apri,    1, 3) #APRI
        parser.transfer_var(self.geo_in.Asec,    2, 3) #ASEC
        parser.transfer_var(self.ATHP,    3, 3)
        parser.transfer_var(self.CFG,     4, 3)
        parser.transfer_var(self.DELMIX,  5, 3)
        parser.transfer_var(self.DELPE,   6, 3)
        parser.transfer_var(self.DELPI,   7, 3)
        parser.transfer_var(self.geo_in.ChuteAngles,  8, 3) #DIVANG
        parser.transfer_var(self.geo_in.AR,   9, 3) #EJASP
        parser.transfer_var(self.EJD,    10, 3)
        parser.transfer_var(self.geo_in.length,    11, 3) # EJL
        parser.transfer_var(self.EJLIN,  12, 3)
        parser.transfer_var(self.FLIN,   13, 3)
        parser.transfer_var(self.geo_in.LhMh,    14, 3) #PEN
        parser.transfer_var(self.PLUGD,  15, 3)
        parser.transfer_var(self.PSI0,   16, 3)
        parser.transfer_var(self.SPOKE,  17, 3)
        parser.transfer_var(self.flow_in.pri.Tt,   18, 3) #TPRI
        parser.transfer_var(self.flow_in.sec.Tt,   19, 3) #TSEC
        parser.transfer_var(self.flow_in.pri.Vel,   20, 3) #VPRI
        parser.transfer_var(self.flow_in.sec.Vel,   21, 3) #VSEC
        parser.transfer_var(self.flow_in.pri.W,   22, 3) #WPRI
        parser.transfer_var(self.flow_in.sec.W,   23, 3) #WSEC
        parser.transfer_var(self.geo_in.AeAt,   24, 3) #XMAR
        
        # Replace JET2IN variables in test.input
        # --------------------------------------
        parser.mark_anchor("$JET2IN")
        parser.transfer_var(self.APT,     1, 3)
        parser.transfer_var(self.AS,      2, 3)
        parser.transfer_var(self.CER,     3, 3)
        parser.transfer_var(self.DHP,     4, 3)
        parser.transfer_var(self.DL,      5, 3)
        parser.transfer_var(self.DM,      6, 3)
        parser.transfer_var(self.FPK,     7, 3)
        parser.transfer_var(self.GAMMAC,  8, 3)
        parser.transfer_var(self.HEX,     9, 3)
        parser.transfer_var(self.IEX,    10, 3)
        parser.transfer_var(self.ISUPPR, 11, 3)
        parser.transfer_var(self.LBE,    12, 3)
        parser.transfer_var(self.LBS,    13, 3)
        parser.transfer_var(self.LE,     14, 3)
        parser.transfer_var(self.LPE,    15, 3)
        parser.transfer_var(self.LPS,    16, 3)
        parser.transfer_var(self.MMC,    17, 3)
        parser.transfer_var(self.MPD,    18, 3)
        parser.transfer_var(self.PC,     19, 3)
        parser.transfer_var(self.PEN,    20, 3)
        parser.transfer_var(self.SAR,    21, 3)
        parser.transfer_var(self.SUPPK,  22, 3)
        parser.transfer_var(self.TC,     23, 3)
        parser.transfer_var(self.TEX,    24, 3)
        parser.transfer_var(self.VEX,    25, 3)
        parser.transfer_var(self.WEX,    26, 3)
        parser.transfer_var(self.WSWP,   27, 3)
        
        parser.generate()
        
    def parse_output(self):
        """Parses the HSRNOISE output file and extracts data."""
        
        outfile = FileParser()
        outfile.set_file('test.output')
        
        outfile.mark_anchor("JN8C4 JET NOISE MODULE")
        outfile.mark_anchor("TOTAL")
        self.thetas = outfile.transfer_array(4,2,4,18)
        self.Freq = outfile.transfer_2Darray(7,1,30,1)        
        self.SPL = outfile.transfer_2Darray(7,2,30,18)
        outfile.mark_anchor("DBA")
        self.OASPL = outfile.transfer_array(-1, 2, -1, 18)
        self.OASPL30 = outfile.transfer_var(-1, 4)
        self.OASPL60 = outfile.transfer_var(-1, 7)
        self.OASPL90 = outfile.transfer_var(-1, 10)
        self.OASPL120 = outfile.transfer_var(-1, 13)
        self.OASPL150 = outfile.transfer_var(-1, 16)

        outfile.mark_anchor("EPNL SUMMARY")
        self.TotalEPNL = outfile.transfer_var(9, 2)
        self.TotalMaxPNLT = outfile.transfer_var(9, 5)
        self.JetEPNL = outfile.transfer_var(8, 2)
    
    def load_model(self):
        """Loads an existing HSRNOISE input file."""
        
if __name__ == "__main__":
    MyComp = HSRNOISE()
    MyComp.ALTEVO = 10.0
    MyComp.HMIC = 0.0
    MyComp.SL = 0.0
    MyComp.JETMETHOD = 1
    MyComp.geo_in.Apri = 8.65620
    MyComp.geo_in.Asec = 16.832
    MyComp.geo_in.ChuteAngles = 0
    MyComp.geo_in.AR = 1.5
    MyComp.geo_in.length = 13.3
    MyComp.geo_in.LhMh = 0.925
    MyComp.flow_in.pri.Tt = 1553.0
    MyComp.flow_in.sec.Tt = 529.0
    MyComp.flow_in.pri.Vel = 2378.0
    MyComp.flow_in.sec.Vel = 449.0
    MyComp.flow_in.pri.W = 811.3
    MyComp.flow_in.sec.W = 566.9
    MyComp.geo_in.AeAt = 0.97
    MyComp.flow_in.gamma = 1.4
    MyComp.flow_in.pri.Mach = 1.3
    
    MyComp.run()
    print MyComp.TotalEPNL
    print MyComp.TotalMaxPNLT
    print MyComp.JetEPNL
    print MyComp.thetas
    print MyComp.SPL
    print MyComp.dist
    print MyComp.corr
    print MyComp.SPL_corr
