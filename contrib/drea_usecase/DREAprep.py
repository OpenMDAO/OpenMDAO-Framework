"""
    DREAprep.py - Allows for inputs to be passed to multiple components
"""

from openmdao.main.api import Component, Slot
from openmdao.main.datatypes.api import Float

from MEflows import MEflows

class DREAprep(Component):
    """ Passes input variables to output variables so they can be passed to multiple components. """

    # Variables from MEflows variable tree
    # -------------------------
    flow_out = Slot(MEflows)    
    
    # Input variables
    # -----------------------------
    alt = Float(0.0, iotype='in', units='ft', desc='Altitude')
    Mach = Float(0.0, iotype='in', desc='Freestream Mach number')
    dTs = Float(0.0, iotype='in', units='degR', desc='Delta in total temperature from standard day')
    PC = Float(100.0, iotype='in', desc='Engine power code')
    deltaPt = Float(0.0, iotype='in', units='lbf/ft**2', desc='Change in primary total pressure from baseline')
    deltaTt = Float(0.0, iotype='in', units='degR', desc='Change in primary total temperature from baseline')
    deltaM = Float(0.0, iotype='in', desc='Change in primary Mach number from baseline')
    gamma = Float(1.4, iotype='in', desc='Ratio of specific heats')
    
    # Output variables
    # -----------------------------
    #MPRI = Float(iotype='out', desc='Design Mach number of the primary nozzle')
    #PPRI = Float(iotype='out', units='lbf/ft**2', desc='Primary jet total pressure')
    #PSEC = Float(iotype='out', units='lbf/ft**2', desc='Secondary jet total pressure')
    #TPRI = Float(iotype='out', units='degR', desc='Primary jet total temperature')
    #TSEC = Float(iotype='out', units='degR', desc='Secondary jet total temperature')
    #Pstatic = Float(iotype='out', units='lbf/ft**2', desc='Freestream static pressure')

    def __init__(self):
        super(DREAprep,self).__init__()
        
        self.add('flow_out', MEflows())

    
    def execute(self):
        """ Pass variables from inputs to outputs """

        # Calculate freestream flow conditions based on regression of 
        # NPSS Ambient element and isentropic relationships
        # (regressions valid to 15,000 ft altitude)
        # --------------------------------------------------------------
        self.flow_out.Pstatic = (0.00000000655102*self.alt**2-0.000524157*self.alt+14.6889)*144
        Tstatic = (-0.00356559*self.alt+518.668)+self.dTs
        self.flow_out.sec.Pt = self.flow_out.Pstatic*(1+(self.gamma-1)/2*self.Mach**2)**(self.gamma/(self.gamma-1))
        self.flow_out.sec.Tt = Tstatic*(1+(self.gamma-1)/2*self.Mach**2)
        self.flow_out.sec.Mach = 0.5
        
        # Calculate primary flow conditions based on regression of an
        # NPSS model provided by Jon Seidel
        # (deltas can be applied to each regression)
        # --------------------------------------------------------------
        self.flow_out.pri.Pt = (1890.56567716063+1159.46218542673*self.Mach-0.0723607918787817*self.alt+20.1498381047601*self.PC+self.Mach**2*733.160417217374+self.Mach*self.alt*-0.0429594524792568+self.alt**2*0.0000013101980809133+self.Mach*self.PC*4.15624348452779+self.alt*self.PC*-0.00074491162440819+self.PC**2*0.0544282167543358)+self.deltaPt
        self.flow_out.pri.Tt = (650.074441611223+48.9268880551154*self.Mach+0.000461490510019391*self.alt+4.80356170478831*self.PC+self.Mach**2*195.739109744029+self.Mach*self.alt*0.000946845170794506+self.alt**2*-0.0000004886019754503+self.Mach*self.PC*-0.820921508833817+self.alt*self.PC*-0.0000319239880056947+self.PC**2*-0.00610646669887009)+self.deltaTt
        self.flow_out.pri.Mach = (0.0278253956300395+1.08027846320383*self.Mach-0.0000001216209649966*self.alt+0.0160466635733684*self.PC+self.Mach**2*-0.0746368646020014+self.Mach*self.alt*0.0000025326149842305+self.alt**2*-4.55813373388211e-11+self.Mach*self.PC*-0.00857252256331783+self.alt*self.PC*0.0000000077897757696+self.PC**2*-0.0000520093017579281)+self.deltaM
        
        
if __name__ == "__main__":
    MyComp = DREAprep()

    MyComp.dTs = 0.0

    MyComp.APRI = 12.0
    MyComp.AsAp = 1.75
    MyComp.AR = 3.0 
    MyComp.XMAR = 0.95
    
    MyComp.Mach = 0.0
    MyComp.alt = 0.0
    MyComp.PC = 90
    MyComp.deltaPt = 0.0
    MyComp.deltaTt = 0.0
    MyComp.deltaM = 0.0

    MyComp.run()
    print '\n\n'
    print '----------OUTPUTS----------'
    print 'APRI_DREA       : ',MyComp.APRI_DREA
    print 'ASEC_DREA       : ',MyComp.ASEC_DREA
    print 'AEXIT_DREA      : ',MyComp.AEXIT_DREA
    print 'Width           : ',MyComp.width
    print 'Pstatic         : ',MyComp.Pstatic
    print 'PSEC            : ',MyComp.PSEC
    print 'TSEC            : ',MyComp.TSEC
    print 'PPRI            : ',MyComp.PPRI
    print 'TPRI            : ',MyComp.TPRI
    print 'MPRI            : ',MyComp.MPRI
