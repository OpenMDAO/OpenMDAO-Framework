"""
    DREA_HSRnoise.py - Code to run a DREA followed by HSRnoise for M-E off-design analysis
"""

from openmdao.main.api import Assembly, Slot, Case, Instance
from openmdao.main.datatypes.api import Float, Enum
from openmdao.lib.casehandlers.api import ListCaseIterator, DBCaseRecorder
from openmdao.lib.drivers.api import CaseIteratorDriver
#from openmdao.lib.drivers.api import DOEdriver
#from openmdao.lib.doegenerators.api import FullFactorial
from DREA import DREA
from hsrnoise import HSRNOISE
from DREAprep import DREAprep
from geometry import Geometry
from ACDgen import ACDgen

class DREA_HSRnoise(Assembly):
    """Assembly to execute on DREA followed by HSRnoise."""
    
    geo = Instance(Geometry, iotype='in')
    alt = Float(0.0, iotype='in', units='ft', desc='Altitude')
    point = Enum(1, [1,2,3],  iotype='in', desc='Certification observer point')
    
    def __init__(self):
        """Creates an Assembly to run DREA and HSRnoise."""
        super(DREA_HSRnoise, self).__init__()
        
        FO1 = Case(inputs=[('point', 1),('dreaprep.Mach',0.28),('alt',2000.0),('dreaprep.PC',100.0),('hsrnoise.phi', 0.0)], outputs=[('drea.CFG',0.0),('hsrnoise.thetas',0.0),('hsrnoise.Freq',0.0),('hsrnoise.SPL_corr',0),('hsrnoise.OASPL30',0.0),('hsrnoise.OASPL60',0.0),('hsrnoise.OASPL90',0.0),('hsrnoise.OASPL120',0.0),('hsrnoise.OASPL150',0.0)])
        FO2 = Case(inputs=[('point', 1),('dreaprep.Mach',0.28),('alt',2000.0),('dreaprep.PC', 65.0),('hsrnoise.phi', 0.0)], outputs=[('drea.CFG',0.0),('hsrnoise.thetas',0.0),('hsrnoise.Freq',0.0),('hsrnoise.SPL_corr',0),('hsrnoise.OASPL30',0.0),('hsrnoise.OASPL60',0.0),('hsrnoise.OASPL90',0.0),('hsrnoise.OASPL120',0.0),('hsrnoise.OASPL150',0.0)])
        App = Case(inputs=[('point', 2),('dreaprep.Mach',0.20),('alt', 394.0),('dreaprep.PC', 30.0),('hsrnoise.phi', 0.0)], outputs=[('drea.CFG',0.0),('hsrnoise.thetas',0.0),('hsrnoise.Freq',0.0),('hsrnoise.SPL_corr',0),('hsrnoise.OASPL30',0.0),('hsrnoise.OASPL60',0.0),('hsrnoise.OASPL90',0.0),('hsrnoise.OASPL120',0.0),('hsrnoise.OASPL150',0.0)])
        SL1 = Case(inputs=[('point', 3),('dreaprep.Mach',0.25),('alt',1000.0),('dreaprep.PC',100.0),('hsrnoise.phi', 0.0)], outputs=[('drea.CFG',0.0),('hsrnoise.thetas',0.0),('hsrnoise.Freq',0.0),('hsrnoise.SPL_corr',0),('hsrnoise.OASPL30',0.0),('hsrnoise.OASPL60',0.0),('hsrnoise.OASPL90',0.0),('hsrnoise.OASPL120',0.0),('hsrnoise.OASPL150',0.0)])
        SL2 = Case(inputs=[('point', 3),('dreaprep.Mach',0.25),('alt',1000.0),('dreaprep.PC',100.0),('hsrnoise.phi',30.0)], outputs=[('drea.CFG',0.0),('hsrnoise.thetas',0.0),('hsrnoise.Freq',0.0),('hsrnoise.SPL_corr',0),('hsrnoise.OASPL30',0.0),('hsrnoise.OASPL60',0.0),('hsrnoise.OASPL90',0.0),('hsrnoise.OASPL120',0.0),('hsrnoise.OASPL150',0.0)])
        SL3 = Case(inputs=[('point', 3),('dreaprep.Mach',0.25),('alt',1000.0),('dreaprep.PC',100.0),('hsrnoise.phi',60.0)], outputs=[('drea.CFG',0.0),('hsrnoise.thetas',0.0),('hsrnoise.Freq',0.0),('hsrnoise.SPL_corr',0),('hsrnoise.OASPL30',0.0),('hsrnoise.OASPL60',0.0),('hsrnoise.OASPL90',0.0),('hsrnoise.OASPL120',0.0),('hsrnoise.OASPL150',0.0)])
        SL4 = Case(inputs=[('point', 3),('dreaprep.Mach',0.25),('alt',1000.0),('dreaprep.PC',100.0),('hsrnoise.phi',90.0)], outputs=[('drea.CFG',0.0),('hsrnoise.thetas',0.0),('hsrnoise.Freq',0.0),('hsrnoise.SPL_corr',0),('hsrnoise.OASPL30',0.0),('hsrnoise.OASPL60',0.0),('hsrnoise.OASPL90',0.0),('hsrnoise.OASPL120',0.0),('hsrnoise.OASPL150',0.0)])
        
        cases = ListCaseIterator([FO1,FO2,App,SL1,SL2,SL3,SL4])
        db_recorder = DBCaseRecorder()
        
        self.add('geo', Geometry())
        
        self.add('dreaprep', DREAprep())
        self.add('drea', DREA())
        self.add('hsrnoise', HSRNOISE())
        self.add('ACDgen', ACDgen())
        
        self.add('analysis',CaseIteratorDriver())
        self.analysis.iterator = cases
        self.analysis.recorders = [db_recorder]
        self.ACDgen.case_data = db_recorder.get_iterator()
        
        # Set up the workflows
        #---------------------------
        #self.analysis.workflow.add(['dreaprep', 'drea', 'hsrnoise'])
        #self.driver.workflow.add(['analysis','ACDgen'])
        
        self.driver.workflow.add(['dreaprep', 'drea', 'hsrnoise'])
                
        # Connections
        #---------------------------
        self.connect('geo',['drea.geo_in','hsrnoise.geo_in'])
        self.connect('alt',['dreaprep.alt','hsrnoise.ALTEVO'])
        self.connect('dreaprep.flow_out','drea.flow_in')
        self.connect('drea.flow_out','hsrnoise.flow_in')
        self.connect('drea.CFG','hsrnoise.CFG')
        
if __name__ == "__main__":
    asy = DREA_HSRnoise()

    import time
    tt = time.time()
    asy.drea.mode = 'Subsonic'
    asy.dreaprep.dTs = 18.3
    asy.dreaprep.Mach = 0.0
    asy.alt = 0.0
    asy.hsrnoise.phi = 90.0
    asy.dreaprep.PC = 80
    asy.dreaprep.deltaPt = 0.0
    asy.dreaprep.deltaTt = 0.0
    asy.dreaprep.deltaM = 0.0

    asy.geo.Apri = 12.0
    asy.geo.AsAp = 1.75
    asy.geo.AR = 3.0 
    asy.geo.AeAt = .95
    asy.geo.length = 8.0 
    asy.geo.ChuteAngles = 10
    asy.geo.calc_geom(asy.geo.length,asy.geo.Apri,asy.geo.AsAp,asy.geo.AR,asy.geo.AeAt,asy.geo.LhMh,asy.geo.LhWave)

    
    asy.run()
    print '\n\n'
    print 'alt             : ',asy.dreaprep.alt
    print 'Mach            : ',asy.dreaprep.Mach
    print 'HMIC            : ',asy.hsrnoise.HMIC
    print 'SL              : ',asy.hsrnoise.SL
    print '\n'
    print 'INPUTS            DREA       HSRNoise'
    print 'Length          : ',asy.drea.geo_in.length, '     ', asy.hsrnoise.geo_in.length
    print 'Width           : ',asy.drea.geo_in.width, '     ', asy.hsrnoise.geo_in.width
    print 'Primary Area    : ',asy.drea.geo_in.Apri, '     ', asy.hsrnoise.geo_in.Apri
    print 'Secondary Area  : ',asy.drea.geo_in.Asec, '     ', asy.hsrnoise.geo_in.Asec
    print 'Exit Area       : ',asy.drea.geo_in.Aexit, '     ', asy.hsrnoise.geo_in.Aexit
    print 'Primary Pressure: ',asy.drea.flow_in.pri.Pt,  '     ', asy.hsrnoise.flow_in.pri.Pt
    print 'Primary Temp    : ',asy.drea.flow_in.pri.Tt,  '     ', asy.hsrnoise.flow_in.pri.Tt
    print 'Primary Mach    : ',asy.drea.flow_in.pri.Mach,  '     ', asy.hsrnoise.flow_in.pri.Mach
    print '\n'
    print 'RESULTS'
    print 'Gross Thrust    : ',asy.drea.GrossThrust
    print 'Exit Velocity   : ',asy.drea.ExitVelocity
    print 'Exit Mach       : ',asy.drea.ExitMach
    print 'CFG:            : ',asy.drea.CFG
    print 'Pumping Ratio   : ',asy.drea.PumpingRatio
    print 'Mass Flow (Pri) : ',asy.drea.flow_out.pri.W
    print 'Mass Flow (Sec) : ',asy.drea.flow_out.sec.W
    print 'Secondary Mach  : ',asy.drea.flow_out.sec.Mach
    print 'NPR             : ',asy.drea.NPR
    print 'Degree of mixing: ',asy.drea.DegreeOfMixing
    print 'OASPL30         : ',asy.hsrnoise.OASPL30
    print 'OASPL60         : ',asy.hsrnoise.OASPL60
    print 'OASPL90         : ',asy.hsrnoise.OASPL90
    print 'OASPL120        : ',asy.hsrnoise.OASPL120
    print 'OASPL150        : ',asy.hsrnoise.OASPL150
    #print 'test: ', asy.hsrnoise.SPL_corr
    print "\n"
    print "Elapsed time: ", time.time()-tt, "seconds"

    

        


