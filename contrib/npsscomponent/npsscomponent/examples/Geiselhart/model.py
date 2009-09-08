"""
PARTIAL! Port of Karl's ModelCenter SBJ propulsion process model.

It's a fairly complex example of running multiple linked components
in sequence.  Each NPSS component is run in it's own subdirectory,
which is where the NPSS output is written.  The log file also has some
information written to it in addition to what's shown on screen.

Known problems:
    - The components run, but since the port is only partial,
      the results are quite wrong.
"""
import os.path

from enthought.traits.api import Array, Float
from openmdao.main.api import Assembly, Component, Container, FileTrait, \
                              set_as_top

from npsscomponent import NPSScomponent


# pylint: disable-msg=E1101
# "Instance of <class> has no <attr> member"


class Design(Component):
    """ Design variables. """

    FanPRdes = Float(3.615, iostatus='out')
    TOCThrust = Float(7000., iostatus='out')
    MN = Float(1.8, iostatus='out')
    alt = Float(53700., iostatus='out')
    Knoz = Float(0.88, iostatus='out')
    extractionRatio = Float(1.05, iostatus='out')
    HpcPRdes = Float(6.51937, iostatus='out')
    Cfg = Float(0.985, iostatus='out')
    maxDiamFact = Float(1.17, iostatus='out')
    I_externalFact = Float(0.48, iostatus='out')
    I_divergFact = Float(0.5, iostatus='out')
    cowl_angle = Float(6., iostatus='out')
        
    def execute(self):
        """ Just to trace execution. """
        print self.get_pathname(), 'execution begins'
        super(Design, self).execute()
        print self.get_pathname(), '    complete'


class PropulsionData(Component):
    """ Computed propulsion data. """

    link_bladeTipRadius = Float(0., iostatus='in')
    link_contRingRadialThickness = Float(0., iostatus='in')
    link_maxDiamFact = Float(0., iostatus='in')
    link_Acapture = Float(0., iostatus='in')
    link_inletLength = Float(0., iostatus='in')
    link_length = Float(0., iostatus='in')

    Acapture = Float(0., iostatus='out')

    def __init__(self):
        super(PropulsionData, self).__init__()

        self.add_container('FLOPS', FLOPSdata())
        self.add_container('Plume', PlumeData())
        self.add_container('USM3D', USM3Ddata())
        self.add_container('Nacelle', NacelleData())
        self.add_container('ANOPP', ANOPPdata())

    def execute(self):
        """ Evaluate link expressions. """
        print self.get_pathname(), 'execution begins'

#       PropulsionData.FLOPS.dnac =
#           2*(NPSS_WATE.engine.WATE.WATE_fan.bladeTipRadius
#              + NPSS_WATE.engine.WATE.WATE_fan.contRingRadialThickness/12)
#           * Design.maxDiamFact
        self.FLOPS.dnac = \
           2*(self.link_bladeTipRadius \
              + self.link_contRingRadialThickness/12) \
           * self.link_maxDiamFact

#       PropulsionData.Acapture =
#           NPSS_ADP.engine.inlet.S_install.Acapture / 144.
        self.Acapture = \
            self.link_Acapture / 144.

#       PropulsionData.FLOPS.xnac =
#           (NPSS_WATE.engine.WATE.WATE_inlet.length
#            + NPSS_WATE.engine.WATE.length) / 12.
        self.FLOPS.xnac = \
            (self.link_inletLength \
             + self.link_length) / 12.

        print self.get_pathname(), '    complete'


class FLOPSdata(Container):

    engdeck = FileTrait(iostatus='in')
    thrso = Float(0., iostatus='in')
    weng = Float(0., iostatus='in')
    xnac = Float(0., iostatus='in')
    dnac = Float(0., iostatus='in')
        
    def __init__(self):
        super(FLOPSdata, self).__init__()
        self.engdeck.filename = 'engdeck'


class PlumeData(Container):

    m_dot = Float(0., iostatus='in')
    Ptj = Float(0., iostatus='in')
    Ttj = Float(0., iostatus='in')
    far = Float(0., iostatus='in')
    Astar = Float(0., iostatus='in')
    Ae = Float(0., iostatus='in')
    Amax = Float(0., iostatus='in')
    I_diverg = Float(0., iostatus='in')
    I_external = Float(0., iostatus='in')
    

class USM3Ddata(Container):

    fuel = Float(0., iostatus='in')
    gammaj = Float(0., iostatus='in')
    pjet = Float(0., iostatus='in')
    p0jet = Float(0., iostatus='in')
    Rratio = Float(0., iostatus='in')
    T0jet = Float(0., iostatus='in')
    
    def __init__(self):
        super(USM3Ddata, self).__init__()
        self.add_container('Inputs', USM3Dinputs())


class USM3Dinputs(Container):

    def __init__(self):
        super(USM3Dinputs, self).__init__()

        self.add_container('Freestream', USM3Dfreestream())
        self.add_container('Plenum', USM3Dplenum())
        self.add_container('Throat', USM3Dthroat())
        self.add_container('Exit', USM3Dexit())


class USM3Dfreestream(Container):

    a = Float(0., iostatus='in')
    gamma = Float(0., iostatus='in')
    rho = Float(0., iostatus='in')
    R = Float(0., iostatus='in')
    Ps = Float(0., iostatus='in')
    Pt = Float(0., iostatus='in')
    Ts = Float(0., iostatus='in')
    Tt = Float(0., iostatus='in')
    


class USM3Dplenum(Container):

    gamma_s = Float(0., iostatus='in')
    gamma_t = Float(0., iostatus='in')
    Rt = Float(0., iostatus='in')
    Rs = Float(0., iostatus='in')
    Ps = Float(0., iostatus='in')
    Pt = Float(0., iostatus='in')
    Ts = Float(0., iostatus='in')
    Tt = Float(0., iostatus='in')
    


class USM3Dthroat(Container):

    gamma_s = Float(0., iostatus='in')
    gamma_t = Float(0., iostatus='in')
    Rt = Float(0., iostatus='in')
    Rs = Float(0., iostatus='in')
    Ps = Float(0., iostatus='in')
    Pt = Float(0., iostatus='in')
    Ts = Float(0., iostatus='in')
    Tt = Float(0., iostatus='in')
    

class USM3Dexit(Container):

    gamma_s = Float(0., iostatus='in')
    gamma_t = Float(0., iostatus='in')
    Rt = Float(0., iostatus='in')
    Rs = Float(0., iostatus='in')
    Ps = Float(0., iostatus='in')
    Pt = Float(0., iostatus='in')
    Ts = Float(0., iostatus='in')
    Tt = Float(0., iostatus='in')
    


class NacelleData(Container):

    X = Array('d', shape=(None,), value=[], iostatus='in')
    Y = Array('d', shape=(None,), value=[], iostatus='in')
    


class ANOPPdata(Container):
    pass


class TracingNPSS(NPSScomponent):
    """ Simply overrides execute() to trace start & stop times. """

    def execute(self):
        print self.get_pathname(), 'execution begins'
        super(TracingNPSS, self).execute()
        print self.get_pathname(), '    complete'


class SBJ_Propulsion(Assembly):
    """ SBJ propulsion model. """

    def __init__(self, *args, **kwargs):
        super(SBJ_Propulsion, self).__init__(*args, **kwargs)
        self.external_files.append({'path':'README.txt', 'constant':True})

        model_dir = os.path.join('..', 'Full_Model', 'Cycle', 'run')
        includes = [
            '-I', '../Full_Model/Components',
            '-I', '../Full_Model/Cycle',
            '-I', '../Full_Model/Cycle/view',
            '-I', '../Full_Model/Cycle/src',
            '-I', '../Full_Model/Cycle/maps',
            '-I', '../Full_Model/Cycle/run',
            '-I', '../Full_Model/ROSE',
            '-I', '../Full_Model/ROSE/BaseClasses']

        # Design variables.
        self.add_container('Design', Design())

        # ADP.
        arglist = []
        arglist.extend(includes)
        arglist.append(os.path.join(model_dir, 'MC_ADP.mdl'))
        self.add_container('NPSS_ADP', 
                           TracingNPSS(directory='NPSS_ADP', arglist=arglist, 
                                       output_filename='NPSS.out'))
        self.NPSS_ADP.external_files.append(
            {'path':os.path.join(model_dir, 'MC_ADP.run')})
        self.NPSS_ADP.run_command = 'mcRun()'
        self.NPSS_ADP.reload_flag = 'mcReload'

        # SLS.
        arglist = []
        arglist.extend(includes)
        arglist.append(os.path.join(model_dir, 'MC_SLS.mdl'))
        self.add_container('NPSS_SLS', 
                           TracingNPSS(directory='NPSS_SLS', arglist=arglist, 
                                       output_filename='NPSS.out'))
        self.NPSS_SLS.external_files.append(
            {'path':os.path.join(model_dir, 'MC_SLS.run')})
        self.NPSS_SLS.run_command = 'mcRun()'
        self.NPSS_SLS.reload_flag = 'mcReload'

        # WATE.
        wate_dir = os.path.join('..', 'Full_Model', 'Weight', 'run')
        arglist = [
            '-I', '../Full_Model/Components',
            '-I', '../Full_Model/Weight',
            '-I', '../Full_Model/Weight/view',
            '-I', '../Full_Model/Weight/src',
            '-I', '../Full_Model/Weight/maps',
            '-I', '../Full_Model/Weight/run',
            '-I', '../Full_Model/ROSE',
            '-I', '../Full_Model/ROSE/BaseClasses']
        arglist.append(os.path.join(wate_dir, 'MCengine.mdl'))
        self.add_container('NPSS_WATE',
                           TracingNPSS(directory='NPSS_WATE', arglist=arglist, 
                                       output_filename='NPSS.out'))
        self.NPSS_WATE.external_files.append(
            {'path':os.path.join(wate_dir, 'MCengine.run')})
        self.NPSS_WATE.run_command = 'mcRun()'
        self.NPSS_WATE.reload_flag = 'mcReload'

        # FLOPS.
        arglist = []
        arglist.extend(includes)
        arglist.append(os.path.join(model_dir, 'MCengine.mdl'))
        self.add_container('NPSS_FLOPS',
                           TracingNPSS(directory='NPSS_FLOPS', arglist=arglist, 
                                       output_filename='NPSS.out'))
        self.NPSS_FLOPS.external_files.append(
            {'path':os.path.join(model_dir, 'MCengine.run')})
        self.NPSS_FLOPS.run_command = 'mcRun()'
        self.NPSS_FLOPS.reload_flag = 'mcReload'

        # ANOPP
        arglist = []
        arglist.extend(includes)
        arglist.append(os.path.join(model_dir, 'MCnoise.mdl'))
        self.add_container('NPSS_ANOPP',
                           TracingNPSS(directory='NPSS_ANOPP', arglist=arglist, 
                                       output_filename='NPSS.out'))
        self.NPSS_ANOPP.external_files.append(
            {'path':os.path.join(model_dir, 'MCnoise.run')})
        self.NPSS_ANOPP.run_command = 'mcRun()'
        self.NPSS_ANOPP.reload_flag = 'mcReload'

        # Propulsion data.
        self.add_container('PropulsionData', PropulsionData())

    def tree_defined(self):
        super(SBJ_Propulsion, self).tree_defined()

        # ADP.
        self.connect('Design.alt',       'NPSS_ADP.engine.alt')
        self.connect('Design.extractionRatio',
                     'NPSS_ADP.engine.extractionRatio')
        self.connect('Design.FanPRdes',  'NPSS_ADP.engine.FanPRdes')
        self.connect('Design.HpcPRdes',  'NPSS_ADP.engine.HpcPRdes')
        self.connect('Design.Knoz',      'NPSS_ADP.engine.Knoz')
        self.connect('Design.MN',        'NPSS_ADP.engine.MN')
        self.connect('Design.Cfg',       'NPSS_ADP.engine.nozzle.Cfg')
        self.connect('Design.TOCThrust', 'NPSS_ADP.engine.TOCThrust')

        # SLS.
        self.connect('Design.alt',       'NPSS_SLS.engine.alt')
        self.connect('Design.extractionRatio',
                     'NPSS_SLS.engine.extractionRatio')
        self.connect('Design.FanPRdes',  'NPSS_SLS.engine.FanPRdes')
        self.connect('Design.HpcPRdes',  'NPSS_SLS.engine.HpcPRdes')
        self.connect('Design.Knoz',      'NPSS_SLS.engine.Knoz')
        self.connect('Design.MN',        'NPSS_SLS.engine.MN')
        self.connect('Design.Cfg',       'NPSS_SLS.engine.nozzle.Cfg')
        self.connect('Design.TOCThrust', 'NPSS_SLS.engine.TOCThrust')

        # WATE.
        self.connect('Design.alt',       'NPSS_WATE.engine.ambient.Zalt')
        self.connect('Design.MN',        'NPSS_WATE.engine.ambient.ZMN')
        self.connect('Design.extractionRatio',
                     'NPSS_WATE.engine.extractionRatio')
        self.connect('Design.FanPRdes',  'NPSS_WATE.engine.FanPRdes')
        self.connect('Design.HpcPRdes',  'NPSS_WATE.engine.HpcPRdes')
        self.connect('Design.Cfg',       'NPSS_WATE.engine.nozzle.Cfg')
        self.connect('Design.TOCThrust', 'NPSS_WATE.engine.TOCThrust')

        # FLOPS.
        self.connect('Design.alt',       'NPSS_FLOPS.engine.alt')
        self.connect('Design.extractionRatio',
                     'NPSS_FLOPS.engine.extractionRatio')
        self.connect('Design.FanPRdes',  'NPSS_FLOPS.engine.FanPRdes')
        self.connect('Design.HpcPRdes',  'NPSS_FLOPS.engine.HpcPRdes')
        self.connect('Design.Knoz',      'NPSS_FLOPS.engine.Knoz')
        self.connect('Design.MN',        'NPSS_FLOPS.engine.MN')
        self.connect('Design.Cfg',       'NPSS_FLOPS.engine.nozzle.Cfg')
        self.connect('Design.TOCThrust', 'NPSS_FLOPS.engine.TOCThrust')

        # ANOPP
        self.connect('Design.alt',       'NPSS_ANOPP.engine.alt')
        self.connect('Design.extractionRatio',
                     'NPSS_ANOPP.engine.extractionRatio')
        self.connect('Design.FanPRdes',  'NPSS_ANOPP.engine.FanPRdes')
        self.connect('Design.HpcPRdes',  'NPSS_ANOPP.engine.HpcPRdes')
        self.connect('Design.Knoz',      'NPSS_ANOPP.engine.Knoz')
        self.connect('Design.MN',        'NPSS_ANOPP.engine.MN')
        self.connect('Design.Cfg',       'NPSS_ANOPP.engine.nozzle.Cfg')
        self.connect('Design.TOCThrust', 'NPSS_ANOPP.engine.TOCThrust')

        self.connect('NPSS_SLS.engine.PERF.Fn', 'NPSS_ANOPP.MaxThrust')

#       NPSS_ANOPP.A_ref =
#           pi*144*NPSS_WATE.engine.WATE.WATE_fan.bladeTipRadius**2
        self.connect('NPSS_WATE.engine.WATE.WATE_fan.bladeTipRadius',
                     'NPSS_ANOPP.link_bladeTipRadius')

#       NPSS_ANOPP.last_LPT_stg =
#           NPSS_WATE.engine.WATE.WATE_LPT.numStages-1
        self.connect('NPSS_WATE.engine.WATE.WATE_LPT.numStages',
                     'NPSS_ANOPP.link_numStages')

#       NPSS_ANOPP.LP_RPM_scalar =
#           NPSS_WATE.engine.WATE.WATE_fan.spoolRPM
#               / NPSS_WATE.engine.WATE.WATE_LP_Shaft.speed
        self.connect('NPSS_WATE.engine.WATE.WATE_fan.spoolRPM',
                     'NPSS_ANOPP.link_spoolRPM')
        self.connect('NPSS_WATE.engine.WATE.WATE_LP_Shaft.speed',
                     'NPSS_ANOPP.link_speed')

        self.connect('NPSS_WATE.engine.WATE.WATE_burner.innerDiam',
                     'NPSS_ANOPP.WATE_Burner.innerDiam')
        self.connect('NPSS_WATE.engine.WATE.WATE_burner.outerDiam',
                     'NPSS_ANOPP.WATE_Burner.outerDiam')

        self.connect('NPSS_WATE.engine.WATE.WATE_Nozzle.outPort.axialPosition',
                     'NPSS_ANOPP.WATE_Byp_Nozz.outPort.axialPosition')
        self.connect('NPSS_WATE.engine.WATE.WATE_Nozzle.outPort.innerRadius',
                     'NPSS_ANOPP.WATE_Byp_Nozz.outPort.innerRadius')
        self.connect('NPSS_WATE.engine.WATE.WATE_Nozzle.outPort.outerRadius',
                     'NPSS_ANOPP.WATE_Byp_Nozz.outPort.outerRadius')
        self.connect('NPSS_WATE.engine.WATE.WATE_Nozzle.outPort.axialPosition',
                     'NPSS_ANOPP.WATE_Core_Nozz.outPort.axialPosition')
        self.connect('NPSS_WATE.engine.WATE.WATE_Nozzle.outPort.innerRadius',
                     'NPSS_ANOPP.WATE_Core_Nozz.outPort.innerRadius')
        self.connect('NPSS_WATE.engine.WATE.WATE_Nozzle.outPort.outerRadius',
                     'NPSS_ANOPP.WATE_Core_Nozz.outPort.outerRadius')

        self.connect('NPSS_WATE.engine.WATE.WATE_fan.AR_stg',
                     'NPSS_ANOPP.WATE_Fan.AR_stg')
        self.connect('NPSS_WATE.engine.WATE.WATE_fan.areaIn',
                     'NPSS_ANOPP.WATE_Fan.areaIn')
        self.connect('NPSS_WATE.engine.WATE.WATE_fan.bypassLen_stg',
                     'NPSS_ANOPP.WATE_Fan.bypassLen_stg')
        self.connect('NPSS_WATE.engine.WATE.WATE_fan.hubRadius_stg',
                     'NPSS_ANOPP.WATE_Fan.hubRadius_stg')
        self.connect('NPSS_WATE.engine.WATE.WATE_fan.maxSpdRatio',
                     'NPSS_ANOPP.WATE_Fan.maxSpdRatio')
        self.connect('NPSS_WATE.engine.WATE.WATE_fan.numBlades_stg',
                     'NPSS_ANOPP.WATE_Fan.numBlades_stg')
        self.connect('NPSS_WATE.engine.WATE.WATE_fan.numStatorBlades_stg',
                     'NPSS_ANOPP.WATE_Fan.numStatorBlades_stg')
        self.connect('NPSS_WATE.engine.WATE.WATE_fan.spoolRPM',
                     'NPSS_ANOPP.WATE_Fan.spoolRPM')
        self.connect('NPSS_WATE.engine.WATE.WATE_fan.stg1TipRadius',
                     'NPSS_ANOPP.WATE_Fan.stg1TipRadius')
        self.connect('NPSS_WATE.engine.WATE.WATE_fan.tipRadius_stg',
                     'NPSS_ANOPP.WATE_Fan.tipRadius_stg')

        self.connect('NPSS_WATE.engine.WATE.WATE_inlet.avgCowlDiam',
                     'NPSS_ANOPP.WATE_Inlet.avgCowlDiam')
        self.connect('NPSS_WATE.engine.WATE.WATE_inlet.fanExhaustLength',
                     'NPSS_ANOPP.WATE_Inlet.fanExhaustLength')
        self.connect('NPSS_WATE.engine.WATE.WATE_inlet.fanLength',
                     'NPSS_ANOPP.WATE_Inlet.fanLength')
        self.connect('NPSS_WATE.engine.WATE.WATE_inlet.mostFwdToEngFFLength',
                     'NPSS_ANOPP.WATE_Inlet.mostFwdToEngFFLength')
        self.connect('NPSS_WATE.engine.WATE.WATE_LPT.area_stg',
                     'NPSS_ANOPP.WATE_LPT.area_stg')
        self.connect('NPSS_WATE.engine.WATE.WATE_LPT.hubRadius_stg',
                     'NPSS_ANOPP.WATE_LPT.hubRadius_stg')
        self.connect('NPSS_WATE.engine.WATE.WATE_LPT.numBlades_stg',
                     'NPSS_ANOPP.WATE_LPT.numBlades_stg')
        self.connect('NPSS_WATE.engine.WATE.WATE_LPT.numStages',
                     'NPSS_ANOPP.WATE_LPT.numStages')
        self.connect('NPSS_WATE.engine.WATE.WATE_LPT.tipRadius_stg',
                     'NPSS_ANOPP.WATE_LPT.tipRadius_stg')

        # Propulsion data.
#       PropulsionData.FLOPS.dnac =
#           2*(NPSS_WATE.engine.WATE.WATE_fan.bladeTipRadius
#              + NPSS_WATE.engine.WATE.WATE_fan.contRingRadialThickness/12)
#           * Design.maxDiamFact
        self.connect('Design.maxDiamFact',
                     'PropulsionData.link_maxDiamFact')
        self.connect('NPSS_WATE.engine.WATE.WATE_fan.bladeTipRadius',
                     'PropulsionData.link_bladeTipRadius')
        self.connect('NPSS_WATE.engine.WATE.WATE_fan.contRingRadialThickness',
                     'PropulsionData.link_contRingRadialThickness')

#       PropulsionData.Acapture =
#           NPSS_ADP.engine.inlet.S_install.Acapture / 144.
        self.connect('NPSS_ADP.engine.inlet.S_install.Acapture',
                     'PropulsionData.link_Acapture')

#       PropulsionData.FLOPS.xnac =
#           (NPSS_WATE.engine.WATE.WATE_inlet.length
#            + NPSS_WATE.engine.WATE.length) / 12.
        self.connect('NPSS_WATE.engine.WATE.WATE_inlet.length',
                     'PropulsionData.link_inletLength')
        self.connect('NPSS_WATE.engine.WATE.length',
                     'PropulsionData.link_length')

        self.connect('NPSS_WATE.engine.weight',
                     'PropulsionData.FLOPS.weng')

        self.connect('NPSS_FLOPS.engine.FLOPsheetStream',
                     'PropulsionData.FLOPS.engdeck')

        self.connect('NPSS_FLOPS.engine.PERF.Fn',
                     'PropulsionData.FLOPS.thrso')


if __name__ == '__main__':
    top = set_as_top(SBJ_Propulsion())
#    top.run()
    top.check_save_load()

