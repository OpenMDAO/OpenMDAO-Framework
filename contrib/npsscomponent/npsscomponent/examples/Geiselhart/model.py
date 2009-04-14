"""
PARTIAL! Port of Karl's ModelCenter SBJ propulsion process model.

It's a fairly complex example of running multiple linked components
in sequence.  Each NPSS component is run in it's own subdirectory,
which is where the NPSS output is written.  The log file also has some
information written to it in addition to what's shown on screen.

Known problems:
    - The components run, but since the port is only partial,
      the results are quite wrong.
    - 'Design' variables are set as OUTPUTS and 'PropulsionData'
      variables are set as INPUTS.  This looks odd, but is neccessary
      until some more issues in the framework are ironed-out.
"""
import os.path

from openmdao.main import Assembly, Component, Container, Float, \
                          ArrayVariable, FileVariable
from openmdao.main.variable import INPUT, OUTPUT

from npsscomponent import NPSScomponent

# pylint: disable-msg=E1101
# "Instance of <class> has no <attr> member"


class Design(Component):
    """ Design variables. """

    def __init__(self, name='Design', parent=None):
        super(Design, self).__init__(name, parent)

        Float('FanPRdes',        self, OUTPUT, default=3.615)
        Float('TOCThrust',       self, OUTPUT, default=7000)
        Float('MN',              self, OUTPUT, default=1.8)
        Float('alt',             self, OUTPUT, default=53700)
        Float('Knoz',            self, OUTPUT, default=0.88)
        Float('extractionRatio', self, OUTPUT, default=1.05)
        Float('HpcPRdes',        self, OUTPUT, default=6.51937)
        Float('Cfg',             self, OUTPUT, default=0.985)
        Float('maxDiamFact',     self, OUTPUT, default=1.17)
        Float('I_externalFact',  self, OUTPUT, default=0.48)
        Float('I_divergFact',    self, OUTPUT, default=0.5)
        Float('cowl_angle',      self, OUTPUT, default=6)


class PropulsionData(Component):
    """ Computed propulsion data. """

    def __init__(self, name='PropulsionData', parent=None):
        super(PropulsionData, self).__init__(name, parent)

        Float('link_bladeTipRadius', self, INPUT, default=0.)
        Float('link_contRingRadialThickness', self, INPUT, default=0.)
        Float('link_maxDiamFact', self, INPUT, default=0.)
        Float('link_Acapture', self, INPUT, default=0.)
        Float('link_inletLength', self, INPUT, default=0.)
        Float('link_length', self, INPUT, default=0.)

        Float('Acapture', self, OUTPUT, default=0.)

        FLOPSdata(parent=self)
        PlumeData(parent=self)
        USM3Ddata(parent=self)
        NacelleData(parent=self)
        ANOPPdata(parent=self)

    def execute(self):
        """ Evaluate link expressions. """

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


class FLOPSdata(Container):

    def __init__(self, name='FLOPS', parent=None):
        super(FLOPSdata, self).__init__(name, parent)

        FileVariable('engdeck', self, INPUT, default='engdeck')
        Float('thrso', self, INPUT, default=0.)
        Float('weng',  self, INPUT, default=0.)
        Float('xnac',  self, INPUT, default=0.)
        Float('dnac',  self, INPUT, default=0.)


class PlumeData(Container):

    def __init__(self, name='Plume', parent=None):
        super(PlumeData, self).__init__(name, parent)

        Float('m_dot',      self, INPUT, default=0.)
        Float('Ptj',        self, INPUT, default=0.)
        Float('Ttj',        self, INPUT, default=0.)
        Float('far',        self, INPUT, default=0.)
        Float('Astar',      self, INPUT, default=0.)
        Float('Ae',         self, INPUT, default=0.)
        Float('Amax',       self, INPUT, default=0.)
        Float('I_diverg',   self, INPUT, default=0.)
        Float('I_external', self, INPUT, default=0.)


class USM3Ddata(Container):

    def __init__(self, name='USM3D', parent=None):
        super(USM3Ddata, self).__init__(name, parent)

        Float('fuel',   self, INPUT, default=0.)
        Float('gammaj', self, INPUT, default=0.)
        Float('pjet',   self, INPUT, default=0.)
        Float('p0jet',  self, INPUT, default=0.)
        Float('Rratio', self, INPUT, default=0.)
        Float('T0jet',  self, INPUT, default=0.)

        USM3Dinputs(parent=self)


class USM3Dinputs(Container):

    def __init__(self, name='Inputs', parent=None):
        super(USM3Dinputs, self).__init__(name, parent)

        USM3Dfreestream(parent=self)
        USM3Dplenum(parent=self)
        USM3Dthroat(parent=self)
        USM3Dexit(parent=self)


class USM3Dfreestream(Container):

    def __init__(self, name='Freestream', parent=None):
        super(USM3Dfreestream, self).__init__(name, parent)

        Float('a',     self, INPUT, default=0.)
        Float('gamma', self, INPUT, default=0.)
        Float('rho',   self, INPUT, default=0.)
        Float('R',     self, INPUT, default=0.)
        Float('Ps',    self, INPUT, default=0.)
        Float('Pt',    self, INPUT, default=0.)
        Float('Ts',    self, INPUT, default=0.)
        Float('Tt',    self, INPUT, default=0.)


class USM3Dplenum(Container):

    def __init__(self, name='Plenum', parent=None):
        super(USM3Dplenum, self).__init__(name, parent)

        Float('gamma_s', self, INPUT, default=0.)
        Float('gamma_t', self, INPUT, default=0.)
        Float('Rs',      self, INPUT, default=0.)
        Float('Rt',      self, INPUT, default=0.)
        Float('Ps',      self, INPUT, default=0.)
        Float('Pt',      self, INPUT, default=0.)
        Float('Ts',      self, INPUT, default=0.)
        Float('Tt',      self, INPUT, default=0.)


class USM3Dthroat(Container):

    def __init__(self, name='Throat', parent=None):
        super(USM3Dthroat, self).__init__(name, parent)

        Float('gamma_s', self, INPUT, default=0.)
        Float('gamma_t', self, INPUT, default=0.)
        Float('Rs',      self, INPUT, default=0.)
        Float('Rt',      self, INPUT, default=0.)
        Float('Ps',      self, INPUT, default=0.)
        Float('Pt',      self, INPUT, default=0.)
        Float('Ts',      self, INPUT, default=0.)
        Float('Tt',      self, INPUT, default=0.)


class USM3Dexit(Container):

    def __init__(self, name='Exit', parent=None):
        super(USM3Dexit, self).__init__(name, parent)

        Float('gamma_s', self, INPUT, default=0.)
        Float('gamma_t', self, INPUT, default=0.)
        Float('Rs',      self, INPUT, default=0.)
        Float('Rt',      self, INPUT, default=0.)
        Float('Ps',      self, INPUT, default=0.)
        Float('Pt',      self, INPUT, default=0.)
        Float('Ts',      self, INPUT, default=0.)
        Float('Tt',      self, INPUT, default=0.)


class NacelleData(Container):

    def __init__(self, name='Nacelle', parent=None):
        super(NacelleData, self).__init__(name, parent)

        ArrayVariable('X', self, INPUT, float, default=[])
        ArrayVariable('Y', self, INPUT, float, default=[])


class ANOPPdata(Container):

    def __init__(self, name='ANOPP', parent=None):
        super(ANOPPdata, self).__init__(name, parent)


class TracingNPSS(NPSScomponent):
    """ Simply overrides execute() to trace start & stop times. """

    def execute(self):
        print self.get_pathname(), 'execution begins'
        super(TracingNPSS, self).execute()
        print self.get_pathname(), '    complete'


class Model(Assembly):
    """ SBJ propulsion model. """

    def connect(self, src_path, dst_path):
        """ Overriding default to dynamically make_public() for NPSS. """
        comp, rest = src_path.split('.', 1)
        src_comp = getattr(self, comp)
        if isinstance(src_comp, NPSScomponent):
            src_comp.make_public((rest, '', OUTPUT))

        comp, rest = dst_path.split('.', 1)
        dst_comp = getattr(self, comp)
        if isinstance(dst_comp, NPSScomponent):
            dst_comp.make_public(rest)
            
        super(Model, self).connect(src_path, dst_path)

    def __init__(self, name='SBJ_Propulsion', *args, **kwargs):
        super(Model, self).__init__(name, *args, **kwargs)

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
        Design(parent=self)
        self.workflow.add_node(self.Design)

        # ADP.
        arglist = []
        arglist.extend(includes)
        arglist.append(os.path.join(model_dir, 'MC_ADP.mdl'))
        TracingNPSS('NPSS_ADP', self, directory='NPSS_ADP',
                    arglist=arglist, output_filename='NPSS.out')
        self.NPSS_ADP.external_files.append(
            {'path':os.path.join(model_dir, 'MC_ADP.run')})
        self.NPSS_ADP.run_command = 'mcRun()'
        self.NPSS_ADP.reload_flag = 'mcReload'
        self.workflow.add_node(self.NPSS_ADP)

        self.connect('Design.alt',       'NPSS_ADP.engine.alt')
        self.connect('Design.extractionRatio', 'NPSS_ADP.engine.extractionRatio')
        self.connect('Design.FanPRdes',  'NPSS_ADP.engine.FanPRdes')
        self.connect('Design.HpcPRdes',  'NPSS_ADP.engine.HpcPRdes')
        self.connect('Design.Knoz',      'NPSS_ADP.engine.Knoz')
        self.connect('Design.MN',        'NPSS_ADP.engine.MN')
        self.connect('Design.Cfg',       'NPSS_ADP.engine.nozzle.Cfg')
        self.connect('Design.TOCThrust', 'NPSS_ADP.engine.TOCThrust')

        # SLS.
        arglist = []
        arglist.extend(includes)
        arglist.append(os.path.join(model_dir, 'MC_SLS.mdl'))
        TracingNPSS('NPSS_SLS', self, directory='NPSS_SLS',
                    arglist=arglist, output_filename='NPSS.out')
        self.NPSS_SLS.external_files.append(
            {'path':os.path.join(model_dir, 'MC_SLS.run')})
        self.NPSS_SLS.run_command = 'mcRun()'
        self.NPSS_SLS.reload_flag = 'mcReload'
        self.workflow.add_node(self.NPSS_SLS)

        self.connect('Design.alt',       'NPSS_SLS.engine.alt')
        self.connect('Design.extractionRatio', 'NPSS_SLS.engine.extractionRatio')
        self.connect('Design.FanPRdes',  'NPSS_SLS.engine.FanPRdes')
        self.connect('Design.HpcPRdes',  'NPSS_SLS.engine.HpcPRdes')
        self.connect('Design.Knoz',      'NPSS_SLS.engine.Knoz')
        self.connect('Design.MN',        'NPSS_SLS.engine.MN')
        self.connect('Design.Cfg',       'NPSS_SLS.engine.nozzle.Cfg')
        self.connect('Design.TOCThrust', 'NPSS_SLS.engine.TOCThrust')

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
        TracingNPSS('NPSS_WATE', self, directory='NPSS_WATE',
                    arglist=arglist, output_filename='NPSS.out')
        self.NPSS_WATE.external_files.append(
            {'path':os.path.join(wate_dir, 'MCengine.run')})
        self.NPSS_WATE.run_command = 'mcRun()'
        self.NPSS_WATE.reload_flag = 'mcReload'
        self.workflow.add_node(self.NPSS_WATE)

        self.connect('Design.alt',       'NPSS_WATE.engine.ambient.Zalt')
        self.connect('Design.MN',        'NPSS_WATE.engine.ambient.ZMN')
        self.connect('Design.extractionRatio', 'NPSS_WATE.engine.extractionRatio')
        self.connect('Design.FanPRdes',  'NPSS_WATE.engine.FanPRdes')
        self.connect('Design.HpcPRdes',  'NPSS_WATE.engine.HpcPRdes')
        self.connect('Design.Cfg',       'NPSS_WATE.engine.nozzle.Cfg')
        self.connect('Design.TOCThrust', 'NPSS_WATE.engine.TOCThrust')

        # FLOPS.
        arglist = []
        arglist.extend(includes)
        arglist.append(os.path.join(model_dir, 'MCengine.mdl'))
        TracingNPSS('NPSS_FLOPS', self, directory='NPSS_FLOPS',
                    arglist=arglist, output_filename='NPSS.out')
        self.NPSS_FLOPS.external_files.append(
            {'path':os.path.join(model_dir, 'MCengine.run')})
        self.NPSS_FLOPS.run_command = 'mcRun()'
        self.NPSS_FLOPS.reload_flag = 'mcReload'
        self.workflow.add_node(self.NPSS_FLOPS)

        self.connect('Design.alt',       'NPSS_FLOPS.engine.alt')
        self.connect('Design.extractionRatio', 'NPSS_FLOPS.engine.extractionRatio')
        self.connect('Design.FanPRdes',  'NPSS_FLOPS.engine.FanPRdes')
        self.connect('Design.HpcPRdes',  'NPSS_FLOPS.engine.HpcPRdes')
        self.connect('Design.Knoz',      'NPSS_FLOPS.engine.Knoz')
        self.connect('Design.MN',        'NPSS_FLOPS.engine.MN')
        self.connect('Design.Cfg',       'NPSS_FLOPS.engine.nozzle.Cfg')
        self.connect('Design.TOCThrust', 'NPSS_FLOPS.engine.TOCThrust')

        # ANOPP
        arglist = []
        arglist.extend(includes)
        arglist.append(os.path.join(model_dir, 'MCnoise.mdl'))
        TracingNPSS('NPSS_ANOPP', self, directory='NPSS_ANOPP',
                    arglist=arglist, output_filename='NPSS.out')
        self.NPSS_ANOPP.external_files.append(
            {'path':os.path.join(model_dir, 'MCnoise.run')})
        self.NPSS_ANOPP.run_command = 'mcRun()'
        self.NPSS_ANOPP.reload_flag = 'mcReload'
        self.workflow.add_node(self.NPSS_ANOPP)

        self.connect('Design.alt',       'NPSS_ANOPP.engine.alt')
        self.connect('Design.extractionRatio', 'NPSS_ANOPP.engine.extractionRatio')
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
        PropulsionData(parent=self)
        self.workflow.add_node(self.PropulsionData)

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


def print_info(root, level=0):
    """ Print some internal data sarting at root. """
    from openmdao.main.interfaces import IComponent
    try:
        pub = getattr(root, '_pub')
    except AttributeError:
        return

    if IComponent.providedBy(root):
        for metadata in root.external_files:
            print '%s%s' % ('    '*level, str(metadata))

    for name in sorted(pub.keys()):
        obj = root.get(name)
        print '%s.%s = %s' % ('    '*level, name, str(obj))
        print_info(obj, level+1)


def test_save_load():
    """ Save model and then reload & run. """
    import os
    import shutil

    model = Model()
    egg_name = model.save_to_egg()
#    print_info(model)

    if os.path.exists('test_dir'):
        shutil.rmtree('test_dir')
    os.mkdir('test_dir')
    os.chdir('test_dir')

    new_model = Container.load_from_egg(os.path.join('..', egg_name))
#    print_info(new_model)

    print '\nrunning new model...'
    new_model.run()


if __name__ == '__main__':
#    Model().run()

    test_save_load()

