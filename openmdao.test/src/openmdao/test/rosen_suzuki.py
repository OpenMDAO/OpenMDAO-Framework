"""
Model based on CONMIN test.
"""

from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.lib.casehandlers.api import ListCaseRecorder
from openmdao.main.datatypes.api import Array, Float
from openmdao.lib.drivers.conmindriver import CONMINdriver
from openmdao.lib.drivers.slsqpdriver import SLSQPdriver


class PreProc(Component):
    """ Dummy pre-processor. """
    x_in = Array([1., 1., 1., 1.], iotype='in', low=-10, high=99)
    x_out = Array(iotype='out')

    def execute(self):
        self.x_out = self.x_in


class ScalingPreProc(PreProc):
    """ Scaling pre-processor. """
    scaler = Float(1.0, iotype='in')

    def execute(self):
        super(ScalingPreProc, self).execute()
        self.x_out *= self.scaler


class OptRosenSuzukiComponent(Component):

    x = Array(iotype='in', low=-10, high=99)
    result = Float(iotype='out')
    opt_objective = Float(iotype='out')

    def configure(self):
        self.result = 0.
        self.opt_objective = 6.
        self.opt_design_vars = [0., 1., 2., -1.]

    def execute(self):
        """calculate the new objective value"""
        self.result = (self.x[0]**2 - 5.*self.x[0] +
                       self.x[1]**2 - 5.*self.x[1] +
                       2.*self.x[2]**2 - 21.*self.x[2] +
                       self.x[3]**2 + 7.*self.x[3] + 50)


class PostProc(Component):
    """ Dummy post-processor. """
    result_in = Float(iotype='in')
    result_out = Float(iotype='out')

    def execute(self):
        self.result_out = self.result_in


class ScalingPostProc(PostProc):
    """ Scaling post-processor. """
    scaler = Float(1.0, iotype='in')

    def execute(self):
        super(ScalingPostProc, self).execute()
        self.result_out *= self.scaler


class Simulation(Assembly):

    def configure(self):
        self.add('driver', CONMINdriver())
        self.add('preproc', PreProc())
        self.add('comp', OptRosenSuzukiComponent())
        self.add('postproc', PostProc())
        self.driver.workflow.add(('preproc', 'comp', 'postproc'))
        self.driver.iprint = 0
        self.driver.itmax = 30

        self.connect('preproc.x_out', 'comp.x')
        self.connect('comp.result', 'postproc.result_in')
        self.driver.add_objective('postproc.result_out')
        self.driver.add_parameter(('preproc.x_in[0]', 'preproc.x_in[1]',
                                   'preproc.x_in[2]', 'preproc.x_in[3]'))

        # pylint: disable=C0301
        map(self.driver.add_constraint, [
            'comp.x[0]**2+comp.x[0]+comp.x[1]**2-comp.x[1]+comp.x[2]**2+comp.x[2]+comp.x[3]**2-comp.x[3] < 8',
            'comp.x[0]**2-comp.x[0]+2*comp.x[1]**2+comp.x[2]**2+2*comp.x[3]**2-comp.x[3] < 10',
            '2*comp.x[0]**2+2*comp.x[0]+comp.x[1]**2-comp.x[1]+comp.x[2]**2-comp.x[3] < 5'])
        self.recorders = [ListCaseRecorder()]


class NestedSimulation(Assembly):

    def configure(self):
        self.add('sim', Simulation())
        self.driver.workflow.add('sim')


if __name__ == '__main__':
    sim = set_as_top(Simulation())
    sim.run()
    print 'objective', sim.comp.opt_objective, sim.driver.eval_objective()
    for i in range(len(sim.preproc.x_in)):
        print 'design_var', i, sim.comp.opt_design_vars[i], sim.preproc.x_in[i]

    sim.replace('preproc', ScalingPreProc())
    sim.replace('driver', SLSQPdriver())
