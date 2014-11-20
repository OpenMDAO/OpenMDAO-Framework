"""
    Solution of the Sellar analytical problem using classic BLISS.
    (Bi-Level Integrated System Synthesis)

    MDA solved with a Broyden solver.
    Global sensitivity calculated by finite-differencing the MDA-coupled
    system. The MDA should be replaced with solution of the GSE to fully
    match the original Sobiesky-Agte implementation.
"""

from openmdao.main.api import Assembly, Component
from openmdao.main.datatypes.api import Float, Array
from openmdao.lib.drivers.api import CONMINdriver, BroydenSolver, \
                                     SensitivityDriver, FixedPointIterator

from openmdao.lib.optproblems import sellar


class Dis1Linear(Component):
    """ Linear model of one a sellar model or system. """

    x1 = Float(0., iotype='in')
    x1_store = Float(0., iotype='in')

    sa_dis1_F = Array([0.0], iotype='in')
    sa_dis1_G = Array([0.0, 0.0], iotype='in')
    sa_dis1_dF = Array([0.0], iotype='in')
    sa_dis1_dG = Array([[0.0], [0.0]], iotype='in')

    obj = Float(0.0, iotype='out')
    con1 = Float(0.0, iotype='out')
    con2 = Float(0.0, iotype='out')

    def execute(self):

        self.obj  = self.sa_dis1_F[0] + self.sa_dis1_dF[0]*(self.x1_store - self.x1)
        self.con1 = self.sa_dis1_G[0] + self.sa_dis1_dG[0][0]*(self.x1_store - self.x1)
        self.con2 = self.sa_dis1_G[1] + self.sa_dis1_dG[1][0]*(self.x1_store - self.x1)
        
        #print "%s x1: %f  x1_store: %f  sa_dis1_F: %s  sa_dis1_G: %s  sa_dis1_dF: %s  sa_dis1_dG: %s" % (self.name, self.x1, self.x1_store, self.sa_dis1_F,
                                                                                                         #self.sa_dis1_G, self.sa_dis1_dF, self.sa_dis1_dG)
        #print "%s obj: %f  con1: %f  con2: %f" % (self.name, self.obj, self.con1, self.con2)

class Dis12Linear(Component):
    """ Linear model of one a sellar model or system. """

    z1 = Float(0., iotype='in')
    z2 = Float(0., iotype='in')
    z_store = Array([0., 0.], iotype='in')

    ssa_F = Array([0.0], iotype='in')
    ssa_G = Array([0.0, 0.0], iotype='in')
    ssa_dF = Array([0.0, 0.0], iotype='in')
    ssa_dG = Array([[0.0, 0.0], [0.0, 0.0]], iotype='in')

    obj = Float(0.0, iotype='out')
    con1 = Float(0.0, iotype='out')
    con2 = Float(0.0, iotype='out')

    def execute(self):
        #print self.z_store, self.z1, self.z2
        self.obj = self.ssa_F[0] + self.ssa_dF[0]*(self.z_store[0] - self.z1) + \
                                   self.ssa_dF[1]*(self.z_store[1] - self.z2)
        self.con1 = self.ssa_G[0] + self.ssa_dG[0][0]*(self.z_store[0] - self.z1) + \
                                    self.ssa_dG[0][1]*(self.z_store[1] - self.z2)
        self.con2 = self.ssa_G[1] + self.ssa_dG[1][0]*(self.z_store[0] - self.z1) + \
                                    self.ssa_dG[1][1]*(self.z_store[1] - self.z2)
        
        #print "%s z1: %f  z2: %f  z_store: %s  ssa_F: %s  ssa_G: %s  ssa_dF: %s  ssa_dG: %s" % (self.name, self.z1, self.z2, self.z_store,
                                                                                                         #self.ssa_F, self.ssa_G, self.ssa_dF, self.ssa_dG)
        #print "%s obj: %f  con1: %f  con2: %f" % (self.name, self.obj, self.con1, self.con2)


class SellarBLISS(Assembly):
    """ Optimization of the Sellar problem using the BLISS algorithm
    Disciplines coupled with FixedPointIterator.
    """

    #z_store = Array([0., 0.], dtype=Float, iotype='in')
    #x1_store = Float(0.0, iotype='in')

    def configure(self):
        """ Creates a new Assembly with this problem

        Optimal Design at (1.9776, 0, 0)

        Optimal Objective = 3.18339"""

        # Disciplines
        self.add('dis1pre', sellar.Discipline1()) # Used for global sensitivity analysis
        self.add('dis1', sellar.Discipline1())
        self.add('dis2', sellar.Discipline2())
        self.add('dis1lin', Dis1Linear())
        self.add('dis12lin', Dis12Linear())

        #self.connect('z_store', 'dis12lin.z_store')
        #self.connect('x1_store', 'dis1lin.x1_store')
        self.connect('dis1pre.y2', 'dis1.y2')

        objective = '(dis1.x1)**2 + dis1.z2 + dis1.y1 + exp(-dis2.y2)'
        constraint1 = 'dis1.y1 > 3.16'
        constraint2 = 'dis2.y2 < 24.0'

        # Top level is Fixed-Point Iteration
        self.add('driver', FixedPointIterator())
        self.driver.add_parameter(('dis1pre.x1', 'dis1.x1', 'dis1lin.x1'), low=0.0, high=10.0, start=1.0)
        #self.driver.add_parameter(('dis1.x1', 'dis1pre.x1', 'dis1lin.x1'), low=0.0, high=10.0, start=1.0)
        self.driver.add_parameter(['dis1pre.z1', 'dis1.z1', 'dis2.z1', 'dis12lin.z1'], low=-10.0, high=10.0)
        self.driver.add_parameter(['dis1pre.z2', 'dis1.z2', 'dis2.z2', 'dis12lin.z2'], low=  0.0, high=10.0)
        self.driver.add_constraint('dis1lin.x1_store = dis1.x1')
        self.driver.add_constraint('dis12lin.z_store[0] = dis1pre.z1')
        self.driver.add_constraint('dis12lin.z_store[1] = dis1pre.z2')
        self.driver.max_iteration = 50
        self.driver.tolerance = .001

        # Multidisciplinary Analysis
        self.add('mda', BroydenSolver())
        self.mda.add_parameter(('dis1pre.y2'), start=0.0)
        self.mda.add_constraint('dis2.y2 = dis1pre.y2')
        self.mda.add_parameter('dis2.y1', start=3.16)
        self.mda.add_constraint('dis2.y1 = dis1pre.y1')

        # Discipline 1 Sensitivity Analysis
        self.add('sa_dis1', SensitivityDriver())
        self.sa_dis1.add_parameter('dis1.x1', low=0.0, high=10.0, fd_step=.001)
        self.sa_dis1.add_constraint(constraint1)
        self.sa_dis1.add_constraint(constraint2)
        self.sa_dis1.add_objective(objective, name='obj')
        self.connect('sa_dis1.F', 'dis1lin.sa_dis1_F')
        self.connect('sa_dis1.G', 'dis1lin.sa_dis1_G')
        self.connect('sa_dis1.dF', 'dis1lin.sa_dis1_dF')
        self.connect('sa_dis1.dG', 'dis1lin.sa_dis1_dG')

        # Discipline 2 Sensitivity Analysis
        # dis2 has no local parameter, so there is no need to treat it as
        # a subsystem.

        # System Level Sensitivity Analysis
        # Note, we cheat here and run an MDA instead of solving the
        # GSE equations. Have to put this on the TODO list.
        self.add('ssa', SensitivityDriver())
        self.ssa.add_parameter(['dis1pre.z1', 'dis2.z1'], low=-10.0, high=10.0)
        self.ssa.add_parameter(['dis1pre.z2', 'dis2.z2'], low=  0.0, high=10.0)
        objective_pre = '(dis1pre.x1)**2 + dis1pre.z2 + dis1pre.y1 + exp(-dis2.y2)'
        constraint1_pre = 'dis1pre.y1 > 3.16'
        self.ssa.add_constraint(constraint1_pre)
        self.ssa.add_constraint(constraint2)
        self.ssa.add_objective(objective_pre, name='obj')
        self.connect('ssa.F', 'dis12lin.ssa_F')
        self.connect('ssa.G', 'dis12lin.ssa_G')
        self.connect('ssa.dF', 'dis12lin.ssa_dF')
        self.connect('ssa.dG', 'dis12lin.ssa_dG')

        # Discipline Optimization
        # (Only discipline1 has an optimization input)
        self.add('bbopt1', CONMINdriver())
        self.bbopt1.add_parameter('dis1lin.x1_store', low=0.0, high=10.0, start=1.0)
        self.bbopt1.add_objective('dis1lin.obj')
        self.bbopt1.add_constraint('dis1lin.con1 < 0')
        #this one is technically unncessary
        self.bbopt1.add_constraint('dis1lin.con2 < 0')

        self.bbopt1.add_constraint('(dis1lin.x1_store-dis1lin.x1)<.5')
        self.bbopt1.add_constraint('(dis1lin.x1_store-dis1lin.x1)>-.5')
        self.bbopt1.iprint = 0
        self.bbopt1.linobj = True

        # Global Optimization
        self.add('sysopt', CONMINdriver())
        self.sysopt.add_parameter('dis12lin.z_store[0]', low=-10.0, high=10.0, start=5.0)
        self.sysopt.add_parameter('dis12lin.z_store[1]', low=0.0, high=10.0, start=2.0)
        self.sysopt.add_objective('dis12lin.obj')

        self.sysopt.add_constraint('dis12lin.con1 < 0')
        self.sysopt.add_constraint('dis12lin.con2 < 0')

        self.sysopt.add_constraint('dis12lin.z_store[0]-dis12lin.z1<.5')
        self.sysopt.add_constraint('dis12lin.z_store[0]-dis12lin.z1>-.5')
        self.sysopt.add_constraint('dis12lin.z_store[1]-dis12lin.z2<.5')
        self.sysopt.add_constraint('dis12lin.z_store[1]-dis12lin.z2>-.5')
        self.sysopt.iprint = 0
        self.sysopt.linobj = True

        self.driver.workflow.add(['ssa', 'sa_dis1', 'bbopt1', 'sysopt'])
        self.ssa.workflow.add(['mda'])
        self.mda.workflow.add(['dis1pre', 'dis2'])
        self.sa_dis1.workflow.add(['dis1'])
        self.bbopt1.workflow.add(['dis1lin'])
        self.sysopt.workflow.add(['dis12lin'])


if __name__ == "__main__": # pragma: no cover

    import time
    import math

    prob = SellarBLISS()
    prob.name = 'top'

    prob.dis1.z1 = prob.dis2.z1 = prob.dis12lin.z1 = prob.dis1pre.z1 = 5.0
    prob.dis1.z2 = prob.dis2.z2 = prob.dis12lin.z2 = prob.dis1pre.z2 = 2.0
    prob.dis1.x1 = prob.dis1lin.x1 = 1.0
    #prob.dis2.y1 = 3.16
    tt = time.time()
    prob.run()
    print "These should be equal"
    print prob.dis1pre.z1, prob.dis1.z1, prob.dis2.z1, prob.dis12lin.z1
    print "\n"
    print "Minimum found at (%f, %f, %f)" % (prob.dis1pre.z1,
                                             prob.dis1pre.z2,
                                             prob.dis1pre.x1)
    print "Targets at (%f, %f, %f)" % (prob.dis12lin.z_store[0],
                                             prob.dis12lin.z_store[1],
                                             prob.dis1lin.x1_store)
    print "Couping vars: %f, %f" % (prob.dis1.y1, prob.dis2.y2)
    print "Minimum objective: ", (prob.dis1.x1)**2 + prob.dis1.z2 + \
                                  prob.dis1.y1 + math.exp(-prob.dis2.y2)
    print "Elapsed time: ", time.time()-tt, "seconds"
