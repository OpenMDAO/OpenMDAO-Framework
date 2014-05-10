"""
    Solution of the Sellar analytical problem using classic BLISS.
    (Bi-Level Integrated System Synthesis)

    MDA solved with a Broyden solver.
    Global sensitivity calculated by finite-differencing the MDA-coupled
    system. The MDA should be replaced with solution of the GSE to fully
    match the original Sobiesky-Agte implementation.
"""

from openmdao.main.api import Assembly
from openmdao.main.datatypes.api import Float, Array
from openmdao.lib.drivers.api import CONMINdriver, BroydenSolver, \
                                     SensitivityDriver, FixedPointIterator

from openmdao.lib.optproblems import sellar


class SellarBLISS(Assembly):
    """ Optimization of the Sellar problem using the BLISS algorithm
    Disciplines coupled with FixedPointIterator.
    """

    z_store = Array([0., 0.], dtype=Float, iotype='in')
    x1_store = Float(0.0, iotype='in')

    def configure(self):
        """ Creates a new Assembly with this problem

        Optimal Design at (1.9776, 0, 0)

        Optimal Objective = 3.18339"""

        # Disciplines
        self.add('dis1', sellar.Discipline1())
        self.add('dis2', sellar.Discipline2())

        objective = '(dis1.x1)**2 + dis1.z2 + dis1.y1 + exp(-dis2.y2)'
        constraint1 = 'dis1.y1 > 3.16'
        constraint2 = 'dis2.y2 < 24.0'

        # Top level is Fixed-Point Iteration
        self.add('driver', FixedPointIterator())
        self.driver.add_parameter('dis1.x1', low=0.0, high=10.0, start=1.0)
        self.driver.add_parameter(['dis1.z1','dis2.z1'], low=-10.0, high=10.0, start=5.0)
        self.driver.add_parameter(['dis1.z2','dis2.z2'], low=  0.0, high=10.0, start=2.0)
        self.driver.add_constraint('x1_store = dis1.x1')
        self.driver.add_constraint('z_store[0] = dis1.z1')
        self.driver.add_constraint('z_store[1] = dis1.z2')
        self.driver.max_iteration = 50
        self.driver.tolerance = .001

        # Multidisciplinary Analysis
        self.add('mda', BroydenSolver())
        self.mda.add_parameter('dis1.y2', start=0.0)
        self.mda.add_constraint('dis2.y2 = dis1.y2')
        self.mda.add_parameter('dis2.y1', start=3.16)
        self.mda.add_constraint('dis2.y1 = dis1.y1')

        # Discipline 1 Sensitivity Analysis
        self.add('sa_dis1', SensitivityDriver())
        self.sa_dis1.workflow.add(['dis1'])
        self.sa_dis1.add_parameter('dis1.x1', low=0.0, high=10.0, fd_step=.001)
        self.sa_dis1.add_constraint(constraint1)
        self.sa_dis1.add_constraint(constraint2)
        self.sa_dis1.add_objective(objective, name='obj')

        # Discipline 2 Sensitivity Analysis
        # dis2 has no local parameter, so there is no need to treat it as
        # a subsystem.

        # System Level Sensitivity Analysis
        # Note, we cheat here and run an MDA instead of solving the
        # GSE equations. Have to put this on the TODO list.
        self.add('ssa', SensitivityDriver())
        self.ssa.workflow.add(['mda'])
        self.ssa.add_parameter(['dis1.z1','dis2.z1'], low=-10.0, high=10.0)
        self.ssa.add_parameter(['dis1.z2','dis2.z2'], low=  0.0, high=10.0)
        self.ssa.add_constraint(constraint1)
        self.ssa.add_constraint(constraint2)
        self.ssa.add_objective(objective, name='obj')

        # Discipline Optimization
        # (Only discipline1 has an optimization input)
        self.add('bbopt1', CONMINdriver())
        self.bbopt1.add_parameter('x1_store', low=0.0, high=10.0, start=1.0)
        self.bbopt1.add_objective('sa_dis1.F[0] + sa_dis1.dF[0][0]*(x1_store-dis1.x1)')
        self.bbopt1.add_constraint('sa_dis1.G[0] + sa_dis1.dG[0][0]*(x1_store-dis1.x1) < 0')
        #this one is technically unncessary
        self.bbopt1.add_constraint('sa_dis1.G[1] + sa_dis1.dG[1][0]*(x1_store-dis1.x1) < 0')

        self.bbopt1.add_constraint('(x1_store-dis1.x1)<.5')
        self.bbopt1.add_constraint('(x1_store-dis1.x1)>-.5')
        self.bbopt1.iprint = 0
        self.bbopt1.linobj = True

        # Global Optimization
        self.add('sysopt', CONMINdriver())
        self.sysopt.add_parameter('z_store[0]', low=-10.0, high=10.0, start=5.0)
        self.sysopt.add_parameter('z_store[1]', low=0.0, high=10.0, start=2.0)
        self.sysopt.add_objective('ssa.F[0]+ ssa.dF[0][0]*(z_store[0]-dis1.z1) + ssa.dF[0][1]*(z_store[1]-dis1.z2)')

        self.sysopt.add_constraint('ssa.G[0] + ssa.dG[0][0]*(z_store[0]-dis1.z1) + ssa.dG[0][1]*(z_store[1]-dis1.z2) < 0')
        self.sysopt.add_constraint('ssa.G[1] + ssa.dG[1][0]*(z_store[0]-dis1.z1) + ssa.dG[1][1]*(z_store[1]-dis1.z2) < 0')

        self.sysopt.add_constraint('z_store[0]-dis1.z1<.5')
        self.sysopt.add_constraint('z_store[0]-dis1.z1>-.5')
        self.sysopt.add_constraint('z_store[1]-dis1.z2<.5')
        self.sysopt.add_constraint('z_store[1]-dis1.z2>-.5')
        self.sysopt.iprint = 0
        self.sysopt.linobj = True

        self.driver.workflow.add(['ssa', 'sa_dis1', 'bbopt1', 'sysopt'])


if __name__ == "__main__": # pragma: no cover

    import time
    import math

    prob = SellarBLISS()
    prob.name = 'top'

    tt = time.time()
    prob.run()
    print "\n"
    print "Minimum found at (%f, %f, %f)" % (prob.dis1.z1,
                                             prob.dis1.z2,
                                             prob.dis1.x1)
    print "Couping vars: %f, %f" % (prob.dis1.y1, prob.dis2.y2)
    print "Minimum objective: ", (prob.dis1.x1)**2 + prob.dis1.z2 + \
                                  prob.dis1.y1 + math.exp(-prob.dis2.y2)
    print "Elapsed time: ", time.time()-tt, "seconds"
