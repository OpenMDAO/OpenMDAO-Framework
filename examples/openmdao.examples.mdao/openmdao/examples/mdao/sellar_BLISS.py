"""
    Solution of the Sellar analytical problem using classic BLISS.
    (Bi-Level Integrated System Synthesis)
    
    MDA solved with a Broyden solver.
    Global sensitivity calculated by finite-differencing the MDA-coupled
    system. The MDA should be replaced with solution of the GSE to fully
    match the original Sobiesky-Agte implementation.
"""

from openmdao.examples.mdao.disciplines import SellarDiscipline1,\
                                               SellarDiscipline2
from openmdao.main.api import Assembly
from openmdao.lib.datatypes.api import Float
from openmdao.lib.differentiators.finite_difference import FiniteDifference
from openmdao.lib.drivers.api import CONMINdriver, BroydenSolver, \
                                     SensitivityDriver, FixedPointIterator


class SellarBLISS(Assembly):
    """ Optimization of the Sellar problem using the BLISS algorithm
    Disciplines coupled with FixedPointIterator.
    """
    
    z1_store = Float(0.0)
    z2_store = Float(0.0)
    x1_store = Float(0.0)
    
    def __init__(self):
        """ Creates a new Assembly with this problem
        
        Optimal Design at (1.9776, 0, 0)
        
        Optimal Objective = 3.18339"""
                
        super(SellarBLISS, self).__init__()

        # Disciplines
        self.add('dis1', SellarDiscipline1())
        self.add('dis2', SellarDiscipline2())
        
        objective = '(dis1.x1)**2 + dis1.z2 + dis1.y1 + exp(-dis2.y2)'
        constraint1 = '3.16 < dis1.y1'
        constraint2 = 'dis2.y2 < 24.0'
        
        # Top level is Fixed-Point Iteration
        self.add('driver', FixedPointIterator())
        self.driver.add_parameter('dis1.x1', low=  0.0, high=10.0)
        self.driver.add_parameter(['dis1.z1','dis2.z1'], low=-10.0, high=10.0)
        self.driver.add_parameter(['dis1.z2','dis2.z2'], low=  0.0, high=10.0)
        self.driver.add_constraint('x1_store = dis1.x1')
        self.driver.add_constraint('z1_store = dis1.z1')
        self.driver.add_constraint('z2_store = dis1.z2')
        self.driver.max_iteration = 50
        self.driver.tolerance = .001
        
        # Multidisciplinary Analysis
        self.add('mda', BroydenSolver())
        self.mda.workflow.add(['dis1','dis2'])
        self.mda.add_parameter('dis1.y2', low=-9.e99, high=9.e99)
        self.mda.add_constraint('dis2.y2 = dis1.y2')
        self.mda.add_parameter('dis2.y1', low=-9.e99, high=9.e99)
        self.mda.add_constraint('dis2.y1 = dis1.y1')
        
        # Discipline 1 Sensitivity Analysis
        self.add('sa_dis1', SensitivityDriver())
        self.sa_dis1.workflow.add(['dis1', 'dis2'])
        self.sa_dis1.add_parameter('dis1.x1', low=  0.0, high=10.0, fd_step=.01)
        self.sa_dis1.add_objective('dis1.y1')
        self.sa_dis1.add_objective(objective, name='obj')
        self.sa_dis1.differentiator = FiniteDifference(self.sa_dis1)
        self.sa_dis1.default_stepsize = 1.0e-6
        
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
        self.ssa.add_objective('dis1.y1')
        self.ssa.add_objective('dis2.y2')
        self.ssa.add_objective(objective, name='obj')
        self.ssa.differentiator = FiniteDifference(self.ssa)
        self.ssa.default_stepsize = 1.0e-6
        
        # Discipline Optimization
        # (Only discipline1 has an optimization input)
        self.add('bbopt1', CONMINdriver())
        self.bbopt1.add_parameter('x1_store', low=0.0, high=10.0)
        self.bbopt1.add_objective('sa_dis1.dF[1][0]*(x1_store-dis1.x1)')
        self.bbopt1.add_constraint('dis1.y1 + sa_dis1.dF[0][0]*(x1_store-dis1.x1) > 3.16')
        self.bbopt1.linobj = True
        self.bbopt1.iprint = 0
        self.bbopt1.force_execute = True
        
        # Global Optimization
        self.add('sysopt', CONMINdriver())
        self.sysopt.add_parameter('z1_store', low=-10.0, high=10.0)
        self.sysopt.add_parameter('z2_store', low=0.0, high=10.0)
        self.sysopt.add_objective('ssa.dF[2][0]*(z1_store-dis1.z1) + ssa.dF[2][1]*(z2_store-dis1.z2)')
        self.sysopt.add_constraint('dis1.y1 + ssa.dF[0][0]*(z1_store-dis1.z1) + ssa.dF[0][1]*(z2_store-dis1.z2) > 3.16')
        self.sysopt.add_constraint('dis2.y2 + ssa.dF[1][0]*(z1_store-dis1.z1) + ssa.dF[1][1]*(z2_store-dis1.z2) < 24.0')
        self.sysopt.linobj = True
        self.sysopt.iprint = 0
        self.sysopt.force_execute = True
            
        self.driver.workflow.add(['ssa', 'sa_dis1', 'bbopt1', 'sysopt'])
        

        
if __name__ == "__main__": # pragma: no cover         

    import time
    import math
    from openmdao.main.api import set_as_top
    
    prob = SellarBLISS()
    prob.name = "top"
    set_as_top(prob)
            
    prob.dis1.z1 = prob.dis2.z1 = z1_store = 5.0
    prob.dis1.z2 = prob.dis2.z2 = z2_store = 2.0
    prob.dis1.x1 = x1_store = 1.0
    
    
    tt = time.time()
    prob.run()
    
    print "\n"
    print prob.z1_store, prob.z2_store, prob.x1_store
    print "\n"
    print "Minimum found at (%f, %f, %f)" % (prob.dis1.z1, \
                                             prob.dis1.z2, \
                                             prob.dis1.x1)
    print "Couping vars: %f, %f" % (prob.dis1.y1, prob.dis2.y2)
    print "Minimum objective: ", (prob.dis1.x1)**2 + prob.dis1.z2 + prob.dis1.y1 + math.exp(-prob.dis2.y2)
    print "Elapsed time: ", time.time()-tt, "seconds"

