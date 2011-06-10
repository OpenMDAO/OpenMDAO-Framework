"""
    Solution of the sellar analytical problem using classic BLISS.
    Disciplines coupled using Fixed Point Iteration
"""

from openmdao.examples.mdao.disciplines import SellarDiscipline1, \
                                               SellarDiscipline2
from openmdao.main.api import Assembly
from openmdao.lib.datatypes.api import Float
from openmdao.lib.differentiators.finite_difference import FiniteDifference
from openmdao.lib.drivers.api import CONMINdriver, BroydenSolver, SensitivityDriver


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
        
        # Multidisciplinary Analysis
        self.add('mda', BroydenSolver())
        self.mda.workflow.add(['dis1','dis2'])
        self.mda.add_parameter('dis1.y2', low=-9.e99, high=9.e99)
        self.mda.add_constraint('dis2.y2 = dis1.y2')
        self.mda.add_parameter('dis2.y1', low=-9.e99, high=9.e99)
        self.mda.add_constraint('dis2.y1 = dis1.y1')
        
        # System Level Sensitivity Analysis
        # (The mda is run as part of the finite-difference)
        self.add('ssa', SensitivityDriver())
        self.ssa.workflow.add(['mda'])
        self.ssa.add_objective('dis1.y1')
        self.ssa.add_objective('dis2.y2')
        self.ssa.add_parameter(['dis1.z1','dis2.z1'], low=-10.0, high=10.0)
        self.ssa.add_parameter(['dis1.z2','dis2.z2'], low=  0.0, high=10.0)
        self.ssa.create_outputs()
        self.ssa.differentiator = FiniteDifference(self.ssa)
        
        # Discipline 1 Sensitivity Analysis
        self.add('sa_dis1', SensitivityDriver())
        self.sa_dis1.workflow.add(['dis1', 'dis2'])
        self.sa_dis1.add_objective('dis1.y1')
        self.sa_dis1.add_parameter('dis1.x1', low=  0.0, high=10.0, fd_step=.01)
        self.sa_dis1.create_outputs()
        self.sa_dis1.differentiator = FiniteDifference(self.sa_dis1)
        
        # Discipline 2 Sensitivity Analysis
        # dis2 has no local parameter, so there is no need to treat it as
        # a subsystem.
        
        # Discipline Optimization
        # (Only discipline1 has an optimization input)
        self.add('bbopt1', CONMINdriver())
        self.bbopt1.add_parameter('x1_store', low=-1.e99, high=1.e99)
        self.bbopt1.add_objective('(dis1.x1 + x1_store)**2 + ' + \
                                  'dis1.z2 + ' + \
                                  'dis1.y1 + sa_dis1.d__dis1_y1__dis1_x1*x1_store + ' + \
                                  'exp(-dis2.y2)')
        self.bbopt1.add_constraint('dis1.y1 + sa_dis1.d__dis1_y1__dis1_x1*x1_store > 3.16')
        self.bbopt1.add_constraint('dis1.x1 + x1_store >= 0.0')
        self.bbopt1.add_constraint('dis1.x1 + x1_store <= 10.0')
        self.bbopt1.linobj = True
        self.bbopt1.iprint = 2
        
        # Global Optimization
        self.add('sysopt', CONMINdriver())
        self.sysopt.add_parameter('z1_store', low=-1.e99, high=1.e99)
        self.sysopt.add_parameter('z2_store', low=-1.e99, high=1.e99)
        self.sysopt.add_objective('(dis1.x1)**2 + ' + \
                                  'dis1.z2 + z2_store + ' + \
                                  'dis1.y1 + ssa.d__dis1_y1__dis1_z1*z1_store + ssa.d__dis1_y1__dis1_z2*z2_store + ' + \
                                  'exp(-dis2.y2 - ssa.d__dis2_y2__dis1_z1*z1_store - ssa.d__dis2_y2__dis1_z2*z2_store)')
        self.sysopt.add_constraint('dis1.y1 + ssa.d__dis1_y1__dis1_z1*z1_store + ssa.d__dis1_y1__dis1_z2*z2_store > 3.16')
        self.sysopt.add_constraint('dis2.y2 + ssa.d__dis2_y2__dis1_z1*z1_store + ssa.d__dis2_y2__dis1_z2*z2_store < 24.0')
        self.sysopt.add_constraint('dis1.z1 + z1_store >= -10.0')
        self.sysopt.add_constraint('dis1.z1 + z1_store <= 10.0')
        self.sysopt.add_constraint('dis1.z2 + z2_store >= 0.0')
        self.sysopt.add_constraint('dis1.z2 + z2_store <= 10.0')
        self.sysopt.linobj = True
            
        self.driver.workflow.add(['ssa', 'sa_dis1', 'bbopt1', 'sysopt'])
        

        
if __name__ == "__main__": # pragma: no cover         

    import time
    import math
    from openmdao.main.api import set_as_top
    
    prob = SellarBLISS()
    prob.name = "top"
    set_as_top(prob)
            
    prob.dis1.z1 = prob.dis2.z1 = 5.0
    prob.dis1.z2 = prob.dis2.z2 = 2.0
    prob.dis1.x1 = 1.0
    
    
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

