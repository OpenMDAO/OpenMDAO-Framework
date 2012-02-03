"""
    Solution of the Sellar analytical problem using classic BLISS.
    (Bi-Level Integrated System Synthesis)
    
    MDA solved with a Broyden solver.
    Global sensitivity calculated by finite-differencing the MDA-coupled
    system. The MDA should be replaced with solution of the GSE to fully
    match the original Sobiesky-Agte implementation.
"""

from openmdao.main.api import Assembly
from openmdao.lib.datatypes.api import Float, Array
from openmdao.lib.differentiators.finite_difference import FiniteDifference
from openmdao.lib.drivers.api import CONMINdriver, BroydenSolver, \
                                     SensitivityDriver, FixedPointIterator

from openmdao.lib.optproblems import sellar


class SellarBLISS2000(Assembly):
    """ Optimization of the Sellar problem using the BLISS algorithm
    Disciplines coupled with FixedPointIterator.
    """

    z_store = Array([0,0],dtype=Float)
    x1_store = Float(0.0)
    
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
        
        

        
if __name__ == "__main__": # pragma: no cover         

    import time
    import math
    
    prob = SellarBLISS()
    prob.name = "top"
            
    tt = time.time()
    prob.run()
    print "\n"
    print "Minimum found at (%f, %f, %f)" % (prob.dis1.z1, \
                                             prob.dis1.z2, \
                                             prob.dis1.x1)
    print "Couping vars: %f, %f" % (prob.dis1.y1, prob.dis2.y2)
    print "Minimum objective: ", (prob.dis1.x1)**2 + prob.dis1.z2 + prob.dis1.y1 + math.exp(-prob.dis2.y2)
    print "Elapsed time: ", time.time()-tt, "seconds"