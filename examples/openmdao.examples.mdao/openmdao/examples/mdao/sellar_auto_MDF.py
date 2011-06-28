"""
    Solution of the sellar analytical problem using MDF.
    Problem forumulation is specified, and MDF is automatically
    set up for you. 
"""
from openmdao.examples.mdao.disciplines import SellarDiscipline1, \
                                               SellarDiscipline2


from openmdao.main.api import Assembly, Slot, implements, Component

from openmdao.main.problem_formulation import ArchitectureAssembly

from openmdao.lib.architectures.api import MDF
        

class SellarMDF(ArchitectureAssembly):
    """ Optimization of the Sellar problem using MDF
    Disciplines coupled with BroydenSolver.
    """
    
    def __init__(self):
        """ Creates a new Assembly with this problem
        
        Optimal Design at (1.9776, 0, 0)
        
        Optimal Objective = 3.18339"""
        
        super(SellarMDF, self).__init__()
        
        #add the discipline components to the assembly
        self.add('dis1', SellarDiscipline1())
        self.add('dis2', SellarDiscipline2())
        
        #START OF MDAO Problem Definition
        #Global Des Vars
        self.add_parameter(("dis1.z1","dis2.z1"),low=-10,high=10)
        self.add_parameter(("dis1.z2","dis2.z2"),low=0,high=10)
        
        #Local Des Vars 
        self.add_parameter("dis1.x1",low=0,high=10)
        
        #Coupling Vars
        self.add_coupling_var("dis2.y1","dis1.y1")
        self.add_coupling_var("dis1.y2","dis2.y2")
                           
        self.add_objective('(dis1.x1)**2 + dis1.z2 + dis1.y1 + math.exp(-dis2.y2)')
        self.add_constraint('3.16 < dis1.y1')
        self.add_constraint('dis2.y2 < 24.0')
        
        #END OF MDAO Problem Definition
         
        self.architecture = MDF()
    
        
        
if __name__ == "__main__": # pragma: no cover

    import time
    from openmdao.main.api import set_as_top
    
    solution = (1.9776, 0, 0)
    
    prob = SellarMDF()
    set_as_top(prob)
        
    prob.dis1.z1 = prob.dis2.z1 = 5.0
    prob.dis1.z2 = prob.dis2.z2 = 2.0
    prob.dis1.x1 = 1.0
    prob.dis1.y2 = 3.0

    tt = time.time()
    prob.run()

    print "\n"
    print "CONMIN Iterations: ", prob.driver.iter_count
    print "Minimum found at (%f, %f, %f)" % (prob.dis1.z1,
                                             prob.dis1.z2,
                                             prob.dis1.x1)
    print "Minimum differs from expected by (%f, %f, %f)" % (prob.dis1.z1-solution[0],
                                                             prob.dis1.z2-solution[1],
                                                             prob.dis1.x1-solution[2])
    print "Couping vars: %f, %f" % (prob.dis1.y1, prob.dis2.y2)
    print "Minimum objective: ", prob.driver.eval_objective()
    print "Elapsed time: ", time.time()-tt, "seconds"
    
    