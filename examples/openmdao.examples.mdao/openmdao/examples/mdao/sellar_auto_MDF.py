"""
    Solution of the sellar analytical problem using MDF.
    Problem forumulation is specified, and MDF is automatically
    set up for you. 
"""

# pylint: disable-msg=E0611,F0401
from openmdao.examples.mdao.disciplines import SellarDiscipline1, \
                                               SellarDiscipline2


from openmdao.main.api import Assembly, set_as_top
from openmdao.lib.drivers.api import CONMINdriver, BroydenSolver
from openmdao.lib.components.api import FloatBroadcaster
from openmdao.lib.datatypes.api import Float


class GlobalDesVar(object): 
    def __init__(self): 
        self.name = None
        self.vars = []
        self.low = None
        self.high = None
        
class LocalDesVar(object): 
    def __init__(self): 
        self.var = []
        self.low = None
        self.high = None      
        
class CouplingVar(object): 
    def __init__(self): 
        self.vary = None
        self.constraint= None

class SellarMDF(Assembly):
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
        
        g1 = GlobalDesVar()
        g1.name = "z1"
        g1.vars = ('dis1.z1',"dis2.z1")
        g1.low = -10
        g1.high = 10
        
        g2 = GlobalDesVar()
        g2.name = "z2"
        g2.vars = ("dis1.z2","dis2.z2")
        g2.low = 0
        g2.high = 10
        
        loc1 = LocalDesVar()
        loc1.var = "dis1.x1"
        loc1.low = 0
        loc1.high = 10
        
        c1 = CouplingVar()
        c1.vary = "dis2.y1"
        c1.constraint = "dis1.y1=dis2.y1"
        
        c2 = CouplingVar()
        c2.vary = "dis1.y2"
        c2.constraint = "dis1.y2=dis2.y2"
        
        
        self.global_des_vars = [g1,g2]
        self.local_des_vars = [loc1,]
        self.coupling_vars = [c1,c2]
        
        self.objective = '(dis1.x1)**2 + bcast.z2 + dis1.y1 + math.exp(-dis2.y2)'
        self.constraints = ['3.16 < dis1.y1',
                            'dis2.y2 < 24.0' ]
        
        
    def configure_MDF(self,discipline1,discipline2): 
        """setup and MDF architecture inside this assembly.
        
        discipline1: Name of component which should comprise Discipline 1 for the MDF
        discipline2: Name of component which should comprise Discipline 2 for the MDF
        """
        
        #make a broadcaster for the globals
        glb_names = [g.name for g in self.global_des_vars]
        self.add('bcast',FloatBroadcaster(glb_names))
        
        #create the top level optimizer
        self.add("driver",CONMINdriver())
        self.driver.cons_is_linear = [1]*len(self.constraints)
        self.driver.iprint = 0
        self.driver.itmax = 30
        self.driver.fdch = .001
        self.driver.fdchm = .001
        self.driver.delfun = .0001
        self.driver.dabfun = .000001
        self.driver.ctlmin = 0.0001
        
        #connect the broadcast outputs to the disciplines
        # and add the broadcast parameters to the driver
        for glb_var in self.global_des_vars: 
            for var in glb_var.vars:
                self.set('bcast.%s_in'%glb_var.name,self.get(var))
                self.connect('bcast.%s'%glb_var.name,var) 
            self.driver.add_parameter('bcast.%s_in'%glb_var.name,low=glb_var.low,high=glb_var.high)    
                
        #add the constraints to the driver
        for const in self.constraints: 
            self.driver.add_constraint(const)
        #add the local design variables to the driver
        for loc_var in self.local_des_vars:
            self.driver.add_parameter(loc_var.var,low=loc_var.low,high=loc_var.high)
            
        #set the global objective
        self.driver.add_objective(self.objective)
            
        #setup the inner loop solver    
        self.add('solver',BroydenSolver())    
        self.solver.itmax = 10
        self.solver.alpha = .4
        self.solver.tol = .0000001
        self.solver.algorithm = "broyden2"
        
        #add the coupling vars parameters/constraints to the solver
        for cpl_var in self.coupling_vars: 
            self.solver.add_parameter(cpl_var.vary, low=-9.e99, high=9.e99)
            self.solver.add_constraint(cpl_var.constraint)
            
        #setup the workflows
        self.driver.workflow.add(['bcast','solver'])
        self.solver.workflow.add([discipline1,discipline2])
        
        
if __name__ == "__main__": # pragma: no cover         

    import time
    
    prob = SellarMDF()
    set_as_top(prob)
    prob.configure_MDF('dis1','dis2')
    # pylint: disable-msg=E1101
        
    prob.bcast.z1_in = 5.0
    prob.bcast.z2_in = 2.0
    prob.dis1.x1 = 1.0
    prob.dis2.z1_in = 5.0
    prob.dis2.z2_in = 2.0
    
    
    
    tt = time.time()
    prob.run()

    print "\n"
    print "CONMIN Iterations: ", prob.driver.iter_count
    print "Minimum found at (%f, %f, %f)" % (prob.bcast.z1_in, \
                                             prob.bcast.z2_in, \
                                             prob.dis1.x1)
    print "Couping vars: %f, %f" % (prob.dis1.y1, prob.dis2.y2)
    print "Minimum objective: ", prob.driver.eval_objective()
    print "Elapsed time: ", time.time()-tt, "seconds"        
            
            

        
        
        