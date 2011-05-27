"""
    Solution of the sellar analytical problem using MDF.
    Problem forumulation is specified, and MDF is automatically
    set up for you. 
"""
from openmdao.examples.mdao.disciplines import SellarDiscipline1, \
                                               SellarDiscipline2


from openmdao.main.api import Assembly, set_as_top, Slot, implements, Component

from openmdao.util.decorators import add_delegate
from openmdao.main.problem_formulation import IArchitecture, ArchitectureAssembly

from openmdao.lib.drivers.api import CONMINdriver, BroydenSolver
from openmdao.lib.datatypes.api import Float, List

class MDF(object): 
    implements(IArchitecture)
    
    def configure(self): 
        """setup and MDF architecture inside this assembly.
        """
                
        #create the top level optimizer
        self.parent.add("driver",CONMINdriver())
        self.parent.driver.cons_is_linear = [1]*len(self.parent.list_constraints())
        self.parent.driver.iprint = 0
        self.parent.driver.itmax = 30
        self.parent.driver.fdch = .001
        self.parent.driver.fdchm = .001
        self.parent.driver.delfun = .0001
        self.parent.driver.dabfun = .000001
        self.parent.driver.ctlmin = 0.0001
        
        params = self.parent.get_parameters()
        global_dvs = []
        local_dvs = []
        
        #For MDF all disciplines get solved in the MDA, but other architectures might need to 
        #identify disciplines on a more granular level. This is all done via the parameter
        disciplines = set()

        #TODO: methods in has parameters for this? 
        #need to split into global and local des vars
        for k,v in params.iteritems(): 
            if isinstance(k,tuple): 
                global_dvs.append(v)
                disciplines.update(v.get_referenced_compnames())
            else: 
                local_dvs.append(v)
                disciplines.update(v.get_referenced_compnames())
        
        
        #TODO: possibly add methods for passing parameters directly?         
        #connect the broadcast outputs to the disciplines
        # and add the broadcast parameters to the driver
        for glb_var in global_dvs: 
            self.parent.driver.add_parameter(glb_var.targets,low=glb_var.low,high=glb_var.high)   
            
        #TODO: possibly add methods for passing parameters directly?     
        #add the local design variables to the driver
        for loc_var in local_dvs:
            self.parent.driver.add_parameter(loc_var.targets,low=loc_var.low,high=loc_var.high)    
         
        #TODO: possibly add method for passing constraint directly?     
        #add the constraints to the driver
        for const in self.parent.list_constraints(): 
            self.parent.driver.add_constraint(str(const))
            
        #set the global objective
        self.parent.driver.add_objective(self.parent.list_objective())
            
        #setup the inner loop solver    
        self.parent.add('solver',BroydenSolver())    
        self.parent.solver.itmax = 10
        self.parent.solver.alpha = .4
        self.parent.solver.tol = .0000001
        self.parent.solver.algorithm = "broyden2"
        
        #add the coupling vars parameters/constraints to the solver
        for indep,dep in self.parent.list_coupling_vars(): 
            self.parent.solver.add_parameter(indep, low=-9.e99, high=9.e99)
            self.parent.solver.add_constraint("%s=%s"%(indep,dep))
            
        #setup the workflows
        self.parent.driver.workflow.add(['solver'])
        self.parent.solver.workflow.add(disciplines)
        
    #TODO: would like some kind of an automatic handling of this, but for now
    #      I could just by hand tear it all down. 
    def tear_down(self): 
        pass
        

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
        self.add_parameter(('dis1.z1',"dis2.z1"),low=-10,high=10)
        self.add_parameter(("dis1.z2","dis2.z2"),low=0,high=10)
        
        #Local Des Vars 
        self.add_parameter("dis1.x1",low=0,high=10)
        
        #Coupling Vars
        self.add_coupling_var("dis2.y1","dis1.y1=dis2.y1")        
        self.add_coupling_var("dis1.y2","dis1.y2=dis2.y2")
        
        self.add_objective('(dis1.x1)**2 + dis1.z2 + dis1.y1 + math.exp(-dis2.y2)')
        self.add_constraint('3.16 < dis1.y1')
        self.add_constraint('dis2.y2 < 24.0')
        
        #END OF MDAO Problem Definition
         
        self.architecture = MDF()
    
        
        
if __name__ == "__main__": # pragma: no cover         

    import time
    
    prob = SellarMDF()
    set_as_top(prob)
    prob.configure()
        
    prob.dis1.z1 = prob.dis2.z1 = 5.0
    prob.dis1.z2 = prob.dis2.z2 = 2.0
    prob.dis1.x1 = 1.0

    tt = time.time()
    prob.run()

    print "\n"
    print "CONMIN Iterations: ", prob.driver.iter_count
    print "Minimum found at (%f, %f, %f)" % (prob.dis1.z1, \
                                             prob.dis1.z2, \
                                             prob.dis1.x1)
    print "Couping vars: %f, %f" % (prob.dis1.y1, prob.dis2.y2)
    print "Minimum objective: ", prob.driver.eval_objective()
    print "Elapsed time: ", time.time()-tt, "seconds"        