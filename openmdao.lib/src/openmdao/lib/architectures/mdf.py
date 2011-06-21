from openmdao.main.api import Driver, Architecture, implements
from openmdao.lib.drivers.api import CONMINdriver, BroydenSolver

class MDF(Architecture):
    
    def __init__(self, *args, **kwargs):
        super(MDF, self).__init__(*args, **kwargs)
        
        # the following variables determine the behavior of check_config
        self.param_types = ['continuous']
        self.constraint_types = ['ineq']
        self.num_allowed_objectives = 1
        self.has_coupling_vars = True
    
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
        
        #For MDF all disciplines get solved in the MDA, but other
        #architectures might need to identify disciplines on a more granular
        #level. This is all done via the parameter
        #disciplines = set()

        for k,v in self.parent.get_global_des_vars(): 
            global_dvs.append(v)
            #disciplines.update(v.get_referenced_compnames())
        for k,v in self.parent.get_local_des_vars(): 
            local_dvs.append(v)
            #disciplines.update(v.get_referenced_compnames())
        
        #TODO: possibly add methods for passing parameters directly?
        #connect the broadcast outputs to the disciplines
        # and add the broadcast parameters to the driver
        for glb_var in global_dvs: 
            self.parent.driver.add_parameter(glb_var.targets,low=glb_var.low,
                                             high=glb_var.high)   
            
        #TODO: possibly add methods for passing parameters directly?
        #add the local design variables to the driver
        for loc_var in local_dvs:
            self.parent.driver.add_parameter(loc_var.targets,low=loc_var.low,
                                             high=loc_var.high)
         
        #TODO: possibly add method for passing constraint directly?
        #add the constraints to the driver
        for const in self.parent.list_constraints(): 
            self.parent.driver.add_constraint(const)
            
        #set the global objective
        self.parent.driver.add_objectives(self.parent.get_objectives().keys())
            
        #setup the inner loop solver
        self.parent.add('solver', BroydenSolver())    
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
        #self.parent.solver.workflow.add(disciplines)
