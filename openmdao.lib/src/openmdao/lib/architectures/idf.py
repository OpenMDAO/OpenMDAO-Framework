from openmdao.main.api import Driver, Architecture
from openmdao.lib.drivers.api import CONMINdriver

class IDF(Architecture):
    
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
        

        for k,v in self.parent.get_global_des_vars(): 
            global_dvs.append(v)
            self.parent.driver.add_parameter(v,name=k) # and add the broadcast parameters to the driver
        
        for k,v in self.parent.get_local_des_vars(): 
            local_dvs.append(v)
            #add the local design variables to the driver
            self.parent.driver.add_parameter(v,name=k)

        #TODO: possibly add method for passing constraint directly?
        #add the constraints to the driver
        for const in self.parent.list_constraints(): 
            self.parent.driver.add_constraint(const)
            
        #set the objective
        objective = self.parent.get_objectives().items()[0]
        self.parent.driver.add_objective(objective[1].text, name=objective[0])
        
        #add the coupling vars parameters/constraints to the solver
        for key,couple in self.parent.get_coupling_vars().iteritems(): 
            self.parent.driver.add_parameter(couple.indep.target, low=-9.e99, high=9.e99,name=key)
            self.parent.driver.add_constraint("%s<=%s"%(couple.indep.target,couple.dep.target))
            self.parent.driver.add_constraint("%s>=%s"%(couple.indep.target,couple.dep.target))
