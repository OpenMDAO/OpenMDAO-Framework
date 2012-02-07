from openmdao.main.api import Driver, Architecture
from openmdao.lib.drivers.api import CONMINdriver

class IDF(Architecture):
    
    """ Architecture that uses a single top level optimizer, 
    enforcing consitency with equality constraints"""
    
    def __init__(self, *args, **kwargs):
        super(IDF, self).__init__(*args, **kwargs)
        
        # the following variables determine the behavior of check_config
        self.param_types = ['continuous']
        self.constraint_types = ['ineq']
        self.num_allowed_objectives = 1
        self.has_coupling_vars = True
        self.has_global_des_vars = False
    
    def configure(self): 
        """setup and IDF architecture inside this assembly.
        """
        #create the top level optimizer
        self.parent.add("driver",CONMINdriver())
        self.parent.driver.iprint = 0
        self.parent.driver.itmax = 50
        self.parent.driver.fdch = .0001
        self.parent.driver.fdchm = .0001
        self.parent.driver.delfun = .0001
        self.parent.driver.dabfun = .0001
        self.parent.driver.ctlmin = .00001
        #self.parent.driver.ct = -0.01
        
        self.parent.driver.recorders = self.data_recorders
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
            self.parent.driver.add_constraint("(%s-%s)<=0"%(couple.indep.target,couple.dep.target))
            self.parent.driver.add_constraint("(%s-%s)<=0"%(couple.dep.target,couple.indep.target))
            
