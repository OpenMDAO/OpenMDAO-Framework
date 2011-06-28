from openmdao.main.api import Driver, Architecture

from openmdao.lib.datatypes.api import Float, Array
from openmdao.lib.differentiators.finite_difference import FiniteDifference
from openmdao.lib.drivers.api import CONMINdriver, BroydenSolver, \
                                     SensitivityDriver, FixedPointIterator

class BLISS(Architecture): 
    """Bi-Level Integrated Systems Synthesis architecture"""
    
    def __init__(self): 
        super(BLISS,self).__init__()
        
        # the following variables determine the behavior of check_config
        self.param_types = ['continuous']
        self.constraint_types = ['ineq']
        self.num_allowed_objectives = 1
        self.has_coupling_vars = True
        
    def configure(self): 
        
        global_dvs = self.parent.get_global_des_vars()
        local_dvs = self.parent.get_local_des_vars_by_comp()
        objective = self.parent.get_objective()
        constraints = self.parent.get_constraints()
        
        self.parent.add('driver',FixedPointIterator())
        self.parent.driver.max_iteration = 50
        self.parent.driver.tolerance = .001
        
        self.parent.add_trait('global_des_vars',Array([0]*len(global_dvs)))
        for i,param in enumerate(global_dvs): 
            targets = list(param.targets)
            self.parent.driver.add_parameter(targets,low=param.low,high=param.high)
            self.parent.driver.add_constraint("global_des_vars[%d]=%s"%(i,target[0]))
        
        for comp,local_params in local_dvs.iteritems(): 
            self.parent.add_trait('%s_local_des_vars'%comp,Array([0]*len(local_param)))
            for i,param in enumerate(local_params): 
                self.parent.driver.add_parameter(param.targets,low=param.low,high=param.high)
                self.parent.driver.add_constraint('%s_local_des_vars[%d]=%s'%(comp,i,param.targets))
            
        
        
        
        for comp,local_params in local_dvs.iteritems(): 
            