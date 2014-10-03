"""Implementation of the Collaborative Optimization Architecture"""

import numpy as np

from openmdao.main.api import Driver, Architecture
from openmdao.lib.drivers.api import SLSQPdriver#, COBYLAdriver as SLSQPdriver
from openmdao.main.datatypes.api import Float, Array

class CO(Architecture): 
    
    def __init__(self, *args, **kwargs):
        super(CO, self).__init__(*args, **kwargs)
        
        # the following variables determine the behavior of check_config
        self.param_types = ['continuous']
        self.constraint_types = ['ineq']
        self.num_allowed_objectives = 1
        self.has_coupling_vars = True
        self.has_global_des_vars = True
        
    def configure(self): 
         
        global_dvs = self.parent.get_global_des_vars()        
        local_dvs = self.parent.get_local_des_vars()
        all_dvs_by_comp = self.parent.get_des_vars_by_comp()
        
        objective = self.parent.get_objectives()
        
        constraints = self.parent.list_constraints()
        constraints_by_comp = self.parent.get_constraints_by_comp()        
        
        coupling = self.parent.list_coupling_vars()
        coupl_indeps_by_comp = self.parent.get_coupling_indeps_by_comp()
        coupl_deps_by_comp = self.parent.get_coupling_deps_by_comp()
        
        self.target_var_map = dict()
        
        #Global Driver    
        global_opt = self.parent.add('driver', SLSQPdriver())
        global_opt.recorders = self.data_recorders

        #set initial values 
        for comp,param in global_dvs: 
            param.initialize(self.parent, self.parent)

        for comp,param in local_dvs: 
            param.initialize(self.parent, self.parent)

        for key,couple in coupling.iteritems(): 
            couple.indep.set(couple.start)   
        
        initial_conditions = np.array([param.evaluate() for comp,param in global_dvs]).flatten()
        self.parent.add_trait('global_des_var_targets',Array(initial_conditions, iotype="in"))
        for i,(comp,param) in enumerate(global_dvs): 
            target_var = 'global_des_var_targets[%d]'%i
            
            global_opt.add_parameter(target_var,low=param.low,high=param.high)
            
            #associate all targets with this target variable for global optimizer
            for var in param.targets: 
                self.target_var_map[var] = target_var
                
        initial_conditions = [couple.indep.evaluate() for key,couple in coupling.iteritems()]   
        #print "coupling initial conditions: ", initial_conditions
        self.parent.add_trait('coupling_var_targets',Array(initial_conditions, iotype="in"))
        for i,(key,couple) in enumerate(coupling.iteritems()): 
            target_var = 'coupling_var_targets[%d]'%i
            low = couple.indep.low or -1e99
            high = couple.indep.high or 1e99
            global_opt.add_parameter(target_var,low=low,high=high)
            self.target_var_map[couple.indep.target] = target_var
            self.target_var_map[couple.dep.target] = target_var
            
        
        initial_conditions = [param.evaluate()[0] for comp,param in local_dvs]    
        #print "local initial conditions: ", initial_conditions, initial_conditions.shape; exit()
        self.parent.add_trait("local_des_var_targets",Array(initial_conditions, iotype="in"))
        for i,(comp,param) in enumerate(local_dvs):
            #Target variables for the local optimizations
            target_var = 'local_des_var_targets[%d]'%i
            self.target_var_map[param.target] = target_var
            global_opt.add_parameter(target_var,low=param.low,high=param.high)
            #print "param: ",target_var,param.low,param.high
        
            
        #create the new objective with the target variables
        obj = objective.items()[0]

        new_objective = str(obj[1].text)
        for old_var,new_var in sorted(self.target_var_map.items(),key=lambda x: len(x[0]), reverse=True):    
            new_objective = new_objective.replace(old_var,new_var)
            
        global_opt.add_objective(new_objective)
        


        #setup the local optimizations
        for comp,params in all_dvs_by_comp.iteritems(): 
            local_opt = self.parent.add('local_opt_%s'%comp,SLSQPdriver())
            local_opt.iprint = 0
            global_opt.workflow.add(local_opt.name)
            residuals = []
            for param in params: 
                local_opt.add_parameter(param.target,low=param.low,high=param.high)
                residuals.append("(%s-%s)**2"%(self.target_var_map[param.target],param.target))
            if comp in coupl_indeps_by_comp: 
                for couple in coupl_indeps_by_comp[comp]: 
                    if couple.indep.low is not None: 
                        low = couple.indep.low 
                    else: 
                        low = -1e99
                    
                    if couple.indep.high is not None: 
                        high = couple.indep.high 
                    else: 
                        high = 1e99

                    #print couple, couple.indep.get_metadata()
                    #exit()
                    local_opt.add_parameter(couple.indep.target,low=low,high=high)
                    residuals.append("(%s-%s)**2"%(self.target_var_map[couple.indep.target],couple.indep.target))
            if comp in coupl_deps_by_comp: 
                for couple in coupl_deps_by_comp[comp]: 
                    residuals.append("(%s-%s)**2"%(self.target_var_map[couple.dep.target],couple.dep.target))     
            if comp in constraints_by_comp: 
                for const in constraints_by_comp[comp]: 
                    local_opt.add_constraint(str(const))
                
            residuals = "+".join(residuals)    
            global_constraint = "%s<=0"%residuals
            global_opt.add_constraint(global_constraint)
            local_opt.add_objective(residuals)



if __name__ == "__main__": 
    from openmdao.lib.optproblems.api import SellarProblem
    #from openmdao.main.api import ArchitectureAssembly


    prob = SellarProblem()
    
    prob.architecture = CO()

    prob.run()

    print "\n"
    print "Minimum found at (%f, %f, %f)" % (prob.dis1.z1, \
                                             prob.dis1.z2, \
                                             prob.dis1.x1)
    print "Minimum target was at (%f, %f, %f)" % (prob.global_des_var_targets[0], \
                                             prob.global_des_var_targets[1], \
                                             prob.local_des_var_targets[0])
    print "Coupling vars: %f, %f" % (prob.dis1.y1, prob.dis2.y2)
    print "Coupling var targets: %f, %f" % (prob.coupling_var_targets[0], prob.coupling_var_targets[1])
    print "Minimum objective: ", prob.driver.eval_objective()

