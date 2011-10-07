from openmdao.main.api import Driver, Architecture
from openmdao.lib.drivers.api import CONMINdriver
from openmdao.lib.datatypes.api import Float, Array


class CO(Architecture): 
    
    def __init__(self, *args, **kwargs):
        super(CO, self).__init__(*args, **kwargs)
        
        # the following variables determine the behavior of check_config
        self.param_types = ['continuous']
        self.constraint_types = ['ineq']
        self.num_allowed_objectives = 1
        self.has_coupling_vars = True
        
    def configure(self): 
         
         
        global_dvs = self.parent.get_global_des_vars()        
        local_dvs = self.parent.get_local_des_vars()
        all_dvs_by_comp = self.parent.get_des_vars_by_comp()
        
        objective = self.parent.get_objectives()
        
        constraints = self.parent.list_constraints()
        constraints_by_comp = self.parent.get_constraints_by_comp()        
        
        coupling = self.parent.get_coupling_vars()
        coupl_indeps_by_comp = self.parent.get_coupling_indeps_by_comp()
        coupl_deps_by_comp = self.parent.get_coupling_deps_by_comp()
        
        self.target_var_map = dict()
        
        #Global Driver    
        global_opt = self.parent.add('driver', CONMINdriver()) 
        global_opt.print_vars = ['dis1.y1', 'dis2.y2']
        global_opt.iprint = 0
        global_opt.itmax = 100
        global_opt.fdch = .003
        global_opt.fdchm = .003
        global_opt.delfun = .0001
        global_opt.dabfun = .00001
        global_opt.ct = -.0008
        global_opt.ctlmin = 0.0008  
        
        
        
        
        initial_conditions = [param.evaluate() for comp,param in global_dvs]
        #print "global initial conditions: ", initial_conditions
        self.parent.add_trait('global_des_var_targets',Array(initial_conditions))
        for i,(comp,param) in enumerate(global_dvs): 
            target_var = 'global_des_var_targets[%d]'%i
            
            global_opt.add_parameter(target_var,low=param.low,high=param.high)
            
            #associate all targets with this target variable for global optimizer
            for var in param.targets: 
                self.target_var_map[var] = target_var
                
        initial_conditions = [couple.indep.evaluate() for key,couple in coupling.iteritems()]   
        #print "coupling initial conditions: ", initial_conditions
        self.parent.add_trait('coupling_var_targets',Array(initial_conditions))
        for i,(key,couple) in enumerate(coupling.iteritems()): 
            target_var = 'coupling_var_targets[%d]'%i
            low = couple.indep.low or -1e99
            high = couple.indep.high or 1e99
            global_opt.add_parameter(target_var,low=low,high=high)
            self.target_var_map[couple.indep.target] = target_var
            self.target_var_map[couple.dep.target] = target_var
            
        
        initial_conditions = [param.evaluate() for comp,param in local_dvs]    
        #print "local initial conditions: ", initial_conditions
        self.parent.add_trait("local_des_var_targets",Array(initial_conditions))
        for i,(comp,param) in enumerate(local_dvs):
            #Target variables for the local optimizations
            target_var = 'local_des_var_targets[%d]'%i
            self.target_var_map[param.target] = target_var
            global_opt.add_parameter(target_var,low=param.low,high=param.high)
            #print "param: ",target_var,param.low,param.high
        
            
        #create the new objective with the target variables
        obj = objective.items()[0]
        varpaths = obj[1].get_referenced_varpaths()
        new_objective = obj[1].text         
        for var in varpaths: 
            new_objective = new_objective.replace(var,self.target_var_map[var])
        global_opt.add_objective(new_objective,name=obj[1])
        
        #setup the local optimizations
        for comp,params in all_dvs_by_comp.iteritems(): 
            local_opt = self.parent.add('local_opt_%s'%comp,CONMINdriver())
            global_opt.workflow.add(local_opt.name)
            residuals = []
            for param in params: 
                local_opt.add_parameter(param.target,low=param.low,high=param.high)
                residuals.append("(%s-%s)**2"%(self.target_var_map[param.target],param.target))
            if comp in coupl_indeps_by_comp: 
                for indep in coupl_indeps_by_comp[comp]: 
                    low = indep.low or -1e99
                    high = indep.high or 1e99
                    local_opt.add_parameter(indep.target,low=low,high=high)
                    residuals.append("(%s-%s)**2"%(self.target_var_map[indep.target],indep.target))
            if comp in coupl_deps_by_comp: 
                for dep in coupl_deps_by_comp[comp]: 
                    residuals.append("(%s-%s)**2"%(self.target_var_map[dep.target],dep.target))     
            if comp in constraints_by_comp: 
                for const in constraints_by_comp[comp]: 
                    local_opt.add_constraint(str(const))
                
            residuals = "+".join(residuals)    
            global_constraint = "%s<=.001"%residuals
            global_opt.add_constraint(global_constraint)
            local_opt.add_objective(residuals)
            local_opt.iprint = 0
            local_opt.itmax = 100
            local_opt.fdch = .001
            local_opt.fdchm = .001
            local_opt.delfun = .0001
            local_opt.dabfun = .000001
            local_opt.force_execute = True

        """    print local_opt.name
            print local_opt.get_objectives().keys()[0]
            for param in local_opt.get_parameters(): 
                print param
            print "constraints: "     
            for constraint in local_opt.list_constraints(): 
                print constraint
            
            print 
            print
            
        print global_opt.name
        print global_opt.get_objectives().keys()[0]
        for param in global_opt.get_parameters(): 
            print param
        print "constraints: "     
        for constraint in global_opt.list_constraints(): 
            print constraint
        
        print 
        print"""
            
            
            