from string import Template

from openmdao.main.api import Driver, Architecture

from openmdao.lib.datatypes.api import Float, Array
from openmdao.lib.differentiators.finite_difference import FiniteDifference
from openmdao.lib.drivers.api import CONMINdriver, BroydenSolver, \
                                     SensitivityDriver, FixedPointIterator

class BLISS(Architecture): 
    """Bi-Level Integrated Systems Synthesis architecture"""
    
    move_limit_percent = Float(20.0,low=0.0,high=100.0,iotype="in",desc="percentage of the total variable range "
                               "to use for the move limit. Value will be used to limit the optimization"
                               "of the linear models to + or - this percentage from the current value")    
    

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
        coupling = self.parent.get_coupling_vars()
        
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
            
        # Multidisciplinary Analysis
        self.parent.add('mda', BroydenSolver())
        self.parent.force_execute=True
        for indep,dep in coupling: 
            self.parent.mda.add_parameter(indep,low=-9.e99, high=9.e99)
            self.parent.add_constraint("%s=%s"%(indep,dep))
            

        #Global Sensitivity Analysis
        #TODO: Need to solve GSE here instead of FD on MDA
        ssa = self.parent.add("ssa",SensitivityDriver())
        ssa.workflow.add("mda")
        ssa.differentiator = FiniteDifference(ssa)
        ssa.default_stepsize = 1.0e-6
        ssa.force_execute = True
        ssa.add_objective(objective)
        for param in global_dvs: 
            ssa.add_parameter(param.targets,low=param.low,high=param.high)
        for constraint in constraints: 
            ssa.add_constraint(constraint)
        ssa.add_objective(objective)
        self.parent.driver.workflow.add("ssa")
            
        #discipline sensitivity analyses
        for comp,local_params in local_dvs.iteritems(): 
            sa = self.parent.add('sa_%s'%comp, SensitivityDriver())    
            sa.differentiator = FiniteDifference(sa)  #TODO: Fix FinitDifference so you don't need to explicitly pass scope
            sa.default_stepsize = 1.0e-6
            sa.force_execute = True
            for param in local_params: 
                sa.add_parameter(param.targets,low=param.low,high=param.high)
            for constraint in constraints:
                sa.add_constraint(constraint)
            sa.add_objective(objective)
            self.parent.driver.workflow.add('sa_%s'%comp)
        
        
        #Linear System Optimizations
        
        # Discipline Optimization
        # (Only discipline1 has an optimization input)
        delta_x = []
        df = []
        dg = []
        for comp,local_params in local_dvs.iteritems(): 
            bbopt = self.parent.add('bbopt_%s'%comp,CONMINdriver())
            bbopt.linobj = True
            bbopt.iprint = 0
            bbopt.force_execute = True
            
            x_store = "%s_local_des_vars"%comp
            
            for i,param in enumerate(local_params): 
                bbopt.add_parameter(param.targets,low=param.low,high=param.high)
                delta_x.append("(%s[%d]-%s)"%(x_store,i,param.targets))
                move_limit = (param.high-param.low)*100.0/self.move_limit_percent
                bbopt.add_constraint("%s < %f"%(delta_x[-1],move_limit))
                bbopt.add_constraint("%s > -%f"%(delta_x[-1],move_limit))
                
                df.append("sa_%s.dF[0][%d]"%(comp,i))
                dg_j = ["sa_%s.dG[%d][%d]"%(j,i) for j,const in enumerate(constraints)]
                dg.append(dg_j)    
            
            #build the linear constraint string    
            for j,dg_j in enumerate(dg): 
                constraint_parts = ["sa_%s.G[%d]"%(comp,j)].extend(dg_j)
                lin_constraint = "%s < 0"%constraint_pars.join("+")
                bbopt.add_constraint(lin_constraint)
            
            #build the linear objective string    
            objective_parts = ["sa_%s.F[0]"%comp].extend(delta_x)
            lin_objective = objective_parts.join("+")
            bbopt.add_objective(lin_objective)
            
            self.parent.driver.workflow.add('bbopt_%s'%comp)
        
            