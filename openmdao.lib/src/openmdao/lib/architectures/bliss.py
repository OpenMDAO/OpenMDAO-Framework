from string import Template

from openmdao.main.api import Driver, Architecture,SequentialWorkflow

from openmdao.lib.datatypes.api import Float, Array
from openmdao.lib.differentiators.finite_difference import FiniteDifference
from openmdao.lib.drivers.api import CONMINdriver, BroydenSolver, \
                                     SensitivityDriver, FixedPointIterator

class BLISS(Architecture): 
    """Bi-Level Integrated Systems Synthesis architecture"""
    
    move_limit_percent = Float(default=20.0,low=0.0,high=100.0,iotype="in",desc="percentage of the total variable range "
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
        objective = self.parent.get_objectives().keys()[0]
        constraints = self.parent.list_constraints()
        coupling = self.parent.get_coupling_vars()
        
        self.parent.add('driver',FixedPointIterator())
        self.parent.driver.max_iteration = 50
        self.parent.driver.tolerance = .001
        
        initial_conditions = [self.parent.get(param.target) for comp,param in global_dvs]
        self.parent.add_trait('global_des_vars',Array(initial_conditions))
        for i,(comps,param) in enumerate(global_dvs): 
            targets = comps
            self.parent.driver.add_parameter(targets,low=param.low,high=param.high)
            self.parent.driver.add_constraint("global_des_vars[%d]=%s"%(i,targets[0]))
            
        for comp,local_params in local_dvs.iteritems(): 
            initial_conditions = [self.parent.get(param.targets[0]) for param in local_params]
            self.parent.add_trait('%s_local_des_vars'%comp,Array(initial_conditions))
            for i,param in enumerate(local_params): 
                self.parent.driver.add_parameter(param.targets,low=param.low,high=param.high)
                self.parent.driver.add_constraint('%s_local_des_vars[%d]=%s'%(comp,i,param.targets[0]))
            
        # Multidisciplinary Analysis
        mda = self.parent.add('mda', BroydenSolver())
        self.parent.force_execute=True
        for indep,dep in coupling: 
            mda.add_parameter(indep.target,low=-9.e99, high=9.e99)
            mda.add_constraint("%s=%s"%(indep.target,dep.target))
                
        #Global Sensitivity Analysis
        #TODO: Need to solve GSE here instead of FD on MDA
        ssa = self.parent.add("ssa",SensitivityDriver())
        ssa.workflow.add("mda")
        ssa.differentiator = FiniteDifference(ssa)
        ssa.default_stepsize = 1.0e-6
        ssa.force_execute = True
        ssa.add_objective(objective)
        for comps,param in global_dvs: 
            
            ssa.add_parameter(param.targets,low=param.low,high=param.high)
        for constraint in constraints: 
            ssa.add_constraint(constraint)
    
        #discipline sensitivity analyses
        sa_s = []
        for comp,local_params in local_dvs.iteritems(): 
            sa = self.parent.add('sa_%s'%comp, SensitivityDriver())    
            sa.default_stepsize = 1.0e-6
            sa.force_execute = True
            sa_s.append('sa_%s'%comp)
            for param in local_params: 
                sa.add_parameter(param.targets,low=param.low,high=param.high,fd_step=.001)
            for constraint in constraints:
                sa.add_constraint(constraint)
            sa.add_objective(objective)
            sa.differentiator = FiniteDifference(sa)  #TODO: Fix FinitDifference so you don't need to explicitly pass scope

        
        
        #Linear System Optimizations
        
        # Discipline Optimization
        # (Only discipline1 has an optimization input)
        delta_x = []
        df = []
        dg = []
        
        bbopts = []
        for comp,local_params in local_dvs.iteritems(): 
            bbopt = self.parent.add('bbopt_%s'%comp,CONMINdriver())
            bbopt.linobj = True
            bbopt.iprint = 0
            bbopt.force_execute = True
            bbopts.append('bbopt_%s'%comp)
            
            x_store = "%s_local_des_vars"%comp
            
            for i,param in enumerate(local_params): 
                x_store_i = "%s[%d]"%(x_store,i)
                bbopt.add_parameter(x_store_i,low=param.low,high=param.high)
                dx = "(%s-%s)"%(x_store_i,param.targets[0])
                delta_x.append(dx)
                move_limit = (param.high-param.low)*20.0/100.0
                bbopt.add_constraint("%s < %f"%(dx,move_limit))
                bbopt.add_constraint("%s > -%f"%(dx,move_limit))
                
                df.append("sa_%s.dF[0][%d]*%s"%(comp,i,dx))
            
            #build the linear constraint string for each constraint    
            for j,const in enumerate(constraints): 
                dg_j = ["sa_%s.dG[%d][%d]*%s"%(comp,j,i,x) for i,x in enumerate(delta_x)]
                constraint_parts = ["sa_%s.G[%d]"%(comp,j)]
                constraint_parts.extend(dg_j)
                lin_constraint = "%s < 0"%"+".join(constraint_parts)
                bbopt.add_constraint(lin_constraint)
            

            #build the linear objective string    
            objective_parts = ["sa_%s.F[0]"%comp]
            objective_parts.extend(df)
            lin_objective = "+".join(objective_parts)
            bbopt.add_objective(lin_objective)
            
            
        # Global Optimization
        delta_z = []
        df = []
        dg = []
        
        sysopt = self.parent.add('sysopt', CONMINdriver())
        sysopt.linobj = True
        sysopt.iprint = 0
        sysopt.force_execute = True    
        for i,(comps,param) in enumerate(global_dvs): 
            z_store = "global_des_vars[%d]"%i
            target = list(param.targets)[0]
            sysopt.add_parameter(z_store,low=param.low,high=param.high)
            dz = "(%s-%s)"%(z_store,target)
            delta_z.append(dz)
            move_limit = (param.high-param.low)*20.00/100.0  
            sysopt.add_constraint("%s < %f"%(dz,move_limit))
            sysopt.add_constraint("%s > -%f"%(dz,move_limit))
            
            df.append("ssa.dF[0][%d]*%s"%(i,dz))
            dg_j = ["ssa.dG[%d][%d]*%s"%(j,i,dz) for j,const in enumerate(constraints)]
            dg.append(dg_j)
            
        objective_parts = ["ssa.F[0]"]
        objective_parts.extend(df)
        lin_objective = "+".join(objective_parts)
        sysopt.add_objective(lin_objective)
        
        #build the linear constraint string for each constraint    
        for j,const in enumerate(constraints): 
            dg_j = ["ssa.dG[%d][%d]*%s"%(j,i,x) for i,x in enumerate(delta_z)]
            constraint_parts = ["ssa.G[%d]"%j]
            constraint_parts.extend(dg_j)
            lin_constraint = "%s < 0"%"+".join(constraint_parts)
            sysopt.add_constraint(lin_constraint)

        self.parent.driver.workflow = SequentialWorkflow()
        self.parent.driver.workflow.add("ssa")
        self.parent.driver.workflow.add(sa_s)
        self.parent.driver.workflow.add(bbopts)
        self.parent.driver.workflow.add("sysopt")
        