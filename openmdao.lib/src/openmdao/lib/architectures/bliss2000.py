from openmdao.main.api import Driver, Architecture,SequentialWorkflow, Component, Assembly
from openmdao.lib.drivers.api import CONMINdriver, BroydenSolver,IterateUntil,FixedPointIterator,NeiborhoodDOEdriver
from openmdao.lib.surrogatemodels.api import ResponseSurface
from openmdao.lib.doegenerators.api import CentralComposite, OptLatinHypercube
from openmdao.lib.components.api import MetaModel
from openmdao.lib.datatypes.api import Float, Array, Slot
from openmdao.lib.casehandlers.api import DBCaseRecorder

class SubSystemObj(Component): 

    f_wy = Float(0.0,iotype="out",desc="subsystem objective")
    
    def __init__(self,num_state_vars): 
        super(SubSystemObj,self).__init__()
        
        self.var_names = []
        self.weights = []
        for i in range(0,num_state_vars): 
            name = "y%d"%i
            self.add_trait(name,Float(0.0,
                                   iotype="in",
                                   desc=" input variable #%d"%i))
            self.var_names.append(name)
            
            name = "w%d"%i
            self.add_trait(name,Float(1.0,
                                   iotype="in",
                                   desc="weighting factor for input variable #%d"%i))
            
            self.weights.append(name)
            
    def execute(self):         
        self.f_wy = sum([getattr(self,w)*getattr(self,v) for w,v in zip(self.weights,self.var_names)])
        
class SubSystemOpt(Assembly): 
    """ assembly which takes global inputs, coupling indeps, and weight factors as inputs, 
    and runs a local optimization on the local des vars"""
    
    def __init__(self,component,global_params,local_params,couple_deps,couple_indeps): 
        super(SubSystemOpt,self).__init__()
        
        dep_state_vars = set([c.dep.target for c in couple_deps])        
        
        self.add('objective_comp',SubSystemObj(len(dep_state_vars)))
        self.add(component.name,component)
        for p in global_params:
            
            self.create_passthrough(p.target) #promote the global des vars
    
        if local_params: #if there are none, you don't do an optimization
            self.add('driver',CONMINdriver())
            self.driver.add_objective("objective_comp.f_wy")
            #this is not really necessary, but you might want to track it anyway...
            self.create_passthrough("objective_comp.f_wy") #promote the objective function    
            self.driver.fdch = .0001
            self.driver.fdchm = .0001

            for p in local_params: 
                target = p.target
                var_name = target.split(".")[-1]
                
                #TODO: since the local variables are optimized, they become outputs now
                self.add_trait(var_name,Float(0.0,iotype="in",desc="localy optimized value for %s"%target))
                setattr(self,var_name,self.get(target))
                
                #have the optimizer also set the output value here, so it's correct
                self.driver.add_parameter([target,var_name],low=p.low,high=p.high)
                
                
                
                        
        for c in couple_indeps: 
            self.create_passthrough(c.indep.target) #promote the couple inputs to the component
        
        self.weights = self.objective_comp.weights
        self.var_names = self.objective_comp.var_names
        
        for w,var,c in zip(self.objective_comp.weights,
                           self.objective_comp.var_names,
                           dep_state_vars): 
            self.create_passthrough(c) #prmote the state vars to be outputs
            self.connect(c,"objective_comp.%s"%var) #also connect the state vars to the inputs of the objective come
            self.create_passthrough("objective_comp.%s"%w) #promote the weights
                
            
class BLISS2000(Architecture):
    
    def __init__(self, *args, **kwargs):
        super(BLISS2000, self).__init__(*args, **kwargs)
        
        # the following variables determine the behavior of check_config
        self.param_types = ['continuous']
        self.constraint_types = ['ineq']
        self.num_allowed_objectives = 1
        self.has_coupling_vars = True
        self.requires_global_des_vars = True

    
    def configure(self): 
        """setup and BLISS2000 architecture inside this assembly.
        """
        
        global_dvs = self.parent.get_global_des_vars()
        des_vars=self.parent.get_des_vars_by_comp()
        local_dvs_by_comp = self.parent.get_local_des_vars_by_comp()
        global_dvs_by_comp = self.parent.get_global_des_vars_by_comp()
        
        locals=self.parent.get_local_des_vars()
        
        
        objective = self.parent.get_objectives().items()[0]
        constraints = self.parent.list_constraints()
        comp_constraints = self.parent.get_constraints_by_comp()
        coupling = self.parent.get_coupling_vars()
        couple_deps = self.parent.get_coupling_deps_by_comp()
        couple_indeps = self.parent.get_coupling_indeps_by_comp()
        
        driver=self.parent.add("driver",FixedPointIterator())
               
        driver.workflow = SequentialWorkflow()           
        driver.max_iteration=100
        
        meta_models = {}
        self.sub_system_opts = {}
        for comp in des_vars: 
            mm_name = "meta_model_%s"%comp
            meta_model = self.parent.add(mm_name,MetaModel()) #metamodel now replaces old component with same name 
            meta_models[comp] = meta_model
            meta_model.surrogate = {'default':ResponseSurface()}
            #if there are locals, you need to make a SubSystemOpt assembly
            comp_obj = self.parent.get(comp)
            if local_dvs_by_comp.get(comp): 
                sso = self.parent.add('sub_system_opt_%s'%comp,
                                      SubSystemOpt(comp_obj,
                                      global_dvs_by_comp[comp],
                                      local_dvs_by_comp[comp],
                                      couple_deps[comp],
                                      couple_indeps[comp]))
                self.sub_system_opts[comp] = sso
                meta_model.model = sso 
            else: #otherwise, just use the comp
                meta_model.model = comp_obj
            meta_model.recorder = DBCaseRecorder()
            
            #add a doe trainer for each metamodel
            dis_doe=self.parent.add("DOE_Trainer_%s"%comp,NeiborhoodDOEdriver())
            
            for couple in couple_indeps[comp] :
                dis_doe.add_parameter("meta_model_%s"%couple.indep.target,low=-1e99,high=1e99) #change to -1e99/1e99 
                
            for param,group in global_dvs:
                dis_doe.add_parameter("meta_model_%s.%s"%(comp,param),low=group.low, high=group.high,start=group.start)
            
            if local_dvs_by_comp.get(comp): #add weights if they are there
                for w in meta_model.model.weights: 
                    dis_doe.add_parameter("meta_model_%s.%s"%(comp,w),low=-3,high=3)
            dis_doe.DOEgenerator = CentralComposite()
            dis_doe.alpha= .3
            dis_doe.beta = .01

            dis_doe.add_event("meta_model_%s.train_next"%comp)
            dis_doe.force_execute = True
            driver.workflow.add(dis_doe.name) #run all doe training before system optimziation
                
        for l in locals:
            s=l[0].replace('.','_')
            self.parent.add('%s_store'%s,Float(0.0))
        for l in global_dvs:
            self.parent.add('%s_store'%l[0],Float(0.0))        
        
        #optimization of system objective function using the discipline meta models
        sysopt=self.parent.add('sysopt', CONMINdriver())       
        sysopt.fdch = .0001
        sysopt.fdchm = .0001
        
        obj2= objective[1].text
        for comp in objective[1].get_referenced_compnames():            
            obj2=obj2.replace(comp,"meta_model_%s"%comp)        
        sysopt.add_objective(obj2)
        
        #add global design variables as parameters
        for param,group in global_dvs:
            plist=[]
            for comp,globalt in des_vars.iteritems():
                mm_name = "meta_model_%s.%s"%(comp,param)
                plist.append(mm_name)
            sysopt.add_parameter(plist, low=group.low, high=group.high,start=group.start)
            
        
        #add the subsytem weights to the system optimization
        for comp,sso in self.sub_system_opts.iteritems(): 
            mm_name = "meta_model_%s"%comp
            for w in sso.weights: 
                sysopt.add_parameter("%s.%s"%(mm_name,w),low=-3,high=3)
        
        for key,couple in coupling.iteritems():
            s=couple.indep.target
            sysopt.add_parameter("meta_model_%s"%s, low=-1e99, high=1e99) #fix later
            
            #feasibility constraints, referenced to metamodels
            s1,s2= "meta_model_"+couple.dep.target,"meta_model_"+couple.indep.target
            
            sysopt.add_constraint('%s<=%s'%(s2,s1))
            sysopt.add_constraint('%s>=%s'%(s2,s1))
        
        #add constraints, referenced to metamodels
        for constraint in constraints:
            c= constraint
            for comp,globalt in des_vars.iteritems(): 
                c=c.replace(comp,"meta_model_%s"%comp)
            sysopt.add_constraint(c)
        sysopt.force_execute=True    
        
        driver.workflow.add('sysopt')
        driver.tolerance = .001

        #setup paramter for fixedpointiterator
        
        comp=des_vars.keys()[0]
        mm='meta_model_%s'%comp
        
        for l in locals:
            s=l[0].replace('.','_')
            vname= l[0].split('.')[1]
            s2='%s_store'%s
            driver.add_parameter(s2 , low=l[1].low, high=l[1].high)
            driver.add_constraint('%s.%s = %s'%(mm,vname,s2))
            
        for l in global_dvs:
            s2='%s_store'%l[0]
            driver.add_parameter(s2 , low=l[1].low, high=l[1].high)
            driver.add_constraint('%s.%s = %s'%(mm,l[0],s2))             
        
        #create the top level driver. Runs a single MDA then begins the BLISS2000 iterative process.
        

