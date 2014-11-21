"""Implementation of the BLISS2000 optimization architecture based on the work described in 
the following journal article:

J. Sobieszczanski-Sobieski, T. Altus, M. Phillips, and Sandu, Bilevel Integrated System Synthesis 
for Concurrent and Distributed Processing, AIAA journal, vol. 41, no. 10, pp. 1996-2003, 2003.

"""


from openmdao.main.api import Driver, Architecture, Component, Assembly
from openmdao.lib.drivers.api import SLSQPdriver, BroydenSolver, \
                                     IterateUntil, FixedPointIterator, \
                                     NeighborhoodDOEdriver, CONMINdriver as SLSQPdriver
from openmdao.lib.surrogatemodels.api import ResponseSurface
from openmdao.lib.doegenerators.api import CentralComposite, \
                                           OptLatinHypercube, LatinHypercube
from openmdao.lib.components.api import MetaModel
from openmdao.main.datatypes.api import Float, Array, Slot
from openmdao.lib.casehandlers.api import DBCaseRecorder

class SubSystemObj(Component): 
    """Component which adds the weight factors for each output state variable 
    for a given subsystem. """
    
    f_wy = Float(0.0,iotype="out",desc="subsystem objective")
    
    def __init__(self,num_vars): 
        super(SubSystemObj,self).__init__()
        self.num_vars = num_vars
        self.var_names = []
        self.weights = []
        
    def configure(self):    
        for i in range(0,self.num_vars): 
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
        
class Broadcast(Component): 
    """Used to create outputs in the SubSytemOpt assembly."""
    
    input = Float(0.0,iotype="in")
    output = Float(0.0,iotype="out")
    
    def execute(self):
        self.output = self.input        
        
class SubSystemOpt(Assembly): 
    """ Assembly which takes global inputs, coupling indeps, and weight factors as inputs 
    and runs a local optimization on the local des vars."""
    
    def __init__(self,component,global_params,local_params,couple_deps,couple_indeps,constraints): 
        super(SubSystemOpt,self).__init__()
       
        self.component = component
        self.global_params = global_params
        self.local_params = local_params
        self.couple_deps = couple_deps
        self.couple_indeps = couple_indeps
        self.constraints = constraints
        
        self.var_map = {}
        self.weights= []
        self.var_names = []
        
    def configure(self):     
        dep_couple_vars = set([c.dep.target for c in self.couple_deps])        
        
        self.add(self.component.name,self.component)
        for i,p in enumerate(self.global_params):
            name = "global_%d"%i
            self.var_map[p.target] = name
            self.add_trait(name,Float(0.0,iotype="in",desc="global design var for %s"%p.target.split(".")[-1]))
                        
            self.connect(name,p.target) #promote the global des vars
            #setattr(self,name,self.get(p.target))
    
        if self.local_params: #if there are none, you don't do an optimization
            self.add('objective_comp',SubSystemObj(len(dep_couple_vars)))
            self.weights = self.objective_comp.weights
            self.var_names = self.objective_comp.var_names
        
            self.add('driver',SLSQPdriver())
            self.driver.add_objective("objective_comp.f_wy")
            self.driver.fdch = .00001
            #self.driver.fdchm = .0001
            
            #this is not really necessary, but you might want to track it anyway...
            self.create_passthrough("objective_comp.f_wy") #promote the objective function    

            for i,p in enumerate(self.local_params): 
                target = p.target
                var_name = "local_%d"%i
                self.var_map[target] = var_name
                
                #since the local variables are optimized, they become outputs now
                broadcast_name = 'output_%s'%var_name
                self.add(broadcast_name,Broadcast())
                self.add_trait(var_name,Float(0.0,iotype="out",desc="localy optimized value for %s"%target))
                #setattr(self,var_name,self.get(target))

                self.connect("%s.output"%broadcast_name,target) #connect broadcast output to input of component
                self.connect("%s.output"%broadcast_name,var_name) #connect broadcast output to variable in assembly
                self.driver.add_parameter("%s.input"%broadcast_name,low=p.low,high=p.high, start=p.start) #optimizer varries broadcast input
            
            for c in self.constraints: 
                self.driver.add_constraint(str(c))
                
            for i,(w,var,c) in enumerate(zip(self.objective_comp.weights,
                           self.objective_comp.var_names,
                           dep_couple_vars)): 
                name = "couple_dep_%d"%i
                self.var_map[c] = name
                self.add_trait(name,Float(0.0,iotype="out",desc="coupling dependent for %s"%c))
                self.connect(c,name) #prmote the coupling deps to be outputs
                self.connect(c,"objective_comp.%s"%var) #also connect the state vars to the inputs of the objective come
                self.create_passthrough("objective_comp.%s"%w) #promote the weights    
                
        else: #no locals, so just promote the coupling deps
            #no optimizer, so add the comp to the default workflow
            self.driver.workflow.add(self.component.name)
            for i,c in enumerate(dep_couple_vars): 
                name = "couple_dep_%d"%i
                self.var_map[c] = name
                self.add_trait(name,Float(0.0,iotype="out",desc="coupling dependent for %s"%c))
                self.connect(c,name) #prmote the coupling deps to be outputs
                
        for i,c in enumerate(self.couple_indeps):
            name = "couple_indep_%d"%i
            self.var_map[c.indep.target] = name
            self.add_trait(name,Float(0.0,iotype="in",desc="coupling independent for %s"%c))
            self.connect(name,c.indep.target)        
                
            
class BLISS2000(Architecture):
    
    def __init__(self, *args, **kwargs):
        super(BLISS2000, self).__init__()
        
        # the following variables determine the behavior of check_config
        self.param_types = ['continuous']
        self.constraint_types = ['ineq']
        self.num_allowed_objectives = 1
        self.has_coupling_vars = True
        self.requires_global_des_vars = True

    
    def configure(self): 
        """Setup a BLISS2000 architecture inside this assembly.
        """
        
        global_dvs = self.parent.get_global_des_vars()
        des_vars=self.parent.get_des_vars_by_comp()
        local_dvs_by_comp = self.parent.get_local_des_vars_by_comp()
        global_dvs_by_comp = self.parent.get_global_des_vars_by_comp()
        
        locals=self.parent.get_local_des_vars()

        #set initial values 
        for comp,param in global_dvs: 
            param.initialize(self.parent, self.parent)

        for comp,local_params in local_dvs_by_comp.iteritems(): 
            for param in local_params: 
                param.initialize(self.parent, self.parent)
        
        objective = self.parent.get_objectives().items()[0]
        comp_constraints = self.parent.get_constraints_by_comp()
        coupling = self.parent.list_coupling_vars()
        couple_deps = self.parent.get_coupling_deps_by_comp()
        couple_indeps = self.parent.get_coupling_indeps_by_comp()

        for key,couple in coupling.iteritems(): 
            couple.indep.set(couple.start)   
        
        driver=self.parent.add("driver",FixedPointIterator())
               
        driver.max_iteration=15 #should be enough to converge
        driver.tolerance = .005
        meta_models = {}
        self.sub_system_opts = {}
        
        system_var_map = {}
        for comp in des_vars: 
            mm_name = "meta_model_%s"%comp
            meta_model = self.parent.add(mm_name,MetaModel()) #metamodel now replaces old component with same name 
            #driver.add_event("%s.reset_training_data"%mm_name)

            meta_models[comp] = meta_model
            meta_model.default_surrogate = ResponseSurface()
            #if there are locals, you need to make a SubSystemOpt assembly
            comp_obj = self.parent.get(comp)
             
            sso = self.parent.add('sub_system_opt_%s'%comp,
                                  SubSystemOpt(comp_obj,
                                  global_dvs_by_comp.get(comp),
                                  local_dvs_by_comp.get(comp),
                                  couple_deps.get(comp),
                                  couple_indeps.get(comp),
                                  comp_constraints.get(comp)))
            self.sub_system_opts[comp] = sso
            meta_model.model = sso 
            for name,mapped_name in sso.var_map.iteritems():
                system_var_map[name] = "%s.%s"%(mm_name,mapped_name)
                                
            meta_model.recorder = DBCaseRecorder()
            
            #add a doe trainer for each metamodel
            dis_doe=self.parent.add("DOE_Trainer_%s"%comp,NeighborhoodDOEdriver())
            
            for couple in couple_indeps[comp] :
                mapped_name = system_var_map[couple.indep.target]
                dis_doe.add_parameter(mapped_name,low=-1e99,high=1e99) #change to -1e99/1e99 
                
            for dv in global_dvs_by_comp[comp]:
                dis_doe.add_parameter(system_var_map[dv.target],low=dv.low, high=dv.high)
            if local_dvs_by_comp.get(comp): #add weights if they are there
                for w in meta_model.model.weights: 
                    dis_doe.add_parameter("meta_model_%s.%s"%(comp,w),low=-3,high=3)
            num_params = len(dis_doe.get_parameters())        
            dis_doe.DOEgenerator = LatinHypercube((num_params**2+3*num_params+2)/2)
            dis_doe.alpha= .1
            dis_doe.beta = .01

            dis_doe.add_event("%s.train_next"%meta_model.name)
            driver.workflow.add(dis_doe.name) #run all doe training before system optimziation
                
      
        
        #optimization of system objective function using the discipline meta models
        sysopt=self.parent.add('sysopt', SLSQPdriver())   
        sysopt.recorders = self.data_recorders
        sysopt.iprint = 0
        
        obj2= objective[1].text
        #for comp in objective[1].get_referenced_compnames():            
        #    obj2=obj2.replace(comp,"meta_model_%s"%comp)  
        for var_name, mapped_name in system_var_map.iteritems(): 
            obj2=obj2.replace(var_name,mapped_name)
        sysopt.add_objective(obj2)
        #add global design variables as parameters

        for param,group in global_dvs:
            plist=[system_var_map[t] for t in group.targets]
            sysopt.add_parameter(plist, low=group.low, high=group.high,start=group.start)
        
        #add the subsytem weights to the system optimization
        for comp,sso in self.sub_system_opts.iteritems(): 
            mm_name = "meta_model_%s"%comp
            for w in sso.weights: 
                sysopt.add_parameter("%s.%s"%(mm_name,w),low=-3,high=3)
        
        for key,couple in coupling.iteritems():
            s=couple.indep.target
            mapped_name = system_var_map[s]
            sysopt.add_parameter(mapped_name, low=-1e99, high=1e99)
            
            #feasibility constraints, referenced to metamodels
            s1,s2= system_var_map[couple.dep.target], system_var_map[couple.indep.target]
            sysopt.add_constraint('(%s-%s)**2<=0.0001'%(s2,s1))
            #sysopt.add_constraint('%s>=%s'%(s2,s1))
            
        
        #add constraints, referenced to metamodels
        for comp,constraints in comp_constraints.iteritems():
            for c in constraints:  
                new_c = str(c)
                for var,mapped_name in system_var_map.iteritems():
                    new_c = new_c.replace(var,mapped_name)
                sysopt.add_constraint(new_c)
        
        driver.workflow.add('sysopt')

        #setup paramter for fixedpointiterator
        
        comp=des_vars.keys()[0]
        mm='meta_model_%s'%comp

        #create some placeholder variables for the fixed point iteration         
        for l in locals:
            s=system_var_map[l[0]].replace(".","_")
            
            s2='%s_store'%s
            self.parent.add(s2,Float(0.0,iotype="in"))
            driver.add_parameter(s2 , low=l[1].low, high=l[1].high)
            driver.add_constraint('%s = %s'%(system_var_map[l[1].target],s2))
            
            
        for i,g in enumerate(global_dvs):
            s2='global%d_store'%i
            self.parent.add(s2,Float(0.0,iotype="in")) 
            driver.add_parameter(s2 , low=g[1].low, high=g[1].high)
            driver.add_constraint('%s = %s'%(system_var_map[g[1].target],s2))       
            

        #driver.workflow.add(['DOE_Trainer_dis2','DOE_Trainer_dis1'])
            
if __name__=="__main__": 
    
    from openmdao.lib.optproblems.api import SellarProblem
    
    p = SellarProblem()
    
    p.architecture = BLISS2000()

    #print [param for param in p.DOE_Trainer_dis2.get_parameters()]
    #exit()
    p.run()

    for k,v in p.check_solution().iteritems(): 
        print "    ",k,": ",v
