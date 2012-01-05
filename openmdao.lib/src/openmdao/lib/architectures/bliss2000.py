from openmdao.main.api import Driver, Architecture,SequentialWorkflow
from openmdao.lib.drivers.api import CONMINdriver, BroydenSolver,IterateUntil,FixedPointIterator,DOEdriver
from openmdao.lib.surrogatemodels.api import ResponseSurface
from openmdao.lib.doegenerators.api import CentralComposite
from openmdao.lib.components.api import MetaModel
from openmdao.lib.datatypes.api import Float, Array, Slot
from openmdao.lib.casehandlers.api import DBCaseRecorder

class BLISS2000(Architecture):
    
    def __init__(self, *args, **kwargs):
        super(BLISS2000, self).__init__(*args, **kwargs)
        
        # the following variables determine the behavior of check_config
        self.param_types = ['continuous']
        self.constraint_types = ['ineq']
        self.num_allowed_objectives = 1
        self.has_coupling_vars = True
    
    def configure(self): 
        """setup and BLISS2000 architecture inside this assembly.
        """
        
        global_dvs = self.parent.get_global_des_vars()
        des_vars=self.parent.get_des_vars_by_comp()
        local_dvs = self.parent.get_local_des_vars_by_comp()
        
        locals=self.parent.get_local_des_vars()
        
        objective = self.parent.get_objectives().items()[0]
        constraints = self.parent.list_constraints()
        comp_constraints = self.parent.get_constraints_by_comp()
        coupling = self.parent.get_coupling_vars()
        
        
        driver=self.parent.add("driver",FixedPointIterator())
               
        
        '''
        self.parent.driver.workflow=SequentialWorkflow()        
        self.parent.driver.workflow.add(['driver'])        
        '''
        driver.workflow = SequentialWorkflow()           
        
        for comp,globalt in des_vars.iteritems(): 
            mm_name = "meta_model_%s"%comp
            meta_model = self.parent.add(mm_name,MetaModel()) #metamodel now replaces old component with same name 
            meta_model.surrogate = {'default':ResponseSurface()}
            meta_model.model = self.parent.get(comp)
            meta_model.recorder = DBCaseRecorder()
        

        #optimization of each discpline with respect to local design variables
        
        for comp,local in local_dvs.iteritems():
            local_opt = self.parent.add("local_opt_%s"%comp,CONMINdriver()) #metamodel now replaces old component with same name  
            local_opt.force_execute=True         
            local_opt.workflow=SequentialWorkflow()
            local_opt.workflow.add('meta_model_%s'%comp) 
            for lvar in local:
                mm= "meta_model_%s"%comp+lvar.name[len(comp):]
                local_opt.add_parameter(mm, low=lvar.low, high=lvar.high) 
            local_opt.add_event("meta_model_%s.train_next"%comp)    
            
            for constraint in comp_constraints[comp]:
                c=str(constraint)
                c=c.replace(comp,"meta_model_%s"%comp)
                local_opt.add_constraint(str(c))
            
            
            for key,couple in coupling.iteritems(): 
                if comp==couple.dep.target[:-len(key)-1]:
                    local_opt.add_objective('meta_model_'+couple.dep.target)
                    break
            driver.workflow.add(local_opt.name)            
        
        
        reset_train=self.parent.add('reset_train',Driver())
        reset_train.add_event('meta_model_dis1.reset_training_data')
        reset_train.add_event('meta_model_dis2.reset_training_data')        
        reset_train.force_execute = True
        driver.workflow.add('reset_train')
        
        
        
        for comp,globalt in des_vars.iteritems(): 
            dis_doe=self.parent.add("DOE_Trainer_%s"%comp,DOEdriver())
            for key,couple in coupling.iteritems(): 
                if comp==couple.indep.target[:-len(key)-1]:
                    dis_doe.add_parameter("meta_model_%s.%s"%(comp,key),low=0,high=20)  #fix this later
            for param,group in global_dvs:
                dis_doe.add_parameter("meta_model_%s.%s"%(comp,param),low=group.low, high=group.high,start=group.start)
            dis_doe.DOEgenerator = CentralComposite()
            dis_doe.alpha=0.1
            dis_doe.add_event("meta_model_%s.train_next"%comp)
            dis_doe.force_execute = True
            driver.workflow.add(dis_doe.name)


        
        for l in locals:
            s=l[0].replace('.','_')
            self.parent.add('%s_store'%s,Float(0.0))
        for l in global_dvs:
            self.parent.add('%s_store'%l[0],Float(0.0))        
        
        #optimization of system objective function using the discipline meta models
        sysopt=self.parent.add('sysopt', CONMINdriver())       

        
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
        
        #add coupling variables (independents) as parameters
        for key,couple in coupling.iteritems():
            s=couple.indep.target
            sysopt.add_parameter("meta_model_%s"%s, low=0, high=20) #fix later
        #feasibility constraints, referenced to metamodels
        
        for key,couple in coupling.iteritems():
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
        driver.tolerance = .0001

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
        

