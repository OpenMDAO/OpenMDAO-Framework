from openmdao.main.api import Assembly, Component

from openmdao.lib.components.metamodel import MetaModel
from openmdao.lib.components.kriging_surrogate import KrigingSurrogate
from openmdao.lib.components.pareto_filter import ParetoFilter
from openmdao.lib.drivers.doedriver import DOEdriver
from openmdao.lib.drivers.single_obj_ei import SingleObjectiveExpectedImprovement
from openmdao.lib.doegenerators.optlh import OptLatinHypercube
from openmdao.lib.doegenerators.full_factorial import FullFactorial
from openmdao.lib.caserecorders.dbcaserecorder import DBCaseRecorder
from openmdao.lib.traits.float import Float
from openmdao.main.api import SequentialWorkflow

from openmdao.main.uncertain_distributions import convert_norm_dist

from openmdao.examples.singleEI.branin_component import BraninComponent

class Broadcaster(Component): 
    x_in = Float(iotype="in",low=-5,high=10)
    x_out = Float(iotype="out")
    
    y_in = Float(iotype="in",low=0,high=15)
    y_out = Float(iotype="out")
    
    def execute(self): 
        self.x_out = self.x_in
        self.y_out = self.y_in

class Analysis(Assembly): 
    def __init__(self,*args,**kwargs):
        super(Analysis,self).__init__(self,*args,**kwargs)
        
        #Drivers
        self.add("DOE_trainer",DOEdriver())
        self.DOE_trainer.DOEgenerator = OptLatinHypercube(15,2,0,0)
        
        self.add("DOE_tester",DOEdriver())
        self.DOE_tester.DOEgenerator = FullFactorial(2,2)
        
        #Components
        self.add("branin_meta_model",MetaModel())
        self.branin_meta_model.surrogate = KrigingSurrogate()
        self.branin_meta_model.model = BraninComponent()
        self.branin_meta_model.recorder = DBCaseRecorder('branin_meta_model.db')
        
        self.add("branin",BraninComponent())
        self.add("broadcaster",Broadcaster())
        
        
        #Iteration Heirarchy                
        self.DOE_trainer.workflow.add(self.branin_meta_model)
        self.DOE_trainer.workflow.add(self.broadcaster)
        
        self.DOE_tester.workflow.add(self.branin_meta_model)
        self.DOE_tester.workflow.add(self.branin)
        self.DOE_tester.workflow.add(self.broadcaster)
        
        self.driver.workflow.add(self.DOE_trainer)
        self.driver.workflow.add(self.DOE_tester)
        
        #Driver Configuration
        self.DOE_trainer.add_parameter("broadcaster.x_in")
        self.DOE_trainer.add_parameter("broadcaster.y_in")
        self.DOE_trainer.add_event_var("branin_meta_model.train_next")
        self.DOE_trainer.case_outputs = ["branin_meta_model.f_xy"]
        self.DOE_trainer.recorder = DBCaseRecorder('trainer.db')
        
        self.DOE_tester.add_parameter("broadcaster.x_in")
        self.DOE_tester.add_parameter("broadcaster.y_in")
        self.DOE_tester.case_outputs = ["branin_meta_model.f_xy",'branin.f_xy']
        self.DOE_tester.recorder = DBCaseRecorder('tester.db')
        
        #Data Connections
        self.connect("broadcaster.x_out","branin_meta_model.x")
        self.connect("broadcaster.x_out","branin.x")
        
        self.connect("broadcaster.y_out","branin_meta_model.y")
        self.connect("broadcaster.y_out","branin.y")
        
        #self.add("filter",ParetoFilter())
        #self.filter.criteria = "f_xy"
        #self.filter.case_set = dbCaseIterator('branin_meta_model.db')
          
        #self.add("EI_driver",SingleObjectiveExpectedImprovement())
        #self.EI_driver.workflow.add(self.branin_meta_model)
        #self.EI_driver.add_parameter("branin_meta_model.x")
        #self.EI_driver.add_parameter("branin_meta_model.y")
        #self.EI_driver.best_case = 
        #self.EI_driver.objective = "f_xy"
        
        
        
        #self.driver.workflow.add(self.filter)
        #self.driver.workflow.add(self.EI_driver)
        
        
        
if __name__ == "__main__":
    from openmdao.main.api import set_as_top
    from openmdao.util.plot import case_db_to_dict
    from matplotlib import pyplot as py
    
    analysis = Analysis()
    set_as_top(analysis)
    analysis.run()
    
    data_train = case_db_to_dict('trainer.db',['broadcaster.y_in','broadcaster.x_in','branin_meta_model.f_xy'])
    data_test =case_db_to_dict('tester.db',['broadcaster.y_in','broadcaster.x_in','branin_meta_model.f_xy','branin.f_xy'])
    
    #convert the database data to python objects
    data_train['branin_meta_model.f_xy'] = [convert_norm_dist(x).mu for x in data_train['branin_meta_model.f_xy']]
    data_test['branin_meta_model.f_xy'] = [convert_norm_dist(x).mu for x in data_test['branin_meta_model.f_xy']]

    
    for key,value in data_test.iteritems(): 
        print key,value
    exit()    
    py.figure()
    py.scatter(data_train['branin_meta_model.x'],data_train['branin_meta_model.y'])
    py.scatter(data_test['branin_meta_model.x'],data_test['branin_meta_model.y'],c='r',s=35)
    py.show()
    