from openmdao.main.api import Assembly

from openmdao.lib.components.metamodel import MetaModel
from openmdao.lib.components.kriging_surrogate import KrigingSurrogate
from openmdao.lib.components.pareto_filter import ParetoFilter
from openmdao.lib.drivers.doedriver import DOEdriver
from openmdao.lib.drivers.single_obj_ei import SingleObjectiveExpectedImprovement
from openmdao.lib.doegenerators.optlh import OptLatinHypercube
from openmdao.lib.api import DBCaseRecorder

from openmdao.examples.singleEI.branin_component import BraninComponent

class Analysis(Assembly): 
    def __init__(self,*args,**kwargs):
        super(Analysis,self).__init__(self,*args,**kwargs)
        
        self.add("DOE_trainer",DOEdriver())
        self.DOE_trainer.DOEgenerator = OptLatinHypercube(20,2)
        self.DOE_trainer.recorder = DBCaseRecorder('trainer.db')
        
        
        self.add("DOE_tester",DOEdriver())
        self.DOE_tester.DOEgenerator = OptLatinHypercube(20,2)
        self.DOE_tester.recorder = DBCaseRecorder('tester.db')
        
        self.add("bob",MetaModel())
        self.bob.surrogate = KrigingSurrogate()
        self.bob.model = BraninComponent()
        
        self.bob.recorder = DBCaseRecorder('bob.db')
        
        self.DOE_trainer.workflow.add(self.bob)
        self.DOE_trainer.add_parameter("bob.x")
        self.DOE_trainer.add_parameter("bob.y")
        self.DOE_trainer.add_event_var("bob.train_next")
        self.DOE_trainer.case_outputs = ["bob.f_xy"]
        
        self.DOE_tester.workflow.add(self.bob)
        self.DOE_tester.add_parameter("bob.x")
        self.DOE_tester.add_parameter("bob.y")
                
        #self.add("filter",ParetoFilter())
        #self.filter.criteria = "f_xy"
        #self.filter.case_set = dbCaseIterator('bob.db')
          
        #self.add("EI_driver",SingleObjectiveExpectedImprovement())
        #self.EI_driver.workflow.add(self.bob)
        #self.EI_driver.add_parameter("bob.x")
        #self.EI_driver.add_parameter("bob.y")
        #self.EI_driver.best_case = 
        #self.EI_driver.objective = "f_xy"
        
        self.driver.workflow.add(self.DOE_trainer)
        self.driver.workflow.add(self.DOE_tester)
        
        #self.driver.workflow.add(self.filter)
        #self.driver.workflow.add(self.EI_driver)
        
        
        
if __name__ == "__main__":
    from openmdao.main.api import set_as_top
    from openmdao.util.plot import case_db_to_dict
    
    analysis = Analysis()
    set_as_top(analysis)
    analysis.run()
    
    data = case_db_to_dict('trainer.db',['bob.y','bob.x','bob.f_xy'])
    
    for key,value in data.iteritems(): 
        print key,value
    
    
    
    
 