from openmdao.main.api import Assembly

from openmdao.lib.components.metamodel import MetaModel
from openmdao.lib.components.kriging_surrogate import KrigingSurrogate
from openmdao.lib.drivers.doedriver import DOEdriver
from openmdao.lib.doegenerators.optlh import OptLatinHypercube
from openmdao.lib.api import DumpCaseRecorder

from openmdao.examples.singleEI.branin_component import BraninComponent

class Analysis(Assembly): 
    def __init__(self,*args,**kwargs):
        super(Analysis,self).__init__(self,*args,**kwargs)
        
        
        
        self.add("DOE",DOEdriver())
        self.DOE.DOEgenerator = OptLatinHypercube(20,2)
        
        driver_dump = open('doe_output.txt','w')
        
        self.DOE.recorder = DumpCaseRecorder(driver_dump)
        
        self.add("bob",MetaModel())
        self.bob.surrogate = KrigingSurrogate()
        self.bob.model = BraninComponent()
        bob_dump = open('bob_output.txt','w')
        self.bob.recorder = DumpCaseRecorder(bob_dump)
        
        self.DOE.workflow.add(self.bob)
        self.DOE.add_parameter("bob.x")
        self.DOE.add_parameter("bob.y")
        self.DOE.add_event_var("bob.train_next")
        
        
        self.driver.workflow.add(self.DOE)
        self.driver.workflow.add(self.bob)
        
        
if __name__ == "__main__":
    from openmdao.main.api import set_as_top
    analysis = Analysis()
    set_as_top(analysis)
    analysis.run()
    
    analysis.bob.execute()
    
    