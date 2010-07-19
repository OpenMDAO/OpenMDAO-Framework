from openmdao.main.api import Assembly

from openmdao.lib.components.metamodel import MetaModel
from openmdao.lib.components.kriging_surrogate import KrigingSurrogate
from openmdao.lib.drivers.doedriver import DOEdriver
from openmdao.lib.doegenerators.optlh import OptLatinHypercube
from openmdao.lib.api import DumpCaseRecorder

from openmdao.examples.single_EI.branin_comp import BraninComponent

class Analysis(Assembly): 
    def __init__(self,*args,**kwargs):
        
        self.add("driver",DOEdriver())
        self.driver.DOEgenerator = OptLatinHypercube(20,2)        
        
        self.add("bob",MetaModel())
        self.bob.model = BraninComponent()
        self.bob.surrogate = KrigingSurrogate()
        self.bob.recorder = DumpCaseRecorder()
        
        self.driver.workflow.add(self.bob)
        self.driver.add_parameter("bob.x")
        self.driver.add_parameter("bob.y")
        self.driver.add_event("bob.train_next")
        
        
if __name__ == "__main__":
    analysis = Analysis()
    analysis.run()