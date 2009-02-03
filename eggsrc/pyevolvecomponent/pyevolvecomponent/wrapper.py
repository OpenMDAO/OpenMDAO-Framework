from openmdao.main.component import Component

from pyevolve import G1Dlist,GsimpleGA,Initializators,Mutators,Consts,DBadapters
from NASAevolve import G1DListCrossOverRealHypersphere


class wrapper(Component):
    def __init__(self,name,parent=None,desc=None): 
        Component.__init__(self,name,parent,desc)
        
        self.state = Component.STATE_IDLE
        self._stop = False
        self._input_changed = False
        
        self.resource_desc = {"python_version": 2.5, "operator": "home_sapien"}
        
        self.genome = G1Dlist(2)
        self.ga = gaEngine.GsimpleGA(self.genome)
        
    def post_config(self):
        pass
        
    def execute(self):
        pass
    
    