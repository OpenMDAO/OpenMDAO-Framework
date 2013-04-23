from openmdao.main.api import VariableTree, Component
from openmdao.main.datatypes.api import Float, Slot

class InVtree(VariableTree): 
    a = Float(iotype='in')
    b = Float(iotype='in')


class OutVtree(VariableTree): 
    x = Float(iotype='out', desc='horizontal distance', units='ft')
    y = Float(iotype='out', desc='vertical distance', units='ft')    


class InandOutTree(Component): 

    ins = Slot(InVtree, iotype='in')

    outs = Slot(OutVtree, iotype='out')

    def __init__(self): 
        super(InandOutTree, self).__init__()
        self.ins = InVtree()
        self.outs = OutVtree()

    def execute(self): 

        self.outs.x = 2*self.ins.a
        self.outs.y = 2*self.ins.a+self.ins.b
