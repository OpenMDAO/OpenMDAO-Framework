from openmdao.main.api import VariableTree, Component
from openmdao.main.datatypes.api import Float, VarTree

class InVtree(VariableTree): 
    a = Float(iotype='in')
    b = Float(iotype='in')


class OutVtree(VariableTree): 
    x = Float(iotype='out', desc='horizontal distance', units='ft')
    y = Float(iotype='out', desc='vertical distance', units='ft')    


class InandOutTree(Component): 

    zzz = Float(iotype='out', desc='nonvartree', units='ft')

    ins = VarTree(InVtree(), iotype='in')
    outs = VarTree(OutVtree(), iotype='out')

    def execute(self): 

        self.outs.x = 2*self.ins.a
        self.outs.y = 2*self.ins.a+self.ins.b
