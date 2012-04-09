from openmdao.main.api import Component,Assembly
from openmdao.lib.datatypes.api import Array,Float
from openmdao.main.expreval import ExprEvaluator


class Dummy(Component):
    x=Float(2,iotype='in')
    #y=Array([2,3],iotype='in')
    y=Float(3,iotype='in')
    z=Float(0,iotype='out')

    def execute(self):
        self.z=self.y.sum()+self.x

class Foo(Assembly):
    
    def configure(self):
        self.add('d',Dummy())
        e=ExprEvaluator("d.x/d.y",self) 
        print e.evaluate_gradient() #[   1/y ,  - x / (y ^ 2)]
        
bar=Foo()