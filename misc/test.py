from openmdao.main.api import Component,Assembly
from openmdao.lib.datatypes.api import Array,Float
from openmdao.main.expreval import ExprEvaluator

class Dummy(Component):
    x=Float(1,iotype='in')
    y=Array([2,3],iotype='in')
    z=Float(0,iotype='out')
    
    def execute(self):
        self.z=self.y.sum()+self.x

class Foo(Assembly):
    
    def configure(self):
        self.add('d',Dummy())
        e=ExprEvaluator("d.x**2+d.y[1]**2",self)
        print e.evaluate_gradient()
        #print e.get_compvar_dict()
        
bar=Foo()