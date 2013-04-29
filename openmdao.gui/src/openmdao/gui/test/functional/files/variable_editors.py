from openmdao.main.api import Component, Assembly
from openmdao.lib.datatypes.api import Float, Enum, Array, Dict, Str, List

class dummy_comp(Component):

    x = Float(0.0, iotype='in')
    e = Enum(0, [0,1,2,3], iotype = 'in')
    d = Dict(value = {'e':2.71, "pi": 3.14159}, value_trait = Float, key_trait = Str, iotype = 'in')
    X = Array([0,1,2,3], iotype = 'in')
    Y = Array([[0,1],[2,3]], iotype = 'in')
    Y2 = Array([[5],[8]], iotype = 'in')
    Y3 = Array([[1]], iotype = 'in')
    Z = List([1,2,3,4], iotype='in')
    
    def execute(self):
        return 
    
class Topp(Assembly):

    def configure(self):
        self.add('p1', dummy_comp())

