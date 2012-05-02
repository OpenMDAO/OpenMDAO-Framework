from openmdao.main.datatypes.api import Array,Int,Dict,  Float, Bool
from openmdao.main.api import Component, Assembly, set_as_top

class TestAssembly(Assembly):

       def __init__(self, *args, **kws):
           super(TestAssembly, self).__init__(*args, **kws)


           self.add('c1', ConstantComponent())
           self.c1.const = 1
           self.driver.workflow.add('c1')

           self.add('c2', ConstantComponent())
           self.c2.const = 2
           self.driver.workflow.add('c2')

           self.add('comp', TestComponent())
           self.driver.workflow.add('comp')

           self.connect('c1.out', 'comp.arr[0]')
           self.connect('c2.out', 'comp.arr[1]')

class ConstantComponent(Component):

       const = Float(iotype='in')
       out = Float(iotype='out')

       def execute(self):
           self.out = self.const


class TestComponent(Component):

       arr = Dict(key_trait=Int, value_trait=Float(units='MW'), iotype='in')
       len = Int(iotype='out')



       def execute(self):
           print "...executing component"
           self.len = len(self.arr)


if __name__ == '__main__':


       t = TestAssembly()
       t.set('comp.arr', 2.0, [(0, 1)], src='c2.out')
       print "===================="
       assembly = set_as_top(t)

       assembly.run()
       print assembly.comp.arr

