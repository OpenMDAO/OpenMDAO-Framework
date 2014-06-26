
import unittest
from openmdao.main.api import set_as_top, Assembly, Component
from openmdao.main.datatypes.api import Float

class Simple(Component):

    a = Float(1.0, iotype='in')
    b = Float(2.0, iotype='in')
    c = Float(3.0, iotype='out')
    d = Float(-1.0, iotype='out')

    def __init__(self):
        super(Simple, self).__init__()

    def execute(self):
        self.c = self.a + self.b
        self.d = self.a - self.b


def _nested_model():
    top = set_as_top(Assembly())
    top.add('sub', Assembly())
    top.add('comp7', Simple())
    top.add('comp8', Simple())
    sub = top.sub
    sub.add('comp1', Simple())
    sub.add('comp2', Simple())
    sub.add('comp3', Simple())
    sub.add('comp4', Simple())
    sub.add('comp5', Simple())
    sub.add('comp6', Simple())

    top.driver.workflow.add(['comp7', 'sub', 'comp8'])
    sub.driver.workflow.add(['comp1','comp2','comp3',
                             'comp4','comp5','comp6'])

    sub.create_passthrough('comp1.a', 'a1')
    sub.create_passthrough('comp2.b', 'b2')
    sub.create_passthrough('comp3.a', 'a3')
    sub.create_passthrough('comp3.d', 'd3')
    sub.create_passthrough('comp4.b', 'b4')
    sub.create_passthrough('comp4.c', 'c4')
    sub.create_passthrough('comp6.b', 'b6')
    sub.create_passthrough('comp2.c', 'c2')
    sub.create_passthrough('comp1.d', 'd1')
    sub.create_passthrough('comp5.d', 'd5')

    sub.connect('comp1.c', 'comp4.a')
    sub.connect('comp5.c', 'comp1.b')
    sub.connect('comp2.d', 'comp5.b')
    sub.connect('comp3.c', 'comp5.a')
    sub.connect('comp4.d', 'comp6.a')
    
    top.connect('sub.c4', 'comp8.a')       
    top.connect('comp7.c', 'sub.a3')
    top.connect('sub.d3', 'comp8.b')

    return top


class VecWrapperTestCase(unittest.TestCase):

    
    def test_getitem(self):
        top = _nested_model()
        top.run()
        #from openmdao.util.dotgraph import plot_graph
        #plot_graph(top.sub._depgraph)
        self.assertEqual(set(top.sub._system.vec['u'].keys()), 
                         set(['a1','b2','a3','c2','d3','d5','d1','b4','c4','b6',
                              'comp2.b','comp2.c','comp2.d',
                              'comp3.a','comp3.c','comp3.d',
                              'comp5.a','comp5.b','comp5.c','comp5.d',
                              'comp1.a','comp1.b','comp1.c','comp1.d',
                              'comp4.a','comp4.b','comp4.c','comp4.d',
                              'comp6.a','comp6.b']))
                
        self.assertEqual(top.sub._system.vec['u'].array.size, 30)
 
    # TODO: add more...

if __name__ == "__main__":
    unittest.main()
