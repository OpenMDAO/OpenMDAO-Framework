# pylint: disable-msg=C0111,C0103

import unittest

from openmdao.main import Model, Assembly, Component, Float, String
from openmdao.main.variable import INPUT, OUTPUT

        
class Simple(Component):
    def __init__(self, name):
        super(Simple, self).__init__(name)
        self.a = 4.
        self.b = 5.
        self.c = 7.
        self.d = 1.5
        self.run_count = 0
        Float('a', self, INPUT, units='cm')
        Float('b', self, INPUT, units='m')
        Float('c', self, OUTPUT, units='cm')
        Float('d', self, OUTPUT, units='mm')

    def execute(self):
        self.run_count += 1
        self.c = self.a + self.b
        self.d = self.a - self.b


class ComponentGraphTestCase(unittest.TestCase):

    def setUp(self):
        top = Assembly('top', None)
        self.top = top
        sub = top.add_child(Assembly('sub'))
        sub.add_child(Simple('comp1'))
        sub.add_child(Simple('comp2'))
        sub.add_child(Simple('comp3'))
        sub.add_child(Simple('comp4'))
        sub.add_child(Simple('comp5'))
        sub.add_child(Simple('comp6'))
        
        sub.create_passthru('comp1.a', 'a1')
        sub.create_passthru('comp3.a', 'a3')
        sub.create_passthru('comp2.b', 'b2')
        sub.create_passthru('comp4.b', 'b4')
        sub.create_passthru('comp6.b', 'b6')
        sub.create_passthru('comp2.c', 'c2')
        sub.create_passthru('comp4.c', 'c4')
        sub.create_passthru('comp1.d', 'd1')
        sub.create_passthru('comp3.d', 'd3')
        sub.create_passthru('comp5.d', 'd5')
        
        sub.connect('comp1.c', 'comp4.a')
        sub.connect('comp5.c', 'comp1.b')
        sub.connect('comp2.d', 'comp5.b')
        sub.connect('comp3.c', 'comp5.a')
        sub.connect('comp4.d', 'comp6.a')
        
    def test_var_preds(self):
        sub = self.top.sub
        
        vars = [sub.getvar('c4')]
        preds = sub.var_preds(vars)        
        self.assertEqual(set([x.name for x in preds]), set(['b4', 'a1', 'a3', 'b2']))
        
        vars = [sub.getvar('d1')]
        preds = sub.var_preds(vars)
        self.assertEqual(set([x.name for x in preds]), set(['a1', 'b2', 'a3']))
        
        vars = [sub.getvar('d5')]
        preds = sub.var_preds(vars)
        self.assertEqual(set([x.name for x in preds]), set(['b2', 'a3']))
        
        vars = [sub.getvar('d3')]
        preds = sub.var_preds(vars)
        self.assertEqual([x.name for x in preds], ['a3'])
        
        vars = [sub.getvar(x) for x in ['c4','c2','d1','d3','d5']]
        preds = sub.var_preds(vars)
        self.assertEqual(set([x.name for x in preds]),  set(['b2', 'a3', 'a1', 'b4']))
        
        sub.disconnect('comp1.b')
        vars = [sub.getvar('c4')]
        preds = sub.var_preds(vars)        
        self.assertEqual(set([x.name for x in preds]), set(['b4', 'a1']))
        
    def test_var_successors(self):
        sub = self.top.sub
        
        vars = [sub.getvar('a1')]
        succs = sub.var_successors(vars)        
        self.assertEqual(set([x.name for x in succs]), set(['d1','c4']))
        
        vars = [sub.getvar('b4')]
        succs = sub.var_successors(vars)
        self.assertEqual([x.name for x in succs], ['c4'])
        
        vars = [sub.getvar('b2')]
        succs = sub.var_successors(vars)
        self.assertEqual(set([x.name for x in succs]), set(['c2','d5','d1','c4']))
        
        vars = [sub.getvar('a3')]
        succs = sub.var_successors(vars)
        self.assertEqual(set([x.name for x in succs]), set(['d3','d5','d1','c4']))
        
        vars = [sub.getvar('b6')]
        succs = sub.var_successors(vars)
        self.assertEqual(succs, set([]))
        
        vars = [sub.getvar(x) for x in ['a1','b4','b2','a3','b6']]
        succs = sub.var_successors(vars)
        self.assertEqual(set([x.name for x in succs]),  set(['c4','c2','d5','d1','d3']))
        
        sub.disconnect('comp1.b')
        vars = [sub.getvar('a3')]
        succs = sub.var_successors(vars)        
        self.assertEqual(set([x.name for x in succs]), set(['d3','d5']))

    def test_lazy(self):
        sub = self.top.sub
        self.assertEqual([0,0,0,0,0,0],
            [sub.get(x).run_count for x in ['comp1','comp2','comp3','comp4','comp5','comp6']])
        self.top.run()        
        self.assertEqual([1,1,1,1,1,1],
            [sub.get(x).run_count for x in ['comp1','comp2','comp3','comp4','comp5','comp6']])
        self.top.run()  
        # run_count should stay at 1 for all comps
        self.assertEqual([1,1,1,1,1,1],
            [sub.get(x).run_count for x in ['comp1','comp2','comp3','comp4','comp5','comp6']])
        
if __name__ == "__main__":
    unittest.main()


