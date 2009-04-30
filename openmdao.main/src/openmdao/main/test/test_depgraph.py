# pylint: disable-msg=C0111,C0103

import unittest
import logging

from openmdao.main import Model, Assembly, Component, Int
from openmdao.main.variable import INPUT, OUTPUT

        
class Simple(Component):
    def __init__(self, name):
        super(Simple, self).__init__(name)
        self.a = 1
        self.b = 2
        self.c = 3
        self.d = -1
        self.run_count = 0
        Int('a', self, INPUT)
        Int('b', self, INPUT)
        Int('c', self, OUTPUT)
        Int('d', self, OUTPUT)

    def execute(self):
        self.run_count += 1
        self.c = self.a + self.b
        self.d = self.a - self.b


class DepGraphTestCase(unittest.TestCase):

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

        top.add_child(Simple('comp7'))

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
        
        top.connect('comp7.c', 'sub.a3')
        
    #def test_var_preds(self):
        #self.setUp()
        #sub = self.top.sub
        
        #vars = [sub.getvar('c4')]
        #self.assertEqual(sub.var_preds(vars), set(['b4', 'a1', 'a3', 'b2']))
        
        #vars = [sub.getvar('d1')]
        #self.assertEqual(sub.var_preds(vars), set(['a1', 'b2', 'a3']))
        
        #vars = [sub.getvar('d5')]
        #self.assertEqual(sub.var_preds(vars), set(['b2', 'a3']))
        
        #vars = [sub.getvar('d3')]
        #self.assertEqual(sub.var_preds(vars), set(['a3']))
        
        #vars = [sub.getvar(x) for x in ['c4','c2','d1','d3','d5']]
        #self.assertEqual(sub.var_preds(vars),  set(['b2', 'a3', 'a1', 'b4']))
        
        #sub.disconnect('comp1.b')
        #vars = [sub.getvar('c4')]
        #self.assertEqual(sub.var_preds(vars), set(['b4', 'a1']))
        
    #def test_var_successors(self):
        #sub = self.top.sub
        
        #vars = [sub.getvar('a1')]
        #self.assertEqual(sub.var_successors(vars), set(['d1','c4']))
        
        #vars = [sub.getvar('b4')]
        #self.assertEqual(sub.var_successors(vars), set(['c4']))
        
        #vars = [sub.getvar('b2')]
        #self.assertEqual(sub.var_successors(vars), set(['c2','d5','d1','c4']))
        
        #vars = [sub.getvar('a3')]
        #self.assertEqual(sub.var_successors(vars), set(['d3','d5','d1','c4']))
        
        #vars = [sub.getvar('b6')]
        #self.assertEqual(sub.var_successors(vars), set([]))
        
        #vars = [sub.getvar(x) for x in ['a1','b4','b2','a3','b6']]
        #self.assertEqual(sub.var_successors(vars),  set(['c4','c2','d5','d1','d3']))
        
        #sub.disconnect('comp1.b')
        #vars = [sub.getvar('a3')]
        #self.assertEqual(sub.var_successors(vars), set(['d3','d5']))

    def test_lazy(self):
        top = self.top
        sub = self.top.sub
        allcomps = ['sub.comp1','sub.comp2','sub.comp3','sub.comp4','sub.comp5','sub.comp6','comp7']
        self.assertEqual([0, 0, 0, 0, 0, 0, 0], [top.get(x).run_count for x in allcomps])
        logging.getLogger('').debug('***** 1st run *****')
        self.top.run()        
        self.assertEqual([1, 1, 1, 1, 1, 1, 1], [top.get(x).run_count for x in allcomps])
        self.assertEqual(sub.get('comp3.c'),5)
        self.assertEqual(sub.get('comp1.c'),5)
        self.assertEqual(sub.get('comp1.d'),-3)
        logging.getLogger('').debug('***** 2nd run *****')
        self.top.run()  
        # run_count should stay at 1 for all comps
        self.assertEqual([1, 1, 1, 1, 1, 1, 1], [top.get(x).run_count for x in allcomps])
        
        sub.set('b6', 777)
        self.top.run()  
        # run_count should change only for comp6
        self.assertEqual([1, 1, 1, 1, 1, 2, 1], [top.get(x).run_count for x in allcomps])
        
        sub.set('a3', 111)
        self.top.run()  
        # run_count should change for all sub comps but comp2
        self.assertEqual([2, 1, 2, 2, 2, 3, 1], [top.get(x).run_count for x in allcomps])
    
        sub.set('comp2.a', 5)
        self.top.run()  
        # run_count should change for all sub comps but comp3 
        self.assertEqual([3, 2, 2, 3, 3, 4, 1], [top.get(x).run_count for x in allcomps])
    
        top.set('comp7.a', 5)
        self.top.run()  
        # run_count should change for comp7 and all subs but comp2
        self.assertEqual([4, 2, 3, 4, 4, 5, 2], [top.get(x).run_count for x in allcomps])
        
        top.set('comp7.b', 4)
        logging.getLogger('').debug('***** running top.sub.comp1 directly *****')
        top.sub.comp1.run()
    
if __name__ == "__main__":
    
    #import cProfile
    #cProfile.run('unittest.main()', 'profout')
    
    #import pstats
    #p = pstats.Stats('profout')
    #p.strip_dirs()
    #p.sort_stats('time')
    #p.print_stats()
    #print '\n\n---------------------\n\n'
    #p.print_callers()
    #print '\n\n---------------------\n\n'
    #p.print_callees()
        
    unittest.main()


