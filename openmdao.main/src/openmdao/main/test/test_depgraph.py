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

allcomps = ['sub.comp1','sub.comp2','sub.comp3','sub.comp4','sub.comp5','sub.comp6',
            'comp7','comp8']
allouts = ['sub.comp1.c', 'sub.comp1.d',
           'sub.comp2.c', 'sub.comp2.d',
           'sub.comp3.c', 'sub.comp3.d',
           'sub.comp4.c', 'sub.comp4.d',
           'sub.comp5.c', 'sub.comp5.d',
           'sub.comp6.c', 'sub.comp6.d',
           'comp7.c', 'comp7.d',
           'comp8.c', 'comp8.d']

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
        top.add_child(Simple('comp8'))

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
        top.connect('sub.c4', 'comp8.a')
        top.connect('sub.d3', 'comp8.b')
        
        self.top.run()        

    def test_lazy1(self):
        self.assertEqual([1, 1, 1, 1, 1, 1, 1, 1], 
                         [self.top.get(x).run_count for x in allcomps])
        outs = [(5,-3),(3,-1),(5,1),(7,3),(4,6),(5,1),(3,-1),(8,6)]
        for comp,vals in zip(allcomps,outs):
            self.assertEqual((comp,vals[0],vals[1]), 
                             (comp,self.top.get(comp+'.c'),self.top.get(comp+'.d')))
        self.top.run()  
        # run_count should stay at 1 for all comps
        self.assertEqual([1, 1, 1, 1, 1, 1, 1, 1], 
                         [self.top.get(x).run_count for x in allcomps])
        
    def test_lazy2(self):
        self.top.set('sub.b6', 3)
        self.top.run()  
        # run_count should change only for comp6
        self.assertEqual([1, 1, 1, 1, 1, 2, 1, 1], 
                         [self.top.get(x).run_count for x in allcomps])
        outs = [(5,-3),(3,-1),(5,1),(7,3),(4,6),(6,0),(3,-1),(8,6)]
        for comp,vals in zip(allcomps,outs):
            self.assertEqual((comp,vals[0],vals[1]), 
                             (comp,self.top.get(comp+'.c'),self.top.get(comp+'.d')))
        
    def test_lazy3(self):
        self.top.set('comp7.a', 3)
        self.top.run()  
        # run_count should change for all sub comps but comp2
        self.assertEqual([2, 1, 2, 2, 2, 2, 2, 2], 
                         [self.top.get(x).run_count for x in allcomps])
        outs = [(7,-5),(3,-1),(7,3),(9,5),(6,8),(7,3),(5,1),(12,6)]
        for comp,vals in zip(allcomps,outs):
            self.assertEqual((comp,vals[0],vals[1]), 
                             (comp,self.top.get(comp+'.c'),self.top.get(comp+'.d')))
    
    def test_lazy4(self):
        self.top.sub.set('b2', 5)
        self.top.run()  
        # run_count should change for all sub comps but comp3 and comp7 
        self.assertEqual([2, 2, 1, 2, 2, 2, 1, 2], 
                         [self.top.get(x).run_count for x in allcomps])
        outs = [(2,0),(6,-4),(5,1),(4,0),(1,9),(2,-2),(3,-1),(5,3)]
        for comp,vals in zip(allcomps,outs):
            self.assertEqual((comp,vals[0],vals[1]), 
                             (comp,self.top.get(comp+'.c'),self.top.get(comp+'.d')))
    
    def test_lazy_inside_out(self):
        self.top.set('comp7.b', 4)
        # now run sub.comp1 directly to make sure it will force
        # running of all components that supply its inputs
        self.top.sub.comp1.run()
        self.assertEqual([2, 1, 2, 1, 2, 1, 2, 1], 
                         [self.top.get(x).run_count for x in allcomps])
        outs = [(7,-5),(3,-1),(7,3),(7,3),(6,8),(5,1),(5,-3),(8,6)]
        for comp,vals in zip(allcomps,outs):
            self.assertEqual((comp,vals[0],vals[1]), 
                             (comp,self.top.get(comp+'.c'),self.top.get(comp+'.d')))
            
        # now run comp8 directly, which should force sub.comp4 to run
        self.top.comp8.run()
        self.assertEqual([2, 1, 2, 2, 2, 1, 2, 2], 
                         [self.top.get(x).run_count for x in allcomps])
        outs = [(7,-5),(3,-1),(7,3),(9,5),(6,8),(5,1),(5,-3),(12,6)]
        for comp,vals in zip(allcomps,outs):
            self.assertEqual((comp,vals[0],vals[1]), 
                             (comp,self.top.get(comp+'.c'),self.top.get(comp+'.d')))
    
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


