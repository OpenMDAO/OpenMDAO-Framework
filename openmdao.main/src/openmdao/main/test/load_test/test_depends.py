# pylint: disable-msg=C0111,C0103

import unittest
import logging
import math
import nose

from openmdao.main.api import Assembly, Component, Driver, set_as_top, Dataflow
from openmdao.lib.datatypes.api import Float, Int, Array, List, Dict
from openmdao.main.hasobjective import HasObjectives
from openmdao.main.hasconstraints import HasConstraints
from openmdao.main.hasparameters import HasParameters
from openmdao.util.decorators import add_delegate
from openmdao.test.execcomp import ExecComp
from openmdao.util.testutil import assert_rel_error

import random

exec_order = []

@add_delegate(HasObjectives, HasParameters, HasConstraints)
class DumbDriver(Driver):
    def __init__(self):
        self.oldval = None
        super(DumbDriver, self).__init__()
        
    def execute(self):
        global exec_order
        exec_order.append(self.name)
        newval = self.oldval
        while newval == self.oldval:
            newval = random.randint(0,999)
        self.oldval = newval
        
        self.set_parameters([newval]*len(self.get_parameters()))
        super(DumbDriver, self).execute()


class Simple(Component):
    a = Float(iotype='in', units='ft')
    b = Float(iotype='in', units='ft')
    c = Float(iotype='out', units='ft')
    d = Float(iotype='out', units='ft')
    
    def __init__(self):
        super(Simple, self).__init__()
        self.a = 1
        self.b = 2
        self.c = 3
        self.d = -1

    def execute(self):
        global exec_order
        exec_order.append(self.name)
        self.c = self.a + self.b
        self.d = self.a - self.b
        

allcomps = ['sub.comp1','sub.comp2','sub.comp3','sub.comp4','sub.comp5','sub.comp6',
            'comp7','comp8']

topouts = ['sub.c2', 'sub.c4', 'sub.d1', 'sub.d3','sub.d5'
           'comp7.c', 'comp7.d','comp8.c', 'comp8.d']

topins = ['sub.a1', 'sub.a3', 'sub.b2', 'sub.b4','sub.b6'
          'comp7.a', 'comp7.b','comp8.a', 'comp8.b']

subins = ['comp1.a', 'comp1.b',
          'comp2.a', 'comp2.b',
          'comp3.a', 'comp3.b',
          'comp4.a', 'comp4.b',
          'comp5.a', 'comp5.b',
          'comp6.a', 'comp6.b',]

subouts = ['comp1.c', 'comp1.d',
           'comp2.c', 'comp2.d',
           'comp3.c', 'comp3.d',
           'comp4.c', 'comp4.d',
           'comp5.c', 'comp5.d',
           'comp6.c', 'comp6.d',]


subvars = subins+subouts

def _nested_model():
    global exec_order
    exec_order = []
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
    sub.create_passthrough('comp4.b', 'b4')
    sub.create_passthrough('comp4.c', 'c4')
    sub.create_passthrough('comp6.b', 'b6')
    sub.create_passthrough('comp2.c', 'c2')
    sub.create_passthrough('comp1.d', 'd1')
    sub.create_passthrough('comp5.d', 'd5')
    
    return top

class DependsTestCase(unittest.TestCase):

        
    def setUp(self):
        top = self.top = _nested_model()
        sub = top.sub
        sub.connect('comp1.c', 'comp4.a')
        sub.connect('comp5.c', 'comp1.b')
        sub.connect('comp2.d', 'comp5.b')
        sub.connect('comp3.c', 'comp5.a')
        sub.connect('comp4.d', 'comp6.a')
        
        top.connect('sub.c4', 'comp8.a')
        
        # 'auto' passthroughs
        top.connect('comp7.c', 'sub.comp3.a')
        top.connect('sub.comp3.d', 'comp8.b')

    def test_simple(self):
        top = set_as_top(Assembly())
        top.add('comp1', Simple())
        top.driver.workflow.add('comp1')
        vars = ['a','b','c','d']
        self.assertEqual(top.comp1.exec_count, 0)
        valids = top.comp1.get_valid(vars)
        self.assertEqual(valids, [True, True, False, False])
        top.run()
        self.assertEqual(top.comp1.exec_count, 1)
        self.assertEqual(top.comp1.c, 3)
        self.assertEqual(top.comp1.d, -1)
        valids = top.comp1.get_valid(vars)
        self.assertEqual(valids, [True, True, True, True])
        top.set('comp1.a', 5)
        valids = top.comp1.get_valid(vars)
        self.assertEqual(valids, [True, True, False, False])
        top.run()
        self.assertEqual(top.comp1.exec_count, 2)
        self.assertEqual(top.comp1.c, 7)
        self.assertEqual(top.comp1.d, 3)
        top.run()
        self.assertEqual(top.comp1.exec_count, 2) # exec_count shouldn't change
        valids = top.comp1.get_valid(vars)
        self.assertEqual(valids, [True, True, True, True])
        
        # now add another comp and connect them
        top.add('comp2', Simple())
        top.driver.workflow.add('comp2')
        top.connect('comp1.c', 'comp2.a')
        self.assertEqual(top.comp2.exec_count, 0)
        self.assertEqual(top.comp2.c, 3)
        self.assertEqual(top.comp2.d, -1)
        valids = top.comp2.get_valid(vars)
        self.assertEqual(valids, [False, True, False, False])
        top.run()
        self.assertEqual(top.comp1.exec_count, 2)
        self.assertEqual(top.comp2.exec_count, 1)
        self.assertEqual(top.comp2.c, 9)
        self.assertEqual(top.comp2.d, 5)
        valids = top.comp2.get_valid(vars)
        self.assertEqual(valids, [True, True, True, True])
        
    def test_disconnect(self):
        self.top.disconnect('comp7.c', 'sub.comp3.a')
        self.top.sub.disconnect('c4')
        self.top.disconnect('comp8')
        
    def test_disconnect2(self):
        self.assertEqual(set(self.top.sub.list_outputs(connected=True)),
                         set(['comp3.d','c4']))
        self.top.disconnect('comp8')
        self.assertEqual(self.top.sub.list_outputs(connected=True),
                         [])
        self.assertEqual(self.top.sub._exprmapper.get_source('c4'), 'comp4.c')
        
    def test_lazy1(self):
        self.top.run()
        exec_counts = [self.top.get(x).exec_count for x in allcomps]
        self.assertEqual([1, 1, 1, 1, 1, 1, 1, 1], exec_counts)
        outs = [(5,-3),(3,-1),(5,1),(7,3),(4,6),(5,1),(3,-1),(8,6)]
        newouts = []
        for comp in allcomps:
            newouts.append((self.top.get(comp+'.c'),self.top.get(comp+'.d')))
        self.assertEqual(outs, newouts)
        self.top.run()  
        # exec_count should stay at 1 for all comps
        self.assertEqual([1, 1, 1, 1, 1, 1, 1, 1], 
                         [self.top.get(x).exec_count for x in allcomps])
        
    def test_lazy2(self):
        vars = ['a','b','c','d']
        self.top.run()        
        exec_count = [self.top.get(x).exec_count for x in allcomps]
        self.assertEqual([1, 1, 1, 1, 1, 1, 1, 1], exec_count)
        valids = self.top.sub.comp6.get_valid(vars)
        self.assertEqual(valids, [True, True, True, True])
        self.top.sub.b6 = 3
        valids = self.top.sub.comp6.get_valid(vars)
        self.assertEqual(valids, [True, False, False, False])
        self.top.run()  
        # exec_count should change only for comp6
        exec_count = [self.top.get(x).exec_count for x in allcomps]
        self.assertEqual([1, 1, 1, 1, 1, 2, 1, 1], exec_count)
        outs = [(5,-3),(3,-1),(5,1),(7,3),(4,6),(6,0),(3,-1),(8,6)]
        for comp,vals in zip(allcomps,outs):
            self.assertEqual((comp,vals[0],vals[1]), 
                             (comp,self.top.get(comp+'.c'),self.top.get(comp+'.d')))
            
    def test_lazy3(self):
        vars = ['a','b','c','d']
        self.top.run()        
        exec_count = [self.top.get(x).exec_count for x in allcomps]
        self.assertEqual([1, 1, 1, 1, 1, 1, 1, 1], exec_count)
        valids = self.top.sub.comp3.get_valid(vars)
        self.assertEqual(valids, [True, True, True, True])
        self.top.comp7.a = 3
        valids = self.top.sub.comp1.get_valid(vars)
        self.assertEqual(valids, [True, False, False, False])
        valids = self.top.sub.comp2.get_valid(vars)
        self.assertEqual(valids, [True, True, True, True])
        valids = self.top.sub.comp3.get_valid(vars)
        self.assertEqual(valids, [False, True, False, False])
        valids = self.top.sub.comp4.get_valid(vars)
        self.assertEqual(valids, [False, True, False, False])
        valids = self.top.sub.comp5.get_valid(vars)
        self.assertEqual(valids, [False, True, False, False])
        valids = self.top.sub.comp6.get_valid(vars)
        self.assertEqual(valids, [False, True, False, False])
        valids = self.top.comp7.get_valid(vars)
        self.assertEqual(valids, [True, True, False, False])
        valids = self.top.comp8.get_valid(vars)
        self.assertEqual(valids, [False, False, False, False])
        self.top.run()  
        # exec_count should change for all sub comps but comp2
        exec_count = [self.top.get(x).exec_count for x in allcomps]
        self.assertEqual([2, 1, 2, 2, 2, 2, 2, 2], exec_count)
        outs = [(7,-5),(3,-1),(7,3),(9,5),(6,8),(7,3),(5,1),(12,6)]
        for comp,vals in zip(allcomps,outs):
            self.assertEqual((comp,vals[0],vals[1]), 
                             (comp,self.top.get(comp+'.c'),self.top.get(comp+'.d')))
    
    def test_lazy4(self):
        self.top.run()
        self.top.sub.set('b2', 5)
        self.assertEqual(self.top.sub.get_valid(subvars),
                         [True,False,
                          True,False,
                          True,True,
                          False,True,
                          True,False,
                          False,True,
                          False,False,
                          False,False,
                          True,True,
                          False,False,
                          False,False,
                          False,False])
        self.top.run()
        # exec_count should change for all sub comps but comp3 and comp7 
        self.assertEqual([2, 2, 1, 2, 2, 2, 1, 2], 
                         [self.top.get(x).exec_count for x in allcomps])
        outs = [(2,0),(6,-4),(5,1),(4,0),(1,9),(2,-2),(3,-1),(5,3)]
        for comp,vals in zip(allcomps,outs):
            self.assertEqual((comp,vals[0],vals[1]), 
                             (comp,self.top.get(comp+'.c'),self.top.get(comp+'.d')))
    
    def test_lazy_inside_out(self):
        self.top.run()
        self.top.comp7.b = 4
        # now run sub.comp1 directly to make sure it will force
        # running of all components that supply its inputs
        self.top.sub.comp1.run()
        exec_count = [self.top.get(x).exec_count for x in allcomps]
        self.assertEqual([2, 1, 2, 1, 2, 1, 2, 1], exec_count)
        outs = [(7,-5),(3,-1),(7,3),(7,3),(6,8),(5,1),(5,-3),(8,6)]
        for comp,vals in zip(allcomps,outs):
            self.assertEqual((comp,vals[0],vals[1]), 
                             (comp,self.top.get(comp+'.c'),self.top.get(comp+'.d')))
            
        # now run comp8 directly, which should force sub.comp4 to run
        self.top.comp8.run()
        exec_count = [self.top.get(x).exec_count for x in allcomps]
        self.assertEqual([2, 1, 2, 2, 2, 1, 2, 2], exec_count)
        outs = [(7,-5),(3,-1),(7,3),(9,5),(6,8),(5,1),(5,-3),(12,6)]
        for comp,vals in zip(allcomps,outs):
            self.assertEqual((comp,vals[0],vals[1]), 
                             (comp,self.top.get(comp+'.c'),self.top.get(comp+'.d')))
            
    def test_sequential(self):
        # verify that if components aren't connected they should execute in the
        # order that they were added to the workflow instead of hash order
        global exec_order
        top = set_as_top(Assembly())
        top.add('c2', Simple())
        top.add('c1', Simple())
        top.add('c3', Simple())
        top.add('c4', Simple())
        top.driver.workflow.add(['c1','c2','c3','c4'])
        top.run()
        self.assertEqual(exec_order, ['c1','c2','c3','c4'])
        top.connect('c4.c', 'c3.a')  # now make c3 depend on c4
        exec_order = []
        top.c4.a = 2  # makes c4 run again
        top.run()
        self.assertEqual(exec_order, ['c4','c3'])
        
        
    def test_expr_deps(self):
        top = set_as_top(Assembly())
        driver1 = top.add('driver1', DumbDriver())
        driver2 = top.add('driver2', DumbDriver())
        top.add('c1', Simple())
        top.add('c2', Simple())
        top.add('c3', Simple())
        
        top.driver.workflow.add(['driver1','driver2','c3'])
        top.driver1.workflow.add('c2')
        top.driver2.workflow.add('c1')
        
        top.connect('c1.c', 'c2.a')
        top.driver1.add_objective("c2.c*c2.d")
        top.driver2.add_objective("c1.c")
        top.run()
        self.assertEqual(exec_order, ['driver2','c1','driver1','c2','c3'])
        

    def test_set_already_connected(self):
        try:
            self.top.sub.comp2.b = 4
        except Exception, err:
            self.assertEqual(str(err), 
                "sub.comp2: 'b' is already connected to source 'parent.b2' and cannot be directly set")
        else:
            self.fail('Exception expected')
        try:
            self.top.set('sub.comp2.b', 4)
        except Exception, err:
            self.assertEqual(str(err), 
                "sub.comp2: 'b' is connected to source 'parent.b2' and cannot be set by source 'None'")
        else:
            self.fail('Exception expected')
            
    def test_force_with_input_updates(self):
        top = set_as_top(Assembly())
        top.add('c2', Simple())
        top.add('c1', Simple())
        top.c2.force_execute = True
        top.connect('c1.c', 'c2.a')
        top.driver.workflow.add(['c1','c2'])
        top.run()
        self.assertEqual(top.c2.a, 3)
        top.c1.a = 2
        top.run()
        self.assertEqual(top.c2.a, 4)

    def test_get_required_compnames(self):
        sub = self.top.sub
        sub.add('driver', DumbDriver())
        sub.driver.add_objective('comp6.c')
        sub.driver.add_objective('comp5.d')
        self.assertEqual(sub.driver._get_required_compnames(),
                         set([]))
        sub.driver.add_parameter('comp2.a', low=0.0, high=10.0)
        self.assertEqual(sub.driver._get_required_compnames(),
                         set(['comp2', 'comp5', 'comp1', 'comp4', 'comp6']))
        sub.driver.add_parameter('comp3.b', low=0.0, high=10.0)
        self.assertEqual(sub.driver._get_required_compnames(),
                         set(['comp6','comp5','comp1','comp4','comp3', 'comp2']))
        
    def test_auto_workflow(self):
        top = set_as_top(Assembly())
        top.add('comp1', Simple())
        top.add('comp2', Simple())
        top.add('comp3', Simple())
        top.add('driver', DumbDriver())
        top.driver.add_parameter('comp2.a',low=-99,high=99)
        top.driver.add_objective('comp3.c')
        top.connect('comp1.c', 'comp2.b')
        top.connect('comp2.c', 'comp3.a')
        vars = ['a','b','c','d']
        self.assertEqual(top.comp1.exec_count, 0)
        self.assertEqual(top.comp2.exec_count, 0)
        self.assertEqual(top.comp3.exec_count, 0)
        top.run()
        self.assertEqual(top.comp1.exec_count, 1)
        self.assertEqual(top.comp2.exec_count, 1)
        self.assertEqual(top.comp3.exec_count, 1)
        top.driver.run()
        self.assertEqual(top.comp1.exec_count, 1)
        self.assertEqual(top.comp2.exec_count, 2)
        self.assertEqual(top.comp3.exec_count, 2)
        top.comp1.a = 9999
        top.driver.run()
        self.assertEqual(top.comp1.exec_count, 2)
        self.assertEqual(top.comp2.exec_count, 3)
        self.assertEqual(top.comp3.exec_count, 3)
        

        
class SimplePTAsm(Assembly):
    def configure(self):
        self.add('c2', Simple())
        self.add('c1', Simple())
        
        self.driver.workflow.add(['c1','c2'])

        self.connect('c1.c', 'c2.a')
        self.connect('c1.d', 'c2.b')
        
        self.create_passthrough('c1.a', 'a1')
        self.create_passthrough('c2.d', 'd2')
    
    
class DependsTestCase2(unittest.TestCase):

    def setUp(self):
        global exec_order
        self.top = set_as_top(Assembly())
        self.top.add('c2', Simple())
        self.top.add('c1', Simple())
        self.top.driver.workflow.add(['c1','c2'])
    
    def test_connected_vars(self):
        self.assertEqual(self.top.c1.list_outputs(connected=True), [])
        self.assertEqual(self.top.c2.list_inputs(connected=True), [])
        self.top.connect('c1.c', 'c2.a')
        self.assertEqual(self.top.c1.list_outputs(connected=True), ['c'])
        self.assertEqual(self.top.c2.list_inputs(connected=True), ['a'])
        self.top.connect('c1.d', 'c2.b')
        self.assertEqual(set(self.top.c1.list_outputs(connected=True)), set(['c', 'd']))
        self.assertEqual(set(self.top.c2.list_inputs(connected=True)), set(['a', 'b']))
        self.top.disconnect('c1.d', 'c2.b')
        self.assertEqual(self.top.c1.list_outputs(connected=True), ['c'])
        self.assertEqual(self.top.c2.list_inputs(connected=True), ['a'])
                
    def test_unconnected_vars(self):
        extras = set(self.top.c1.list_vars())-set(['a','b','c','d'])
        self.assertEqual(set(self.top.c1.list_outputs(connected=False))-extras, set(['c', 'd']))
        self.assertEqual(set(self.top.c2.list_inputs(connected=False))-extras, set(['a', 'b']))
        self.top.connect('c1.c', 'c2.a')
        self.assertEqual(set(self.top.c1.list_outputs(connected=False))-extras, set(['d']))
        self.assertEqual(set(self.top.c2.list_inputs(connected=False))-extras, set(['b']))
        self.top.connect('c1.d', 'c2.b')
        self.assertEqual(set(self.top.c1.list_outputs(connected=False))-extras, set())
        self.assertEqual(set(self.top.c2.list_inputs(connected=False))-extras, set())
        self.top.disconnect('c1.d', 'c2.b')
        self.assertEqual(set(self.top.c1.list_outputs(connected=False))-extras, set(['d']))
        self.assertEqual(set(self.top.c2.list_inputs(connected=False))-extras, set(['b']))
                
    def test_simple_run(self):
        self.top.connect('c1.c', 'c2.a')
        self.top.connect('c1.d', 'c2.b')
        self.top.run()
        self.assertEqual(self.top.c1.a, 1)
        self.assertEqual(self.top.c1.b, 2)
        self.assertEqual(self.top.c1.c, 3)
        self.assertEqual(self.top.c1.d, -1)
        self.assertEqual(self.top.c2.a, 3)
        self.assertEqual(self.top.c2.b, -1)
        self.assertEqual(self.top.c2.c, 2)
        self.assertEqual(self.top.c2.d, 4)
        
        self.top.c1.a = 2
        self.top.run()
        self.assertEqual(self.top.c1.a, 2)
        self.assertEqual(self.top.c1.b, 2)
        self.assertEqual(self.top.c1.c, 4)
        self.assertEqual(self.top.c1.d, 0)
        self.assertEqual(self.top.c2.a, 4)
        self.assertEqual(self.top.c2.b, 0)
        self.assertEqual(self.top.c2.c, 4)
        self.assertEqual(self.top.c2.d, 4)
        
    def test_simple_passthrough(self):
        cnames = ['a','b','c','d']
        modnames = ['a1', 'd2']
        
        self.top.add('model', SimplePTAsm())
        self.top.driver.workflow.add(['model'])
        self.top.connect('c1.c', 'model.a1')
        self.top.connect('model.d2', 'c2.a')
        
        self.assertEqual(self.top.c1.get_valid(cnames), 
                         [True, True, False, False])
        self.assertEqual(self.top.c2.get_valid(cnames), 
                         [False, True, False, False])
        self.assertEqual(self.top.model.get_valid(modnames), 
                         [False, False])
        self.assertEqual(self.top.model.c1.get_valid(cnames), 
                         [False, True, False, False])
        self.assertEqual(self.top.model.c2.get_valid(cnames), 
                         [False, False, False, False])
        
        self.top.run()
        
        self.assertEqual(self.top.c1.get_valid(cnames), 
                         [True, True, True, True])
        self.assertEqual(self.top.c2.get_valid(cnames), 
                         [True, True, True, True])
        self.assertEqual(self.top.model.get_valid(modnames), 
                         [True, True])
        self.assertEqual(self.top.model.c1.get_valid(cnames), 
                         [True, True, True, True])
        self.assertEqual(self.top.model.c2.get_valid(cnames), 
                         [True, True, True, True])

        # test invalidation
        self.top.c1.a = 99
        self.assertEqual(self.top.c1.get_valid(cnames), 
                         [True, True, False, False])
        self.assertEqual(self.top.model.get_valid(modnames), 
                         [False, False])
        self.assertEqual(self.top.model.c1.get_valid(cnames), 
                         [False, True, False, False])
        self.assertEqual(self.top.model.c2.get_valid(cnames), 
                         [False, False, False, False])
        self.assertEqual(self.top.c2.get_valid(cnames), 
                         [False, True, False, False])
        
    def test_array_expr(self):
        class Dummy(Component): 
        
            x = Array([[-1, 1],[-2, 2]],iotype="in",shape=(2,2))
            y = Array([[-1, 1],[-2, 2]],iotype="out",shape=(2,2))
            
            def execute(self): 
                self.y = self.x
                
        class Stuff(Assembly): 
        
            def configure(self):
                self.add('d1',Dummy())
                self.add('d2',Dummy())
                
                self.connect('d1.y[0][0]','d2.x[1][0]')   
                self.connect('d1.y[1][0]','d2.x[0][0]')
                
                self.driver.workflow.add(['d1','d2'])
        
        s = set_as_top(Stuff())
        s.d1.x = [[-5,-6], [-7,-8]]
        s.run()
        self.assertEqual(s.d2.x[0,0], -7)
        self.assertEqual(s.d2.x[1,0], -5)
        self.assertEqual(s.d2.x[0,1], 1)
        self.assertEqual(s.d2.x[1,1], 2)

    def test_units(self):
        top = self.top
        top.c2.add("velocity", Float(3.0, iotype='in', units='inch/s'))
        top.c1.add("length", Float(9.0, iotype='out', units='inch'))
        
        try:
            top.connect('c1.c', 'c2.velocity')
        except Exception as err:
            self.assertEqual(str(err), ": Can't connect 'c1.c' to 'c2.velocity': velocity: units 'ft' are incompatible with assigning units of 'inch/s'")
        else:
            self.fail("Exception expected")
        
        top.c1.a = 1.
        top.c1.b = 2.
        top.c1.length = 24.
        top.connect('c1.length', 'c2.a')
        top.run()
        assert_rel_error(self, top.c2.a, 2., 0.0001)

    def test_index_invalidation(self):
        
        class Dummy(Component): 
        
            x = Array([[-1, 1],[-2, 2]], iotype='in', shape=(2,2))
            xlist = List([1,2], iotype='in')
            xdict = Dict({'a' : 'b'}, iotype='in')
            
            def execute(self): 
                self.y = self.x

        comp = Dummy()
        self.assertEqual(comp.is_valid(), False)
        comp.run()
        self.assertEqual(comp.is_valid(), True)

        comp.xlist.append(3)
        self.assertEqual(comp.is_valid(), False)
        comp.run()
        self.assertEqual(comp.is_valid(), True)
        
        comp.xdict['d'] = 'e'
        self.assertEqual(comp.is_valid(), False)
        comp.run()
        self.assertEqual(comp.is_valid(), True)
        
        # Array invalidation not supported yet
        #comp.x[1][1] = 32.0
        #self.assertEqual(comp.is_valid(), False)
        #comp.run()
        #self.assertEqual(comp.is_valid(), True)
        

class ArrayComp(Component):
    a = Array([1,2,3,4,5], iotype="in")
    b = Array([1,2,3,4,5], iotype='in')
    c = Array([2,4,6,8,10], iotype='out')
    d = Array([0,0,0,0,0], iotype='out')
    
    def execute(self):
        global exec_order
        exec_order.append(self.name)
        self.c = self.a + self.b
        self.d = self.a - self.b

class ExprDependsTestCase(unittest.TestCase):

    def setUp(self):
        global exec_order
        exec_order = []
        self.top = set_as_top(Assembly())
        self.top.add('c2', ArrayComp())
        self.top.add('c1', ArrayComp())
        self.top.driver.workflow.add(['c1','c2'])
        
    def test_basic(self):
        self.top.connect('c1.c', 'c2.a')
        self.top.connect('c1.d', 'c2.b')
        self.top.run()
        self.assertEqual(list(self.top.c1.c), [2,4,6,8,10])
        self.assertEqual(list(self.top.c1.d), [0,0,0,0,0])
        self.assertEqual(list(self.top.c2.a), [2,4,6,8,10])
        self.assertEqual(list(self.top.c2.b), [0,0,0,0,0])
        self.assertEqual(list(self.top.c2.c), [2,4,6,8,10])
        self.assertEqual(list(self.top.c2.d), [2,4,6,8,10])

    def test_entry_connect(self):
        self.top.connect('c1.c[2]', 'c2.a[3]')
        self.top.run()
        self.assertEqual(list(self.top.c1.c), [2,4,6,8,10])
        self.assertEqual(list(self.top.c1.d), [0,0,0,0,0])
        self.assertEqual(list(self.top.c2.a), [1,2,3,6,5])
        self.assertEqual(list(self.top.c2.b), [1,2,3,4,5])
        self.assertEqual(list(self.top.c2.c), [2,4,6,10,10])
        self.assertEqual(list(self.top.c2.d), [0,0,0,2,0])
        
        # now see if we can connect to another entry on c2.a
        self.top.connect('c1.d[2]', 'c2.a[1]')
        self.top.run()
        self.assertEqual(list(self.top.c1.c), [2,4,6,8,10])
        self.assertEqual(list(self.top.c1.d), [0,0,0,0,0])
        self.assertEqual(list(self.top.c2.a), [1,0,3,6,5])
        self.assertEqual(list(self.top.c2.b), [1,2,3,4,5])
        self.assertEqual(list(self.top.c2.c), [2,2,6,10,10])
        self.assertEqual(list(self.top.c2.d), [0,-2,0,2,0])
        
        # make sure only one connection allowed to a particular array entry
        try:
            self.top.connect('c1.d[1]', 'c2.a[1]')
        except Exception as err:
            self.assertEqual(str(err), ": Can't connect 'c1.d[1]' to 'c2.a[1]': : 'c2.a[1]' is already connected to source 'c1.d[2]'")
            
        # let's disconnect one entry and check the valid dict
        self.top.disconnect('c2.a[1]')
        self.assertEqual(self.top.c2._valid_dict['a[3]'], True)
        self.assertTrue('a[1]' not in self.top.c2._valid_dict)

    def test_invalidation(self):
        global exec_order
        vnames = ['a','b','c','d']
        self.top.run()
        valids = self.top.c2.get_valid(vnames)
        self.assertEqual(valids, [True, True, True, True])
        self.top.connect('c1.c[2]', 'c2.a[3]')
        self.assertEqual(self.top.c2.get_valid(['a[3]']), [False])
        exec_order = []
        self.top.run()
        self.assertEqual(self.top.c2.get_valid(['a[3]']), [True])
        self.assertEqual(exec_order, ['c2'])
        exec_order = []
        self.top.c1.a = [9,9,9,9,9]
        self.top.c2.run()
        self.assertEqual(exec_order, ['c1', 'c2'])
        valids = self.top.c2.get_valid(vnames)
        self.assertEqual(valids, [True, True, True, True])
        self.assertEqual(list(self.top.c2.a), [1,2,3,12,5])
        
    #def test_src_exprs(self):
        #global exec_order
        #vnames = ['a','b','c','d']
        #top = _nested_model()
        #top.run()
        #self.assertEqual(top.sub.comp4.get_valid(vnames), [True, True, True, True])
        
        #total = top.sub.comp1.c+top.sub.comp2.c+top.sub.comp3.c
        #top.sub.connect('comp1.c+comp2.c+comp3.c', 'comp4.a')
        #self.assertEqual(top.sub.comp4.get_valid(vnames), [False, True, False, False])
        #exec_order = []
        #top.run()
        #self.assertEqual(exec_order, ['comp4'])
        #self.assertEqual(top.sub.comp4.get_valid(vnames), [True, True, True, True])
        #self.assertEqual(total, top.sub.comp4.a)
        
        #top.sub.comp2.a = 99
        #self.assertEqual(top.sub.comp2.get_valid(vnames), [True, True, False, False])
        #self.assertEqual(top.sub.comp4.get_valid(vnames), [False, True, False, False])
        #exec_order = []
        #top.sub.run()
        #total = top.sub.comp1.c+top.sub.comp2.c+top.sub.comp3.c
        #self.assertEqual(total, top.sub.comp4.a)
        #self.assertEqual(exec_order, ['comp2','comp4'])
        #self.assertEqual(top.sub.comp4.get_valid(vnames), [True, True, True, True])
        #top.sub.comp2.a = 88
        #top.sub.comp3.a = 33
        #self.assertEqual(top.sub.comp4.get_valid(vnames), [False, True, False, False])
        #top.sub.run()
        #total = top.sub.comp1.c+top.sub.comp2.c+top.sub.comp3.c
        #self.assertEqual(total, top.sub.comp4.a)

    def test_float_exprs(self):
        global exec_order
        vnames = ['a','b','c','d']
        top = _nested_model()
        top.run()
        
        total = math.sin(3.14)*top.sub.comp2.c
        top.sub.connect('sin(3.14)*comp2.c', 'comp4.a')
        self.assertEqual(top.sub.comp4.get_valid(vnames), [False, True, False, False])
        exec_order = []
        top.run()
        self.assertEqual(exec_order, ['comp4'])
        self.assertEqual(top.sub.comp4.get_valid(vnames), [True, True, True, True])
        self.assertEqual(total, top.sub.comp4.a)
        
        top.sub.disconnect('sin(3.14)*comp2.c', 'comp4.a')
        total = 3.0*top.sub.comp1.c
        top.sub.connect('3.0*comp1.c', 'comp4.a')
        top.run()
        self.assertEqual(total, top.sub.comp4.a)
        
    def test_slice_exprs(self):
        global exec_order
        vnames = ['a[0:2:]','a','b','c','d']
        top = self.top
        top.run()
        total = top.c1.c[3:]
        top.connect('c1.c[3:]', 'c2.a[0:2]')
        self.assertEqual(top.c2.get_valid(vnames), [False, True, True, False, False])
        exec_order = []
        top.run()
        self.assertEqual(exec_order, ['c2'])
        self.assertEqual(top.c2.get_valid(vnames), [True, True, True, True, True])
        self.assertEqual(list(total), list(top.c2.a[0:2]))
        
    def _all_nested_connections(self, obj):
        """Return a list of all connections from ExprMappers and DepGraphs all the way down."""
        visited = set()
        connection_set = set()
        objstack = [obj]
        while objstack:
            obj = objstack.pop()
            if obj not in visited:
                visited.add(obj)
                if isinstance(obj, Assembly):
                    connection_set.update(obj.list_connections())
                    connection_set.update(obj._exprmapper.list_connections())
                    connection_set.update(obj._depgraph.list_connections())
                    for name in obj.list_containers():
                        comp = getattr(obj, name)
                        if isinstance(comp, Component):
                            connection_set.update(comp._depgraph.list_connections())
                            if isinstance(comp, Assembly):
                                objstack.append(comp)
        return connection_set
        
    def test_connection_cleanup(self):
        global exec_order
        vnames = ['a','b','c','d']
        top = _nested_model()
        initial_connections = set(top.sub.list_connections())
        top.run()
        top.sub.connect('comp1.c', 'comp3.b')
        self.assertEqual(set(top.sub.list_connections())-initial_connections, 
                         set([('comp1.c','comp3.b')]))
        top.sub.disconnect('comp1')
        self.assertEqual(set(top.sub.list_connections())-initial_connections, set())
        for u,v in self._all_nested_connections(top.sub):
            self.assertTrue('comp1.' not in u and 'comp1.' not in v)
        
    def test_connection_cleanup2(self):
        top = _nested_model()
        initial_connections = set(top.sub.list_connections())
        top.run()
        top.sub.connect('comp1.c*3.0', 'comp4.a')
        top.sub.connect('comp1.c', 'comp3.b')
        top.sub.disconnect('comp1.c','comp3.b')
        self.assertEqual(set(top.sub.list_connections())-initial_connections, 
                         set([('comp1.c*3.0', 'comp4.a')]))
        self.assertEqual(initial_connections-set(top.sub.list_connections()), 
                         set())
        for u,v in self._all_nested_connections(top.sub):
            self.assertTrue(not ('comp1.c' in u and 'comp3.b' in v))

    def test_bad_exprs(self):
        top = _nested_model()
        try:
            top.sub.connect('comp1.c', 'comp4.a+comp4.b')
        except Exception as err:
            self.assertEqual(str(err), "sub: Can't connect 'comp1.c' to 'comp4.a+comp4.b': bad connected expression 'comp4.a+comp4.b' must reference exactly one variable")
        else:
            self.fail("Exception expected")
            
        try:
            top.sub.connect('comp1.c', 'comp4.a[foo]')
        except Exception as err:
            self.assertEqual(str(err), "sub: Can't connect 'comp1.c' to 'comp4.a[foo]': bad destination expression 'comp4.a[foo]': only constant indices are allowed for arrays and slices")
        else:
            self.fail("Exception expected")
            
        try:
            top.sub.connect('comp1.c', 'comp4.a(5)')
        except Exception as err:
            self.assertEqual(str(err), "sub: Can't connect 'comp1.c' to 'comp4.a(5)': bad destination expression 'comp4.a(5)': not assignable")
        else:
            self.fail("Exception expected")
                    
                           
        
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


