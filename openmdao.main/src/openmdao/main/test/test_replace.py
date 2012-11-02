
import unittest
import logging
import math
import nose

from openmdao.main.api import Assembly, Component, Driver, set_as_top, Dataflow
from openmdao.lib.datatypes.api import Float, Int, Array
from openmdao.main.hasobjective import HasObjectives
from openmdao.main.hasconstraints import HasConstraints, HasEqConstraints, HasIneqConstraints
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasobjective import HasObjective, HasObjectives
from openmdao.util.decorators import add_delegate


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
        self.c = self.a + self.b
        self.d = self.a - self.b
        
@add_delegate(HasParameters, HasIneqConstraints, HasObjective)
class InEqdriver(Driver):
    pass

@add_delegate(HasParameters, HasEqConstraints, HasObjective)
class Eqdriver(Driver):
    pass

@add_delegate(HasParameters, HasConstraints, HasObjective)
class EqInEqdriver(Driver):
    pass

@add_delegate(HasObjectives)
class Objectivesdriver(Driver):
    pass

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
    
    # 'auto' passthroughs
    top.connect('comp7.c', 'sub.comp3.a')
    top.connect('sub.comp3.d', 'comp8.b')
    
    return top

class ReplaceTestCase(unittest.TestCase):

    def test_replace_comp(self):
        top = _nested_model()
        conns1 = top.list_connections()
        subconns1 = top.sub.list_connections()
        top.run()
        c8_a = top.comp8.a
        c8_b = top.comp8.b
        top.replace('comp8', Simple())
        self.assertEqual(c8_a, top.comp8.a)
        self.assertEqual(c8_b, top.comp8.b)
        
        top2 = _nested_model()
        top2.replace('comp8', Simple())
        conns2 = top2.list_connections()
        subconns2 = top2.sub.list_connections()
        self.assertEqual(conns1, conns2)
        self.assertEqual(subconns1, subconns2)
        top2.run()
        self.assertEqual(c8_a, top2.comp8.a)
        self.assertEqual(c8_b, top2.comp8.b)
        
    def test_replace_driver(self):
        top = set_as_top(Assembly())
        top.add('driver', EqInEqdriver())
        top.add('comp1', Simple())
        top.add('comp2', Simple())
        top.driver.workflow.add(['comp1', 'comp2'])
        top.driver.add_parameter('comp1.a', low=-100, high=100, 
                      scaler=1.2, adder=3, start=7,
                      fd_step=0.034, name='param1', scope=top)
        top.driver.add_parameter('comp2.a', low=-50, high=50, scope=top)
        top.driver.add_objective('comp1.d+comp2.c-comp2.d', scope=top)
        top.driver.add_constraint('comp1.d-comp1.c=.5')
        
        old_params = top.driver.get_parameters()
        old_objectives = top.driver.get_objectives()
        old_constraints = top.driver.get_eq_constraints()
        
        try:
            top.replace('driver', InEqdriver())
        except Exception as err:
            self.assertEqual(str(err), 
                             ": Couldn't replace 'driver' of type EqInEqdriver with type InEqdriver: driver: Equality constraint 'comp1.d-comp1.c = .5' is not supported on this driver")
            
        top.replace('driver', Eqdriver())
        self.assertEqual(old_params, top.driver.get_parameters())
        self.assertEqual(old_objectives, top.driver.get_objectives())
        self.assertEqual(old_constraints, top.driver.get_eq_constraints())
        
        top.add('driver', Objectivesdriver())
        top.driver.add_objective('comp1.d+comp2.c-comp2.d', scope=top)
        top.driver.add_objective('comp1.c-comp2.d*3.5', scope=top)
        
        try:
            top.replace('driver', Eqdriver())
        except Exception as err:
            self.assertEqual(str(err),
                             ": Couldn't replace 'driver' of type Objectivesdriver with type Eqdriver: driver: This driver allows a maximum of 1 objectives, but the driver being replaced has 2")
            
        top.add('driver', InEqdriver())
        top.driver.add_parameter('comp1.a', low=-100, high=100, 
                      scaler=1.2, adder=3, start=7,
                      fd_step=0.034, name='param1', scope=top)
        top.driver.add_objective('comp1.d+comp2.c-comp2.d', scope=top)
        
        try:
            top.replace('driver', Objectivesdriver())
        except Exception as err:
            self.assertEqual(str(err),
                             ": Couldn't replace 'driver' of type InEqdriver with type Objectivesdriver: driver: target delegate '_hasparameters' has no match")
        
        top.add('driver', InEqdriver())
        top.driver.add_objective('comp1.d+comp2.c-comp2.d', scope=top)
        top.driver.add_constraint('comp1.d-comp1.c<.5')
        
        try:
            top.replace('driver', Objectivesdriver())
        except Exception as err:
            self.assertEqual(str(err),
                             ": Couldn't replace 'driver' of type InEqdriver with type Objectivesdriver: driver: target delegate '_hasineqconstraints' has no match")
        
            