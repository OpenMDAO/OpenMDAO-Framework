# pylint: disable-msg=C0111,C0103

import unittest

from enthought.traits.api import TraitError
from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.lib.api import Float, Str, Instance

class Multiplier(Component):
    rval_in = Float(iotype='in')
    rval_out = Float(iotype='out')
    mult = Float(iotype='in')
    
    def __init__(self):
        super(Multiplier, self).__init__()
        self.rval_in = 4.
        self.rval_out = 7.
        self.mult = 1.5
        self.run_count = 0

    def execute(self):
        self.run_count += 1
        self.rval_out = self.rval_in * self.mult
        
class Simple(Component):
    
    a = Float(iotype='in')
    b = Float(iotype='in')
    c = Float(iotype='out')
    d = Float(iotype='out')
    
    def __init__(self):
        super(Simple, self).__init__()
        self.a = 4.
        self.b = 5.
        self.c = 7.
        self.d = 1.5
        self.run_count = 0

    def execute(self):
        self.run_count += 1
        self.c = self.a + self.b
        self.d = self.a - self.b

class DummyComp(Component):
    
    r = Float(iotype='in')
    r2 = Float(iotype='in')
    s = Str(iotype='in')
    rout = Float(iotype='out')
    r2out = Float(iotype='out')
    sout = Str(iotype='out')
    
    dummy_in = Instance(Component, iotype='in')
    dummy_out = Instance(Component, iotype='out')
    
    def __init__(self):
        super(DummyComp, self).__init__()
        self.r = 1.0
        self.r2 = -1.0
        self.rout = 0.0
        self.r2out = 0.0
        self.s = 'a string'
        self.sout = ''
        
        # make a nested container with input and output ContainerVars
        self.add('dummy', Multiplier())
        self.dummy_in = self.dummy
        self.dummy_out = self.dummy
                
    def execute(self):
        self.rout = self.r * 1.5
        self.r2out = self.r2 + 10.0
        self.sout = self.s[::-1]
        # pylint: disable-msg=E1101
        self.dummy.execute()


class AssemblyTestCase(unittest.TestCase):

    def setUp(self):
        """
        top
            comp1
            nested
                comp1
            comp2
            comp3
        """
        top = self.asm = set_as_top(Assembly())
        top.add('comp1', DummyComp())
        nested = top.add('nested', Assembly())
        nested.add('comp1', DummyComp())
        for name in ['comp2', 'comp3']:
            top.add(name, DummyComp())
            
        # driver process definition
        top.driver.workflow.add([top.comp1,top.nested,top.comp2,top.comp3])
        nested.driver.workflow.add([nested.comp1])
        
    def test_lazy_eval(self):
        top = set_as_top(Assembly())
        comp1 = top.add('comp1', Multiplier())
        comp2 = top.add('comp2', Multiplier())
        
        top.driver.workflow.add([comp1, comp2])
        
        top.comp1.mult = 2.0
        top.comp2.mult = 4.0
        top.connect('comp1.rval_out', 'comp2.rval_in')
        top.comp1.rval_in = 5.0
        top.run()
        self.assertEqual(top.get('comp1.rval_out'), 10.)
        self.assertEqual(top.get('comp2.rval_in'), 10.)
        self.assertEqual(top.get('comp2.rval_out'), 40.)
        self.assertEqual(top.comp1.run_count, 1)
        self.assertEqual(top.comp2.run_count, 1)
        
        # now change an input (mult) on comp2. This should only 
        # cause comp2 to execute when we run next time.
        top.set('comp2.mult', 3.0)
        top.run()
        self.assertEqual(top.get('comp1.rval_out'), 10.)
        self.assertEqual(top.get('comp2.rval_in'), 10.)
        self.assertEqual(top.get('comp2.rval_out'), 30.)
        self.assertEqual(top.comp1.run_count, 1)
        self.assertEqual(top.comp2.run_count, 2)
        
     
    def test_data_passing(self):
        comp1 = self.asm.comp1
        comp2 = self.asm.comp2
        self.asm.connect('comp1.rout','comp2.r')
        self.asm.connect('comp1.sout','comp2.s')
        self.asm.comp1.r = 3.0
        self.asm.comp1.s = 'once upon a time'
        self.assertEqual(comp1.get('r'), 3.0)
        self.assertEqual(comp1.get('s'), 'once upon a time')
        self.assertEqual(comp1.r, 3.0)
        self.assertEqual(comp1.s, 'once upon a time')
        
        # also, test that we can't do a direct set of a connected input
        oldval = self.asm.comp2.r
        try:
            self.asm.comp2.r = 44
        except TraitError, err:
            self.assertEqual(str(err), "comp2: 'r' is already connected to source 'comp1.rout'"+
                                       " and cannot be directly set")
        else:
            self.fail("Expected a TraitError when setting a connected input")
        
        # verify that old value of connected input hasn't changed
        self.assertEqual(oldval, self.asm.comp2.r)
        
        self.asm.run()
        
        self.assertEqual(comp1.get('rout'), 4.5)
        self.assertEqual(comp1.get('sout'), 'emit a nopu ecno')
        self.assertEqual(comp1.rout, 4.5)
        self.assertEqual(comp1.sout, 'emit a nopu ecno')
        self.assertEqual(comp2.get('r'), 4.5)
        self.assertEqual(comp2.get('rout'), 6.75)
        self.assertEqual(comp2.r, 4.5)
        self.assertEqual(comp2.rout, 6.75)
        self.assertEqual(comp2.s, 'emit a nopu ecno')
        self.assertEqual(comp2.sout, 'once upon a time')
        
        # now test removal of the error callback when a connected input is disconnected
        self.asm.disconnect('comp1.rout','comp2.r')
        self.asm.comp2.r = 33
        self.assertEqual(33, self.asm.comp2.r)
        
    def test_direct_set_of_connected_input(self):
        comp1 = self.asm.comp1
        comp2 = self.asm.comp2
        self.asm.connect('comp1.rout','comp2.r')
        self.asm.connect('comp1.sout','comp2.s')
        
        # test that we can't do a direct set of a connected input
        oldval = self.asm.comp2.r
        try:
            self.asm.comp2.r = 44
        except TraitError, err:
            self.assertEqual(str(err), "comp2: 'r' is already connected to source 'comp1.rout'"+
                                       " and cannot be directly set")
        else:
            self.fail("Expected a TraitError when setting a connected input")
        
        # verify that old value of connected input hasn't changed
        self.assertEqual(oldval, self.asm.comp2.r)
        
        # now test removal of the error callback when a connected input is disconnected
        self.asm.disconnect('comp1.rout','comp2.r')
        self.asm.comp2.r = 33
        self.assertEqual(33, self.asm.comp2.r)

    def test_connect_containers(self):
        self.asm.set('comp1.dummy_in.rval_in', 75.4)
        self.asm.connect('comp1.dummy_out','comp2.dummy_in')
        self.asm.run()
        self.assertEqual(self.asm.get('comp2.dummy_in.rval_in'), 75.4)
        self.assertEqual(self.asm.get('comp2.dummy_in.rval_out'), 75.4*1.5)
        
    def test_create_passthrough(self):
        self.asm.set('comp3.r', 75.4)
        self.asm.create_passthrough('comp3.rout')
        self.assertEqual(self.asm.get('comp3.r'), 75.4)
        self.assertEqual(self.asm.get('rout'), 0.0)
        self.asm.run()
        self.assertEqual(self.asm.get('comp3.rout'), 75.4*1.5)
        self.assertEqual(self.asm.get('rout'), 75.4*1.5)
        
    def test_passthrough_nested(self):
        self.asm.set('comp1.r', 8.)
        self.asm.nested.create_passthrough('comp1.r')
        self.asm.nested.create_passthrough('comp1.rout', 'foobar')
        self.asm.connect('comp1.rout', 'nested.r')
        self.asm.connect('nested.foobar','comp2.r')
        self.asm.run()
        self.assertEqual(self.asm.get('comp1.rout'), 12.)
        self.assertEqual(self.asm.get('comp2.rout'), 27.)
                
    def test_create_passthrough_alias(self):
        self.asm.nested.set('comp1.r', 75.4)
        self.asm.nested.create_passthrough('comp1.r','foobar')
        self.assertEqual(self.asm.nested.get('foobar'), 75.4)
        self.asm.run()
        self.assertEqual(self.asm.nested.get('foobar'), 75.4)
        
    def test_passthrough_already_connected(self):
        self.asm.connect('comp1.rout','comp2.r')
        self.asm.connect('comp1.sout','comp2.s')
        # this should fail since we're creating a second connection
        # to an input
        try:
            self.asm.create_passthrough('comp2.r')
        except RuntimeError, err:
            self.assertEqual(str(err), ': comp2.r is already connected')
        else:
            self.fail('RuntimeError expected')
        self.asm.set('comp1.s', 'some new string')
        # this one should be OK since outputs can have multiple connections
        self.asm.create_passthrough('comp1.sout')
        self.asm.run()
        self.assertEqual(self.asm.get('sout'), 'some new string'[::-1])
        
    def test_container_passthrough(self):
        self.asm.set('comp1.dummy_out.rval_in', 75.4)
        self.asm.create_passthrough('comp1.dummy_out','dummy_out_passthrough')
        self.asm.run()
        self.assertEqual(self.asm.get('dummy_out_passthrough.rval_out'), 75.4*1.5)

#    def test_discon_reconnect_passthrough(self):
#        self.fail('unfinished test')
        
    def test_invalid_connect(self):
        try:
            self.asm.connect('comp1.rout','comp2.rout')
        except RuntimeError, err:
            self.assertEqual(': comp2.rout must be an input variable',
                             str(err))
        else:
            self.fail('exception expected')
        try:
            self.asm.connect('comp1.r','comp2.rout')
        except RuntimeError, err:
            self.assertEqual(': comp1.r must be an output variable',
                             str(err))
        else:
            self.fail('RuntimeError expected')
            
    def test_self_connect(self):
        try:
            self.asm.connect('comp1.rout','comp1.r')
        except Exception, err:
            self.assertEqual(': Cannot connect comp1.rout to comp1.r. Both are on same component.',
                             str(err))
        else:
            self.fail('exception expected')
     
    def test_attribute_link(self):
        try:
            self.asm.connect('comp1.rout.units','comp2.s')
        except NameError, err:
            self.assertEqual(str(err), 
                    "comp1: Cannot locate trait named 'rout.units'")
        else:
            self.fail('NameError expected')
        
    def test_value_link(self):
        try:
            self.asm.connect('comp1.rout.value','comp2.r2')
        except NameError, err:
            self.assertEqual(str(err), 
                    "comp1: Cannot locate trait named 'rout.value'")
        else:
            self.fail('NameError expected')
        
     
    def test_circular_dependency(self):
        self.asm.connect('comp1.rout','comp2.r')
        try:
            self.asm.connect('comp2.rout','comp1.r')
        except RuntimeError, err:
            self.assertEqual("circular dependency (['comp2', 'comp1']) would be created by"+
                             " connecting comp2.rout to comp1.r", str(err))
        else:
            self.fail('RuntimeError expected')
            
    def test_disconnect(self):
        # first, run connected
        comp2 = self.asm.get('comp2')
        self.asm.connect('comp1.rout', 'comp2.r')
        self.asm.run()
        self.assertEqual(comp2.r, 1.5)
        self.asm.comp1.r = 3.0
        self.asm.run()
        self.assertEqual(comp2.r, 4.5)
        
        # now disconnect
        self.asm.comp1.r = 6.0
        self.asm.disconnect('comp2.r')
        self.asm.run()
        self.assertEqual(comp2.r, 4.5)
        
        # now reconnect
        self.asm.connect('comp1.rout','comp2.r')
        self.asm.run()
        self.assertEqual(comp2.r, 9.0)
        
    def test_input_passthrough_to_2_inputs(self):
        asm = set_as_top(Assembly())
        asm.add('nested', Assembly())
        comp1 = asm.nested.add('comp1', Simple())
        comp2 = asm.nested.add('comp2', Simple())
        
        asm.driver.workflow.add(asm.nested)
        asm.nested.driver.workflow.add([comp1, comp2])
        
        asm.nested.create_passthrough('comp1.a') 
        asm.nested.connect('a', 'comp2.b') 
        self.assertEqual(asm.nested.comp1.a, 4.)
        self.assertEqual(asm.nested.comp2.b, 5.)
        asm.nested.a = 0.5
        # until we run, the values of comp1.a and comp2.b won't change
        self.assertEqual(asm.nested.comp1.a, 4.)
        self.assertEqual(asm.nested.comp2.b, 5.)
        self.assertEqual(asm.nested.comp2.get_valid('b'), False)
        asm.run()
        self.assertEqual(asm.nested.comp1.a, 0.5)
        self.assertEqual(asm.nested.comp2.b, 0.5)
        self.assertEqual(asm.nested.comp1.get_valid('a'), True)
        self.assertEqual(asm.nested.comp2.get_valid('b'), True)
        asm.nested.a = 999.
        self.assertEqual(asm.nested.comp1.get_valid('a'), False)
        self.assertEqual(asm.nested.comp2.get_valid('b'), False)
        self.assertEqual(asm.nested.comp1.a, 0.5)
        self.assertEqual(asm.nested.comp2.b, 0.5)
        asm.run()
        self.assertEqual(asm.nested.comp1.a, 999.)
        self.assertEqual(asm.nested.comp2.b, 999.)
        
    def test_connect_2_outs_to_passthrough(self):
        asm = set_as_top(Assembly())
        asm.add('nested', Assembly())
        asm.nested.add('comp1', Simple())
        asm.nested.add('comp2', Simple())
        asm.nested.create_passthrough('comp1.c')
        try:
            asm.nested.connect('comp2.d', 'c')
        except RuntimeError, err:
            self.assertEqual(str(err), 'nested: c is already connected')
        else:
            self.fail('RuntimeError expected')
        
 
    def test_discon_not_connected(self):
        self.asm.connect('comp1.rout','comp2.r')
        
        # disconnecting something that isn't connected is ok and shouldn't
        # raise an exception
        self.asm.disconnect('comp2.s')

    def test_listcon_with_deleted_objs(self):
        self.asm.add('comp3', DummyComp())
        self.asm.connect('comp1.rout', 'comp2.r')
        self.asm.connect('comp3.sout', 'comp2.s')
        conns = self.asm.list_connections()
        self.assertEqual(conns, [('comp1.rout', 'comp2.r'),
                                 ('comp3.sout', 'comp2.s')])
        self.asm.remove('comp3')
        conns = self.asm.list_connections()
        self.assertEqual(conns, [('comp1.rout', 'comp2.r')])
        self.asm.run()
        
        
if __name__ == "__main__":
    unittest.main()


