import unittest

import openmdao.main.factorymanager as factorymanager
from openmdao.main.component import Component, RUN_OK
from openmdao.main.assembly import Assembly
from openmdao.main.importfactory import ImportFactory
from openmdao.main.variable import INPUT,OUTPUT
from openmdao.main.containervar import ContainerVariable
from openmdao.main.float import Float
from openmdao.main.string import String

factorymanager.register_factory(ImportFactory())

class DummyComp(Component):
    def __init__(self, name, nest=False):
        Component.__init__(self, name)
        self.r = 1.0
        self.r2 = -1.0
        self.rout = 0.0
        self.r2out = 0.0
        self.s = 'a string'
        self.sout = ''
        
        Float('r', self, INPUT, units='cm')
        Float('r2', self, INPUT, units='cm/s')
        String('s', self, INPUT)
        
        Float('rout', self, OUTPUT, units='cm')
        Float('r2out', self, OUTPUT, units='cm/sec')
        String('sout', self, OUTPUT)
        
        # hack a quick way to get nested containers for testing
        if nest is True:
            self.add_child(DummyComp('dummy'))
            ContainerVariable('dummy_in', self, INPUT, ref_name='dummy')
            ContainerVariable('dummy_out', self, OUTPUT, ref_name='dummy')
        
    def execute(self):
        self.rout = self.r * 1.5
        self.r2out = self.r2 + 10.0
        self.sout = self.s[::-1]
        return RUN_OK


class ContainerTestCase(unittest.TestCase):

    def setUp(self):
        """this setup function will be called before each test in this class"""
        self.asm = Assembly('top',None)
        dc = DummyComp('comp1',True)
        self.asm.add_child(dc)
        self.asm.workflow.add_node(dc)
        dc2 = DummyComp('comp2',True)
        self.asm.add_child(dc2)
        self.asm.workflow.add_node(dc2)
        
    
    def tearDown(self):
        """this teardown function will be called after each test in this class"""
        pass

    def test_data_passing(self):
        comp1 = self.asm.get('comp1')
        comp2 = self.asm.get('comp2')
        self.asm.connect('comp1.rout','comp2.r')
        self.asm.connect('comp1.sout','comp2.s')
        self.asm.set('comp1.r', 3.0)
        self.asm.set('comp1.s', 'once upon a time')
        self.assertEqual(comp1.get('r.value'), 3.0)
        self.assertEqual(comp1.get('s.value'), 'once upon a time')
        self.assertEqual(comp1.r, 3.0)
        self.assertEqual(comp1.s, 'once upon a time')
        
        self.asm.run()
        
        self.assertEqual(comp1.get('rout.value'), 4.5)
        self.assertEqual(comp1.get('sout.value'), 'emit a nopu ecno')       
        self.assertEqual(comp1.rout, 4.5)
        self.assertEqual(comp1.sout, 'emit a nopu ecno')
        self.assertEqual(comp2.get('r.value'), 4.5)
        self.assertEqual(comp2.get('rout.value'), 6.75)
        self.assertEqual(comp2.r, 4.5)
        self.assertEqual(comp2.rout, 6.75)
        self.assertEqual(comp2.s, 'emit a nopu ecno')
        self.assertEqual(comp2.sout, 'once upon a time')
        

    def test_connect_containers(self):
        dum1 = self.asm.get('comp1.dummy_out')
        dum1.set('r',75.4)
        self.asm.connect('comp1.dummy_out','comp2.dummy_in')
        self.asm.run()
        self.assertEqual(self.asm.get('comp2.dummy_in.r.value'),75.4)
        
    def test_invalid_connect(self):
        try:
            self.asm.connect('comp1.rout','comp2.rout')
        except RuntimeError, err:
            self.assertEqual('top.comp2.rout must be an INPUT variable',str(err))
        else:
            self.fail('exception expected')
        try:
            self.asm.connect('comp1.r','comp2.rout')
        except RuntimeError, err:
            self.assertEqual('top.comp1.r must be an OUTPUT variable',str(err))
        else:
            self.fail('exception expected')
            
    def test_self_connect(self):
        try:
            self.asm.connect('comp1.rout','comp1.r')
        except RuntimeError, err:
            self.assertEqual('Cannot connect a component (comp1) to itself',str(err))
        else:
            self.fail('exception expected')
     
    def test_attribute_link(self):
        self.asm.connect('comp1.rout.units','comp2.s')
        self.asm.run()
        comp2 = self.asm.get('comp2')
        self.assertEqual(comp2.s, 'cm')
        
    def test_value_link(self):
        self.asm.connect('comp1.rout.value','comp2.r')
        self.asm.run()
        comp2 = self.asm.get('comp2')
        self.assertEqual(comp2.r, 1.5)

    def test_value_bypass(self):
        try:
            self.asm.connect('comp1.rout.value','comp2.r2')
        except TypeError, err:
            self.assertEqual('top.comp1.rout units (cm) are incompatible'+
                             ' with units (cm/s) of top.comp2.r2',str(err))
        else:
            self.fail('exception expected')
        
    def test_attribute_input_link(self):
        try:
            self.asm.connect('comp1.rout','comp1.r.units')
        except RuntimeError, err:
            self.assertEqual('direct linking to attributes within an INPUT Variable is illegal',
                             str(err))
        else:
            self.fail('exception expected')
            
    def test_attribute_type_mismatch(self):
        try:
            self.asm.connect('comp1.rout.units', 'comp2.r')
        except ValueError, err:
            self.assertEqual("top.comp2.r: assignment to incompatible type <type 'str'>",
                             str(err))
        else:
            self.fail('exception expected')
     
    def test_circular_dependency(self):
        self.asm.connect('comp1.rout','comp2.r')
        try:
            self.asm.connect('comp2.rout','comp1.r')
        except RuntimeError, err:
            self.assertEqual('Circular dependency would be created by'+
                             ' connecting comp1.r to comp2.rout',str(err))
        else:
            self.fail('exception expected')

            
    def test_disconnect(self):
        # first, run connected
        comp2 = self.asm.get('comp2')
        self.asm.connect('comp1.rout','comp2.r')
        self.asm.run()
        self.assertEqual(comp2.r, 1.5)
        self.asm.set('comp1.r', 3.0)
        self.asm.run()
        self.assertEqual(comp2.r, 4.5)
        
        # now disconnect
        self.asm.set('comp1.r', 6.0)
        self.asm.disconnect('comp2.r')
        self.asm.run()
        self.assertEqual(comp2.r, 4.5)
        
        # now reconnect
        self.asm.connect('comp1.rout','comp2.r')
        self.asm.run()
        self.assertEqual(comp2.r, 9.0)
 
    def test_discon_not_connected(self):
        comp2 = self.asm.get('comp2')
        self.asm.connect('comp1.rout','comp2.r')
        try:
            self.asm.disconnect('comp2.s')
        except RuntimeError, err:
            self.assertEqual('comp2.s is not connected',str(err))
        else:
            self.fail('exception expected')

    def test_discon_with_deleted_objs(self):
        self.asm.add_child(DummyComp('comp3'))
        self.asm.add_child(DummyComp('comp4'))
        self.asm.connect('comp1.rout','comp2.r')
        self.asm.connect('comp2.rout.value','comp3.r')
        self.asm.connect('comp3.rout','comp4.r')
        
        self.asm.remove_child('comp3')
        self.asm.disconnect('comp4.r')
        
    def test_listcon_with_deleted_objs(self):
        self.asm.add_child(DummyComp('comp3'))
        self.asm.connect('comp1.rout','comp2.r')
        self.asm.connect('comp3.rout.units','comp2.s')
        conns = self.asm.list_connections(fullpath=True)
        self.assertEqual(conns, [('top.comp1.rout','top.comp2.r'),
                                 ('top.comp3.rout.units','top.comp2.s')])
        conns = self.asm.list_connections(fullpath=False)
        self.assertEqual(conns, [('comp1.rout','comp2.r'),
                                 ('comp3.rout.units','comp2.s')])
        self.asm.remove_child('comp3')
        conns = self.asm.list_connections(fullpath=False)
        self.assertEqual(conns, [('comp1.rout','comp2.r')])
        self.asm.run()
        
        
if __name__ == "__main__":
    unittest.main()

