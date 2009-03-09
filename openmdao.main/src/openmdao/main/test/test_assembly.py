# pylint: disable-msg=C0111,C0103

import unittest

import openmdao.main.factorymanager as factorymanager
from openmdao.main.component import Component, RUN_OK
from openmdao.main import Assembly,ImportFactory,Float,String
from openmdao.main.variable import INPUT,OUTPUT

factorymanager.register_factory(ImportFactory())

class DummyComp(Component):
    def __init__(self, name, nest=False):
        super(DummyComp, self).__init__(name)
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
        Float('r2out', self, OUTPUT, units='cm/s')
        String('sout', self, OUTPUT)
        
        # hack a quick way to get nested containers for testing
        if nest is True:
            self.add_child(DummyComp('dummy'), private=True)
            self.make_public([('dummy_in','dummy'), 
                              ('dummy_out','dummy',OUTPUT)])
        
    def execute(self):
        self.rout = self.r * 1.5
        self.r2out = self.r2 + 10.0
        self.sout = self.s[::-1]
        # pylint: disable-msg=E1101
        if hasattr(self, 'dummy'):
            self.dummy.run()
        return RUN_OK


class ContainerTestCase(unittest.TestCase):

    def setUp(self):
        """this setup function will be called before each test in this class"""
        self.asm = Assembly('top', None)
        dc = DummyComp('comp1', True)
        self.asm.add_child(dc)
        self.asm.workflow.add_node(dc)
        dc2 = DummyComp('comp2', True)
        self.asm.add_child(dc2)
        self.asm.workflow.add_node(dc2)
        
    
    def tearDown(self):
        """this teardown function will be called after each test"""
        pass

    def test_data_passing(self):
        comp1 = self.asm.get('comp1.value')
        comp2 = self.asm.get('comp2.value')
        self.asm.connect('comp1.rout','comp2.r')
        self.asm.connect('comp1.sout','comp2.s')
        self.asm.set('comp1.r', 3.0)
        self.asm.set('comp1.s', 'once upon a time')
        self.assertEqual(comp1.get('r'), 3.0)
        self.assertEqual(comp1.get('s'), 'once upon a time')
        self.assertEqual(comp1.r, 3.0)
        self.assertEqual(comp1.s, 'once upon a time')
        
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
        

    def test_connect_containers(self):
        dum1 = self.asm.get('comp1.dummy_out')
        dum1.set('r', 75.4)
        self.asm.connect('comp1.dummy_out','comp2.dummy_in')
        self.asm.run()
        self.assertEqual(self.asm.get('comp2.dummy_in.r'), 75.4)
        
    def test_connect_containers_sub(self):
        dum1 = self.asm.get('comp1.dummy_out')
        dum1.set('r', 75.4)
        self.asm.connect('comp1.dummy_out.rout','comp2.dummy_in.r')
        self.asm.run()
        self.assertEqual(self.asm.get('comp2.dummy_in.r'), 75.4*1.5)
        
    def test_invalid_connect(self):
        try:
            self.asm.connect('comp1.rout','comp2.rout')
        except RuntimeError, err:
            self.assertEqual('top: top.comp2.rout must be an INPUT variable',
                             str(err))
        else:
            self.fail('exception expected')
        try:
            self.asm.connect('comp1.r','comp2.rout')
        except RuntimeError, err:
            self.assertEqual('top: top.comp1.r must be an OUTPUT variable',
                             str(err))
        else:
            self.fail('exception expected')
            
    def test_self_connect(self):
        try:
            self.asm.connect('comp1.rout','comp1.r')
        except RuntimeError, err:
            self.assertEqual(
                'top: Cannot connect a component (comp1) to itself',
                str(err))
        else:
            self.fail('exception expected')
     
    def test_attribute_link(self):
        try:
            self.asm.connect('comp1.rout.units','comp2.s')
        except NameError, err:
            self.assertEqual(str(err), 
                    "top.comp1.rout: 'units' is not a Variable object")
        else:
            self.fail('NameError expected')
        
    def test_value_link(self):
        try:
            self.asm.connect('comp1.rout.value','comp2.r2')
        except NameError, err:
            self.assertEqual(str(err), 
                        "top.comp1.rout: 'value' is not a Variable object")
        else:
            self.fail('exception expected')
        
     
    def test_circular_dependency(self):
        self.asm.connect('comp1.rout','comp2.r')
        try:
            self.asm.connect('comp2.rout','comp1.r')
        except RuntimeError, err:
            self.assertEqual('top: Circular dependency would be created by'+
                             ' connecting comp1.r to comp2.rout', str(err))
        else:
            self.fail('exception expected')

            
    def test_disconnect(self):
        # first, run connected
        comp2 = self.asm.get('comp2')
        self.asm.connect('comp1.rout', 'comp2.r')
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
        self.asm.connect('comp1.rout','comp2.r')
        try:
            self.asm.disconnect('comp2.s')
        except RuntimeError, err:
            self.assertEqual('top: comp2.s is not connected', str(err))
        else:
            self.fail('exception expected')

    def test_discon_with_deleted_objs(self):
        self.asm.add_child(DummyComp('comp3'))
        self.asm.add_child(DummyComp('comp4'))
        self.asm.connect('comp1.rout', 'comp2.r')
        self.asm.connect('comp2.rout', 'comp3.r')
        self.asm.connect('comp3.rout', 'comp4.r')
        
        # this also removes the connection to comp4.r
        self.asm.remove_child('comp3') 
        try:
            self.asm.disconnect('comp4.r')
        except RuntimeError, err:
            self.assertEqual(str(err), 'top: comp4.r is not connected')
        else:            
            self.fail('exception expected')
        
        
    def test_listcon_with_deleted_objs(self):
        self.asm.add_child(DummyComp('comp3'))
        self.asm.connect('comp1.rout', 'comp2.r')
        self.asm.connect('comp3.sout', 'comp2.s')
        conns = self.asm.list_connections(fullpath=True)
        self.assertEqual(conns, [('top.comp3.sout', 'top.comp2.s'),
                                 ('top.comp1.rout', 'top.comp2.r')])
        conns = self.asm.list_connections(fullpath=False)
        self.assertEqual(conns, [('comp3.sout', 'comp2.s'),
                                 ('comp1.rout', 'comp2.r')])
        self.asm.remove_child('comp3')
        conns = self.asm.list_connections(fullpath=False)
        self.assertEqual(conns, [('comp1.rout', 'comp2.r')])
        self.asm.run()
        
        
if __name__ == "__main__":
    unittest.main()

