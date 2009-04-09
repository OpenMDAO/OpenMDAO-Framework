# pylint: disable-msg=C0111,C0103

import unittest

from openmdao.main import Assembly, Component, Float, String
from openmdao.main.variable import INPUT, OUTPUT

class NestedDumb(Component):
    def __init__(self, name):
        super(NestedDumb, self).__init__(name)
        self.rval_in = 4.
        self.rval_out = 7.
        Float('rval_in', self, INPUT, units='cm')
        Float('rval_out', self, OUTPUT, units='cm')

    def execute(self):
        print 'executing %s' % self.get_pathname()
        self.rval_out = self.rval_in * 1.5

class DummyComp(Component):
    def __init__(self, name):
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
        
        # make a nested container with input and output ContainerVars
        self.add_child(NestedDumb('dummy'), private=True)
        self.make_public([('dummy_in','dummy'), 
                          ('dummy_out','dummy',OUTPUT)])
                
    def execute(self):
        print 'executing %s' % self.get_pathname()
        self.rout = self.r * 1.5
        self.r2out = self.r2 + 10.0
        self.sout = self.s[::-1]
        # pylint: disable-msg=E1101
        self.dummy.execute()


class ContainerTestCase(unittest.TestCase):

    def setUp(self):
        """this setup function will be called before each test in this class"""
        self.asm = Assembly('top', None)
        dc = DummyComp('comp1')
        self.asm.add_child(dc)
        self.asm.workflow.add_node(dc)
        dc2 = DummyComp('comp2')
        self.asm.add_child(dc2)
        self.asm.workflow.add_node(dc2)
        dc3 = DummyComp('comp3')  
        self.asm.add_child(dc3)
        self.asm.workflow.add_node(dc3)
        
    
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
        self.asm.set('comp1.dummy_in.rval_in', 75.4)
        self.asm.connect('comp1.dummy_out','comp2.dummy_in')
        self.asm.run()
        self.assertEqual(self.asm.get('comp2.dummy_in.rval_in'), 75.4)
        self.assertEqual(self.asm.get('comp2.dummy_in.rval_out'), 75.4*1.5)
        
    def test_connect_containers_sub(self):
        self.asm.set('comp1.dummy_in.rval_in', 75.4)
        self.asm.connect('comp1.dummy_out.rval_out','comp2.dummy_in.rval_in')
        #from pprint import pprint
        #print '\npre run - comp1\n',pprint(self.asm.comp1.dummy.dump_refs())
        #print '\npre run - comp2\n',pprint(self.asm.comp2.dummy.dump_refs())
        self.asm.run()
        #print '\npost run - comp1\n',pprint(self.asm.comp1.dummy.dump_refs())
        #print '\npost run - comp2\n',pprint(self.asm.comp2.dummy.dump_refs())
        self.assertEqual(self.asm.get('comp2.dummy_in.rval_in'), 75.4*1.5)
        
    def test_create_passthru(self):
        self.asm.set('comp3.r', 75.4)
        self.asm.create_passthru('comp3.rout')
        self.assertEqual(self.asm.get('comp3.r'), 75.4)
        self.assertEqual(self.asm.get('rout'), 0.0)
        self.asm.run()
        self.assertEqual(self.asm.get('comp3.rout'), 75.4*1.5)
        self.assertEqual(self.asm.get('rout'), 75.4*1.5)
        
    def test_create_passthru_alias(self):
        dum1 = self.asm.set('comp1.r', 75.4)
        self.asm.create_passthru('comp1.r','foobar')
        self.assertEqual(self.asm.get('foobar'), 75.4)
        self.asm.run()
        self.assertEqual(self.asm.get('foobar'), 75.4)
        
    #def test_container_passthru(self):
        #self.asm.set('comp1.dummy_out.r', 75.4)
        #self.asm.create_passthru('comp1.dummy_out','dummy_out_passthru')
        #self.asm.run()
        #self.assertEqual(self.asm.get('dummy_out_passthru'), 75.4)
        
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
                    "top: rout.units must be a simple name, not a dotted path")
        else:
            self.fail('NameError expected')
        
    def test_value_link(self):
        try:
            self.asm.connect('comp1.rout.value','comp2.r2')
        except NameError, err:
            self.assertEqual(str(err), 
                        "top: rout.value must be a simple name, not a dotted path")
        else:
            self.fail('NameError expected')
        
     
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
            self.assertEqual('top.comp2.s: not connected', str(err))
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
            self.assertEqual(str(err), 'top.comp4.r: not connected')
        else:            
            self.fail('exception expected')
        
        
    def test_listcon_with_deleted_objs(self):
        self.asm.add_child(DummyComp('comp3'))
        self.asm.connect('comp1.rout', 'comp2.r')
        self.asm.connect('comp3.sout', 'comp2.s')
        conns = self.asm.list_connections()
        self.assertEqual(conns, [('comp3.sout', 'comp2.s'),
                                 ('comp1.rout', 'comp2.r')])
        self.asm.remove_child('comp3')
        conns = self.asm.list_connections()
        self.assertEqual(conns, [('comp1.rout', 'comp2.r')])
        self.asm.run()
        
        
if __name__ == "__main__":
    unittest.main()


