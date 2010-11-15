# pylint: disable-msg=C0111,C0103

import unittest

from enthought.traits.api import TraitError, Event
from openmdao.main.api import Assembly, Component, Driver, set_as_top
from openmdao.main.container import _get_entry_group

class EventComp(Component):
    doit = Event()
    
    def __init__(self):
        super(EventComp, self).__init__()
        self.num_doits = 0
        
    def _doit_fired(self):
        self.num_doits += 1

    def execute(self):
        pass

class DriverTestCase(unittest.TestCase):

    def setUp(self):
        top = self.asm = set_as_top(Assembly())
        top.add('evcomp', EventComp())
            
        # driver process definition
        top.driver.workflow.add(top.evcomp)
        
    def test_add_event(self):
        self.asm.driver.force_execute = True
        self.asm.evcomp.force_execute = True
        for i in range(3):
            self.asm.run()
            self.assertEqual(self.asm.evcomp.exec_count, i+1)
            self.assertEqual(self.asm.evcomp.num_doits, 0)
        
        self.asm.driver.add_event('evcomp.doit')
        for i in range(3):
            self.asm.run()
            self.assertEqual(self.asm.evcomp.exec_count, i+4)
            self.assertEqual(self.asm.evcomp.num_doits, i+1)
        
    def test_get_entry_group(self):
        self.assertEqual(_get_entry_group(Driver()), 'openmdao.driver')

        
if __name__ == "__main__":
    unittest.main()


