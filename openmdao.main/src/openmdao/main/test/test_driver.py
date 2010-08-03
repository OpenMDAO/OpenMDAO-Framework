# pylint: disable-msg=C0111,C0103

import unittest

from enthought.traits.api import TraitError, Event
from openmdao.main.api import Assembly, Component, set_as_top

class EventComp(Component):
    doit = Event()
    
    def __init__(self):
        super(EventComp, self).__init__()
        self.run_count = 0
        self.num_doits = 0
        
    def _doit_fired(self):
        self.num_doits += 1

    def execute(self):
        self.run_count += 1

class DriverTestCase(unittest.TestCase):

    def setUp(self):
        top = self.asm = set_as_top(Assembly())
        top.add('evcomp', EventComp())
            
        # driver process definition
        top.driver.workflow.add(top.evcomp)
        
    def test_events(self):
        self.asm.driver.force_execute = True
        self.asm.evcomp.force_execute = True
        for i in range(3):
            self.asm.run()
            self.assertEqual(self.asm.evcomp.run_count, i+1)
            self.assertEqual(self.asm.evcomp.num_doits, 0)
        
        self.asm.driver.add_event('evcomp.doit')
        for i in range(3):
            self.asm.run()
            self.assertEqual(self.asm.evcomp.run_count, i+4)
            self.assertEqual(self.asm.evcomp.num_doits, i+1)
        
        
if __name__ == "__main__":
    unittest.main()


