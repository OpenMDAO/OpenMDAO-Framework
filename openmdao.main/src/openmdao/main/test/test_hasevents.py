# pylint: disable-msg=C0111,C0103

import unittest

from openmdao.main.api import Assembly, Component, Driver, set_as_top
from openmdao.main.datatypes.api import Int, Event
from openmdao.util.decorators import add_delegate
from openmdao.main.hasevents import HasEvents

@add_delegate(HasEvents)
class MyDriver(Driver):
    def start_iteration(self):
        self.iter_count = 0
        
    def post_iteration(self):
        self.iter_count += 1
        
    def continue_iteration(self):
        return self.iter_count < 3


class MyEvComp(Component):
    doit = Event(desc='Do It!')
    doit2 = Event(desc='Do It Again!')
    doit_count = Int(0, iotype='out')
    doit2_count = Int(0, iotype='out')
    some_int = Int(0, iotype='in')

    def _doit_fired(self):
        self.doit_count += 1
        
    def _doit2_fired(self):
        self.doit2_count += 1
        
    def execute(self):
        pass

class HasEventsTestCase(unittest.TestCase):

    def setUp(self):
        self.asm = set_as_top(Assembly())
        self.asm.add('driver', MyDriver())
        self.asm.add('comp1', MyEvComp())
        self.asm.driver.workflow.add('comp1')
        
    def test_event(self):
        self.asm.run()
        self.assertEqual(self.asm.comp1.exec_count, 3)
        self.assertEqual(self.asm.comp1.doit_count, 0)
        self.assertEqual(self.asm.comp1.doit2_count, 0)
        
        self.asm.comp1.exec_count = 0
        
        self.asm.driver.add_event('comp1.doit')
        self.asm.run()
        self.assertEqual(self.asm.comp1.exec_count, 3)
        self.assertEqual(self.asm.comp1.doit_count, 3)
        self.assertEqual(self.asm.comp1.doit2_count, 0)
        
        self.asm.driver.set_events()
        self.assertEqual(self.asm.comp1.doit_count, 4)
        self.assertEqual(self.asm.comp1.doit2_count, 0)
        
        self.asm.driver.add_event('comp1.doit2')
        self.asm.driver.set_events()
        self.assertEqual(self.asm.comp1.doit_count, 5)
        self.assertEqual(self.asm.comp1.doit2_count, 1)
        
        try:
            self.asm.driver.add_event('comp1.bogus')
        except AttributeError as err:
            self.assertEqual(str(err), "driver: Can't add event 'comp1.bogus' because it doesn't exist")
        else:
            self.fail('Exception expected')
            
        try:
            self.asm.driver.add_event('comp1.some_int')
        except TypeError as err:
            self.assertEqual(str(err), "driver: 'comp1.some_int' is not an event")
        else:
            self.fail('TypeError expected')
            
        try:
            self.asm.driver.remove_event('comp1.bogus')
        except AttributeError as err:
            self.assertEqual(str(err), "driver: Trying to remove event 'comp1.bogus' that is not in the driver.")
        else:
            self.fail('AttributeError expected')
            
        events = self.asm.driver.get_events()
        self.assertEqual(events, ['comp1.doit', 'comp1.doit2'])
        
        self.asm.driver.remove_event('comp1.doit')
        events = self.asm.driver.get_events()
        self.assertEqual(events, ['comp1.doit2'])

        self.asm.driver.set_events()
        self.assertEqual(self.asm.comp1.doit_count, 5)
        self.assertEqual(self.asm.comp1.doit2_count, 2)
        
        self.asm.driver.clear_events()
        events = self.asm.driver.get_events()
        self.assertEqual(events, [])


if __name__ == "__main__":
    unittest.main()


