"""
Test run/step/stop aspects of a simple workflow.
"""

import unittest

from openmdao.main.api import Assembly, Component, set_as_top, Driver
from openmdao.main.exceptions import RunStopped
from openmdao.lib.datatypes.api import Int, Bool

# pylint: disable-msg=E1101,E1103
# "Instance of <class> has no <attr> member"

dummyval = 1


class TestComponent(Component):
    """
    Component which tracks it's total executions
    and can request that the run be stopped.
    """

    dummy_input = Int(0, iotype='in')
    set_stop = Bool(False, iotype='in')
    total_executions = Int(0, iotype='out')

    def execute(self):
        self.total_executions += 1
        if self.set_stop:
            self.parent.driver.stop()



class Model(Assembly):
    """ Just a simple three-component workflow. """

    def configure(self):
        self.add('comp_a', TestComponent())
        self.add('comp_b', TestComponent())
        self.add('comp_c', TestComponent())
        
        self.driver.workflow.add(['comp_a','comp_b','comp_c'])

        self.connect('comp_a.total_executions', 'comp_b.dummy_input')
        self.connect('comp_b.total_executions', 'comp_c.dummy_input')

    def rerun(self):
        """ Called to force the model to run. """
        global dummyval
        dummyval += 1  # force dummyval to be different than last time so
                       # comp1 will be invalidated
        self.comp_a.set('dummy_input', dummyval)
        self.run()


class TestCase(unittest.TestCase):
    """ Test run/step/stop aspects of a simple workflow. """

    def setUp(self):
        """ Called before each test. """
        self.model = set_as_top(Model())

    def tearDown(self):
        """ Called after each test. """
        pass

    def test_bad_workflow_reference(self):
        self.model.driver.workflow.add('foobar')
        try:
            self.model.run()
        except AttributeError as err:
            self.assertEqual(str(err), 
                "driver: 'Model' object has no attribute 'foobar'")

    def test_simple(self):
        self.assertEqual(self.model.comp_a.total_executions, 0)
        self.assertEqual(self.model.comp_b.total_executions, 0)
        self.assertEqual(self.model.comp_c.total_executions, 0)

        self.model.run()

        self.assertEqual(self.model.comp_a.total_executions, 1)
        self.assertEqual(self.model.comp_b.total_executions, 1)
        self.assertEqual(self.model.comp_c.total_executions, 1)

        self.model.rerun()

        self.assertEqual(self.model.comp_a.total_executions, 2)
        self.assertEqual(self.model.comp_b.total_executions, 2)
        self.assertEqual(self.model.comp_c.total_executions, 2)

    def test_stepping(self):
        try:
            self.model.step()
        except RunStopped, exc:
            self.assertEqual(str(exc), 'Step complete')
        else:
            self.fail('Expected RunStopped')

        self.assertEqual(self.model.comp_a.total_executions, 1)
        self.assertEqual(self.model.comp_b.total_executions, 0)
        self.assertEqual(self.model.comp_c.total_executions, 0)

        try:
            self.model.step()
        except RunStopped, exc:
            self.assertEqual(str(exc), 'Step complete')
        else:
            self.fail('Expected RunStopped')

        self.assertEqual(self.model.comp_a.total_executions, 1)
        self.assertEqual(self.model.comp_b.total_executions, 1)
        self.assertEqual(self.model.comp_c.total_executions, 0)

        try:
            self.model.step()
        except RunStopped, exc:
            self.assertEqual(str(exc), 'Step complete')
        else:
            self.fail('Expected RunStopped')

        self.assertEqual(self.model.comp_a.total_executions, 1)
        self.assertEqual(self.model.comp_b.total_executions, 1)
        self.assertEqual(self.model.comp_c.total_executions, 1)

        try:
            self.model.step()
        except StopIteration, exc:
            self.assertEqual(str(exc), '')
        else:
            self.fail('Expected StopIteration')

    def test_run_stop_run(self):
        self.model.comp_b.set_stop = True
        try:
            self.model.run()
        except RunStopped, exc:
            self.assertEqual(str(exc), 'Stop requested')
        else:
            self.fail('Expected RunStopped')

        self.assertEqual(self.model.comp_a.total_executions, 1)
        self.assertEqual(self.model.comp_b.total_executions, 1)
        self.assertEqual(self.model.comp_c.total_executions, 0)

        self.model.comp_b.set_stop = False
        self.model.rerun()
        self.assertEqual(self.model.comp_a.total_executions, 2)
        self.assertEqual(self.model.comp_b.total_executions, 2)
        self.assertEqual(self.model.comp_c.total_executions, 1)

    def test_run_stop_step(self):
        self.model.comp_b.set_stop = True
        try:
            self.model.run()
        except RunStopped, exc:
            self.assertEqual(str(exc), 'Stop requested')
        else:
            self.fail('Expected RunStopped')

        self.assertEqual(self.model.comp_a.total_executions, 1)
        self.assertEqual(self.model.comp_b.total_executions, 1)
        self.assertEqual(self.model.comp_c.total_executions, 0)

        try:
            self.model.step()
        except RunStopped, exc:
            self.assertEqual(str(exc), 'Step complete')
        else:
            self.fail('Expected RunStopped')

        self.assertEqual(self.model.comp_a.total_executions, 1)
        self.assertEqual(self.model.comp_b.total_executions, 1)
        self.assertEqual(self.model.comp_c.total_executions, 1)

        try:
            self.model.step()
        except StopIteration, exc:
            self.assertEqual(str(exc), '')
        else:
            self.fail('Expected StopIteration')
            
    def test_checks(self):
        # Tests out the validity checks.
        
        # Test 1, add a driver to its own workflow
        try:
            self.model.driver.workflow.add('driver', check=True)
        except AttributeError, err:
            msg = 'You cannot add a driver to its own workflow'
            self.assertEqual(str(err), msg)
        else:
            self.fail('Expected AttributeError')
            
        # Test 2, add a comp that is out of scope.
        self.model.add('sub', Assembly())
        self.model.sub.add('comp', Component())
        try:
            self.model.driver.workflow.add('sub.comp', check=True)
        except AttributeError, err:
            msg = "Component 'sub.comp' is not in the scope of the top assembly."
            self.assertEqual(str(err), msg)
        else:
            self.fail('Expected AttributeError')
            
        # Test 3, add a comp that does not exist
        try:
            self.model.driver.workflow.add('stuff', check=True)
        except AttributeError, err:
            msg = "Component 'stuff' does not exist in the top assembly."
            self.assertEqual(str(err), msg)
        else:
            self.fail('Expected AttributeError')
            
        # Test 4, create a driver recursion loop
        self.model.add('driver2', Driver())
        self.model.driver.workflow.add('driver2', check=True)
        try:
            self.model.driver2.workflow.add('driver', check=True)
        except AttributeError, err:
            msg = "Driver recursion loop detected"
            self.assertEqual(str(err), msg)
        else:
            self.fail('Expected AttributeError')


if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()

