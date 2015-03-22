"""
Test run/stop aspects of a simple workflow.
"""

import unittest

from openmdao.main.api import Assembly, Component, set_as_top, Driver
from openmdao.main.case import Case
from openmdao.main.exceptions import RunStopped
from openmdao.main.datatypes.api import Int, Bool, Float
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasobjective import HasObjective
from openmdao.main.test.test_assembly import Simple
from openmdao.main.interfaces import implements, ICaseRecorder
from openmdao.util.decorators import add_delegate

from openmdao.main.case import CaseTreeNode

# pylint: disable=E1101,E1103
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

        self.driver.workflow.add(['comp_a', 'comp_b', 'comp_c'])

        self.connect('comp_a.total_executions', 'comp_b.dummy_input')
        self.connect('comp_b.total_executions', 'comp_c.dummy_input')

class LazyModel(Assembly):

    def configure(self):
        self.add('driver', NTimes(1))

        self.add('D2', NTimes(1))

        self.add('C1', Simple())
        self.add('C2', Simple())
        self.add('C3', Simple())
        self.add('C4', Simple())

        # C1 --> C2 --> C3 --> C4
        self.connect('C1.c', 'C2.a')
        self.connect('C2.c', 'C3.a')
        self.connect('C3.c', 'C4.a')

@add_delegate(HasParameters, HasObjective)
class NTimes(Driver):

    def __init__(self, max_iterations):
        super(NTimes, self).__init__()
        self.max_iterations = max_iterations

    def execute(self):
        for i in range(self.max_iterations):
            super(NTimes, self).execute()


@add_delegate(HasParameters, HasObjective)
class CaseDriver(Driver):

    def __init__(self, max_iterations):
        super(CaseDriver, self).__init__()
        self.max_iterations = max_iterations

    def execute(self):
        for i in range(self.max_iterations):
            self.set_parameters([float(i)])
            super(CaseDriver, self).execute()
            self.eval_objective()


class CaseComponent(Component):

    x = Float(iotype='in')
    y = Float(iotype='out')

    def execute(self):
        self.y = self.x


class DumbRecorder(object):
    """Stores cases in a list."""

    implements(ICaseRecorder)

    def __init__(self):
        self.cases = []
        self._name_map = {}

    def startup(self):
        pass

    def register(self, src, inputs, outputs):
        self._name_map[src] = (inputs, outputs)

    def record_constants(self, constants):
        pass

    def record(self, src, inputs, outputs, err, case_uuid, parent_uuid):
        in_names, out_names = self._name_map[src]
        inputs = zip(in_names, inputs)
        outputs = zip(out_names, outputs)
        self.cases.append(Case(inputs, outputs,
                               case_uuid=case_uuid, parent_uuid=parent_uuid))
    def close(self):
        return

    def get_iterator(self):
        return iter(self.cases)


class TestCase(unittest.TestCase):
    """ Test run/stop aspects of a simple workflow. """

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
        except Exception as err:
            self.assertEqual(str(err),
                "'Model' object has no attribute 'foobar'")

    def test_simple(self):
        self.assertEqual(self.model.comp_a.total_executions, 0)
        self.assertEqual(self.model.comp_b.total_executions, 0)
        self.assertEqual(self.model.comp_c.total_executions, 0)

        self.model.run()

        self.assertEqual(self.model.comp_a.total_executions, 1)
        self.assertEqual(self.model.comp_b.total_executions, 1)
        self.assertEqual(self.model.comp_c.total_executions, 1)

        self.model.run()

        self.assertEqual(self.model.comp_a.total_executions, 2)
        self.assertEqual(self.model.comp_b.total_executions, 2)
        self.assertEqual(self.model.comp_c.total_executions, 2)

    def test_run_stop_run(self):
        self.model.comp_b.set_stop = True
        try:
            self.model.run()
        except RunStopped as exc:
            self.assertTrue('Stop requested' in str(exc))
        else:
            self.fail('Expected RunStopped')

        self.assertEqual(self.model.comp_a.total_executions, 1)
        self.assertEqual(self.model.comp_b.total_executions, 1)
        self.assertEqual(self.model.comp_c.total_executions, 0)

        self.model.comp_b.set_stop = False
        self.model.run()
        self.assertEqual(self.model.comp_a.total_executions, 2)
        self.assertEqual(self.model.comp_b.total_executions, 2)
        self.assertEqual(self.model.comp_c.total_executions, 1)

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

        ### TODO: this test needs to move to setup time
        ## Test 4, create a driver recursion loop
        #self.model.add('driver2', Driver())
        #self.model.driver.workflow.add('driver2', check=True)
        #try:
            #self.model.driver2.workflow.add('driver', check=True)
        #except AttributeError, err:
            #msg = "Driver recursion loop detected"
            #self.assertEqual(str(err), msg)
        #else:
            #self.fail('Expected AttributeError')

    def test_casetree(self):
        # Record tree of cases via workflow.
        top = Assembly()
        top.recorders = [DumbRecorder()]

        top.add('driver2', CaseDriver(3))
        top.add('comp2', CaseComponent())
        top.driver2.workflow.add('comp2')
        top.driver2.add_parameter('comp2.x', low=0, high=10)
        top.driver2.add_objective('comp2.y')

        top.add('driver1', CaseDriver(2))
        top.add('comp1', CaseComponent())
        top.driver1.add_parameter('comp1.x', low=0, high=10)
        top.driver1.add_objective('comp1.y')
        top.driver1.workflow.add(['comp1', 'driver2'])

        top.driver.workflow.add('driver1')
        top.run()

        print
        print 'Forest:'
        roots = CaseTreeNode.sort(top.recorders[0].get_iterator())
        for root in roots:
            root.dump(1)

        print
        print 'Iternames:'
        for root in roots:
            for name in root.iternames():
                print '   ', name

        expected = [
            '1',
            '1-driver1.1',
            '1-driver1.1-driver2.1',
            '1-driver1.1-driver2.2',
            '1-driver1.1-driver2.3',
            '1-driver1.2',
            '1-driver1.2-driver2.1',
            '1-driver1.2-driver2.2',
            '1-driver1.2-driver2.3'
        ]
        for i, name in enumerate(roots[0].iternames()):
            self.assertEqual(name, expected[i])

    def test_lazy_auto_top(self):
        # lazy evaluation with auto determination of top level workflow
        top = set_as_top(LazyModel())
        top.driver.add_parameter('C2.b', low=-99, high=99)
        top.driver.add_objective('C3.d')

        top.run()

        self.assertEqual(top.C2.exec_count, 1)
        self.assertEqual(top.C3.exec_count, 1)
        self.assertEqual(top.C1.exec_count, 1)

        # now test strict mode
        try:
            top.check_config(strict=True)
        except Exception as err:
            self.assertEqual(str(err), ": The following components are not in any workflow but are needed by other workflows: ['C1']")
        else:
            self.fail("Exception expected")


    def test_lazy_auto_nested(self):
        # lazy evaluation with auto determination of D2 workflow
        top = set_as_top(LazyModel())
        top.driver.workflow.add(['D2', 'C1'])
        top.D2.add_parameter('C2.b', low=-99, high=99)
        top.D2.add_objective('C3.d')

        top.run()

        self.assertEqual(top.C2.exec_count, 1)
        self.assertEqual(top.C3.exec_count, 1)
        self.assertEqual(top.C1.exec_count, 1)

        # now test strict mode
        try:
            top.check_config(strict=True)
        except Exception as err:
            self.assertEqual(str(err), ": The following components are not in any workflow and WILL NOT EXECUTE: ['C4']")
        else:
            self.fail("Exception expected")

    def test_lazy_manual_top(self):
        # manual top level workflow
        top = set_as_top(LazyModel())
        top.driver.add_parameter('C2.b', low=-99, high=99)
        top.driver.add_objective('C3.d')
        top.driver.workflow.add(['C2', 'C3'])
        top.run()
        self.assertEqual(top.C2.exec_count, 1)
        self.assertEqual(top.C3.exec_count, 1)
        self.assertEqual(top.C1.exec_count, 1)

        # now test strict mode
        try:
            top.check_config(strict=True)
        except Exception as err:
            self.assertEqual(str(err), ": The following components are not in any workflow but are needed by other workflows: ['C1']")
        else:
            self.fail("Exception expected")


if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao.main')
    sys.argv.append('--cover-erase')
    nose.runmodule()
