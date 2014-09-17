# pylint: disable-msg=C0111,C0103

import unittest

from traits.api import Event
from openmdao.main.api import Assembly, Component, Driver, set_as_top
from openmdao.main.container import _get_entry_group
from openmdao.main.driver import GradientOptions

class EventComp(Component):
    doit = Event()

    def __init__(self):
        super(EventComp, self).__init__()
        self.num_doits = 0

    def _doit_fired(self):
        self.num_doits += 1

    def execute(self):
        pass

class A(Component):
    a = GradientOptions()

class DriverTestCase(unittest.TestCase):

    def setUp(self):
        top = self.asm = set_as_top(Assembly())
        top.add('evcomp', EventComp())

        # driver process definition
        top.driver.workflow.add('evcomp')

    def test_add_event(self):
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

    def test_gradient_options(self):
        options = GradientOptions()

        assert(options.get_metadata("directional_fd")["framework_var"])
        assert(options.get_metadata("derivative_direction")["framework_var"])
        assert(options.get_metadata("fd_form")["framework_var"])
        assert(options.get_metadata("fd_step")["framework_var"])
        assert(options.get_metadata("fd_step_type")["framework_var"])
        assert(options.get_metadata("force_fd")["framework_var"])
        assert(options.get_metadata("lin_solver")["framework_var"])
        assert(options.get_metadata("atol")["framework_var"])
        assert(options.get_metadata("rtol")["framework_var"])
        assert(options.get_metadata("maxiter")["framework_var"])

        assert(Driver().get_metadata("gradient_options")["framework_var"])


if __name__ == "__main__":
    unittest.main()


