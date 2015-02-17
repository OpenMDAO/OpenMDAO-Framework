# pylint: disable-msg=C0111,C0103

import unittest

from traits.api import Event
from openmdao.main.api import Assembly, Component, Driver, set_as_top, VariableTree
from openmdao.main.container import _get_entry_group
from openmdao.main.datatypes.api import Float, Int, VarTree
from openmdao.main.driver import GradientOptions
from openmdao.main.test.test_derivatives import SimpleDriver

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

class DriverTestCase2(unittest.TestCase):

    def test_get_req_compnames_vartree_param_obj(self):
        # Tests a fix for a bug reported by Rick Damiani

        class PileGeoInputs(VariableTree):
            """Basic Geometric Inputs need to build Legs of Jacket"""

            Lp = Float( units='m', desc='Pile Embedment Length.')

        class MyComp(Component):

            x = Float(0.0, iotype='in')
            y = Float(0.0, iotype='in')

            def execute(self):
                self.y = 2.0*self.x

        class Top(Assembly):

            Pileinputs = VarTree(PileGeoInputs(), iotype='in', desc="Pile Input Data")
            SPIstiffness = VarTree(PileGeoInputs(), iotype='out', desc="Pile Input Data")

            def configure(self):

                self.connect('Pileinputs.Lp', 'SPIstiffness.Lp')
                self.disconnect('Pileinputs.Lp', 'SPIstiffness.Lp')

                self.add('comp', MyComp())
                self.driver.workflow.add('comp')
                self.connect('Pileinputs.Lp', 'comp.x')


        top = set_as_top(Top())
        top.replace('driver', SimpleDriver())
        top.driver.add_parameter('Pileinputs.Lp', low=-100, high=100)
        top.driver.add_objective('SPIstiffness.Lp + comp.y')

        top._setup()
        comps = top.driver._get_required_compnames()
        self.assertTrue(len(comps) == 2)
        self.assertTrue('comp' in comps)


if __name__ == "__main__":
    unittest.main()


