"""
Test the FixedPointIterator component
"""

import unittest

# pylint: disable=F0401,E0611
from openmdao.lib.drivers.iterate import FixedPointIterator, IterateUntil
from openmdao.lib.optproblems.sellar import Discipline1_WithDerivatives, \
                                            Discipline2_WithDerivatives
from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.main.datatypes.api import Array, Float
from openmdao.util.testutil import assert_rel_error

class Simple1(Component):
    """ Testing convergence failure"""

    invar = Float(0, iotype='in')
    extra_invar = Float(0, iotype='in')
    outvar = Float(1, iotype='out')

    def execute(self):
        """Will never converge"""
        self.outvar = self.invar + 1


class Simple2(Component):
    """ Testing convergence success"""

    invar = Float(1, iotype='in')
    outvar = Float(1, iotype='out')

    def execute(self):
        """Will always converge"""
        self.outvar = self.invar


class Simple3(Component):
    """ Testing convergence tolerance"""

    invar = Float(1, iotype='in')
    outvar = Float(1.01, iotype='out')

    def execute(self):
        """Will converge if tolerance is loose enough"""
        self.outvar = self.invar + .01


class Simple4(Component):
    """Testing for iteration counting and stop conditions"""

    invar = Float(1, iotype="in")
    outvar = Float(0, iotype="out")

    def execute(self):
        self.outvar = self.outvar + self.invar

class Div10(Component):
    """Testing for iteration counting and stop conditions"""

    invar = Float(1., iotype="in")
    outvar = Float(0., iotype="out")

    def execute(self):
        self.outvar = self.invar / 10.
        print "invar, outvar = %s, %s" % (self.invar, self.outvar)


class Multi(Component):
    """Testing for iteration counting and stop conditions"""

    in1 = Float(1.0, iotype="in")
    in2 = Float(1.0, iotype="in")
    out1 = Float(0, iotype="out")
    out2 = Float(0, iotype="out")

    def execute(self):
        self.out1 = self.in1/10.0
        self.out2 = self.in2/15.0


class ArrayMulti(Component):
    """Testing for iteration counting and stop conditions"""

    arr = Array([1., 1.], iotype="in")
    out = Array([0., 0.], iotype="out")

    def execute(self):
        self.out = self.arr/10.0

class MultiArrayMulti(Component):
    """Testing for iteration counting and stop conditions"""

    arr1 = Array([1., 1.], iotype="in")
    arr2 = Array([1., 1.], iotype="in")
    out1 = Array([0., 0.], iotype="out")
    out2 = Array([0., 0.], iotype="out")

    def execute(self):
        self.out1 = self.arr1/10.0
        self.out2 = self.arr2/10.0

class MixedScalarArrayMulti(Component):
    """Testing for iteration counting and stop conditions"""

    arr1 = Array([1., 1.], iotype="in")
    in2 = Float(1.0, iotype="in")
    out1 = Array([0., 0.], iotype="out")
    out2 = Float(0, iotype="out")

    def execute(self):
        self.out1 = self.arr1/10.0
        self.out2 = self.in2/10.0



class FixedPointIteratorTestCase(unittest.TestCase):
    """test FixedPointIterator component"""

    def setUp(self):
        self.top = set_as_top(Assembly())

    def tearDown(self):
        self.top = None

    def test_success(self):
        self.top.add("driver", FixedPointIterator())
        self.top.add("simple", Simple2())
        self.top.driver.workflow.add('simple')

        self.top.driver.add_constraint('simple.outvar = simple.invar')
        self.top.driver.add_parameter('simple.invar')
        self.top.run()

        self.assertAlmostEqual(self.top.simple.invar,
                               self.top.simple.outvar, places=6)
        self.assertEqual(self.top.driver.current_iteration, 0)

    def test_badcon(self):
        self.top.add("driver", FixedPointIterator())
        self.top.add("simple", Simple2())
        self.top.driver.workflow.add('simple')

        self.top.driver.add_constraint('simple.invar - simple.outvar = 0')
        self.top.driver.add_parameter('simple.invar')

        try:
            self.top.run()
        except RuntimeError, err:
            msg = "driver: Please specify constraints in the form 'A=B'"
            msg += ': simple.invar - simple.outvar = 0'
            self.assertEqual(str(err), msg)
        else:
            self.fail('RuntimeError expected')

        self.top.driver.clear_constraints()
        self.top.driver.add_constraint('simple.invar - simple.outvar = simple.exec_count')
        try:
            self.top.run()
        except RuntimeError, err:
            msg = "driver: Please specify constraints in the form 'A=B'"
            msg += ': simple.invar - simple.outvar = simple.exec_count'
            self.assertEqual(str(err), msg)
        else:
            self.fail('RuntimeError expected')

    def test_multi_success(self):
        self.top.add("driver", FixedPointIterator())
        self.top.add("simple", Multi())
        self.top.driver.workflow.add('simple')

        self.top.driver.add_constraint('simple.out1 = simple.in1')
        self.top.driver.add_constraint('simple.out2 = simple.in2')
        self.top.driver.add_parameter('simple.in1')
        self.top.driver.add_parameter('simple.in2')
        self.top.driver.tolerance = .02

        self.top.run()

        assert_rel_error(self, self.top.simple.in1, .01, .002)
        assert_rel_error(self, self.top.simple.out1, .001, .0002)
        self.assertEqual(self.top.driver.current_iteration, 2)

    def test_multi_swapped(self):
        self.top.add("driver", FixedPointIterator())
        self.top.add("simple", Multi())
        self.top.driver.workflow.add('simple')

        self.top.driver.add_constraint('simple.out2 = simple.in2')
        self.top.driver.add_constraint('simple.out1 = simple.in1')
        self.top.driver.add_parameter('simple.in1')
        self.top.driver.add_parameter('simple.in2')
        self.top.driver.tolerance = .02
        self.top.run()

        assert_rel_error(self, self.top.simple.in1, .01, .002)
        assert_rel_error(self, self.top.simple.out1, .001, .0002)
        self.assertEqual(self.top.driver.current_iteration, 2)

    def test_multi_swapped_reversed(self):
        self.top.add("driver", FixedPointIterator())
        self.top.add("simple", Multi())
        self.top.driver.workflow.add('simple')

        self.top.driver.add_constraint('simple.out2 = simple.in2')
        self.top.driver.add_constraint('simple.in1 = simple.out1')
        self.top.driver.add_parameter('simple.in1')
        self.top.driver.add_parameter('simple.in2')
        self.top.driver.tolerance = .02
        self.top.run()

        assert_rel_error(self, self.top.simple.in1, .01, .002)
        assert_rel_error(self, self.top.simple.out1, .001, .0002)
        self.assertEqual(self.top.driver.current_iteration, 2)

    def test_array_multi(self):
        self.top.add("driver", FixedPointIterator())
        self.top.add("simple", ArrayMulti())
        self.top.driver.workflow.add('simple')

        self.top.driver.add_constraint('simple.out = simple.arr')
        self.top.driver.add_parameter('simple.arr')
        self.top.driver.tolerance = .02
        self.top.run()

        assert_rel_error(self, self.top.simple.arr[0], .01, .002)
        assert_rel_error(self, self.top.simple.out[0], .001, .0002)
        self.assertEqual(self.top.driver.current_iteration, 2)

    def test_simple_div10(self):
        self.top.add("driver", FixedPointIterator())
        self.top.add("simple", Div10())
        self.top.driver.workflow.add('simple')

        self.top.driver.add_constraint('simple.outvar = simple.invar')
        self.top.driver.add_parameter('simple.invar')
        self.top.driver.tolerance = .02
        self.top.run()

        assert_rel_error(self, self.top.simple.invar, .01, .002)
        assert_rel_error(self, self.top.simple.outvar, .001, .0002)
        self.assertEqual(self.top.driver.current_iteration, 2)

    def test_multi_array_multi(self):
        self.top.add("driver", FixedPointIterator())
        self.top.add("simple", MultiArrayMulti())
        self.top.driver.workflow.add('simple')

        self.top.driver.add_constraint('simple.out1 = simple.arr1')
        self.top.driver.add_constraint('simple.out2 = simple.arr2')
        self.top.driver.add_parameter('simple.arr1')
        self.top.driver.add_parameter('simple.arr2')
        self.top.driver.tolerance = .02
        self.top.run()

        assert_rel_error(self, self.top.simple.arr1[0], .01, .002)
        assert_rel_error(self, self.top.simple.arr1[1], .01, .002)
        assert_rel_error(self, self.top.simple.out1[0], .001, .0002)
        assert_rel_error(self, self.top.simple.out1[1], .001, .0002)
        assert_rel_error(self, self.top.simple.arr2[0], .01, .002)
        assert_rel_error(self, self.top.simple.arr2[1], .01, .002)
        assert_rel_error(self, self.top.simple.out2[0], .001, .0002)
        assert_rel_error(self, self.top.simple.out2[1], .001, .0002)
        self.assertEqual(self.top.driver.current_iteration, 2)


    def test_mixed_scalar_array_multi(self):
        self.top.add("driver", FixedPointIterator())
        self.top.add("simple", MixedScalarArrayMulti())

        self.top.driver.workflow.add('simple')

        self.top.driver.add_constraint('simple.out1 = simple.arr1')
        self.top.driver.add_constraint('simple.out2 = simple.in2')
        self.top.driver.add_parameter('simple.arr1')
        self.top.driver.add_parameter('simple.in2')
        self.top.driver.tolerance = .02
        self.top.run()

        assert_rel_error(self, self.top.simple.arr1[0], .01, .002)
        assert_rel_error(self, self.top.simple.arr1[1], .01, .002)
        assert_rel_error(self, self.top.simple.out1[0], .001, .0002)
        assert_rel_error(self, self.top.simple.out1[1], .001, .0002)
        assert_rel_error(self, self.top.simple.in2, .01, .002)
        self.assertEqual(self.top.driver.current_iteration, 2)

    def test_mixed_scalar_array_multi_swapped(self):
        self.top.add("driver", FixedPointIterator())
        self.top.add("simple", MixedScalarArrayMulti())

        self.top.driver.workflow.add('simple')

        self.top.driver.add_constraint('simple.out1 = simple.arr1')
        self.top.driver.add_constraint('simple.in2 = simple.out2')
        self.top.driver.add_parameter('simple.arr1')
        self.top.driver.add_parameter('simple.in2')
        self.top.driver.tolerance = .02
        self.top.run()

        assert_rel_error(self, self.top.simple.arr1[0], .01, .002)
        assert_rel_error(self, self.top.simple.arr1[1], .01, .002)
        assert_rel_error(self, self.top.simple.out1[0], .001, .0002)
        assert_rel_error(self, self.top.simple.out1[1], .001, .0002)
        assert_rel_error(self, self.top.simple.in2, .01, .002)
        self.assertEqual(self.top.driver.current_iteration, 2)

    def test_maxiteration(self):
        self.top.add("driver", FixedPointIterator())
        self.top.add("simple", Simple1())
        self.top.driver.workflow.add('simple')
        self.top.driver.add_constraint('simple.outvar = simple.invar')
        self.top.driver.add_parameter('simple.invar')
        self.top.driver.max_iteration = 3

        self.top.run()
        self.assertEqual(self.top.driver.current_iteration, 3)

    def test_check_config(self):
        self.top.add("driver", FixedPointIterator())
        self.top.add("simple", Multi())
        self.top.driver.workflow.add('simple')

        try:
            self.top.run()
        except RuntimeError, err:
            msg = "driver: FixedPointIterator requires a cyclic workflow, or a " + \
            "parameter/constraint pair."
            self.assertEqual(str(err), msg)
        else:
            self.fail('RuntimeError expected')

        self.top.driver.add_constraint('simple.out1 - simple.in1 = 0')

        try:
            self.top.run()
        except RuntimeError, err:
            msg = "driver: The number of input parameters must equal the " \
                  "number of output constraint equations in FixedPointIterator."
            self.assertEqual(str(err), msg)
        else:
            self.fail('RuntimeError expected')

        self.top.driver.add_parameter('simple.in1')
        self.top.driver.add_parameter('simple.in2')

        try:
            self.top.run()
        except RuntimeError, err:
            msg = "driver: The number of input parameters must equal the " \
                  "number of output constraint equations in FixedPointIterator."
            self.assertEqual(str(err), msg)
        else:
            self.fail('RuntimeError expected')


class Sellar_MDA(Assembly):

    def configure(self):

        self.add('d1', Discipline1_WithDerivatives())
        self.d1.x1 = 1.0
        self.d1.y1 = 1.0
        self.d1.y2 = 1.0
        self.d1.z1 = 5.0
        self.d1.z2 = 2.0

        self.add('d2', Discipline2_WithDerivatives())
        self.d2.y1 = 1.0
        self.d2.y2 = 1.0
        self.d2.z1 = 5.0
        self.d2.z2 = 2.0

        self.connect('d1.y1', 'd2.y1')
        self.connect('d2.y2', 'd1.y2')

        self.add('driver', FixedPointIterator())
        self.driver.workflow.add(['d1', 'd2'])


class Sellar_MDA_subbed(Assembly):

    def configure(self):

        self.add('d1', Discipline1_WithDerivatives())
        self.d1.x1 = 1.0
        self.d1.y1 = 1.0
        self.d1.y2 = 1.0
        self.d1.z1 = 5.0
        self.d1.z2 = 2.0

        self.add('d2', Discipline2_WithDerivatives())
        self.d2.y1 = 1.0
        self.d2.y2 = 1.0
        self.d2.z1 = 5.0
        self.d2.z2 = 2.0

        self.connect('d1.y1', 'd2.y1')
        self.connect('d2.y2', 'd1.y2')

        self.add('subdriver', FixedPointIterator())
        self.driver.workflow.add(['subdriver'])
        self.subdriver.workflow.add(['d1', 'd2'])

class FixedPointIterator_with_Cyclic_TestCase(unittest.TestCase):
    """test the FixedPointIterator with cyclic a workflow"""

    def setUp(self):
        self.top = set_as_top(Sellar_MDA())

    def tearDown(self):
        self.top = None

    def test_gauss_seidel(self):

        self.top.run()

        assert_rel_error(self, self.top.d1.y1,
                               self.top.d2.y1,
                               1.0e-4)
        assert_rel_error(self, self.top.d1.y2,
                               self.top.d2.y2,
                               1.0e-4)
        self.assertTrue(self.top.d1.exec_count < 10)

    def test_gauss_seidel_param_con(self):

        self.top.disconnect('d2.y2')
        self.top.driver.add_parameter('d1.y2', low=-100, high=100)
        self.top.driver.add_constraint('d2.y2 = d1.y2')
        self.top.run()

        assert_rel_error(self, self.top.d1.y1,
                               self.top.d2.y1,
                               1.0e-4)
        assert_rel_error(self, self.top.d1.y2,
                               self.top.d2.y2,
                               1.0e-4)
        self.assertTrue(self.top.d1.exec_count < 10)

    def test_gauss_seidel_sub(self):

        self.top = set_as_top(Sellar_MDA_subbed())
        self.top.subdriver.tolerance = 1.0e-9
        self.top.run()

        assert_rel_error(self, self.top.d1.y1,
                               self.top.d2.y1,
                               1.0e-4)
        assert_rel_error(self, self.top.d1.y2,
                               self.top.d2.y2,
                               1.0e-4)
        self.assertTrue(self.top.d1.exec_count < 10)

        inputs = ['d1.z1', 'd1.z2', 'd2.z1', 'd2.z2']
        outputs = ['d1.y1', 'd2.y2']
        J1 = self.top.driver.calc_gradient(inputs=inputs,
                                           outputs=outputs)
        J2 = self.top.driver.calc_gradient(inputs=inputs,
                                           outputs=outputs,
                                           mode='adjoint')
        J3 = self.top.driver.calc_gradient(inputs=inputs,
                                           outputs=outputs,
                                           mode='fd')

        J = (J1 - J3)
        self.assertTrue(J.max() < 1.0e-3)


        J = (J2 - J3)
        self.assertTrue(J.max() < 1.0e-3)

class TestIterateUntill(unittest.TestCase):
    """Test case for the IterateUntil Driver"""

    def setUp(self):
        self.top = set_as_top(Assembly())

    def tearDown(self):
        self.top = None

    def test_max_iterations(self):
        self.top.add("driver", IterateUntil())
        self.top.driver.max_iterations = 3

        self.top.driver.workflow.add('simple')

        self.top.add('simple', Simple4())
        self.top.simple.invar = 1

        self.top.run()

        self.assertEqual(self.top.driver.iteration, 3)
        self.assertEqual(self.top.simple.outvar, 3)

    def test_stop_conditions(self):
        self.top.add("driver", IterateUntil())
        self.top.driver.max_iterations = 10

        self.top.driver.workflow.add('simple')

        self.top.add('simple', Simple4())
        self.top.simple.invar = 1
        self.top.driver.add_stop_condition("simple.outvar >= 2")

        self.top.run()

        self.assertEqual(self.top.driver.iteration, 2)
        self.assertEqual(self.top.simple.outvar, 2)

    def test_stop_conditions_nested_iter(self):
        self.top.add("iter", IterateUntil())
        self.top.iter.max_iterations = 10

        self.top.driver.workflow.add('iter')

        self.top.iter.workflow.add('simple')

        self.top.add('simple', Simple4())
        self.top.simple.invar = 1
        self.top.iter.add_stop_condition("simple.outvar >= 3")

        self.top.run()

        self.assertEqual(self.top.iter.iteration, 3)
        self.assertEqual(self.top.simple.outvar, 3)


if __name__ == "__main__":
    unittest.main()
