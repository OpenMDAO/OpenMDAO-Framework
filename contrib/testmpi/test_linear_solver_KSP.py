"""
Basic unit testing of the linear solvers. PetSc requires MPI, so it must be
tested separately.
"""
import unittest

from nose import SkipTest
import numpy as np

from openmdao.examples.simple.paraboloid import Paraboloid
from openmdao.lib.drivers.api import NewtonSolver
from openmdao.lib.optproblems.sellar import Discipline1_WithDerivatives, \
                                            Discipline2_WithDerivatives
from openmdao.main.api import Component, Assembly, set_as_top, Driver
from openmdao.main.datatypes.api import Float, Array
from openmdao.main.mpiwrap import PETSc
from openmdao.main.test.simpledriver import SimpleDriver
from openmdao.main.test.test_derivatives import ArrayComp2D
from openmdao.test.execcomp import ExecCompWithDerivatives, ExecComp
from openmdao.util.testutil import assert_rel_error


class SellarMDF(Assembly):

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
        #self.connect('d2.y2', 'd1.y2')

        self.add('driver', SimpleDriver())
        self.add('subdriver', NewtonSolver())
        self.driver.workflow.add(['subdriver'])
        self.subdriver.workflow.add(['d1', 'd2'])

        self.subdriver.add_parameter('d1.y2', low=-1e99, high=1e99)
        self.subdriver.add_constraint('d1.y2 = d2.y2')

        self.driver.add_parameter('d1.x1', low=-1e99, high=1e99)
        self.driver.add_constraint('d1.y1 < 0')
        self.driver.add_constraint('d2.y2 < 0')


class Testcase_PetSc_KSP(unittest.TestCase):
    """ Test PetSC KSP solver. """

    def setUp(self):
        if not PETSc.installed:
            raise SkipTest("PetSc not installed")

    def test_petsc_ksp_single_comp_array(self):

        top = set_as_top(Assembly())
        top.add('comp', Paraboloid())
        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['comp'])
        top.driver.add_parameter('comp.x', low=-1000, high=1000)
        top.driver.add_parameter('comp.y', low=-1000, high=1000)
        top.driver.add_objective('comp.f_xy')

        top.driver.gradient_options.lin_solver = 'petsc_ksp'

        top.comp.x = 3
        top.comp.y = 5
        top.run()

        self.assertEqual(top.comp.f_xy, 93.)
        self.assertEqual(top._pseudo_0.out0, 93.)

        J = top.driver.calc_gradient(inputs=['comp.x', 'comp.y'],
                                              outputs=['comp.f_xy'],
                                              mode='forward')

        assert_rel_error(self, J[0, 0], 5.0, 0.0001)
        assert_rel_error(self, J[0, 1], 21.0, 0.0001)

        J = top.driver.calc_gradient(inputs=['comp.x', 'comp.y'],
                                              mode='adjoint')

        assert_rel_error(self, J[0, 0], 5.0, 0.0001)
        assert_rel_error(self, J[0, 1], 21.0, 0.0001)

        # Make sure we aren't add-scattering out p vector

        top.run()
        J = top.driver.calc_gradient(inputs=['comp.x', 'comp.y'],
                                              mode='forward')
        assert_rel_error(self, J[0, 0], 5.0, 0.0001)
        assert_rel_error(self, J[0, 1], 21.0, 0.0001)

    def test_petsc_ksp_single_comp_dict(self):

        top = set_as_top(Assembly())
        top.add('comp', Paraboloid())
        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['comp'])
        top.driver.add_parameter('comp.x', low=-1000, high=1000)
        top.driver.add_parameter('comp.y', low=-1000, high=1000)
        top.driver.add_objective('comp.f_xy')

        top.driver.gradient_options.lin_solver = 'petsc_ksp'

        top.comp.x = 3
        top.comp.y = 5
        top.run()

        self.assertEqual(top.comp.f_xy, 93.)
        self.assertEqual(top._pseudo_0.out0, 93.)

        J = top.driver.calc_gradient(inputs=['comp.x', 'comp.y'],
                                              outputs=['comp.f_xy'],
                                              mode='forward', return_format='dict')

        assert_rel_error(self, J['comp.f_xy']['comp.x'][0][0], 5.0, 0.0001)
        assert_rel_error(self, J['comp.f_xy']['comp.y'][0][0], 21.0, 0.0001)

        J = top.driver.calc_gradient(inputs=['comp.x', 'comp.y'],
                                              mode='adjoint', return_format='dict')

        assert_rel_error(self, J['_pseudo_0.out0']['comp.x'][0][0], 5.0, 0.0001)
        assert_rel_error(self, J['_pseudo_0.out0']['comp.y'][0][0], 21.0, 0.0001)

        # Make sure we aren't add-scattering out p vector

        top.run()
        J = top.driver.calc_gradient(inputs=['comp.x', 'comp.y'],
                                              mode='forward', return_format='dict')
        assert_rel_error(self, J['_pseudo_0.out0']['comp.x'][0][0], 5.0, 0.0001)
        assert_rel_error(self, J['_pseudo_0.out0']['comp.y'][0][0], 21.0, 0.0001)

    def test_petsc_ksp_Sellar_Newton(self):

        top = set_as_top(SellarMDF())
        top.driver.gradient_options.lin_solver = 'petsc_ksp'
        top.subdriver.gradient_options.lin_solver = 'petsc_ksp'
        top.run()

        J = top.driver.calc_gradient(mode='forward')

        assert_rel_error(self, J[0, 0], 0.9806145, 0.0001)
        assert_rel_error(self, J[1, 0], 0.0969276, 0.0001)

        J = top.driver.calc_gradient(mode='adjoint')

        assert_rel_error(self, J[0, 0], 0.9806145, 0.0001)
        assert_rel_error(self, J[1, 0], 0.0969276, 0.0001)

    def test_petsc_ksp_Sellar_Newton_lin_GS_top(self):

        top = set_as_top(SellarMDF())
        top.driver.gradient_options.lin_solver = 'linear_gs'
        top.subdriver.gradient_options.lin_solver = 'petsc_ksp'
        top.run()

        J = top.driver.calc_gradient(mode='forward')

        assert_rel_error(self, J[0, 0], 0.9806145, 0.0001)
        assert_rel_error(self, J[1, 0], 0.0969276, 0.0001)

        J = top.driver.calc_gradient(mode='adjoint')

        assert_rel_error(self, J[0, 0], 0.9806145, 0.0001)
        assert_rel_error(self, J[1, 0], 0.0969276, 0.0001)

    def test_custom_jacobian(self):

        class AComp(Component):

            x = Array([[1.0, 3.0], [-2.0, 4.0]], iotype='in')
            y = Array(np.zeros((2, 2)), iotype='out')

            def __init__(self):
                super(AComp, self).__init__()
                self.J = np.array([[3.5, -2.5, 1.5, 4.0],
                                   [4.0, 2.0, -1.1, 3.4],
                                   [7.7, 6.6, 4.4, 1.1],
                                   [0.1, 3.3, 6.8, -5.5]])

            def execute(self):
                """ Run arraycomp"""
                y = self.J.dot(self.x.flatten())
                self.y = y.reshape((2,2))

            def list_deriv_vars(self):
                """ x and y """
                input_keys = ('x',)
                output_keys = ('y',)
                return input_keys, output_keys

            def provideJ(self):
                """Analytical first derivatives"""
                return self.J

        def fake_jac():
            """ Returns a User-defined Jacobian. The values are
            totally wrong to facilitate testing. """
            jacs = {}
            jacs['comp.x'] = np.array([[100.0, 101, 102, 103],
                                       [104, 105, 106, 107],
                                       [108, 109, 110, 111],
                                       [112, 113, 114, 115]])

            return jacs

        top = set_as_top(Assembly())
        top.add('driver', SimpleDriver())
        top.add('comp', AComp())
        top.driver.workflow.add('comp')
        top.driver.add_parameter('comp.x', low=10, high=10)
        top.driver.add_constraint('comp.y < 1', jacs=fake_jac)

        # petsc KSP, inequality constraints
        top.run()

        J = top.driver.calc_gradient(mode='forward', return_format='dict')
        J = J['_pseudo_0.out0']['comp.x']
        diff = np.abs(J - top.comp.J)
        assert_rel_error(self, diff.max(), 0.0, 1e-4)

        J = top.driver.calc_gradient(mode='adjoint', return_format='dict')
        J = J['_pseudo_0.out0']['comp.x']
        diff = np.abs(J - fake_jac()['comp.x'])
        assert_rel_error(self, diff.max(), 0.0, 1e-4)

    def test_nested_2Darray(self):

        top = Assembly()
        top.add('nest', Assembly())
        top.nest.add('comp', ArrayComp2D())
        top.driver.gradient_options.lin_solver = 'petsc_ksp'
        top.nest.driver.gradient_options.lin_solver = 'petsc_ksp'

        top.driver.workflow.add(['nest'])
        top.nest.driver.workflow.add(['comp'])
        top.nest.create_passthrough('comp.x')
        top.nest.create_passthrough('comp.y')
        top.run()

        J = top.driver.calc_gradient(inputs=['nest.x',],
                                     outputs=['nest.y'],
                                     mode='forward')

        diff = J - top.nest.comp.J
        assert_rel_error(self, diff.max(), 0.0, .000001)

        J = top.driver.calc_gradient(inputs=['nest.x',],
                                     outputs=['nest.y'],
                                     mode='adjoint')
        diff = J - top.nest.comp.J
        assert_rel_error(self, diff.max(), 0.0, .000001)

        J = top.driver.calc_gradient(inputs=['nest.x[0, 0]',],
                                     outputs=['nest.y[0, 0]'],
                                     mode='forward')

        diff = J - top.nest.comp.J[0, 0]
        assert_rel_error(self, diff.max(), 0.0, .000001)

        J = top.driver.calc_gradient(inputs=['nest.x[0, 0]',],
                                     outputs=['nest.y[0, 0]'],
                                     mode='adjoint')

        diff = J - top.nest.comp.J[0, 0]
        assert_rel_error(self, diff.max(), 0.0, .000001)

        J = top.driver.calc_gradient(inputs=['nest.x[0, 1]',],
                                     outputs=['nest.y[1, 0]'],
                                     mode='forward')

        diff = J - top.nest.comp.J[1, 2]
        assert_rel_error(self, diff.max(), 0.0, .000001)

        J = top.driver.calc_gradient(inputs=['nest.x[0, 1]',],
                                     outputs=['nest.y[1, 0]'],
                                     mode='adjoint')

        diff = J - top.nest.comp.J[1, 2]
        assert_rel_error(self, diff.max(), 0.0, .000001)

        J = top.driver.calc_gradient(inputs=['nest.x[0, 1]',],
                                     outputs=['nest.y[1, 0]'],
                                     mode='fd')

        diff = J - top.nest.comp.J[1, 2]
        assert_rel_error(self, diff.max(), 0.0, .000001)

        J = top.driver.calc_gradient(inputs=['nest.x[0, -1]',],
                                     outputs=['nest.y[-1, 0]'],
                                     mode='forward')

        diff = J - top.nest.comp.J[1, 2]
        assert_rel_error(self, diff.max(), 0.0, .000001)

        J = top.driver.calc_gradient(inputs=['nest.x[0, -1]',],
                                     outputs=['nest.y[-1, 0]'],
                                     mode='adjoint')

        diff = J - top.nest.comp.J[1, 2]
        assert_rel_error(self, diff.max(), 0.0, .000001)

        J = top.driver.calc_gradient(inputs=['nest.x[0, -1]',],
                                     outputs=['nest.y[-1, 0]'],
                                     mode='fd')

        diff = J - top.nest.comp.J[1, 2]
        assert_rel_error(self, diff.max(), 0.0, .000001)

        Jsub = top.nest.comp.J[2:3, 2:3]
        J = top.driver.calc_gradient(inputs=['nest.x[1][:]',],
                                     outputs=['nest.y[1][:]'],
                                     mode='forward')

        diff = J - Jsub

        J = top.driver.calc_gradient(inputs=['nest.x[1][:]',],
                                     outputs=['nest.y[1][:]'],
                                     mode='adjoint')

        diff = J - Jsub

        top.run()
        J = top.driver.calc_gradient(inputs=['nest.x[1][:]',],
                                     outputs=['nest.y[1][:]'],
                                     mode='fd')
        diff = J - Jsub

    def test_nested_2Darray_simul_element_and_full_connection(self):

        top = Assembly()
        top.add('comp', ArrayComp2D())
        top.add('nest', Assembly())
        top.nest.add('comp', ArrayComp2D())
        top.driver.gradient_options.lin_solver = 'petsc_ksp'
        top.nest.driver.gradient_options.lin_solver = 'petsc_ksp'

        top.nest.driver.workflow.add(['comp'])
        top.nest.create_passthrough('comp.x')
        top.nest.create_passthrough('comp.y')

        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['nest', 'comp'])
        top.connect('nest.y', 'comp.x')
        top.driver.add_parameter('nest.x[0][0]', low=-10, high=10)
        top.driver.add_objective('comp.y[0][0]')
        top.driver.add_constraint('nest.y[0][1] < 0')
        top.run()

        J = top.driver.calc_gradient(mode='forward')
        assert_rel_error(self, J[0, 0], 24.0, .000001)
        assert_rel_error(self, J[1, 0], 4.0, .000001)

        J = top.driver.calc_gradient(mode='adjoint')
        assert_rel_error(self, J[0, 0], 24.0, .000001)
        assert_rel_error(self, J[1, 0], 4.0, .000001)

        J = top.driver.calc_gradient(mode='fd')
        assert_rel_error(self, J[0, 0], 24.0, .000001)
        assert_rel_error(self, J[1, 0], 4.0, .000001)

    def test_nested_2Darray_simul_element_and_full_connection2(self):
        # Slightly different config

        top = Assembly()
        top.add('nest', Assembly())
        top.nest.add('comp1', ArrayComp2D())
        top.nest.add('comp2', ArrayComp2D())
        top.driver.gradient_options.lin_solver = 'petsc_ksp'
        top.nest.driver.gradient_options.lin_solver = 'petsc_ksp'

        top.nest.driver.workflow.add(['comp1', 'comp2'])
        top.nest.connect('comp1.y', 'comp2.x')
        top.nest.create_passthrough('comp1.x')
        top.nest.create_passthrough('comp1.y')
        top.nest.add('yy', Array(iotype='out'))
        top.nest.connect('comp2.y', 'yy')

        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['nest'])
        top.driver.add_parameter('nest.x[0][0]', low=-10, high=10)
        top.driver.add_objective('nest.yy[0][0]')
        top.driver.add_constraint('nest.y[0][1] < 0')
        top.run()

        J = top.driver.calc_gradient(mode='forward')
        assert_rel_error(self, J[0, 0], 24.0, .000001)
        assert_rel_error(self, J[1, 0], 4.0, .000001)

        J = top.driver.calc_gradient(mode='adjoint')
        assert_rel_error(self, J[0, 0], 24.0, .000001)
        assert_rel_error(self, J[1, 0], 4.0, .000001)

        J = top.driver.calc_gradient(mode='fd')
        assert_rel_error(self, J[0, 0], 24.0, .000001)
        assert_rel_error(self, J[1, 0], 4.0, .000001)

    def test_nested_array_full_and_partial_passthrough(self):

        top = Assembly()
        top.add('nest', Assembly())
        top.nest.add('comp1', ArrayComp2D())
        top.nest.add('comp2', ArrayComp2D())
        top.driver.gradient_options.lin_solver = 'petsc_ksp'
        top.nest.driver.gradient_options.lin_solver = 'petsc_ksp'

        top.nest.driver.workflow.add(['comp1', 'comp2'])
        top.nest.create_passthrough('comp1.x', 'x')
        top.nest.create_passthrough('comp1.y', 'y1')
        top.nest.create_passthrough('comp2.y', 'y2')
        top.nest.connect('x[-1, -1]', 'comp2.x[-1, -1]')

        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['nest'])
        top.run()

        Jbase = top.nest.comp1.provideJ()

        J = top.driver.calc_gradient(inputs=['nest.x'],
                                     outputs=['nest.y1', 'nest.y2'],
                                     mode='fd')
        diff = abs(J[0:4, :] - Jbase)
        assert_rel_error(self, diff.max(), 0.0, .00001)
        diff = abs(J[0:4, -1] - Jbase[:, -1])
        assert_rel_error(self, diff.max(), 0.0, .00001)
        diff = abs(J[4:, :-1])
        assert_rel_error(self, diff.max(), 0.0, .00001)

        J = top.driver.calc_gradient(inputs=['nest.x'],
                                     outputs=['nest.y1', 'nest.y2'],
                                     mode='forward')
        diff = abs(J[0:4, :] - Jbase)
        assert_rel_error(self, diff.max(), 0.0, .00001)
        diff = abs(J[0:4, -1] - Jbase[:, -1])
        assert_rel_error(self, diff.max(), 0.0, .00001)
        diff = abs(J[4:, :-1])
        assert_rel_error(self, diff.max(), 0.0, .00001)

        Jdict = top.driver.calc_gradient(inputs=['nest.x'],
                                         outputs=['nest.y1', 'nest.y2'],
                                         mode='forward',
                                         return_format='dict')
        diff = Jdict['nest.y1']['nest.x'] - Jbase
        assert_rel_error(self, diff.max(), 0.0, .00001)

        diff = Jdict['nest.y2']['nest.x'] - J[4:, :]
        assert_rel_error(self, diff.max(), 0.0, .00001)

        J = top.driver.calc_gradient(inputs=['nest.x'],
                                     outputs=['nest.y1', 'nest.y2'],
                                     mode='adjoint')
        diff = abs(J[0:4, :] - Jbase)
        assert_rel_error(self, diff.max(), 0.0, .00001)
        diff = abs(J[0:4, -1] - Jbase[:, -1])
        assert_rel_error(self, diff.max(), 0.0, .00001)
        diff = abs(J[4:, :-1])
        assert_rel_error(self, diff.max(), 0.0, .00001)

        Jdict = top.driver.calc_gradient(inputs=['nest.x'],
                                         outputs=['nest.y1', 'nest.y2'],
                                         mode='adjoint',
                                         return_format='dict')
        diff = Jdict['nest.y1']['nest.x'] - Jbase
        assert_rel_error(self, diff.max(), 0.0, .00001)

        diff = Jdict['nest.y2']['nest.x'] - J[4:, :]
        assert_rel_error(self, diff.max(), 0.0, .00001)

    def test_input_as_output(self):

        top = set_as_top(Assembly())
        top.add('comp1', ExecCompWithDerivatives(['y=2.0*x + 3.0*x2'],
                                                 ['dy_dx = 2.0', 'dy_dx2 = 3.0']))
        top.add('comp2', ExecCompWithDerivatives(['y=3.0*x'],
                                                 ['dy_dx = 3.0']))
        top.connect('comp1.y', 'comp2.x')
        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['comp1', 'comp2'])
        #top.driver.add_parameter('comp1.x', low=-100, high=100)
        top.driver.add_objective('comp1.y + comp2.y + 5*comp1.x')
        top.driver.gradient_options.lin_solver = 'petsc_ksp'

        objs = top.driver.get_objectives().values()
        obj = '%s.out0' % objs[0].pcomp_name

        top.comp1.x = 1.0
        top.run()

        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=[obj], mode='forward')

        assert_rel_error(self, J[0, 0], 13.0, 0.0001)

        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                              outputs=[obj], mode='fd')
        assert_rel_error(self, J[0, 0], 13.0, 0.0001)

        top.driver.run()

        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=[obj], mode='adjoint')
        assert_rel_error(self, J[0, 0], 13.0, 0.0001)

        J = top.driver.calc_gradient(inputs=['comp1.x', 'comp1.x2'],
                                     outputs=[obj], mode='forward')
        assert_rel_error(self, J[0, 0], 13.0, 0.0001)
        assert_rel_error(self, J[0, 1], 12.0, 0.0001)

        J = top.driver.calc_gradient(inputs=['comp1.x', 'comp1.x2'],
                                     outputs=[obj], mode='adjoint')
        assert_rel_error(self, J[0, 0], 13.0, 0.0001)
        assert_rel_error(self, J[0, 1], 12.0, 0.0001)

        J = top.driver.calc_gradient(inputs=[('comp1.x',), ('comp1.x2',)],
                                     outputs=[obj], mode='forward')
        assert_rel_error(self, J[0, 0], 13.0, 0.0001)
        assert_rel_error(self, J[0, 1], 12.0, 0.0001)

        J = top.driver.calc_gradient(inputs=[('comp1.x',), ('comp1.x2',)],
                                     outputs=[obj], mode='adjoint')
        assert_rel_error(self, J[0, 0], 13.0, 0.0001)
        assert_rel_error(self, J[0, 1], 12.0, 0.0001)



if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()
