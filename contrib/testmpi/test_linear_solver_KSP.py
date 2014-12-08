"""
Basic unit testing of the linear solvers. PetSc requires MPI, so it must be
tested separately.
"""
import unittest
import numpy as np

from openmdao.main.mpiwrap import PETSc
from openmdao.examples.simple.paraboloid import Paraboloid
from openmdao.lib.drivers.api import NewtonSolver
from openmdao.lib.optproblems.sellar import Discipline1_WithDerivatives, \
                                            Discipline2_WithDerivatives
from openmdao.main.api import Component, Assembly, set_as_top, Driver
from openmdao.main.datatypes.api import Float
from openmdao.main.test.simpledriver import SimpleDriver
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

        J = top.driver.workflow.calc_gradient(inputs=['comp.x', 'comp.y'],
                                              outputs=['comp.f_xy'],
                                              mode='forward')

        assert_rel_error(self, J[0, 0], 5.0, 0.0001)
        assert_rel_error(self, J[0, 1], 21.0, 0.0001)

        J = top.driver.workflow.calc_gradient(inputs=['comp.x', 'comp.y'],
                                              mode='adjoint')

        assert_rel_error(self, J[0, 0], 5.0, 0.0001)
        assert_rel_error(self, J[0, 1], 21.0, 0.0001)

        # Make sure we aren't add-scattering out p vector

        top.run()
        J = top.driver.workflow.calc_gradient(inputs=['comp.x', 'comp.y'],
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

        J = top.driver.workflow.calc_gradient(inputs=['comp.x', 'comp.y'],
                                              outputs=['comp.f_xy'],
                                              mode='forward', return_format='dict')

        assert_rel_error(self, J['comp.f_xy']['comp.x'][0][0], 5.0, 0.0001)
        assert_rel_error(self, J['comp.f_xy']['comp.y'][0][0], 21.0, 0.0001)

        J = top.driver.workflow.calc_gradient(inputs=['comp.x', 'comp.y'],
                                              mode='adjoint', return_format='dict')

        assert_rel_error(self, J['_pseudo_0.out0']['comp.x'][0][0], 5.0, 0.0001)
        assert_rel_error(self, J['_pseudo_0.out0']['comp.y'][0][0], 21.0, 0.0001)

        # Make sure we aren't add-scattering out p vector

        top.run()
        J = top.driver.workflow.calc_gradient(inputs=['comp.x', 'comp.y'],
                                              mode='forward', return_format='dict')
        assert_rel_error(self, J['_pseudo_0.out0']['comp.x'][0][0], 5.0, 0.0001)
        assert_rel_error(self, J['_pseudo_0.out0']['comp.y'][0][0], 21.0, 0.0001)

    def test_petsc_ksp_Sellar_Newton(self):

        top = set_as_top(SellarMDF())
        top.driver.gradient_options.lin_solver = 'petsc_ksp'
        top.subdriver.gradient_options.lin_solver = 'petsc_ksp'
        top.run()
        
        J = top.driver.workflow.calc_gradient(mode='forward')

        assert_rel_error(self, J[0, 0], 0.9806145, 0.0001)
        assert_rel_error(self, J[1, 0], 0.0969276, 0.0001)

        J = top.driver.workflow.calc_gradient(mode='adjoint')

        assert_rel_error(self, J[0, 0], 0.9806145, 0.0001)
        assert_rel_error(self, J[1, 0], 0.0969276, 0.0001)

    def test_petsc_ksp_Sellar_Newton_lin_GS_top(self):

        top = set_as_top(SellarMDF())
        top.driver.gradient_options.lin_solver = 'linear_gs'
        top.subdriver.gradient_options.lin_solver = 'petsc_ksp'
        top.run()
        
        J = top.driver.workflow.calc_gradient(mode='forward')

        assert_rel_error(self, J[0, 0], 0.9806145, 0.0001)
        assert_rel_error(self, J[1, 0], 0.0969276, 0.0001)

        J = top.driver.workflow.calc_gradient(mode='adjoint')

        assert_rel_error(self, J[0, 0], 0.9806145, 0.0001)
        assert_rel_error(self, J[1, 0], 0.0969276, 0.0001)


if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()