
import numpy as np

from openmdao.util.testutil import assert_rel_error
from openmdao.test.mpiunittest import MPITestCase, collective_assert_rel_error, \
                                      MPIContext
from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.main.datatypes.api import Float
from openmdao.main.test.simpledriver import SimpleDriver
from openmdao.test.execcomp import ExecCompWithDerivatives, ExecComp

class Paraboloid(Component):
    """ Evaluates the equation f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3 """

    # set up interface to the framework
    # pylint: disable=E1101
    x = Float(0.0, iotype='in', desc='The variable x')
    y = Float(0.0, iotype='in', desc='The variable y')

    f_xy = Float(iotype='out', desc='F(x,y)')

    def execute(self):
        """f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3
        Optimal solution (minimum): x = 6.6667; y = -7.3333
        """

        x = self.x
        y = self.y

        self.f_xy = (x-3.0)**2 + x*y + (y+4.0)**2 - 3.0

    def provideJ(self):
        """Analytical first derivatives"""

        df_dx = 2.0*self.x - 6.0 + self.y
        df_dy = 2.0*self.y + 8.0 + self.x

        self.J = np.array([[df_dx, df_dy]])
        return self.J

    def list_deriv_vars(self):
        input_keys = ('x', 'y')
        output_keys = ('f_xy',)
        return input_keys, output_keys


class MPITests_2Proc(MPITestCase):

    N_PROCS = 2

    def setUp(self):
        # this model mimics the one in test_derivatives, test_single_comp
        self.top = top = set_as_top(Assembly())
        top.add('comp', Paraboloid())
        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['comp'])
        top.driver.add_parameter('comp.x', low=-1000, high=1000)
        top.driver.add_parameter('comp.y', low=-1000, high=1000)
        top.driver.add_objective('comp.f_xy')

        top.comp.x = 3
        top.comp.y = 5

    def test_run(self):

        self.top.run()

        if self.comm.rank == 0:
            self.assertEqual(self.top.comp.f_xy, 93.)
            self.assertEqual(self.top._pseudo_0.out0, 93.)

    def test_calc_gradient_fwd(self):
        self.top.run()

        J = self.top.driver.calc_gradient(mode='forward',
                                          return_format='dict')

        #J = self.top.driver.workflow._system.get_combined_J(J)
        
        if self.comm.rank == 0:
            assert_rel_error(self, J['_pseudo_0.out0']['comp.x'][0][0], 
                                        5.0, 0.0001)
            assert_rel_error(self, J['_pseudo_0.out0']['comp.y'][0][0], 
                                        21.0, 0.0001)

    def test_calc_gradient_adjoint(self):
        self.top.run()
    
        J = self.top.driver.calc_gradient(mode='adjoint',
                                          return_format='dict')
    
        #J = self.top.driver.workflow._system.get_combined_J(J)
    
        if self.comm.rank == 0:
            assert_rel_error(self, J['_pseudo_0.out0']['comp.x'][0][0], 
                                    5.0, 0.0001)
            assert_rel_error(self, J['_pseudo_0.out0']['comp.y'][0][0], 
                                    21.0, 0.0001)
    
    def test_calc_gradient_fd(self):
        self.top.run()
    
        J = self.top.driver.calc_gradient(mode='fd',
                                          return_format='dict')
    
        if self.comm.rank == 0:
            assert_rel_error(self, J['_pseudo_0.out0']['comp.x'][0][0], 
                                        5.0, 0.0001)
            assert_rel_error(self, J['_pseudo_0.out0']['comp.y'][0][0], 
                                        21.0, 0.0001)
        
    def test_calc_gradient_fwd_linGS(self):
        
        self.top.driver.gradient_options.lin_solver = 'linear_gs'
        self.top.driver.gradient_options.maxiter = 1
        self.top.run()
    
        J = self.top.driver.calc_gradient(inputs=['comp.x','comp.y'], mode='forward',
                                          return_format='dict')
    
        J = self.top.driver.workflow._system.get_combined_J(J)
        
        # TODO: ask Ken why this ever worked before when 'comp.y' was not
        #       specified as an input.
        if self.comm.rank == 0:
            assert_rel_error(self, J['_pseudo_0.out0']['comp.x'][0][0], 
                                        5.0, 0.0001)
            assert_rel_error(self, J['_pseudo_0.out0']['comp.y'][0][0], 
                                        21.0, 0.0001)
    
    def test_two_to_one_forward(self):
        
        top = set_as_top(Assembly())
        
        exp1 = ["y = 3.0*x"]
        exp2 = ["y = -2.0*x"]
        exp3 = ["y = 5.0*x1 + 4.0*x2"]
        
        deriv1 = ["dy_dx = 3.0"]
        deriv2 = ["dy_dx = -2.0"]
        deriv3 = ["dy_dx1 = 5.0", "dy_dx2 = 4.0"]
        
        top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        top.add('comp3', ExecCompWithDerivatives(exp3, deriv3))
        top.add('driver', SimpleDriver())
        
        top.driver.workflow.add(['comp1', 'comp2', 'comp3'])
        top.connect('comp1.y', 'comp3.x1')
        top.connect('comp2.y', 'comp3.x2')
        top.driver.add_parameter('comp1.x', low=-100, high=100)
        top.driver.add_parameter('comp2.x', low=-100, high=100)
        top.driver.add_constraint('comp3.y < 1000')
        top.run()

        J = top.driver.workflow.calc_gradient(mode='forward',
                                              return_format='dict')

        J = top.driver.workflow._system.get_combined_J(J)
        collective_assert_rel_error(self, 
                                    J['_pseudo_0.out0']['comp1.x'][0][0], 
                                    15.0, 0.0001)
        collective_assert_rel_error(self,
                                    J['_pseudo_0.out0']['comp2.x'][0][0], 
                                    -8.0, 0.0001) 

    def test_two_to_one_adjoint(self):
        
        top = set_as_top(Assembly())
        
        exp1 = ["y = 3.0*x"]
        exp2 = ["y = -2.0*x"]
        exp3 = ["y = 5.0*x1 + 4.0*x2"]
        
        deriv1 = ["dy_dx = 3.0"]
        deriv2 = ["dy_dx = -2.0"]
        deriv3 = ["dy_dx1 = 5.0", "dy_dx2 = 4.0"]
        
        top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        top.add('comp3', ExecCompWithDerivatives(exp3, deriv3))
        top.add('driver', SimpleDriver())
        
        top.driver.workflow.add(['comp1', 'comp2', 'comp3'])
        top.connect('comp1.y', 'comp3.x1')
        top.connect('comp2.y', 'comp3.x2')
        top.driver.add_parameter('comp1.x', low=-100, high=100)
        top.driver.add_parameter('comp2.x', low=-100, high=100)
        top.driver.add_constraint('comp3.y < 1000')
        top.run()

        J = top.driver.workflow.calc_gradient(mode='adjoint',
                                              return_format='dict')

        J = top.driver.workflow._system.get_combined_J(J)
        collective_assert_rel_error(self, 
                                    J['_pseudo_0.out0']['comp1.x'][0][0], 
                                    15.0, 0.0001)
        collective_assert_rel_error(self,
                                    J['_pseudo_0.out0']['comp2.x'][0][0], 
                                    -8.0, 0.0001)        

    def test_two_to_one_fd(self):
        
        top = set_as_top(Assembly())
        
        exp1 = ["y = 3.0*x"]
        exp2 = ["y = -2.0*x"]
        exp3 = ["y = 5.0*x1 + 4.0*x2"]
        
        deriv1 = ["dy_dx = 3.0"]
        deriv2 = ["dy_dx = -2.0"]
        deriv3 = ["dy_dx1 = 5.0", "dy_dx2 = 4.0"]
        
        top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        top.add('comp3', ExecCompWithDerivatives(exp3, deriv3))
        top.add('driver', SimpleDriver())
        
        top.driver.workflow.add(['comp1', 'comp2', 'comp3'])
        top.connect('comp1.y', 'comp3.x1')
        top.connect('comp2.y', 'comp3.x2')
        top.driver.add_parameter('comp1.x', low=-100, high=100)
        top.driver.add_parameter('comp2.x', low=-100, high=100)
        top.driver.add_constraint('comp3.y < 1000')
        top.run()

        J = top.driver.workflow.calc_gradient(mode='fd',
                                              return_format='dict')

        J = top.driver.workflow._system.get_combined_J(J)
        collective_assert_rel_error(self, 
                                    J['_pseudo_0.out0']['comp1.x'][0][0], 
                                    15.0, 0.0001)
        collective_assert_rel_error(self,
                                    J['_pseudo_0.out0']['comp2.x'][0][0], 
                                    -8.0, 0.0001)        

    def test_two_to_one_forward_bcast(self):
        
        top = set_as_top(Assembly())
        
        exp1 = ["y = 3.0*x"]
        exp2 = ["y = -2.0*x"]
        exp3 = ["y = 5.0*x1 + 4.0*x2"]
        
        deriv1 = ["dy_dx = 3.0"]
        deriv2 = ["dy_dx = -2.0"]
        deriv3 = ["dy_dx1 = 5.0", "dy_dx2 = 4.0"]
        
        top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        top.add('comp3', ExecCompWithDerivatives(exp3, deriv3))
        top.add('driver', SimpleDriver())
        
        top.driver.workflow.add(['comp1', 'comp2', 'comp3'])
        top.connect('comp1.y', 'comp3.x1')
        top.connect('comp2.y', 'comp3.x2')
        top.driver.add_parameter(('comp1.x', 'comp2.x'), 
                                 low=-100, high=100)
        top.driver.add_constraint('comp3.y < 1000')
        top.run()

        J = top.driver.workflow.calc_gradient(mode='forward',
                                              return_format='dict')

        J = top.driver.workflow._system.get_combined_J(J)
        #print J
        collective_assert_rel_error(self, 
                                    J['_pseudo_0.out0']['comp1.x'][0][0], 
                                    7.0, 0.0001)

    def test_two_to_one_adjoint_bcast(self):
        
        top = set_as_top(Assembly())
        
        exp1 = ["y = 3.0*x"]
        exp2 = ["y = -2.0*x"]
        exp3 = ["y = 5.0*x1 + 4.0*x2"]
        
        deriv1 = ["dy_dx = 3.0"]
        deriv2 = ["dy_dx = -2.0"]
        deriv3 = ["dy_dx1 = 5.0", "dy_dx2 = 4.0"]
        
        top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        top.add('comp3', ExecCompWithDerivatives(exp3, deriv3))
        top.add('driver', SimpleDriver())
        
        top.driver.workflow.add(['comp1', 'comp2', 'comp3'])
        top.connect('comp1.y', 'comp3.x1')
        top.connect('comp2.y', 'comp3.x2')
        top.driver.add_parameter(('comp1.x', 'comp2.x'), 
                                 low=-100, high=100)
        top.driver.add_constraint('comp3.y < 1000')
        top.run()

        J = top.driver.workflow.calc_gradient(mode='adjoint',
                                              return_format='dict')

        J = top.driver.workflow._system.get_combined_J(J)

        #from openmdao.util.dotgraph import plot_system_tree
        #plot_system_tree(top._system)

        collective_assert_rel_error(self, 
                                    J['_pseudo_0.out0']['comp1.x'][0][0], 
                                    7.0, 0.0001)

    def test_one_to_two_forward(self):
        
        top = set_as_top(Assembly())
        
        exp1 = ["y1 = 3.0*x", "y2 = 4.0*x"]
        exp2 = ["y = -2.0*x"]
        exp3 = ["y = 5.0*x"]
        
        deriv1 = ["dy1_dx = 3.0", "dy2_dx = 4.0"]
        deriv2 = ["dy_dx = -2.0"]
        deriv3 = ["dy_dx = 5.0"]
        
        top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        top.add('comp3', ExecCompWithDerivatives(exp3, deriv3))
        top.add('driver', SimpleDriver())
        
        top.driver.workflow.add(['comp1', 'comp2', 'comp3'])
        top.connect('comp1.y1', 'comp2.x')
        top.connect('comp1.y2', 'comp3.x')
        top.driver.add_parameter('comp1.x', low=-100, high=100)
        top.driver.add_constraint('comp2.y < 1000')
        top.driver.add_constraint('comp3.y < 1000')
        top.run()
        
        J = top.driver.workflow.calc_gradient(mode='forward',
                                              return_format='dict')
        J = top.driver.workflow._system.get_combined_J(J)
        
        collective_assert_rel_error(self, 
                                    J['_pseudo_0.out0']['comp1.x'][0][0], 
                                    -6.0, 0.0001)
        collective_assert_rel_error(self,
                                    J['_pseudo_1.out0']['comp1.x'][0][0], 
                                    20.0, 0.0001)        

    def test_one_to_two_adjoint(self):
        
        top = set_as_top(Assembly())
        
        exp1 = ["y1 = 3.0*x", "y2 = 4.0*x"]
        exp2 = ["y = -2.0*x"]
        exp3 = ["y = 5.0*x"]
        
        deriv1 = ["dy1_dx = 3.0", "dy2_dx = 4.0"]
        deriv2 = ["dy_dx = -2.0"]
        deriv3 = ["dy_dx = 5.0"]
        
        top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        top.add('comp3', ExecCompWithDerivatives(exp3, deriv3))
        top.add('driver', SimpleDriver())
        
        top.driver.workflow.add(['comp1', 'comp2', 'comp3'])
        top.connect('comp1.y1', 'comp2.x')
        top.connect('comp1.y2', 'comp3.x')
        top.driver.add_parameter('comp1.x', low=-100, high=100)
        top.driver.add_constraint('comp2.y < 1000')
        top.driver.add_constraint('comp3.y < 1000')
        top.run()
        
        J = top.driver.workflow.calc_gradient(mode='adjoint',
                                              return_format='dict')
        J = top.driver.workflow._system.get_combined_J(J)
        
        collective_assert_rel_error(self, 
                                    J['_pseudo_0.out0']['comp1.x'][0][0], 
                                    -6.0, 0.0001)
        collective_assert_rel_error(self,
                                    J['_pseudo_1.out0']['comp1.x'][0][0], 
                                    20.0, 0.0001)        

    def test_one_to_two_fd(self):
        
        top = set_as_top(Assembly())
        
        exp1 = ["y1 = 3.0*x", "y2 = 4.0*x"]
        exp2 = ["y = -2.0*x"]
        exp3 = ["y = 5.0*x"]
        
        deriv1 = ["dy1_dx = 3.0", "dy2_dx = 4.0"]
        deriv2 = ["dy_dx = -2.0"]
        deriv3 = ["dy_dx = 5.0"]
        
        top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        top.add('comp3', ExecCompWithDerivatives(exp3, deriv3))
        top.add('driver', SimpleDriver())
        
        top.driver.workflow.add(['comp1', 'comp2', 'comp3'])
        top.connect('comp1.y1', 'comp2.x')
        top.connect('comp1.y2', 'comp3.x')
        top.driver.add_parameter('comp1.x', low=-100, high=100)
        top.driver.add_constraint('comp2.y < 1000')
        top.driver.add_constraint('comp3.y < 1000')
        top.run()
        
        J = top.driver.workflow.calc_gradient(mode='fd',
                                              return_format='dict')
        J = top.driver.workflow._system.get_combined_J(J)
        
        collective_assert_rel_error(self, 
                                    J['_pseudo_0.out0']['comp1.x'][0][0], 
                                    -6.0, 0.0001)
        collective_assert_rel_error(self,
                                    J['_pseudo_1.out0']['comp1.x'][0][0], 
                                    20.0, 0.0001)        

    def test_three_comp_diamond_forward(self):
        
        self.top = set_as_top(Assembly())

        exp1 = ['y1 = 50.0*x1',
                'y2 = 1.0*x1']
        deriv1 = ['dy1_dx1 = 50.0',
                  'dy2_dx1 = 1.0']

        exp2 = ['y1 = 1.2*x1']
        deriv2 = ['dy1_dx1 = 1.2']
    
        exp3 = ['y1 = 100.0*x1*x2 + 30*x1 + 0.3*x2']
        deriv3 = ['dy1_dx1 = 100.0*x2 + 30',
                  'dy1_dx2 = 100.0*x1 + 0.3']

        self.top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        self.top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        self.top.add('comp3', ExecCompWithDerivatives(exp3, deriv3))
        self.top.add('driver', SimpleDriver())

        self.top.driver.workflow.add(['comp1', 'comp2', 'comp3'])

        self.top.connect('comp1.y1', 'comp2.x1')
        self.top.connect('comp1.y2', 'comp3.x1')
        self.top.connect('comp2.y1', 'comp3.x2')

        self.top.driver.add_parameter('comp1.x1', low=-100, high=100)
        self.top.driver.add_objective('comp3.y1')
        self.top.comp1.x1 = 2.0
        self.top.run()

        J = self.top.driver.calc_gradient(inputs=['comp1.x1'],
                                          outputs=['comp3.y1'],
                                          mode='forward',
                                          return_format='dict')
        if self.comm.rank == 0:
            assert_rel_error(self, J['comp3.y1']['comp1.x1'][0][0], 
                             24048.0, 0.0001)

    def test_diverge_converge_forward(self):
        
        self.top = set_as_top(Assembly())

        exp1 = ['y1 = 2.0*x1**2',
                'y2 = 3.0*x1']
        deriv1 = ['dy1_dx1 = 4.0*x1',
                  'dy2_dx1 = 3.0']

        exp2 = ['y1 = 0.5*x1']
        deriv2 = ['dy1_dx1 = 0.5']

        exp3 = ['y1 = 3.5*x1']
        deriv3 = ['dy1_dx1 = 3.5']

        exp4 = ['y1 = x1 + 2.0*x2',
                'y2 = 3.0*x1',
                'y3 = x1*x2']
        deriv4 = ['dy1_dx1 = 1.0',
                  'dy1_dx2 = 2.0',
                  'dy2_dx1 = 3.0',
                  'dy2_dx2 = 0.0',
                  'dy3_dx1 = x2',
                  'dy3_dx2 = x1']

        exp5 = ['y1 = x1 + 3.0*x2 + 2.0*x3']
        deriv5 = ['dy1_dx1 = 1.0',
                  'dy1_dx2 = 3.0',
                  'dy1_dx3 = 2.0']

        self.top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        self.top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        self.top.add('comp3', ExecCompWithDerivatives(exp3, deriv3))
        self.top.add('comp4', ExecCompWithDerivatives(exp4, deriv4))
        self.top.add('comp5', ExecCompWithDerivatives(exp5, deriv5))

        self.top.driver.workflow.add(['comp1', 'comp2', 'comp3', 'comp4', 'comp5'])

        self.top.connect('comp1.y1', 'comp2.x1')
        self.top.connect('comp1.y2', 'comp3.x1')
        self.top.connect('comp2.y1', 'comp4.x1')
        self.top.connect('comp3.y1', 'comp4.x2')
        self.top.connect('comp4.y1', 'comp5.x1')
        self.top.connect('comp4.y2', 'comp5.x2')
        self.top.connect('comp4.y3', 'comp5.x3')
        
        self.top.comp1.x1 = 2.0        
        self.top.run()

        J = self.top.driver.calc_gradient(inputs=['comp1.x1'],
                                          outputs=['comp5.y1'],
                                          mode='forward',
                                          return_format='dict')
        
        collective_assert_rel_error(self, 
                                    J['comp5.y1']['comp1.x1'][0][0], 
                                    313.0, 0.0001)

    def test_diverge_converge_adjoint(self):
        
        self.top = set_as_top(Assembly())

        exp1 = ['y1 = 2.0*x1**2',
                'y2 = 3.0*x1']
        deriv1 = ['dy1_dx1 = 4.0*x1',
                  'dy2_dx1 = 3.0']

        exp2 = ['y1 = 0.5*x1']
        deriv2 = ['dy1_dx1 = 0.5']

        exp3 = ['y1 = 3.5*x1']
        deriv3 = ['dy1_dx1 = 3.5']

        exp4 = ['y1 = x1 + 2.0*x2',
                'y2 = 3.0*x1',
                'y3 = x1*x2']
        deriv4 = ['dy1_dx1 = 1.0',
                  'dy1_dx2 = 2.0',
                  'dy2_dx1 = 3.0',
                  'dy2_dx2 = 0.0',
                  'dy3_dx1 = x2',
                  'dy3_dx2 = x1']

        exp5 = ['y1 = x1 + 3.0*x2 + 2.0*x3']
        deriv5 = ['dy1_dx1 = 1.0',
                  'dy1_dx2 = 3.0',
                  'dy1_dx3 = 2.0']

        self.top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        self.top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        self.top.add('comp3', ExecCompWithDerivatives(exp3, deriv3))
        self.top.add('comp4', ExecCompWithDerivatives(exp4, deriv4))
        self.top.add('comp5', ExecCompWithDerivatives(exp5, deriv5))

        self.top.driver.workflow.add(['comp1', 'comp2', 'comp3', 'comp4', 'comp5'])

        self.top.connect('comp1.y1', 'comp2.x1')
        self.top.connect('comp1.y2', 'comp3.x1')
        self.top.connect('comp2.y1', 'comp4.x1')
        self.top.connect('comp3.y1', 'comp4.x2')
        self.top.connect('comp4.y1', 'comp5.x1')
        self.top.connect('comp4.y2', 'comp5.x2')
        self.top.connect('comp4.y3', 'comp5.x3')
        
        self.top.comp1.x1 = 2.0        
        self.top.run()

        J = self.top.driver.calc_gradient(inputs=['comp1.x1'],
                                          outputs=['comp5.y1'],
                                          mode='adjoint',
                                          return_format='dict')
        #from openmdao.util.dotgraph import plot_system_tree
        #plot_system_tree(self.top._system)
        #print J
        collective_assert_rel_error(self, 
                                    J['comp5.y1']['comp1.x1'][0][0], 
                                    313.0, 0.0001)

    def test_diverge_converge_nondiff_comp3_forward(self):
        
        self.top = set_as_top(Assembly())

        exp1 = ['y1 = 2.0*x1**2',
                'y2 = 3.0*x1']
        deriv1 = ['dy1_dx1 = 4.0*x1',
                  'dy2_dx1 = 3.0']

        exp2 = ['y1 = 0.5*x1']
        deriv2 = ['dy1_dx1 = 0.5']

        exp3 = ['y1 = 3.5*x1']
        deriv3 = ['dy1_dx1 = 3.5']

        exp4 = ['y1 = x1 + 2.0*x2',
                'y2 = 3.0*x1',
                'y3 = x1*x2']
        deriv4 = ['dy1_dx1 = 1.0',
                  'dy1_dx2 = 2.0',
                  'dy2_dx1 = 3.0',
                  'dy2_dx2 = 0.0',
                  'dy3_dx1 = x2',
                  'dy3_dx2 = x1']

        exp5 = ['y1 = x1 + 3.0*x2 + 2.0*x3']
        deriv5 = ['dy1_dx1 = 1.0',
                  'dy1_dx2 = 3.0',
                  'dy1_dx3 = 2.0']
        
        self.top.add('driver', SimpleDriver())
        self.top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        self.top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        self.top.add('comp3', ExecComp(exp3))
        self.top.add('comp4', ExecCompWithDerivatives(exp4, deriv4))
        self.top.add('comp5', ExecCompWithDerivatives(exp5, deriv5))

        self.top.driver.workflow.add(['comp1', 'comp2', 'comp3', 'comp4', 'comp5'])

        self.top.connect('comp1.y1', 'comp2.x1')
        self.top.connect('comp1.y2', 'comp3.x1')
        self.top.connect('comp2.y1', 'comp4.x1')
        self.top.connect('comp3.y1', 'comp4.x2')
        self.top.connect('comp4.y1', 'comp5.x1')
        self.top.connect('comp4.y2', 'comp5.x2')
        self.top.connect('comp4.y3', 'comp5.x3')
        
        self.top.driver.add_parameter('comp1.x1', low=-100, high=100)
        self.top.driver.add_objective('comp5.y1')
        
        self.top.comp1.x1 = 2.0        
        self.top.run()

        J = self.top.driver.calc_gradient(inputs=['comp1.x1'],
                                          outputs=['comp5.y1'],
                                          mode='forward',
                                          return_format='dict')
        
        collective_assert_rel_error(self, 
                                    J['comp5.y1']['comp1.x1'][0][0], 
                                    313.0, 0.0001)

# FIXME: running this file as main currently doesn't work...
# if __name__ == '__main__':
#     import unittest
#     unittest.main()
