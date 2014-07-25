
import unittest

from numpy import array

from openmdao.main.api import Component, Driver, Assembly, set_as_top
from openmdao.main.datatypes.api import Float
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasobjective import HasObjective
from openmdao.main.hasconstraints import HasConstraints
from openmdao.main.interfaces import IHasParameters, implements
from openmdao.main.vecwrapper import idx_merge
from openmdao.util.decorators import add_delegate
from openmdao.util.testutil import assert_rel_error

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

        self.J = array([[df_dx, df_dy]])
        return self.J

    def list_deriv_vars(self):
        input_keys = ('x', 'y')
        output_keys = ('f_xy',)
        return input_keys, output_keys


@add_delegate(HasParameters, HasObjective, HasConstraints)
class SimpleDriver(Driver):
    """Driver with Parameters"""

    implements(IHasParameters)


class TestcaseParaboloid(unittest.TestCase):
    def setUp(self):
        self.top = top = set_as_top(Assembly())
        top.add('comp', Paraboloid())
        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['comp'])

    def test_single_comp(self):
        top = self.top

        top.driver.add_parameter('comp.x', low=-1000, high=1000)
        top.driver.add_parameter('comp.y', low=-1000, high=1000)
        top.driver.add_objective('comp.f_xy')
        top.comp.x = 3
        top.comp.y = 5
        #top.run()
        top._setup()

        # test some System stuff first
        drvsys = top._system.find('driver')
        wfsys = drvsys.find("('comp', '_pseudo_0')")
        compsys = wfsys.find("comp")
        pseudosys = wfsys.find("_pseudo_0")

        self.assertEqual(compsys._in_nodes, [('comp.x', ('comp.x',)), ('comp.y', ('comp.y',))])
        self.assertEqual(compsys.get_inputs(), ['comp.x', 'comp.y'])
        self.assertEqual(compsys._out_nodes, [('comp.f_xy', ('_pseudo_0.in0',))])
        self.assertEqual(compsys.get_outputs(), ['comp.f_xy'])
        self.assertEqual(compsys._owned_args, [('comp.x', ('comp.x',)), ('comp.y', ('comp.y',))])

        self.assertEqual(compsys.variables.keys(), [('comp.f_xy', ('_pseudo_0.in0',)),
                                                    ('comp.x', ('comp.x',)),
                                                    ('comp.y', ('comp.y',))])

        self.assertEqual(compsys.scatter_full.scatter_conns, 
                         [('comp.x', ('comp.x',)), ('comp.y', ('comp.y',))])
        self.assertTrue(all(compsys.scatter_full.var_idxs ==
                        compsys.vec['u'].multi_indices(compsys._in_nodes)))
        self.assertTrue(all(compsys.scatter_full.input_idxs ==
                         compsys.vec['p'].multi_indices(compsys._in_nodes)))
        
        self.assertEqual(pseudosys._in_nodes, [('comp.f_xy', ('_pseudo_0.in0',))])
        self.assertEqual(pseudosys.get_inputs(), ['_pseudo_0.in0'])
        self.assertEqual(pseudosys._out_nodes, [('_pseudo_0.out0', ('_pseudo_0.out0',))])
        self.assertEqual(pseudosys.get_outputs(), ['_pseudo_0.out0'])
        self.assertEqual(pseudosys._owned_args, [])

        self.assertEqual(pseudosys.variables.keys(), [('_pseudo_0.out0', ('_pseudo_0.out0',))])

        self.assertEqual(wfsys._in_nodes, [('comp.x', ('comp.x',)), ('comp.y', ('comp.y',))])
        self.assertEqual(wfsys.get_inputs(), ['comp.x', 'comp.y', '_pseudo_0.in0'])
        self.assertEqual(wfsys._out_nodes, [('_pseudo_0.out0', ('_pseudo_0.out0',)),
                                            ('comp.f_xy', ('_pseudo_0.in0',))])
        self.assertEqual(wfsys.get_outputs(), ['comp.f_xy', '_pseudo_0.out0'])
        self.assertEqual(wfsys._owned_args, [('comp.f_xy', ('_pseudo_0.in0',))])

        self.assertEqual(wfsys.variables.keys(), [('comp.f_xy', ('_pseudo_0.in0',)),
                                                  ('comp.x', ('comp.x',)),
                                                  ('comp.y', ('comp.y',)),
                                                  ('_pseudo_0.out0', ('_pseudo_0.out0',))])
        
        self.assertEqual(drvsys._in_nodes, [])
        self.assertEqual(drvsys.get_inputs(), ['comp.x', 'comp.y', '_pseudo_0.in0'])
        self.assertEqual(drvsys._out_nodes, [('_pseudo_0.out0', ('_pseudo_0.out0',)),
                                            ('comp.f_xy', ('_pseudo_0.in0',)),
                                            ('comp.x', ('comp.x',)),
                                            ('comp.y', ('comp.y',))])
        self.assertEqual(drvsys.get_outputs(), ['comp.f_xy', '_pseudo_0.out0'])
        self.assertEqual(drvsys._owned_args, [('comp.f_xy', ('_pseudo_0.in0',))])
        self.assertEqual([0], drvsys.arg_idx[('comp.f_xy', ('_pseudo_0.in0',))])

        self.assertEqual(drvsys.variables.keys(), [('comp.f_xy', ('_pseudo_0.in0',)),
                                                   ('comp.x', ('comp.x',)),
                                                   ('comp.y', ('comp.y',)),
                                                   ('_pseudo_0.out0', ('_pseudo_0.out0',))
                                                   ])
        top.run()
        
        # See if model gets the right answer
        self.assertEqual(top.comp.f_xy, 93.)
        self.assertEqual(top._pseudo_0.in0, 93.)
        self.assertEqual(top._pseudo_0.out0, 93.)

    def test_boundary_out(self):
        top = self.top

        top.driver.add_parameter('comp.x', low=-1000, high=1000)
        top.driver.add_parameter('comp.y', low=-1000, high=1000)
        top.driver.add_objective('comp.f_xy')

        top.create_passthrough('comp.f_xy')

        top.comp.x = 3
        top.comp.y = 5

        top.run()

        # See if model gets the right answer
        self.assertEqual(top.f_xy, 93.)

    def test_boundary_in_out(self):
        top = self.top

        top.driver.add_parameter('comp.y', low=-1000, high=1000)
        top.driver.add_objective('comp.f_xy')

        top.create_passthrough('comp.x')
        top.create_passthrough('comp.f_xy')

        top.x = 3
        top.comp.y = 5

        top.run()

        # See if model gets the right answer
        self.assertEqual(top.f_xy, 93.)
