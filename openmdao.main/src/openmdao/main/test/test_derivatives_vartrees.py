import unittest

from numpy import array, ones, linalg

from openmdao.main.api import Component, VariableTree, Driver, Assembly, set_as_top
from openmdao.main.datatypes.api import Array, Float, VarTree
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasobjective import HasObjective
from openmdao.main.hasconstraints import HasConstraints
from openmdao.main.interfaces import IHasParameters, implements
from openmdao.util.decorators import add_delegate
from openmdao.util.testutil import assert_rel_error


class TreeWithFloat(VariableTree):
    x1 = Float()
    x2 = Float()


class TreeWithSubTree(VariableTree):
    y = Float(3.)
    x = VarTree(TreeWithFloat())


class TreeWithFloat2(VariableTree):
    z = Float(3.)


class TreeWithArray(VariableTree):
    x = Array(array((1.0, 2.0)))


class DummyComp(Component):
    x = Float(iotype="in")
    y = Float(iotype="out")

    def execute(self):
        self.y = self.x

    def provideJ(self):
        return array([[1]])

    def list_deriv_vars(self):
        return ("x",), ("y",)


class CompWithVarTreeSubTree(Component):
    ins = VarTree(TreeWithSubTree(), iotype="in")
    outs = VarTree(TreeWithFloat2(), iotype="out")
    outs2 = VarTree(TreeWithSubTree(), iotype="out")
    z = Float(iotype='out')

    def execute(self):
        self.outs.z = 2*self.ins.x.x1 + 3*self.ins.x.x2 + 4*self.ins.y
        self.z = self.outs.z

    def provideJ(self):
        self.J = ones((2, 3))
        self.J[:, 0] *= 2
        self.J[:, 1] *= 3
        self.J[:, 2] *= 4
        return self.J

    def list_deriv_vars(self):
        ins  = ('ins.x.x1', 'ins.x.x2', 'ins.y')
        outs = ('outs.z', 'z')

        return ins, outs


class CompWithVarTree(Component):
    x1 = Float(iotype="in")
    ins = VarTree(TreeWithFloat2(), iotype="in")
    outs = VarTree(TreeWithFloat2(), iotype="out")
    z = Float(iotype="out")

    def execute(self):
        self.outs.z = 2*self.ins.z + 6*self.x1
        self.z = 4*self.ins.z + 6*self.x1

    def provideJ(self):
        self.J = array([[2., 6.], [4., 6.]])
        return self.J

    def list_deriv_vars(self):
        return ('ins.z', 'x1'), ('outs.z', 'z')


class CompWithArrayVarTree(Component):
    ins = VarTree(TreeWithArray(), iotype="in")
    outs = VarTree(TreeWithArray(), iotype="out")

    def execute(self):
        self.outs.x[0] = 2*self.ins.x[0] + 6*self.ins.x[1]
        self.outs.x[1] = 4*self.ins.x[0] + 6*self.ins.x[1]

    def provideJ(self):
        self.J = array([[2., 6.], [4., 6.]])
        return self.J

    def list_deriv_vars(self):
        return ('ins.x',), ('outs.x',)


class DummyCompVarTree(Component):
    ins = VarTree(TreeWithFloat(), iotype="in")
    y = Float(iotype="out")

    def execute(self):
        self.y = self.ins.x1

    def provideJ(self):
        return array([[1]])

    def list_deriv_vars(self):
        return ("ins.x1",), ("y",)


class AssemblyWrapperDummyCompVarTree(Assembly):

    def configure(self):
        self.add('d1', DummyCompVarTree())
        self.create_passthrough('d1.ins')
        self.create_passthrough('d1.y')

        self.driver.workflow.add('d1')


class CompWithVarTreeMissingDeriv(Component):
    x1 = Float(iotype="in")
    outs = VarTree(TreeWithFloat(), iotype="out")
    z = Float(iotype="out")

    def execute(self):
        self.outs.x1 = 6*self.x1**2
        self.outs.x2 = 10.0
        self.z = 7.0

    def provideJ(self):
        self.J = array([[12*self.x1, ]])
        return self.J

    def list_deriv_vars(self):
        return ('x1',), ('outs.x1',)


@add_delegate(HasParameters, HasObjective, HasConstraints)
class SimpleDriver(Driver):
    """Driver with Parameters"""
    implements(IHasParameters)


class AssemblyWithCompVarTree(Assembly):

    x1 = Float(iotype="in")
    x2 = Float(iotype="in")
    z = Float(iotype="out")

    def configure(self):
        self.add('comp1', CompWithVarTree())
        self.connect('x1', 'comp1.x1')
        self.connect('x2', 'comp1.ins.z')
        self.connect('comp1.z', 'z')

        self.driver.workflow.add('comp1')


class AssemblyWithBoundryVarTree(Assembly):

    def configure(self):
        self.add('comp1', DummyCompVarTree())
        self.create_passthrough('comp1.ins')
        self.create_passthrough('comp1.y')

        self.driver.workflow.add('comp1')


class AssemblyWithBurriedVarTree(Assembly):
    x1 = Float(iotype="in")
    x2 = Float(iotype="in")
    z = Float(iotype="out")

    def configure(self):
        self.add('comp0', DummyComp())
        self.connect('x1', 'comp0.x')

        self.add('comp1', CompWithVarTree())
        self.connect('x2', 'comp1.x1')
        self.connect('comp0.y', 'comp1.ins.z')
        self.connect('comp1.z', 'z')

        self.driver.workflow.add(['comp0', 'comp1'])


class AssemblyWithConnectedVarTree(Assembly):
    x1 = Float(iotype="in")
    x2 = Float(iotype="in")
    z = Float(iotype="out")

    def configure(self):
        self.add('comp1', CompWithVarTree())
        self.connect('x1', 'comp1.x1')
        self.connect('x2', 'comp1.ins.z')

        self.add('comp2', CompWithVarTree())
        self.connect('comp1.outs', 'comp2.ins')
        self.connect('comp2.z', 'z')

        self.driver.workflow.add(['comp1', 'comp2'])


class TestDerivativeVarTreeSubAssembly(unittest.TestCase):

    def test_varTree_on_boundary_subassembly(self):
        top = set_as_top(Assembly())
        top.add('comp', AssemblyWithBoundryVarTree())
        top.add('driver', SimpleDriver())
        top.driver.workflow.add('comp')
        top.driver.add_parameter('comp.ins.x1', low=-100, high=100)
        top.driver.add_objective('comp.y')
        top.comp.comp1.missing_deriv_policy = 'assume_zero'

        top.run()

        # check for invalidation problems
        top.comp.ins.x1 = 123.4
        top.run()
        self.assertEqual(top.comp.ins.x1, top.comp.comp1.ins.x1)

        #print top.comp.driver.calc_gradient(['ins.x1'], ['y'], mode='fd')
        inputs = ['comp.ins.x1', ]
        outputs = ['comp.y']
        J_fd = top.driver.calc_gradient(inputs, outputs, mode='fd')
        J_forward = top.driver.calc_gradient(inputs, outputs, mode="forward")
        J_reverse = top.driver.calc_gradient(inputs, outputs, mode="adjoint")

        assert_rel_error(self, linalg.norm(J_fd - J_forward), 0, .00001)
        assert_rel_error(self, linalg.norm(J_fd - J_reverse), 0, .00001)

        # Piggyback testing of the is_variable_local function -- make sure it
        # correctly identifies boundary vartrees.
        system = top.comp.driver.workflow._system
        self.assertTrue(system.is_variable_local('ins.x1') is True)


    def test_varTree_in_subassembly(self):
        top = set_as_top(Assembly())
        top.add('comp', AssemblyWithCompVarTree())
        top.add('driver', SimpleDriver())
        top.driver.workflow.add('comp')
        top.driver.add_parameter('comp.x1', low=-100, high=100)
        top.driver.add_parameter('comp.x2', low=-100, high=100)
        top.driver.add_objective('comp.z')
        top.driver.add_constraint('comp.x2 + comp.x1 < 10')

        top.run()

        inputs = top.driver.list_param_group_targets()

        obj = ["%s.out0" % item.pcomp_name for item in
               top.driver.get_objectives().values()]
        con = ["%s.out0" % item.pcomp_name for item in
               top.driver.get_constraints().values()]

        J_fd = top.driver.calc_gradient(inputs, obj+con, mode='fd')
        J_forward = top.driver.calc_gradient(inputs, obj+con, mode="forward")
        J_reverse = top.driver.calc_gradient(inputs, obj+con, mode="adjoint")

        J_true = array([[6, 4], [1, 1]])

        assert_rel_error(self, linalg.norm(J_true - J_fd), 0, .00001)
        assert_rel_error(self, linalg.norm(J_fd - J_forward), 0, .00001)
        assert_rel_error(self, linalg.norm(J_fd - J_reverse), 0, .00001)

    def test_connected_varTree_in_subassembly(self):
        top = set_as_top(Assembly())
        top.add('comp', AssemblyWithConnectedVarTree())
        top.add('driver', SimpleDriver())
        top.driver.workflow.add('comp')
        top.driver.add_parameter('comp.x1', low=-100, high=100)
        top.driver.add_parameter('comp.x2', low=-100, high=100)
        top.driver.add_objective('comp.z')
        top.driver.add_constraint('comp.x2 + comp.x1 < 10')

        top.run()

        inputs = top.driver.list_param_group_targets()

        obj = ["%s.out0" % item.pcomp_name for item in
               top.driver.get_objectives().values()]
        con = ["%s.out0" % item.pcomp_name for item in
               top.driver.get_constraints().values()]

        J_fd = top.driver.calc_gradient(inputs, obj+con, mode='fd')
        J_forward = top.driver.calc_gradient(inputs, obj+con, mode="forward")
        J_reverse = top.driver.calc_gradient(inputs, obj+con, mode="adjoint")

        J_true = array([[24, 8], [1, 1]])

        assert_rel_error(self, linalg.norm(J_true - J_fd), 0, .00001)
        assert_rel_error(self, linalg.norm(J_fd - J_forward), 0, .00001)
        assert_rel_error(self, linalg.norm(J_fd - J_reverse), 0, .00001)

    def test_vartree_subassy_behavior(self):

        class tree(VariableTree):
            x = Float(3.0)
            y = Float(4.0)
            z = Float(5.0)

        class MyComp(Component):
            ins = VarTree(tree(), iotype='in')
            outs = VarTree(tree(), iotype='out')

            def execute(self):
                self.outs = self.ins.copy()
                self.outs.z = 2.0*self.ins.x

        top = set_as_top(Assembly())
        top.add('sub', Assembly())
        top.driver.workflow.add('sub')
        top.sub.add('comp1', MyComp())
        top.sub.add('comp2', MyComp())
        top.sub.driver.workflow.add(['comp1', 'comp2'])
        top.sub.create_passthrough('comp1.ins')
        top.sub.create_passthrough('comp1.outs')
        top.sub.create_passthrough('comp2.outs', 'zzz')
        top.sub.connect('ins.z', 'comp2.ins.x')

        top.run()

        J = top.driver.calc_gradient(['sub.ins.x'], ['sub.outs.z'])
        assert_rel_error(self, J[0, 0], 2.0, .00001)

        J = top.driver.calc_gradient(['sub.ins.z'], ['sub.zzz.z'])
        print J

    def test_connected_varTree_in_subassembly_replace(self):
        top = set_as_top(Assembly())
        top.add('comp', AssemblyWithConnectedVarTree())
        top.add('driver', SimpleDriver())
        top.driver.workflow.add('comp')
        top.driver.add_parameter('comp.x1', low=-100, high=100)
        top.driver.add_parameter('comp.x2', low=-100, high=100)
        top.driver.add_objective('comp.z')
        top.driver.add_constraint('comp.x2 + comp.x1 < 10')

        top.run()

        top.replace('comp', AssemblyWithCompVarTree())

        top.run()


class TestDerivativeVarTree(unittest.TestCase):

    # def test_varTree_parameter2(self):
    #     top = set_as_top(Assembly())
    #     top.add('comp', CompWithVarTreeSubTree())
    #     top.add('comp2', CompWithVarTreeSubTree())
    #     top.add('driver', SimpleDriver())
    #     top.driver.workflow.add(['comp','comp2'])
    #     top.connect('comp.outs2', 'comp2.ins')
    #     top.driver.add_parameter('comp.ins.x.x1', low=-1000, high=1000)
    #     top.driver.add_parameter('comp.ins.x.x2', low=-1000, high=1000)
    #     top.driver.add_parameter('comp.ins.y', low=-1000, high=1000)

    #     top.driver.add_objective('comp.z')
    #     top.driver.add_constraint('comp.outs.z < 0')
    #     top.driver.add_constraint('(comp.ins.x.x1 +  comp.ins.x.x2) < 0')

    #     top.comp.ins.x.x1 = 3
    #     top.comp.ins.x.x2 = 3
    #     top.comp.ins.y = 5
    #     top.comp.run()

    #     inputs = top.driver.list_param_group_targets()

    #     obj = ["%s.out0" % item.pcomp_name for item in \
    #            top.driver.get_objectives().values()]
    #     con = ["%s.out0" % item.pcomp_name for item in \
    #            top.driver.get_constraints().values()]

    #     top.run()
    #     J_fd = top.driver.calc_gradient(inputs, obj+con, mode='fd')
    #     J_forward = top.driver.calc_gradient(inputs, obj+con, mode="forward")
    #     J_reverse = top.driver.calc_gradient(inputs, obj+con, mode="adjoint")

    def test_varTree_parameter(self):
        top = set_as_top(Assembly())
        top.add('comp', CompWithVarTreeSubTree())
        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['comp'])
        top.driver.add_parameter('comp.ins.x.x1', low=-1000, high=1000)
        top.driver.add_parameter('comp.ins.x.x2', low=-1000, high=1000)
        top.driver.add_parameter('comp.ins.y', low=-1000, high=1000)

        top.driver.add_objective('comp.z')
        top.driver.add_constraint('comp.outs.z < 0')
        top.driver.add_constraint('(comp.ins.x.x1 +  comp.ins.x.x2) < 0')

        top.comp.ins.x.x1 = 3
        top.comp.ins.x.x2 = 3
        top.comp.ins.y = 5
        top.comp.run()

        inputs = top.driver.list_param_group_targets()

        obj = ["%s.out0" % item.pcomp_name for item in
               top.driver.get_objectives().values()]
        con = ["%s.out0" % item.pcomp_name for item in
               top.driver.get_constraints().values()]

        top.run()
        J_fd = top.driver.calc_gradient(inputs, obj+con, mode='fd')
        J_forward = top.driver.calc_gradient(inputs, obj+con, mode="forward")
        J_reverse = top.driver.calc_gradient(inputs, obj+con, mode="adjoint")

        J_true = array([[2., 3., 4.],   # obj
                        [2., 3., 4.],   # c1
                        [1., 1., 0.]])  # c2

        assert_rel_error(self, linalg.norm(J_true - J_fd), 0, .00001)
        assert_rel_error(self, linalg.norm(J_true - J_forward), 0, .00001)
        assert_rel_error(self, linalg.norm(J_true - J_reverse), 0, .00001)

    def test_check_deriv_vartrees(self):
        top = set_as_top(Assembly())
        top.add('comp', CompWithVarTreeSubTree())
        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['comp'])
        top.driver.add_parameter('comp.ins.x.x1', low=-1000, high=1000)
        top.driver.add_parameter('comp.ins.x.x2', low=-1000, high=1000)
        top.driver.add_parameter('comp.ins.y', low=-1000, high=1000)

        top.driver.add_objective('comp.z')

        top.comp.ins.x.x1 = 3
        top.comp.ins.x.x2 = 3
        top.comp.ins.y = 5
        top.run()

        # Not sure the point of this test, unless the output will be verified.
        J_forward = top.driver.calc_gradient(mode="forward")
        J_reverse = top.driver.calc_gradient(mode="adjoint")
        J_fd = top.driver.calc_gradient(mode='fd')

        J_true = array([[2, 3, 4]])

        assert_rel_error(self, linalg.norm(J_true - J_fd), 0, .00001)
        assert_rel_error(self, linalg.norm(J_true - J_forward), 0, .00001)
        assert_rel_error(self, linalg.norm(J_true - J_reverse), 0, .00001)

    def test_varTree_connections_whole_tree(self):
        top = set_as_top(Assembly())
        top.add('driver', SimpleDriver())
        top.add('comp1', CompWithVarTreeSubTree())
        top.add('comp2', CompWithVarTree())
        top.driver.workflow.add(['comp1', 'comp2'])

        inputs = ['comp1.ins.x.x1', 'comp1.ins.x.x2', 'comp1.ins.y']
        outputs = ['comp2.z', 'comp1.outs.z', 'comp1.ins.x.x1']

        top.driver.add_parameter('comp1.ins.x.x1', low=-1000, high=1000)
        top.driver.add_parameter('comp1.ins.x.x2', low=-1000, high=1000)
        top.driver.add_parameter('comp1.ins.y', low=-1000, high=1000)

        top.connect('comp1.outs', 'comp2.ins')

        top.driver.add_objective('comp2.z')
        top.driver.add_constraint('comp1.outs.z+comp1.ins.x.x1 < 0')

        top.run()
        J_true = array([[8., 12., 16.],  # obj
                        [3., 3., 4.]])   # c1

        obj = ["%s.out0" % item.pcomp_name for item in
               top.driver.get_objectives().values()]
        con = ["%s.out0" % item.pcomp_name for item in
               top.driver.get_constraints().values()]

        J_fd = top.driver.calc_gradient(inputs, obj+con, mode='fd')
        J_forward = top.driver.calc_gradient(inputs, obj+con, mode="forward")
        J_reverse = top.driver.calc_gradient(inputs, obj+con, mode="adjoint")

        assert_rel_error(self, linalg.norm(J_true - J_fd), 0, .00001)
        assert_rel_error(self, linalg.norm(J_true - J_forward), 0, .00001)
        assert_rel_error(self, linalg.norm(J_true - J_reverse), 0, .00001)

    def test_varTree_with_Array(self):
        top = set_as_top(Assembly())
        top.add('driver', SimpleDriver())
        top.add('comp', CompWithArrayVarTree())
        top.driver.workflow.add(['comp'])

        top.run()
        inputs = ['comp.ins.x']
        outputs = ['comp.outs.x']

        J = top.driver.calc_gradient(inputs, outputs, mode="forward")
        J_true = top.comp.J
        assert_rel_error(self, linalg.norm(J_true - J), 0, .00001)

        J = top.driver.calc_gradient(inputs, outputs, mode="adjoint")
        assert_rel_error(self, linalg.norm(J_true - J), 0, .00001)

        J = top.driver.calc_gradient(inputs, outputs, mode='fd')
        assert_rel_error(self, linalg.norm(J_true - J), 0, .00001)

    def test_vartree_missing_derivs_error(self):
        top = set_as_top(Assembly())

        top.add('driver', SimpleDriver())
        top.add('dis1', CompWithVarTreeMissingDeriv())
        top.add('dis2', DummyCompVarTree())

        top.dis1.missing_deriv_policy = 'error'
        top.dis2.missing_deriv_policy = 'error'

        top.connect('dis1.outs', 'dis2.ins')

        top.driver.add_objective('(dis2.y)**2')
        top.driver.add_parameter('dis1.x1', low=-10.0, high=10.0)
        top.driver.add_constraint('dis1.outs.x2 < 24.0')  # missing derivative

        top.run()

        try:
            J = top.driver.calc_gradient(mode='forward')
        except Exception as err:
            self.assertEqual(str(err), "'dis1 (1-dis1): does not provide analytical derivatives for outs.x2'")
        else:
            self.fail("exception expected")

        top.disconnect('dis1.outs', 'dis2.ins')
        top.driver.remove_constraint('dis1.outs.x2 < 24.0')
        top.driver.add_constraint('dis2.ins.x2 < 5')
        top.driver.remove_parameter('dis1.x1')
        top.driver.add_parameter('dis2.ins.x2', low=-10.0, high=10.0)

        try:
            J = top.driver.calc_gradient(mode='forward')
        except Exception as err:
            self.assertEqual(str(err), "'dis2 (1-dis2): does not provide analytical derivatives for ins.x2'")
        else:
            self.fail("exception expected")

    def test_vartree_missing_derivs_zero(self):
        top = set_as_top(Assembly())

        top.add('driver', SimpleDriver())
        top.add('dis1', CompWithVarTreeMissingDeriv())
        top.add('dis2', DummyCompVarTree())

        top.dis1.missing_deriv_policy = 'assume_zero'
        top.dis2.missing_deriv_policy = 'assume_zero'

        top.connect('dis1.outs', 'dis2.ins')

        top.driver.add_objective('(dis2.y)**2')
        top.driver.add_parameter('dis1.x1', low=-10.0, high=10.0)
        top.driver.add_constraint('dis1.outs.x2 < 24.0')  # missing derivative

        top.run()

        J_forward = top.driver.calc_gradient(mode='forward')
        J_fd = top.driver.calc_gradient(mode='forward')

        assert_rel_error(self, linalg.norm(J_forward - J_fd), 0, .00001)

        # self.top.driver.remove_constraint('dis1.outs.x1 < 24.0')
        # self.top.driver.add_constraint('dis2.ins.x2 < 5')
        # self.top.driver.remove_parameter('dis1.x')
        # self.top.driver.add_parameter('dis1.ins.x2', low=-10.0, high=10.0)

    def test_graph_pruning(self):
        top = set_as_top(Assembly())
        top.add('comp1', CompWithVarTreeSubTree())
        top.add('comp2', DummyComp())
        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['comp1', 'comp2'])

        top.connect('comp1.z',  'comp2.x')
        top.driver.add_parameter('comp1.ins.x.x1', low=-100, high=100)
        top.driver.add_constraint('comp1.ins.x.x2 + comp2.y < 0')

        top.run()
        J = top.driver.calc_gradient()
        assert_rel_error(self, J[0, 0], 2.0, .00001)


if __name__ == "__main__":
    unittest.main()
