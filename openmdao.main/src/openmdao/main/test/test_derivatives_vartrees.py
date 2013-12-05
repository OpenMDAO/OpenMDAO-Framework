from cStringIO import StringIO
import re
import unittest

try:
    from numpy import zeros, array, identity, ones, linalg
except ImportError as err:
    from openmdao.main.numpy_fallback import zeros, array, identity

from openmdao.main.api import Component, VariableTree, Driver, Assembly, set_as_top
from openmdao.main.datatypes.api import Array, Float, VarTree
from openmdao.main.derivatives import applyJ, applyJT
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasobjective import HasObjective
from openmdao.main.hasconstraints import HasConstraints
from openmdao.main.interfaces import IHasParameters, implements
from openmdao.test.execcomp import ExecCompWithDerivatives, ExecComp
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
    x = Array([0,0])
    y = Array([0,0,0])


class DummyComp(Component): 

    x = Float(iotype="in")
    y = Float(iotype="out")

    def execute(self): 
        self.y = self.x

    def provideJ(self): 

        return ("x",),("y",),array([1])

class CompWithVarTreeSubTree(Component): 

    ins = VarTree(TreeWithSubTree(), iotype="in")
    outs = VarTree(TreeWithFloat2(), iotype="out")
    outs2 = VarTree(TreeWithSubTree(), iotype="out")
    z = Float(iotype='out')

    def execute(self): 

        self.outs.z = 2*self.ins.x.x1 + 3*self.ins.x.x2 + 4*self.ins.y
        self.z = self.outs.z

    def linearize(self): 

        self.J = ones((2,3))
        self.J[:,0] *= 2 
        self.J[:,1] *= 3
        self.J[:,2] *= 4

    def provideJ(self): 
        ins = ('ins.x.x1', 'ins.x.x2', 'ins.y')
        outs = ('outs.z','z')

        return ins, outs, self.J

class CompWithVarTree(Component): 
    x1 = Float(iotype="in")
    ins = VarTree(TreeWithFloat2(), iotype="in")
    outs = VarTree(TreeWithFloat2(), iotype="out")
    z = Float(iotype="out")

    def execute(self): 
        self.outs.z = 2*self.ins.z + 6*self.x1
        self.z = 4*self.ins.z + 6*self.x1

    def linearize(self): 
        self.J = array([[2., 6.],[4., 6.]])

    def provideJ(self): 
        return ('ins.z', 'x1'), ('outs.z', 'z'), self.J


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
        self.connect('x1','comp1.x1')
        self.connect('x2','comp1.ins.z')
        self.connect('comp1.z','z')

        self.driver.workflow.add('comp1') 

class AssemblyWithBurriedVarTree(Assembly): 

    x1 = Float(iotype="in")
    x2 = Float(iotype="in")
    z = Float(iotype="out")

    def configure(self): 
        self.add('comp0', DummyComp())
        self.connect('x1','comp0.x')

        self.add('comp1', CompWithVarTree())
        self.connect('x2', 'comp1.x1')
        self.connect('comp0.y','comp1.ins.z')
        self.connect('comp1.z','z')

        self.driver.workflow.add(['comp0','comp1']) 


class AssemblyWithConnectedVarTree(Assembly): 

    x1 = Float(iotype="in")
    x2 = Float(iotype="in")
    z = Float(iotype="out")

    def configure(self): 

        self.add('comp1', CompWithVarTree())
        self.connect('x1','comp1.x1')
        self.connect('x2','comp1.ins.z')

        self.add('comp2', CompWithVarTree())
        self.connect('comp1.outs','comp2.ins')
        self.connect('comp2.z', 'z')

        self.driver.workflow.add(['comp1','comp2']) 



class TestDerivativeVarTreeSubAssembly(unittest.TestCase): 

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

        obj = ["%s.out0" % item.pcomp_name for item in \
               top.driver.get_objectives().values()]
        con = ["%s.out0" % item.pcomp_name for item in \
               top.driver.get_constraints().values()]

        J_fd = top.driver.workflow.calc_gradient(inputs, obj+con, mode='fd')
        top.driver.workflow.config_changed()
        J_forward = top.driver.workflow.calc_gradient(inputs, obj+con, mode="forward")
        top.driver.workflow.config_changed()
        J_reverse = top.driver.workflow.calc_gradient(inputs, obj+con, mode="adjoint")

        #J_true = array([[24,8],[1,1]])

        #assert_rel_error(self, linalg.norm(J_true - J_fd), 0, .00001)
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

        obj = ["%s.out0" % item.pcomp_name for item in \
               top.driver.get_objectives().values()]
        con = ["%s.out0" % item.pcomp_name for item in \
               top.driver.get_constraints().values()]

        J_fd = top.driver.workflow.calc_gradient(inputs, obj+con, mode='fd')
        top.driver.workflow.config_changed()
        J_forward = top.driver.workflow.calc_gradient(inputs, obj+con, mode="forward")
        top.driver.workflow.config_changed()
        J_reverse = top.driver.workflow.calc_gradient(inputs, obj+con, mode="adjoint")

        #J_true = array([[24,8],[1,1]])

        #assert_rel_error(self, linalg.norm(J_true - J_fd), 0, .00001)
        assert_rel_error(self, linalg.norm(J_fd - J_forward), 0, .00001)
        assert_rel_error(self, linalg.norm(J_fd - J_reverse), 0, .00001)

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
    #     J_fd = top.driver.workflow.calc_gradient(inputs, obj+con, mode='fd')
    #     top.driver.workflow.config_changed()
    #     J_forward = top.driver.workflow.calc_gradient(inputs, obj+con, mode="forward")
    #     top.driver.workflow.config_changed()
    #     J_reverse = top.driver.workflow.calc_gradient(inputs, obj+con, mode="adjoint")


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

        obj = ["%s.out0" % item.pcomp_name for item in \
               top.driver.get_objectives().values()]
        con = ["%s.out0" % item.pcomp_name for item in \
               top.driver.get_constraints().values()]

        top.run()
        J_fd = top.driver.workflow.calc_gradient(inputs, obj+con, mode='fd')
        top.driver.workflow.config_changed()
        J_forward = top.driver.workflow.calc_gradient(inputs, obj+con, mode="forward")
        top.driver.workflow.config_changed()
        J_reverse = top.driver.workflow.calc_gradient(inputs, obj+con, mode="adjoint")
        
        J_true = array([[2., 3., 4.], #obj
                        [2., 3., 4.], #c1 
                        [1., 1., 0.]]) #c2

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
        J_forward = top.driver.workflow.calc_gradient(mode="forward")
        top.driver.workflow.config_changed()
        J_reverse = top.driver.workflow.calc_gradient(mode="adjoint")
        top.driver.workflow.config_changed()
        J_fd = top.driver.workflow.calc_gradient(mode='fd')
        
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
        J_true = array([[8., 12., 16.],  #obj
                        [3., 3., 4.]]) #c1 
                       
        obj = ["%s.out0" % item.pcomp_name for item in \
               top.driver.get_objectives().values()]
        con = ["%s.out0" % item.pcomp_name for item in \
               top.driver.get_constraints().values()]

        
        top.driver.workflow.config_changed()
        J_fd = top.driver.workflow.calc_gradient(inputs, obj+con, mode='fd')
        top.driver.workflow.config_changed()
        J_forward = top.driver.workflow.calc_gradient(inputs, obj+con, mode="forward")
        top.driver.workflow.config_changed()
        J_reverse = top.driver.workflow.calc_gradient(inputs, obj+con, mode="adjoint")


        assert_rel_error(self, linalg.norm(J_true - J_fd), 0, .00001)
        assert_rel_error(self, linalg.norm(J_true - J_forward), 0, .00001)
        assert_rel_error(self, linalg.norm(J_true - J_reverse), 0, .00001)

        
if __name__ == "__main__": 
    unittest.main()
