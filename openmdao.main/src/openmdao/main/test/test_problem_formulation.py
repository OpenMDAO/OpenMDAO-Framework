import unittest
import ordereddict
from openmdao.main.api import Component, Assembly, Driver, \
                              Architecture, set_as_top
from openmdao.main.expreval import ExprEvaluator
from openmdao.main.problem_formulation import ArchitectureAssembly, HasCouplingVars
from openmdao.main.hasconstraints import HasConstraints
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasobjective import HasObjectives

from openmdao.lib.datatypes.api import Float, Int, Enum, Array
from openmdao.lib.drivers.api import CONMINdriver

from openmdao.util.decorators import add_delegate

@add_delegate(HasCouplingVars, HasObjectives, HasConstraints)
class GlobalAssembly(ArchitectureAssembly): 
    pass

class Dummy(Component): 
    a = Float(iotype="in")
    b = Float(iotype="in")
    x = Float(iotype="out")
    y = Float(iotype="out")
    i = Int(iotype="in")
    j = Int(iotype="out")
    farr = Array([1.1, 2.2, 3.3])
    iarr = Array([1,2,3])
    en = Enum(values=['foo','bar','baz'], iotype='in')
    
    
class DummyArchitecture(Architecture):
    def configure(self):
        pass


class ProblemFormulationTest(unittest.TestCase):
    
    def setUp(self): 
        self.asm = set_as_top(GlobalAssembly())
        self.asm.add("D1",Dummy())
        self.asm.add("D2",Dummy()) 
        self.asm.add("D3",Dummy()) 
        self.asm.add("D4",Dummy()) 
        self.asm.add("D5",Dummy()) 
        self.asm.add("D6",Dummy()) 
        
    def test_get_global_des_vars_by_comp(self): 
        self.asm.add_parameter(('D1.a','D2.a','D2.b'),0,1e99)
        
        data = self.asm.get_global_des_vars_by_comp()
        
        self.assertEqual(set(data.keys()),set(['D1','D2']))
        self.assertEqual(set([param.target for param in data['D1']]),set(['D1.a']))  
        self.assertEqual(set([param.target for param in data['D2']]),set(['D2.a','D2.b']))  
         
        
    def test_coupling_vars(self): 
        c1 = self.asm.add_coupling_var("D1.a","D2.a")
        c2 = self.asm.add_coupling_var("D4.a","D5.a")
        c3 = self.asm.add_coupling_var("D6.a","D5.b")
        
        try: 
            self.asm.add_coupling_var("D1.a","D2.a")
        except Exception as err:
            self.assertEqual(": Coupling variable with indep 'D1.a' already exists in assembly",str(err))
        else: 
            self.fail("Exception expected")
        
        try: 
            self.asm.add_coupling_var("D3.a","D2.a")
        except Exception as err:         
            self.assertEqual(": Coupling variable with dep 'D2.a' already exists in assembly",str(err))
        else: 
            self.fail("Exception expected")            
            
        try: 
            self.asm.add_coupling_var("D1.z","D2.a")
        except Exception as err: 
            self.assertEqual(": Cant add coupling variable with indep 'D1.z' because is not a valid variable",str(err))
        else: 
            self.fail("Exception expected")
            
            
        self.assertEqual([c1,c2,c3],
                         self.asm.get_coupling_vars())
        
        self.asm.remove_coupling_var(('D1.a','D2.a'))
        self.assertEqual([c2,c3],
                         self.asm.get_coupling_vars())
        try: 
            self.asm.remove_coupling_var(('D1.a','D2.a'))
        except Exception as err: 
            self.assertEqual(": No coupling variable of ('D1.a','D2.a') exists in assembly",str(err))
        else: 
            self.fail("Exception expected")
            
        self.asm.architecture = DummyArchitecture()
        self.asm.architecture.has_coupling_vars = True
        self.asm.check_config()
        
        self.asm.architecture.has_coupling_vars = False
        try:
            self.asm.check_config()
        except Exception as err:
            self.assertEqual(str(err), "this Architecture doesn't support coupling variables")
        else:
            self.fail("Exception expected")
            
        self.asm.add_coupling_var("D1.a","D2.a")
        self.asm.clear_coupling_vars()
        self.assertEqual([],self.asm.get_coupling_vars())
        
    def test_double_set_arch(self):
        self.asm.architecture = DummyArchitecture()
        # no exception expected since arch isn'g configured yet
        self.asm.architecture = DummyArchitecture()
        self.asm.configure()
        arch = self.asm.architecture
        try:
            self.asm.architecture = DummyArchitecture()
        except RuntimeError as err:
            self.assertEqual(str(err), ": This Assembly was already configured with another architecture.")
        else:
            self.fail("Exception expected")
        self.assertEqual(arch, self.asm.architecture)
        
    def test_check_config_params(self):
        self.asm.architecture = arch = DummyArchitecture()
        
        arch.param_types = ['continuous']
        self.asm.add_parameter("D1.a", low=0.1, high=9.9)
        self.asm.check_config()
        self.asm.add_parameter("D1.i", low=0, high=9)
        try:
            self.asm.check_config()
        except Exception as err:
            self.assertEqual(str(err), "this Architecture doesn't support the following parameter types: ['discrete']")
        else:
            self.fail("Exception expected")
            
        arch.param_types.append('discrete')
        self.asm.check_config()
        
        self.asm.add_parameter("D1.en")
        try:
            self.asm.check_config()
        except Exception as err:
            self.assertEqual(str(err), "this Architecture doesn't support the following parameter types: ['enum']")
        else:
            self.fail("Exception expected")

        arch.param_types.append('enum')
        self.asm.check_config()
        
        # now look at array entries
        self.asm.clear_parameters()
        arch.param_types = ['continuous']
        self.asm.add_parameter("D1.farr[1]", low=0.1, high=9.9)
        self.asm.check_config()
        self.asm.add_parameter("D1.iarr[2]", low=0, high=9)
        try:
            self.asm.check_config()
        except Exception as err:
            self.assertEqual(str(err), "this Architecture doesn't support the following parameter types: ['discrete']")
        else:
            self.fail("Exception expected")
            
        try:
            arch.param_types = ['eq', 'continuous', 'blah']
        except Exception as err:
            self.assertEqual(str(err), "the following parameter types are "
                             "invalid: ['blah', 'eq']. Allowed values are: ['discrete', 'enum', 'continuous']")
        else:
            self.fail("Exception expected")
            
        arch.param_types = None
        try:
            self.asm.check_config()
        except RuntimeError as err:
            self.assertEqual(str(err), "this Architecture doesn't support parameters, "
                             "but parameter types ['discrete', 'continuous'] were found "
                             "in parent")
        else:
            self.fail("RuntimeError expected")

        
    def test_check_config_constraints(self):
        self.asm.architecture = arch = DummyArchitecture()
        
        arch.constraint_types = ['eq']
        self.asm.add_constraint("D1.x = D2.y")
        self.asm.check_config()
        
        self.asm.add_constraint("D1.x < D2.y")
        try:
            self.asm.check_config()
        except Exception as err:
            self.assertEqual(str(err), "this Architecture doesn't support the "
                             "following constraint types: ['ineq']")
        else:
            self.fail("Exception expected")
        arch.constraint_types = ['eq', 'ineq']
        self.asm.check_config()

        self.asm.clear_constraints()
        self.asm.add_constraint("D1.x = D2.y")
        arch.constraint_types = ['ineq']
        try:
            self.asm.check_config()
        except Exception as err:
            self.assertEqual(str(err), "this Architecture doesn't support the "
                             "following constraint types: ['eq']")
        else:
            self.fail("Exception expected")
        
        try:
            arch.constraint_types = ['eq', 'blah']
        except Exception as err:
            self.assertEqual(str(err), "the following constraint types are "
                             "invalid: ['blah']. Allowed values are: ['eq', 'ineq']")
        else:
            self.fail("Exception expected")
            
        arch.constraint_types = None
        try:        
            self.asm.check_config()
        except RuntimeError as err:
            self.assertEqual(str(err), "this Architecture doesn't support constraints")
        else: 
            self.fail("RuntimeError expected")
            
    def test_check_config_objectives(self):
        self.asm.add_objective("D1.x + D2.y")
        self.asm.architecture = arch = DummyArchitecture()
        arch.num_allowed_objectives = 1
        self.asm.check_config()
        self.asm.add_objective("D1.a - D2.b")
        
        try:
            self.asm.check_config()
        except Exception as err:
            self.assertEqual(str(err), "this Architecture supports 1 objectives, but 2 were found in the parent")
        else:
            self.fail("Exception expected")
        
        arch.num_allowed_objectives = 2
        self.asm.check_config()
        
        arch.num_allowed_objectives = None
        try:
            self.asm.check_config()
        except Exception as err:
            self.assertEqual(str(err), "this Architecture doesn't support objectives, "
                             "but 2 were found in the parent")
        else:
            self.fail("Exception expected")

if __name__ == "__main__":
    unittest.main()