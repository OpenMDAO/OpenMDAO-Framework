import unittest
import ordereddict
from openmdao.main.api import Component, Assembly, Driver, \
                              Architecture, set_as_top
from openmdao.main.expreval import ExprEvaluator
from openmdao.main.problem_formulation import ArchitectureAssembly, HasCouplingVars
from openmdao.main.hasconstraints import HasConstraints
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasobjective import HasObjective

from openmdao.lib.datatypes.api import Float
from openmdao.lib.drivers.api import CONMINdriver

from openmdao.util.decorators import add_delegate

@add_delegate(HasCouplingVars)
class GlobalAssembly(ArchitectureAssembly): 
    pass

class Dummy(Component): 
    x = Float(iotype="in")
    y = Float(iotype="in")
    
class DummyArchitecture(Architecture):
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
         
        
    def test_coupling_vars(self): 
        self.asm.add_coupling_var("D1.x","D2.x")
        self.asm.add_coupling_var("D4.x","D5.x")
        self.asm.add_coupling_var("D6.x","D5.y")
        
        try: 
            self.asm.add_coupling_var("D1.x","D2.x")
        except Exception as err:
            self.assertEqual(": Coupling variable with indep 'D1.x' already exists in assembly",str(err))
        else: 
            self.fail("Exception expected")
        
        try: 
            self.asm.add_coupling_var("D3.x","D2.x")
        except Exception as err:         
            self.assertEqual(": Coupling variable with dep 'D2.x' already exists in assembly",str(err))
        else: 
            self.fail("Exception expected")            
            
        try: 
            self.asm.add_coupling_var("D1.z","D2.x")
        except Exception as err: 
            self.assertEqual(": Cant add coupling variable with indep 'D1.z' because is not a valid variable",str(err))
        else: 
            self.fail("Exception expected")
            
            
        self.assertEqual([("D1.x","D2.x"),("D4.x","D5.x"),("D6.x","D5.y")],
                         self.asm.list_coupling_vars())
        
        self.asm.remove_coupling_var(('D1.x','D2.x'))
        self.assertEqual([("D4.x","D5.x"),("D6.x","D5.y")],
                         self.asm.list_coupling_vars())
        try: 
            self.asm.remove_coupling_var(('D1.x','D2.x'))
        except Exception as err: 
            self.assertEqual(": No coupling variable of ('D1.x','D2.x') exists in assembly",str(err))
        else: 
            self.fail("Exception expected")
            
        self.asm.add_coupling_var("D1.x","D2.x")
        self.asm.clear_coupling_vars()
        self.assertEqual([],self.asm.list_coupling_vars())
        
    def test_check_config(self):
        pass
            
if __name__ == "__main__":
    unittest.main()