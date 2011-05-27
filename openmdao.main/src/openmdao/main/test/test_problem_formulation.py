import unittest
import ordereddict
from openmdao.main.api import Component,Assembly, Driver, set_as_top
from openmdao.main.expreval import ExprEvaluator
from openmdao.main.problem_formulation import HasGlobalDesVars,\
     HasCouplingVars, HasLocalDesVars

from openmdao.lib.datatypes.api import Float
from openmdao.lib.drivers.api import CONMINdriver

from openmdao.util.decorators import add_delegate

@add_delegate(HasGlobalDesVars,HasLocalDesVars,HasCouplingVars)
class GlobalAssembly(Assembly): 
    pass

class Dummy(Component): 
    x = Float(iotype="in")
    
    

class ProblemFormulationTest(unittest.TestCase):
    
    def setUp(self): 
        self.asm = set_as_top(GlobalAssembly())
        self.asm.add("D1",Dummy())
        self.asm.add("D2",Dummy()) 
         
        
    def test_coupling_vars(self): 
        self.asm.add_coupling_var("D1.x","D1.x=D2.x")
        
        try: 
            self.asm.add_coupling_var("D1.x","D1.x=D2.x")
        except Exception as err:         
            self.assertEqual(": Coupling variable with indep 'D1.x' already exists in assembly",str(err))
        else: 
            self.fail("Exception expected")
        
        try: 
            self.asm.add_coupling_var("D1.y","D1.x=D2.x")
        except Exception as err: 
            self.assertEqual(": Cant add coupling variable with indep 'D1.y' because is not a valid variable",str(err))
        else: 
            self.fail("Exception expected")
            
        try: 
            self.asm.add_coupling_var("D2.x","D1.x=D2.x")
        except Exception as err: 
            self.assertEqual(": Coupling variable with constraint 'D1.x=D2.x' already exists in assembly",str(err))
        else: 
            self.fail("Exception expected")
            
        self.assertEqual(['D1.x'],self.asm.list_coupling_vars())
        
        self.asm.remove_coupling_var('D1.x')
        self.assertEqual([],self.asm.list_coupling_vars())
        try: 
            self.asm.remove_coupling_var('D1.x')
        except Exception as err: 
            self.assertEqual(": No coupling variable with the indep 'D1.x' exists in assembly",str(err))
        else: 
            self.fail("Exception expected")
            
        self.asm.add_coupling_var("D1.x","D1.x=D2.x")
        self.asm.clear_coupling_vars()
        self.assertEqual([],self.asm.list_coupling_vars())
        
            
            
if __name__ == "__main__":
    unittest.main()