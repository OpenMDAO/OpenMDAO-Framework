import unittest
import ordereddict
from openmdao.main.api import Component,Assembly, Driver, set_as_top
from openmdao.main.expreval import ExprEvaluator
from openmdao.main.problem_formulation import HasGlobalDesVars,\
     HasCouplingVars, HasLocalDesVars

from openmdao.lib.datatypes.api import Float

from openmdao.util.decorators import add_delegate

@add_delegate(HasGlobalDesVars,HasLocalDesVars,HasCouplingVars)
class GlobalAssembly(Assembly): 
    pass

class Dummy(Component): 
    x = Float(iotype="in")

class HasGlobalDesVarsTest(unittest.TestCase):
    
    def setUp(self): 
        self.asm = set_as_top(GlobalAssembly())
        self.asm.add("D1",Dummy())
        self.asm.add("D2",Dummy()) 
    
    def test_global_des_var(self):
        try:         
            self.asm.add_global_des_var('x',['D1.y+D1.x','D2.x'],1.0,0.0)
        except Exception as err: 
            self.assertEqual(": Cant add global design variable 'x' because the target 'D1.y+D1.x' is an invalid target",str(err))
        else: 
            self.fail("Exception Expected")
            
        try:         
            self.asm.add_global_des_var('x',['D1.y','D2.x'],1.0,0.0)
        except Exception as err: 
            self.assertEqual(": Cant add global design variable 'x' because the target 'D1.y' is an invalid target",str(err))
        else: 
            self.fail("Exception Expected")    
            
        self.asm.add_global_des_var('x',['D1.x','D2.x'],1.0,0.0)
        try:
            self.asm.add_global_des_var('x',['D1.x','D2.x'],1.0,0.0)
        except Exception as err: 
            self.assertEqual(": A global design variable named 'x' already exists in this assembly",str(err))
        else: 
            self.fail("Exception expected")    
        
        self.assertEqual(['x'],self.asm.list_global_des_vars())
        
        self.asm.setup_global_broadcaster('bcstr')
        
        self.assertTrue(hasattr(self.asm,"bcstr"))
        self.assertTrue(hasattr(self.asm.bcstr,"x"))
        self.assertTrue(hasattr(self.asm.bcstr,"x_in"))
        
        self.assertEqual(set(self.asm.list_connections()),set([('bcstr.x','D1.x'),('bcstr.x','D2.x')]))
            
        try: 
            self.asm.remove_global_des_var('y')
        except Exception as err: 
            self.assertEqual(": No global design variable names 'y' exists "
                             "in this assembly",str(err))
        else: 
            self.fail("Exception expected")
            
        self.asm.remove_global_des_var('x')
        self.assertEqual(set(),set(self.asm.list_global_des_vars()))
        
        
        self.asm.add_global_des_var('x',
                                    ['D1.x','D2.x'],
                                    1.0,0.0)
        self.asm.clear_global_des_vars()
        self.assertEqual(set(),set(self.asm.list_global_des_vars()))
        
    def test_local_des_var(self): 
        self.asm.add_local_des_var("D1.x")
        self.assertEqual([('D1','D1.x')],self.asm.list_local_des_vars(True))
        try: 
            self.asm.add_local_des_var("D1.y")
        except Exception as err: 
            self.assertEqual(": Cant add local design variable for 'D1.y' because 'D1.y' is invalid",str(err))
        else: 
            self.fail("Exception expected")
            
        try: 
            self.asm.add_local_des_var("D1.x+D1.x")
        except Exception as err: 
            self.assertEqual(": Cant add local design variable for 'D1.x+D1.x' because 'D1.x+D1.x' is invalid",str(err))    
        else: 
            self.fail("Exception expected")
            
        try: 
            self.asm.add_local_des_var("D1.x")    
        except Exception as err: 
            self.assertEqual(': A LocalDesVar with target "D1.x" has already been added to this assembly',str(err))
        else: 
            self.fail("Exception expected")
            
        self.assertEqual(['D1.x'],self.asm.list_local_des_vars())  
        
        
        self.asm.remove_local_des_var('D1.x')
        self.assertEqual([],self.asm.list_local_des_vars())  
        
        self.asm.add_local_des_var("D1.x")
        self.asm.clear_local_des_vars()
        self.assertEqual([],self.asm.list_local_des_vars())  
        
    def test_coupling_vars(self): 
        self.asm.add_coupling_var("D1.x","D1.x=D2.x")
        
        try: 
            self.asm.add_coupling_var("D1.x","D1.x=D2.x")
        except Exception as err:         
            self.assertEqual(": Coupling variable with indep 'D1.x' already exists in the assembly",str(err))
        else: 
            self.fail("Exception expected")
        
        try: 
            self.asm.add_coupling_var("D1.y","D1.x=D2.x")
        except Exception as err: 
            self.assertEqual(": Can't add coupling variable with indep 'D1.y' because is not a valid variable",str(err))
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
            self.assertEqual(": No coupling variable with the indep 'D1.x' exists in the assembly",str(err))
        else: 
            self.fail("Exception expected")
            
        self.asm.add_coupling_var("D1.x","D1.x=D2.x")
        self.asm.clear_coupling_vars()
        self.assertEqual([],self.asm.list_coupling_vars())
        
            
            
if __name__ == "__main__":
    unittest.main()