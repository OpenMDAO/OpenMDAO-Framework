import unittest
import ordereddict
from openmdao.main.api import Component,Assembly, Driver, set_as_top
from openmdao.main.expreval import ExprEvaluator
from openmdao.main.problem_formulation import HasGlobalDesVars,\
     HasCouplingVars, HasLocalDesVars, GlobalDesVar, LocalDesVar, CouplingVar

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
    
        gdv = GlobalDesVar('x',
                           [ExprEvaluator('D1.x',self.asm),ExprEvaluator('D2.x',self.asm,)]
                           ,1.0,0.0)
        self.assertEqual(gdv,self.asm.get_global_des_vars('x'))
        try: 
            self.asm.get_global_des_vars('y')
        except Exception as err: 
            self.assertEqual(": No global design variable named 'y' "
                             "has been added to the assembly",str(err))
        else: 
            self.fail("Exception expected")        
        
        self.assertEqual(ordereddict.OrderedDict({'x':gdv}),self.asm.get_global_des_vars())
            
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
        
if __name__ == "__main__":
    unittest.main()