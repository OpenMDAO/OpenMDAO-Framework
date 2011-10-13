import unittest

from openmdao.main.problem_formulation import OptProblem

from openmdao.lib.optproblems.sellar import SellarProblem
from openmdao.lib.optproblems.branin import BraninProblem

from openmdao.util.arch_test_suite import build_arch_list, build_optproblem_list


class TestArchTestSuite(unittest.TestCase): 
    
    def test_build_optproblem_list(self): 
        
        probs = build_arch_list()
        
        self.assertTrue(all([isinstance(p,OptProblem) for p in probs]))
        
        try: 
            build_arch_list(incude=['SellarProblem'],exclude=["BraninProblem"])
        except ValueError as err: 
            self.assertEqual(str(err), "Can't set both include and exlude")
        else: 
            self.fail("ValueError Expected") 
            
        probs = build_arch_list(incude=['SellarProblem']) 
        self.assertEqual(probs,[SellarProblem])
        
        probs = build_arch_list(exclude=['SellarProblem']) 
        self.assertFalse(SellarProblem in probs)