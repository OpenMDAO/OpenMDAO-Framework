import unittest

from openmdao.main.problem_formulation import OptProblem
from openmdao.main.arch import Architecture

from openmdao.lib.optproblems.sellar import SellarProblem
from openmdao.lib.architectures.mdf import MDF
from openmdao.util.arch_test_suite import build_arch_list, build_optproblem_list

class TestArchTestSuite(unittest.TestCase): 
    
    def test_build_optproblem_list(self): 
        
        probs = build_optproblem_list()
        self.assertTrue(probs)
        self.assertTrue(all([isinstance(p,OptProblem) for p in probs]))
        
        try: 
            build_optproblem_list(include=['SellarProblem'],exclude=["BraninProblem"])
        except ValueError as err: 
            self.assertEqual(str(err), "Can't set both include and exlude")
        else: 
            self.fail("ValueError Expected") 
            
        probs = build_optproblem_list(include=['SellarProblem']) 
        self.assertTrue(SellarProblem in [p.__class__ for p in probs])
        self.assertEqual(probs[0].__class__,SellarProblem)
        
        probs = build_optproblem_list(exclude=['SellarProblem']) 
        self.assertFalse(SellarProblem in [p.__class__ for p in probs])
        
        
    def test_build_arch_list(self): 
        
        archs = build_arch_list()
        self.assertTrue(archs)
        self.assertTrue(all([isinstance(a,Architecture) for a in archs]))
        
        try: 
            build_arch_list(include=['MDF'],exclude=["CO"])
        except ValueError as err: 
            self.assertEqual(str(err), "Can't set both include and exlude")
        else: 
            self.fail("ValueError Expected") 
            
        archs = build_arch_list(include=['MDF']) 
        self.assertTrue(MDF in [a.__class__ for a in archs])
        self.assertEqual(archs[0].__class__, MDF)
        
        archs = build_arch_list(exclude=['MDF']) 
        self.assertFalse(MDF in [a.__class__ for a in archs])    
        
   
        