import unittest

from openmdao.lib.optproblems.api import SellarProblem

from openmdao.lib.architectures.mdf import MDF

class TestMDF(unittest.TestCase): 
    
    def test_mdf_arch(self): 
        prob = SellarProblem()
        prob.architecture = MDF()
        
        prob.run()
        
        error = prob.check_solution()