import unittest

from openmdao.lib.optproblems.api import SellarProblem

from openmdao.lib.architectures.mdf import MDF

class TestMDF(unittest.TestCase): 
    
    def test_mdf_arch(self): 
        prob = SellarProblem()
        prob.architecture = MDF()
        
        prob.run()
        
        error = prob.check_solution()
        
        #should be able to solve this problem with very high accuracy!
        self.assertTrue(sum([v for k,v in error.iteritems()]) < .0001)