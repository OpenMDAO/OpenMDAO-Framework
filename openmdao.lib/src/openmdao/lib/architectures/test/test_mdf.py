import unittest

from openmdao.lib.optproblems.api import SellarProblemWithDeriv

from openmdao.lib.architectures.mdf import MDF

class TestMDF(unittest.TestCase): 
    
    def test_mdf_arch(self): 
        prob = SellarProblemWithDeriv()
        prob.architecture = MDF()

        prob.run()

        solver_params = prob.solver.get_parameters()
        coupling = prob.get_coupling_vars()

        params = prob.get_parameters()
        opt_params = prob.driver.get_parameters()

        self.assertEqual(set(solver_params.keys()),set(coupling.keys()))
        self.assertEqual(params,opt_params)
        
        
if __name__ == "__main__":
    unittest.main()        
