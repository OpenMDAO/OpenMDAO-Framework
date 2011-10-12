import unittest

from openmdao.main.problem_formulation import ArchitectureAssembly, OptProblem
from openmdao.main.api import Architecture

from openmdao.lib.optproblems import sellar

#create a simple run once architecture
class OptProblemSolutionCheck(Architecture): 
    def __init__(self, *args, **kwargs):
        super(OptProblemSolutionCheck, self).__init__(*args, **kwargs)
        
        # the following variables determine the behavior of check_config
        self.param_types = ['continuous']
        self.constraint_types = ['ineq']
        self.num_allowed_objectives = 1
        self.has_coupling_vars = True    
    
    
    def configure(self): 
        des_vars = self.parent.get_des_vars_by_comp()
        coupling_indeps = self.parent.get_coupling_indeps_by_comp()
        
        for k,v in des_vars.iteritems(): 
            for param in v: 
                param.set(self.parent.solution[param.name])
            self.parent.driver.workflow.add(k)
            
        self.parent.init_coupling_vars()
        for k,v in coupling_indeps.iteritems(): 
            for couple in v:
                couple.indep.set(self.parent.solution[couple.name])
            if k not in self.parent.driver.workflow: 
                self.parent.driver.workflow.add(k)
 
#set all parameters to initial values
#set set all coupling var indeps to initial values
#check the objective solution 
#check the coupling dependents

#opt_problems = [item for item in dir(openmdao.lib.optproblems.api) if isinstance(item,OptProblem)]

class TestOptProblems(unittest.TestCase): 
    def test_optproblems_solution(self): 
        
        prob = sellar.SellarProblem()
        
        prob.architecture = OptProblemSolutionCheck()
        prob.configure()
        pass
        
if __name__ == "__main__":
    unittest.main()        