import unittest

import os.path

import openmdao.lib

from openmdao.main.problem_formulation import ArchitectureAssembly, OptProblem
from openmdao.main.api import Architecture

import openmdao.lib.optproblems

from openmdao.util.dep import PythonSourceTreeAnalyser

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
    """Test set to check the validity of all optproblems in the standard library""" 
    
    def assertAccuracy(self,prob_name,error_dict,tol): 
        for k,v in error_dict.iteritems(): 
            if isinstance(v,tuple): 
                error = abs(v[0]-v[1]) #error of coupling vars
                
            else: 
                error = abs(v) #just error on a normal var or objective
            if error > tol: 
                self.fail("In %s the error for %s was %s. The allowed tollerance is %s"%(prob_name,k,error,tol))
        
    
    def test_optproblems_solution(self): 
        """test to make sure that at the specified solution point, the objective 
        values match what is given in the solution""" 
        
        #find all the optproblems in lib
        startdirs = [os.path.dirname(openmdao.lib.optproblems.__file__),]
        psta = PythonSourceTreeAnalyser(startdirs, os.path.join('*','test','*'))    
        opt_problems = psta.find_inheritors("openmdao.main.problem_formulation.OptProblem")
        
        for prob_name in opt_problems: 
            #print "running %s"%prob_name
            prob_class = prob_name.split(".")[-1]
            prob_package = ".".join(prob_name.split(".")[:-1])
            prob_package = __import__(prob_package,globals(),locals(),[prob_class,],-1)
            
            prob = getattr(prob_package,prob_class)() #create instance of the OptProblem
            prob.architecture = OptProblemSolutionCheck()
            prob.configure()
            
            prob.run()
            
            error = prob.check_solution(strict=True)
                
            self.assertAccuracy(prob_name,error,.001)
        
    
if __name__ == "__main__":
    unittest.main()        