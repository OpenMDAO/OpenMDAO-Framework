"""Script to run all Architectures in openmdao.lib.architectures 
against all OptProblems in openmdao.lib.optproblems. 
"""


import os

import openmdao.lib.optproblems
import openmdao.lib.architectures
from openmdao.lib.casehandlers.api import DBCaseRecorder

from openmdao.main.api import set_as_top
# because accessing __file__ on openmdao.main directly can fail under nose on Windows
from openmdao.main import component  
#from openmdao.main.arch import Architecture
#from openmdao.main.problem_formulation import OptProblem

from openmdao.util.dep import PythonSourceTreeAnalyser



def build_arch_list(include=[], exclude=[]):
    """Builds a list of architectures.
    
    include: list of architecture names
        The names of the architectures to test. Only architectures in this list 
        will be tested. Each name should be just the class name (e.g., 'MDF', 'CO'). 
        Must be set to None if excludes is specified.
    
    exclude: list of architecture names
        The names of the architectures not to test. All architectures from
        openmdao.lib.architectures will be tested, except for the ones in this 
        list. Each name should be just the class name (e.g., 'MDF', 'CO'). 
        Must be set to None if includes is specified. 
    """
    
    if include and exclude: 
        raise ValueError("Can't set both include and exlude")
    
    startdirs = [os.path.dirname(openmdao.lib.architectures.__file__),
                 os.path.dirname(component.__file__)]
    psta = PythonSourceTreeAnalyser(startdirs, os.path.join('*','test','*'))    
    architectures = psta.find_inheritors("openmdao.main.arch.Architecture")
    archs = []
    for arch_name in architectures: 
            arch_class = arch_name.split(".")[-1]
            arch_package = ".".join(arch_name.split(".")[:-1])
            if  (not include and not exclude) or (include and arch_class in include) or \
                (exclude and arch_class not in exclude): 
                
                arch_package = __import__(arch_package,globals(),locals(),[arch_class,],-1)
                archs.append(getattr(arch_package,arch_class)()) #create instance of the Architecture

    return archs


def build_optproblem_list(include=[], exclude=[]):
    """Builds a list of optproblems.
    
    include: (optional) list of optproblems names
        The names of the optproblems to test. Only optproblems in this list 
        will be tested. Each name should be just the class name (e.g., 'SellarProblem'). 
        Must be set to None if excludes is specified. If not specified, 
        all OptProblems, except those in exclude, are used. 
    
    exclude: (optional) list of optproblems names
        The names of the optproblems not to test. All optproblems from
        openmdao.lib.optproblems will be tested except for the ones in this 
        list. Each name should just be the class name (e.g., 'SellarProblem'). 
        Must be set to None if includes is specified. 
    """
    
    if include and exclude: 
        raise ValueError("Can't set both include and exlude for OptProblems")
    
    startdirs = [os.path.dirname(openmdao.lib.optproblems.__file__),
                 os.path.dirname(component.__file__)]
    psta = PythonSourceTreeAnalyser(startdirs, os.path.join('*','test','*'))    
    opt_problems = psta.find_inheritors("openmdao.main.problem_formulation.OptProblem")
    
    probs = []
    for prob_name in opt_problems: 
            prob_class = prob_name.split(".")[-1]
            prob_package = ".".join(prob_name.split(".")[:-1])
            if  (not include and not exclude) or (include and prob_class in include) or \
                (exclude and prob_class not in exclude): 
                
                prob_package = __import__(prob_package,globals(),locals(),[prob_class,],-1)
                probs.append(getattr(prob_package,prob_class)()) #create instance of the OptProblem

    return probs

def run_arch_test_suite(arch=[], optproblems=[]): 
    """Runs the architectures against optproblems and records the results.
    
    arch: list of Architectures 
        The architectures to test.
        
    optproblems: list of OptProblems 
        The OptProblems to use for testing the Architectures. 
    """
    
    compat_data = {}
    
    for p in optproblems: 
        arch_data = {}
        prob_name = p.__class__.__name__

        converge_file = open('%s_convergence_data.py'%prob_name,'w')
        
        for a in arch:
            prob = set_as_top(p.__class__())
            arch_name = a.__class__.__name__
            
            prob.architecture = a.__class__()
            recorders = [DBCaseRecorder()]
            prob.architecture.data_recorders = recorders
            
            
            print "Testing %s on %s"%(arch_name,prob_name), "...", 
            try:
                prob.check_config()
                arch_data[p] = True
                
            except RuntimeError: 
                arch_data[p] = False #not compatible, so just move on
                print "Incompatible"
                #raise err
                continue 
                           
            prob.run()
            print "Success"
            
            des_vars = prob.get_des_vars_by_comp()
            print "  Function Evaluations (Derivative Evaluations): "
            for comp_name in des_vars: 
                comp = prob.get(comp_name)
                print "    %s: %d (%d)"%(comp_name,comp.exec_count,comp.derivative_exec_count)
            print "  Errors: "
            for k,v in prob.check_solution().iteritems(): 
                print "    ",k,": ",v
            #print prob.check_solution()

            iter_data = prob.architecture.data_recorders[0].get_iterator()
            data = [case['Objective'] for case in iter_data]
            #converge_file.write('%s = %s'%(arch_name,str(data)))
            print >> converge_file, '%s = %s'%(arch_name,str(data))
            print 
            
        compat_data[a] = arch_data
        
    return compat_data

def cli_arch_test_suite(parser=None, options=None, args=None): 
    """Runs all the architectures against all the test problems. 
    A console script runs this function.
    """ 
    if not parser: #then you're not getting called from cli
        return 
    
    if options.inc_arch and options.excl_arch: 
        raise ValueError("You can either specify architectures to include or to exclude, not both.")
    
    if options.inc_prob and options.excl_prob: 
        raise ValueError("You can either specify problems to include or to exclude, not both.")
    
    archs = build_arch_list(include=options.inc_arch,exclude=options.excl_arch)
    probs = build_optproblem_list(include=options.inc_prob,exclude=options.excl_prob)

       
    data = run_arch_test_suite(archs, probs)
       
# make nose ignore these functions
cli_arch_test_suite.__test__ = False
run_arch_test_suite.__test__ = False


if __name__ == "__main__": 
    archs = build_arch_list(include=['CO',])
    probs = build_optproblem_list(include=["SellarProblem"])

    
    
    data = run_arch_test_suite(archs, probs)    
    
