import os

import openmdao.lib.optproblems
import openmdao.lib.architectures
import openmdao.main
from openmdao.main.arch import Architecture
from openmdao.main.problem_formulation import OptProblem
from openmdao.util.dep import PythonSourceTreeAnalyser


def build_arch_list(include=[], exclude=[]):
    """builds a list of architectures
    
    include: list of architecture names
        the names of the architectures to test. Only architectures in this list 
        will be tested. Each name should just be the class name (e.g. 'MDF', 'CO'). 
        Must be set to None, if excludes is specified
    
    exclude: list of architecture names
        the names of the architectures not to test. All architectures from
        openmdao.lib.architectures will be tested, except for the ones in this 
        list. Each name should just be the class name (e.g. 'MDF', 'CO'). 
        Must be set to None, if includes is specified 
    """
    
    if include and exclude: 
        raise ValueError("Can't set both include and exlude")
    
    startdirs = [os.path.dirname(openmdao.lib.architectures.__file__),
                 os.path.dirname(openmdao.main.__file__)]
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
    """builds a list of optproblems
    
    include: (optional) list of optproblems names
        the names of the optproblems to test. Only optproblems in this list 
        will be tested. Each name should just be the class name (e.g. 'SellarProblem'). 
        Must be set to None, if excludes is specified. If not specified, 
        all OptProblems, except those in exclude are used. 
    
    exclude: (optional) list of optproblems names
        the names of the optproblems not to test. All optproblems from
        openmdao.lib.optproblems will be tested, except for the ones in this 
        list. Each name should just be the class name (e.g. 'SellarProblem'). 
        Must be set to None, if includes is specified. 
    """
    
    if include and exclude: 
        raise ValueError("Can't set both include and exlude")
    
    startdirs = [os.path.dirname(openmdao.lib.optproblems.__file__),
                 os.path.dirname(openmdao.main.__file__)]
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
    """Runs the architectures against optproblems and records the results
    
    arch: list of Architectures 
        the architectures to test
        
    optproblems: list of OptProblems 
        The OptProblems to test the Architectures on. 
    """
    
    compat_data = {}
    for a in arch: 
        arch_data = {}
        for p in optproblems: 
            print "Testing %s on %s"%(a.__class__,p.__class__)
            prob = p.__class__()
            prob.architecture = a.__class__()
            try:
                prob.check_config()
                arch_data[p] = True
            except RuntimeError as err: 
                arch_data[p] = False #not compatible, so just move on
                continue 
            prob.run()
            #print prob.check_solution()
            
        compat_data[a] = arch_data
        
    return compat_data

if __name__ == "__main__": 
    archs = build_arch_list()
    probs = build_optproblem_list()
    
    data = run_arch_test_suite(archs, probs)
    
    print data
    
    