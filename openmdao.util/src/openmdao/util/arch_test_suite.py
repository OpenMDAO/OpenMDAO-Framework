

def build_arch_list(include=[],exclude=[]):
    """builds a list of architectures
    
    includes: list of architecture names
        the names of the architectures to test. Only architectures in this list 
        will be tested. Each name should just be the class name (e.g. 'MDF', 'CO'). 
        Must be set to None, if excludes is specified
    
    includes: list of architecture names
        the names of the architectures not to test. All architectures from
        openmdao.lib.architectures will be tested, except for the ones in this 
        list. Each name should just be the class name (e.g. 'MDF', 'CO'). 
        Must be set to None, if includes is specified 
    """
    
    pass


def build_optproblem_list(include=[],exclude=[]):
    """builds a list of optproblems
    
    includes: list of optproblems names
        the names of the optproblems to test. Only optproblems in this list 
        will be tested. Each name should just be the class name (e.g. 'SellarProblem'). 
        Must be set to None, if excludes is specified
    
    includes: list of optproblems names
        the names of the optproblems not to test. All optproblems from
        openmdao.lib.optproblems will be tested, except for the ones in this 
        list. Each name should just be the class name (e.g. 'SellarProblem'). 
        Must be set to None, if includes is specified 
    """
    
    pass


def run_arch_test_suite(arch=[],optproblems=[]): 
    """Runs the architectures against optproblems and records the results
    
    architectures: list of Architectures 
        the architectures to test
        
    optproblems: list of OptProblems 
        The OptProblems to test the Architectures on. 
    """
    
    pass
