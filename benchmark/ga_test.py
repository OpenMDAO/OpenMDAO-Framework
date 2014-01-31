from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.lib.datatypes.api import Float
from pyopt_driver.pyopt_driver import pyOptDriver

class Rosenbrock(Component):
    """2-dimensional Rosenbrock Function"""
    x1 = Float(4.0,iotype='in',low=0.0,high=5.0,start=4.0)
    x2 = Float(4.0,iotype='in',low=-5.0,high=5.0,start=4.0)
    f = Float(1.0,iotype='out')

    def execute(self):
        self.f = (1-self.x1)**2+100*(self.x2-self.x1**2)**2

class RBOpt(Assembly):
    """Optimization of 2d Rosenbrock"""

    output = Float(1.0,iotype='out')

    def configure(self):
        self.add('func',Rosenbrock())
        self.connect('func.f','output')
        self.add('driver',pyOptDriver())
        self.driver.optimizer='NSGA2'
        self.driver.options={'maxGen':250,'pMut_real':0.4}
        self.driver.add_objective('func.f')
        self.driver.add_parameter('func.x1')
        self.driver.add_parameter('func.x2')
        self.driver.add_constraint('func.x1+func.x2-2.0<=0.0')

if __name__=='__main__':
    import sys
    import time
    prob = set_as_top(RBOpt())
    tt = time.time()

    if '-prof' in sys.argv:
        import cProfile
        import pstats
        
        cProfile.run('prob.run()', 'profout')
        p = pstats.Stats('profout')
        p.strip_dirs()
        p.sort_stats('calls', 'time')
        p.print_stats()
        print '\n\n#####################\n\n'
        p.sort_stats('cumulative', 'time')
        p.print_stats()
        print '\n\n+++++++++++++++++++++\n\n'
        p.print_callers()
        print '\n\n---------------------\n\n'
        p.print_callees()
    else:
        prob.run()

    print prob.func.x1, prob.func.x2
    print "Elapsed time: ", time.time() - tt, "seconds"
    print "Function Evaluations: ", prob.func.exec_count
