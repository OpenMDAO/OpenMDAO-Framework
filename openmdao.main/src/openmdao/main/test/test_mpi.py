
import time

import numpy as np 

from openmdao.main.api import Assembly, dump_iteration_tree, Component, Driver, set_as_top
from openmdao.main.datatypes.api import Float, Array
from openmdao.main.hasobjective import HasObjectives
from openmdao.main.hasconstraints import HasConstraints
from openmdao.main.hasparameters import HasParameters
from openmdao.main.mpiwrap import mpiprint, set_print_rank
from openmdao.util.decorators import add_delegate
from openmdao.main.distsolve import MPINonlinearSolver
from openmdao.test.execcomp import ExecComp

from openmdao.lib.optproblems import sellar

class NTimes(MPINonlinearSolver):
    def __init__(self, maxiter=1):
        super(NTimes, self).__init__()
        self.max_iteration = maxiter

class ABCDArrayComp(Component):
    delay = Float(0.01, iotype='in')
    
    def __init__(self, arr_size=9):
        super(ABCDArrayComp, self).__init__()
        self.add_trait('a', Array(np.ones(arr_size, float), iotype='in'))
        self.add_trait('b', Array(np.ones(arr_size, float), iotype='in'))
        self.add_trait('c', Array(np.ones(arr_size, float), iotype='out'))
        self.add_trait('d', Array(np.ones(arr_size, float), iotype='out'))

    def execute(self):
        time.sleep(self.delay)
        self.c = self.a + self.b
        self.d = self.a - self.b
        mpiprint("%s: c = %s" % (self.name, self.c))
        
def _get_model():
    """1 component"""
    top = set_as_top(Assembly())
    top.add('driver', NTimes(3))
    name = 'C1'
    top.add(name, ABCDArrayComp())
    top.driver.workflow.add(name)
    getattr(top, name).mpi.requested_cpus = 1

    top.driver.add_parameter('C1.a', high=100.0, low=0.0)
    top.driver.add_constraint('C1.d[0]=0')
    return top
    
def _get_model1():
    """2 comps, not connected"""
    top = set_as_top(Assembly())
    top.add('driver', NTimes(3))
    for i in range(1,3):
        name = 'C%d' % i
        top.add(name, ABCDArrayComp())
        top.driver.workflow.add(name)
        getattr(top, name).mpi.requested_cpus = 1

    top.driver.add_parameter('C1.a', high=100.0, low=0.0)
    top.driver.add_constraint('C2.d[0]=0') 
    
    return top

def _get_model2():
    """3 comps, not connected"""
    top = set_as_top(Assembly())
    top.add('driver', NTimes(3))
    for i in range(1,4):
        name = 'C%d' % i
        top.add(name, ABCDArrayComp())
        top.driver.workflow.add(name)
        getattr(top, name).mpi.requested_cpus = 1

    top.driver.add_parameter('C1.a', high=100.0, low=0.0)
    top.driver.add_constraint('C2.d[0]=0') 
    
    return top

def _get_model3():
    """5 comps, connected"""
    top = set_as_top(Assembly())
    top.add('driver', NTimes(1))
    for i in range(1,6):
        name = 'C%d' % i
        top.add(name, ABCDArrayComp())
        top.driver.workflow.add(name)
        getattr(top, name).mpi.requested_cpus = 1

    conns = [
        ('C1.c','C2.a'),
        ('C2.c','C3.a'),
        ('C3.c','C4.a'),
        ('C4.c','C5.a'),
    ]

    for u,v in conns:
        top.connect(u, v)

    # top.driver.add_parameter('C3.b[1]', high=100.0, low=0.0)
    # top.driver.add_constraint('C5.d[0]=0') 
    
    return top

def _get_model4():
    """3 comps with nested drivers"""
    top = set_as_top(Assembly())
    top.add('driver', NTimes(2))
    for i in range(1,4):
        name = 'C%d' % i
        top.add(name, ABCDArrayComp())
        getattr(top, name).mpi.requested_cpus = 1

    top.add('subdriver', NTimes(3))

    top.driver.workflow.add(['subdriver', 'C3'])

    conns = [
        ('C1.c','C2.a'),
        ('C2.c','C3.a'),
    ]

    for u,v in conns:
        top.connect(u, v)

    top.driver.add_parameter('C3.b[1]', high=100.0, low=0.0)
    top.driver.add_constraint('C3.d[0]=0') 
    
    top.subdriver.add_parameter('C1.b[0]', high=100.0, low=0.0)
    top.subdriver.add_constraint('C2.d[0]=0') 
    
    return top

def _get_model5():
    """8 comps, several layers of subsystems"""
    top = set_as_top(Assembly())
    top.add('driver', NTimes(1))
    for i in range(1,9):
        name = 'C%d' % i
        top.add(name, ABCDArrayComp(5))
        top.driver.workflow.add(name)
        getattr(top, name).mpi.requested_cpus = 1

    conns = [
        ('C1.c','C4.a'),
        ('C2.c','C5.a'),
        ('C3.c','C8.a'),
        ('C3.d','C6.a'),
        ('C5.c','C4.b'),
        ('C5.d','C6.b'),
        ('C6.c','C8.b'),
        ('C6.d','C7.a'),
    ]

    for u,v in conns:
        top.connect(u, v)

    top.driver.add_parameter('C3.a[1]', high=100.0, low=0.0)
    top.driver.add_constraint('C8.d[0]=0') 
    
    return top

def _get_modelsimple():
    """2 comps"""
    top = set_as_top(Assembly())
    top.add('driver', MPINonlinearSolver())
    top.driver.max_iteration = 3

    for i in range(1,3):
        name = 'C%d' % i
        top.add(name, ABCDArrayComp())
        top.driver.workflow.add(name)
        getattr(top, name).mpi.requested_cpus = 1

    top.connect('C1.c', 'C2.a')
    #top.connect('C2.c', 'C1.a')
    top.driver.add_parameter('C1.a', low=-1e99, high=1e99)
    top.driver.add_constraint('C2.c = C1.a')
    return top

def _get_modelsellar():
    """Sellar (serial)"""
    prob = set_as_top(SellarMDF(parallel=False, use_params=False))
    return prob, { 'C1.y1': 3.160068, 'C2.y2': 3.755315 }

def _get_modelsellar2():
    """Sellar (serial)"""
    prob = set_as_top(SellarMDF(parallel=False, use_params=True))
    return prob, { 'C1.y1': 3.160068, 'C2.y2': 3.755315 }


class SellarMDF(Assembly):
    """ Optimization of the Sellar problem using MDF
    Disciplines coupled with FixedPointIterator.
    """
    def __init__(self, parallel=False, use_params=True):
        self.parallel = parallel
        self.use_params = use_params
        super(SellarMDF, self).__init__()

    def configure(self):
        """ Creates a new Assembly with this problem
        
        Optimal Design at (1.9776, 0, 0)
        
        Optimal Objective = 3.18339"""
        
        #self.add('driver', FixedPointIterator())
        self.add('driver', MPINonlinearSolver())

        # Inner Loop - Full Multidisciplinary Solve via fixed point iteration
        C1 = self.add('C1', sellar.Discipline1())
        C2 = self.add('C2', sellar.Discipline2())

        self.driver.workflow.add(['C1','C2'])

        #not relevant to the iteration. Just fixed constants
        C1.z1 = C2.z1 = 1.9776
        C1.z2 = C2.z2 = 0
        C1.x1 = 0

        if self.parallel:
            # Use connections for Parallel
            self.driver.add_parameter('C1.y1', low=-1e99, high=1e99)
            self.driver.add_constraint('C1.y1 = C2.y1')
            self.driver.add_parameter('C1.y2', low=-1.e99, high=1.e99)
            self.driver.add_constraint('C2.y2 = C1.y2')
        else:
            # Make connection for serial
            self.connect('C1.y1','C2.y1')

            # Iteration loop
            if self.use_params:
                self.driver.add_parameter('C1.y2', low=-1.e99, high=1.e99)
                #self.driver.add_constraint('C2.y2 = C1.y2')
                self.driver.add_constraint('C1.y2 = C2.y2')
            else:  # use circular connection
                self.connect('C2.y2', 'C1.y2')
        
        # Solver settings
        self.driver.max_iteration = 5
        self.driver.tolerance = 1.e-15
        self.driver.print_convergence = False
        

if __name__ == '__main__':
    import sys
    import traceback
    from openmdao.main.mpiwrap import MPI

    """
    To run various tests, use the following cmdline:   mpirun -n <numprocs> python test_mpi.py --run <modelname>
    where modelname is whatever comes after _get_model in the various _get_model* functions above.
    """

    run = False
    mname = ''

    for arg in sys.argv[1:]:
        if arg.startswith('--run'):
            run = True
        elif arg.startswith('--rank'):
            set_print_rank(int(arg.split('=',1)[1]))
        elif not arg.startswith('-'):
            mname = arg

    ret = globals().get('_get_model%s' % mname)()
    if isinstance(ret, tuple):
        top, expected = ret
    else:
        top = ret
        expected = None

    #dump_iteration_tree(top)

    try:
        if MPI is not None and not run:
            top._setup()
            mpiprint(top.driver.workflow._subsystem.dump_subsystem_tree(stream=None))

            mpiprint("setup DONE")

        if run:
            mpiprint('-'*50)
            top.run()

            mpiprint('-'*50)
            #mpiprint(top.driver.workflow._subsystem.dump_subsystem_tree(stream=None))

            if expected:
                mpiprint('-'*50)
                mpiprint("{0:<17} {1:<17} {2:<17} {3:<17}".format("Name",
                                                               "Expected",
                                                               "Actual",
                                                               "Error"))
                for name, expval in expected.items():
                    val = top.get(name)
                    err = expval - val
                    mpiprint("{0:<17} {1:<17} {2:<17} {3:<17}".format(name, expval, val, err))
    except Exception as err:
        mpiprint(traceback.format_exc())
