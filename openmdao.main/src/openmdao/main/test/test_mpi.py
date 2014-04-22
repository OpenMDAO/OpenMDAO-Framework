
import time

import numpy as np 

from openmdao.main.api import Assembly, Component, Driver, set_as_top
from openmdao.main.datatypes.api import Float, Array
from openmdao.main.hasobjective import HasObjectives
from openmdao.main.hasconstraints import HasConstraints
from openmdao.main.hasparameters import HasParameters
from openmdao.main.mpiwrap import mpiprint
from openmdao.util.decorators import add_delegate

@add_delegate(HasObjectives, HasParameters, HasConstraints)
class NTimes(Driver):
    def __init__(self, n=1):
        super(NTimes, self).__init__()
        self.n = n 
        self._count = 0

    def run(self, force=False, ffd_order=0, case_id=''):
        self.execute()
        
    def continue_iteration(self):
        return self._count < self.n

    def start_iteration(self):
        self._count = 0
        self._paramvals = np.array(self.eval_parameters())

    def pre_iteration(self):
        if self._count > 0:
            self.set_parameters(self._paramvals)

    def post_iteration(self):
        self._count += 1
        self._paramvals += 1.0


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
        #mpiprint("%s: c = %s" % (self.name, self.c))
        
def _get_model_simple():
    top = set_as_top(Assembly())
    top.add('driver', NTimes(3))
    name = 'C1'
    top.add(name, ABCDArrayComp())
    top.driver.workflow.add(name)
    getattr(top, name).mpi.requested_cpus = 1

    top.driver.add_parameter('C1.a', high=100.0, low=0.0)
    top.driver.add_constraint('C1.d[0]>0')
    return top
    
def _get_model():
    top = set_as_top(Assembly())
    top.add('driver', NTimes(3))
    for i in range(1,3):
        name = 'C%d' % i
        top.add(name, ABCDArrayComp())
        top.driver.workflow.add(name)
        getattr(top, name).mpi.requested_cpus = 1

    top.driver.add_parameter('C1.a', high=100.0, low=0.0)
    top.driver.add_constraint('C1.d[0]>C2.d[0]') 
    
    return top

def _get_model1():
    top = set_as_top(Assembly())
    top.add('driver', NTimes(3))
    for i in range(1,4):
        name = 'C%d' % i
        top.add(name, ABCDArrayComp())
        top.driver.workflow.add(name)
        getattr(top, name).mpi.requested_cpus = 1

    top.driver.add_parameter('C1.a', high=100.0, low=0.0)
    top.driver.add_constraint('C1.d[0]>C2.d[0]') 
    
    return top

def _get_model2():
    top = set_as_top(Assembly())
    top.add('driver', NTimes(1))
    for i in range(1,6):
        name = 'C%d' % i
        top.add(name, ABCDArrayComp())
        top.driver.workflow.add(name)
        getattr(top, name).mpi.requested_cpus = 1

    conns = [
        ('C1.c','C3.a'),
        ('C3.c','C5.a'),
        ('C2.c','C4.a'),
        ('C4.d','C5.b'),
    ]

    for u,v in conns:
        top.connect(u, v)

    top.driver.add_parameter('C3.a[1]', high=100.0, low=0.0)
    top.driver.add_constraint('C5.d[0]>0.') 
    
    return top

def _get_model_nested_drivers():
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
    top.driver.add_constraint('C3.d[0]>0.') 
    
    top.subdriver.add_parameter('C1.b[0]', high=100.0, low=0.0)
    top.subdriver.add_constraint('C2.d[0]>0.') 
    
    return top

def _get_model_layers():
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
    top.driver.add_constraint('C6.d[0]>C8.d[0]') 
    
    return top


if __name__ == '__main__':
    from openmdao.main.mpiwrap import MPI_run, under_mpirun, setup_mpi

    top = _get_model2()

    if under_mpirun():
        setup_mpi(top)

        mpiprint(top.driver.workflow.get_subsystem().dump_subsystem_tree(stream=None))
        #mpiprint(top.driver.workflow._subsystem.dump_subsystem_tree(stream=None))
        #mpiprint(top.subdriver.workflow._subsystem.dump_subsystem_tree(stream=None))

    mpiprint('-'*50)
    top.run()

    for i in range(9):
        for v in ['a','b','c','d']:
            name = "C%d.%s" % (i+1,v)
            if top.contains(name):
                mpiprint("%s = %s" % (name,top.get(name)))



        

