
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
        self._count = 0
        super(NTimes, self).run(force=force, ffd_order=ffd_order,
                                case_id=case_id)
        
    def run_iteration(self):
        self._count += 1
        mpiprint("%s: iteration = %d" % (self.get_pathname(), self._count))
        super(NTimes, self).run_iteration()

    def continue_iteration(self):
        return self._count < self.n


class ABCDArrayComp(Component):
    delay = Float(0.01, iotype='in')
    
    def __init__(self, arr_size=12):
        super(ABCDArrayComp, self).__init__()
        self.add_trait('a', Array(np.ones(arr_size, float), iotype='in'))
        self.add_trait('b', Array(np.ones(arr_size, float), iotype='in'))
        self.add_trait('c', Array(np.ones(arr_size, float), iotype='out'))
        self.add_trait('d', Array(np.ones(arr_size, float), iotype='out'))

    def execute(self):
        time.sleep(self.delay)
        self.c = self.a + self.b
        self.d = self.a - self.b
        
def _get_model():
    top = set_as_top(Assembly())
    top.add('driver', NTimes(3))
    for i in range(1,5):
        name = 'C%d' % i
        top.add(name, ABCDArrayComp())
        top.driver.workflow.add(name)
        getattr(top, name).mpi.requested_cpus = 1

    top.driver.add_parameter('C1.a[1]', high=100.0, low=0.0)
    top.driver.add_constraint('C2.d[0]>C3.d[0]') 
    
    return top

def _get_model1():
    top = set_as_top(Assembly())
    top.add('driver', NTimes(1))
    for i in range(1,12):
        name = 'C%d' % i
        top.add(name, ABCDArrayComp(22))
        top.driver.workflow.add(name)
        getattr(top, name).mpi.requested_cpus = 1

    conns = [
        ('C1.c','C6.a'),
        ('C2.c','C5.a'),
        ('C3.c','C11.a'),
        ('C3.d','C8.a'),
        ('C5.c','C6.b'),
        ('C5.d','C8.b'),
        ('C8.c','C11.b'),
        ('C8.d','C10.a'),
    ]

    for u,v in conns:
        top.connect(u, v)

    top.driver.add_parameter('C3.a[1]', high=100.0, low=0.0)
    top.driver.add_constraint('C8.d[0]>C11.d[0]') 
    
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

    top.add('subdriver', NTimes(2))

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

if __name__ == '__main__':
    from openmdao.main.mpiwrap import MPI_run

    top = _get_model1()

    MPI_run(top)

    mpiprint(top.driver.workflow._subsystem.dump_parallel_graph(stream=None))
    #mpiprint(top.subdriver.workflow._subsystem.dump_parallel_graph(stream=None))

        

