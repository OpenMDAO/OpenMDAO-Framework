# pylint: disable-msg=C0111,C0103

import math

from openmdao.main.api import Assembly, Component, Driver, set_as_top
from openmdao.main.datatypes.api import Float, Array, List, Dict
from openmdao.main.hasobjective import HasObjectives
from openmdao.main.hasconstraints import HasConstraints
from openmdao.main.hasparameters import HasParameters
from openmdao.util.decorators import add_delegate
from openmdao.util.testutil import assert_rel_error
import openmdao.main.pseudocomp as pcompmod  # to keep pseudocomp names consistent in tests
from openmdao.util.graph import get_valids

exec_order = []

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
        print "count = ",self._count
        super(NTimes, self).run_iteration()

    def continue_iteration(self):
        return self._count < self.n


class Simple(Component):
    a = Float(iotype='in', units='ft')
    b = Float(iotype='in', units='ft')
    c = Float(iotype='out', units='ft')
    d = Float(iotype='out', units='ft')
    
    def __init__(self):
        super(Simple, self).__init__()
        self.a = 1
        self.b = 2
        self.c = 3
        self.d = -1

    def execute(self):
        global exec_order
        exec_order.append(self.name)
        self.c = self.a + self.b
        self.d = self.a - self.b
        

allcomps = ['sub.comp1','sub.comp2','sub.comp3','sub.comp4','sub.comp5','sub.comp6',
            'comp7','comp8']

topouts = ['sub.c2', 'sub.c4', 'sub.d1', 'sub.d3','sub.d5'
           'comp7.c', 'comp7.d','comp8.c', 'comp8.d']

topins = ['sub.a1', 'sub.a3', 'sub.b2', 'sub.b4','sub.b6'
          'comp7.a', 'comp7.b','comp8.a', 'comp8.b']

subins = ['comp1.a', 'comp1.b',
          'comp2.a', 'comp2.b',
          'comp3.a', 'comp3.b',
          'comp4.a', 'comp4.b',
          'comp5.a', 'comp5.b',
          'comp6.a', 'comp6.b',]

subouts = ['comp1.c', 'comp1.d',
           'comp2.c', 'comp2.d',
           'comp3.c', 'comp3.d',
           'comp4.c', 'comp4.d',
           'comp5.c', 'comp5.d',
           'comp6.c', 'comp6.d',]


subvars = subins+subouts

def fullvnames(cname, vnames):
    return ['.'.join([cname,n]) for n in vnames]

def _nested_model():
    global exec_order
    exec_order = []
    top = set_as_top(Assembly())
    top.add('driver', NTimes(3))
    top.add('sub', Assembly())
    top.sub.add('driver', NTimes(2))
    top.add('comp7', Simple())
    top.add('comp8', Simple())
    sub = top.sub
    sub.add('comp1', Simple())
    sub.add('comp2', Simple())
    sub.add('comp3', Simple())
    sub.add('comp4', Simple())
    sub.add('comp5', Simple())
    sub.add('comp6', Simple())

    top.driver.workflow.add(['comp7', 'sub', 'comp8'])
    sub.driver.workflow.add(['comp1','comp2','comp3',
                             'comp4','comp5','comp6'])

    sub.create_passthrough('comp1.a', 'a1')
    sub.create_passthrough('comp2.b', 'b2')
    sub.create_passthrough('comp3.a', 'a3')
    sub.create_passthrough('comp3.d', 'd3')
    sub.create_passthrough('comp4.b', 'b4')
    sub.create_passthrough('comp4.c', 'c4')
    sub.create_passthrough('comp6.b', 'b6')
    sub.create_passthrough('comp2.c', 'c2')
    sub.create_passthrough('comp1.d', 'd1')
    sub.create_passthrough('comp5.d', 'd5')
    
    return top

if __name__ == '__main__':
    from mpi4py import MPI
    from openmdao.main.mpiwrap import MPI_run

    pcompmod._count = 0
    top = _nested_model()
    sub = top.sub
    sub.connect('comp1.c', 'comp4.a')
    sub.connect('comp5.c', 'comp1.b')
    sub.connect('comp2.d', 'comp5.b')
    sub.connect('comp3.c', 'comp5.a')
    sub.connect('comp4.d', 'comp6.a')
    
    top.connect('sub.c4', 'comp8.a')
    
    top.connect('comp7.c', 'sub.a3')
    top.connect('sub.d3', 'comp8.b')

    MPI_run(top)

        

