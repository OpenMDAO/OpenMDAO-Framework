"""
CaseIterator performance analysis.

What we want to know is at what point is concurrent execution 'worthwhile'.
Initial definition of 'worthwhile' could be reduced overall wall time.
This will depend on both duration of execution and the number of cases
to be evaluated.

We use a CaseIterator which simply returns a copy of a 'template' case until
told to stop.  We also use a CaseRecorder which is told the start time and the
CaseIterator in use.  Once the recording request shows a payoff, the recorder
tells the iterator to stop.
"""

import copy
import logging
import os
import platform
import sys
import time

logging.getLogger().setLevel(logging.DEBUG)
logging.getLogger('mp_distributing').setLevel(logging.DEBUG)

from enthought.traits.api import HasTraits

from openmdao.main.api import Assembly, Case, Component, set_as_top
from openmdao.main.interfaces import ICaseIterator, ICaseRecorder
from openmdao.main.resource import ResourceAllocationManager, ClusterAllocator

from openmdao.lib.caseiterators.api import ListCaseIterator
from openmdao.lib.caserecorders.listcaserecorder import ListCaseRecorder
from openmdao.lib.datatypes.api import Float, implements
from openmdao.lib.drivers.simplecid import SimpleCaseIterDriver
from openmdao.lib.drivers.api import CaseIteratorDriver

from openmdao.util.testutil import find_python

MAX_TRIALS = 1000


class Iterator(HasTraits):

    implements(ICaseIterator)

    def __init__(self, case):
        """ Just keeps returning `case` until told to stop. """
        super(Iterator, self).__init__()
        self.case = case
        self.stop = False

    def get_iter(self):
        return self._next_case()

    def _next_case(self):
        """ Generator which just returns list items in-order"""
        while not self.stop:
            yield copy.copy(self.case)
        raise StopIteration


class Recorder(HasTraits):

    implements(ICaseRecorder)

    def __init__(self, iterator, delay):
        """ Consumes cases until elapsed time is < #cases * delay. """
        super(Recorder, self).__init__()
        self.iterator = iterator
        self.delay = delay
        self.start = None
        self.n_cases = 0
        self.payoff_case = -1

    def record(self, case):
        """ Record the case and possibly tell the iterator to stop. """
        self.n_cases += 1
        et = time.time() - self.start
        if et < (self.n_cases * self.delay):
            if self.payoff_case < 0:
                self.payoff_case = self.n_cases
            else:
                self.iterator.stop = True
        if self.n_cases >= MAX_TRIALS:
            self.iterator.stop = True

    def get_iterator(self):
        """ We don't save anything! """
        return None
 

class Sleeper(Component):
    """ Just sleeps for the requested duration. """

    delay = Float(1., units='s', min=0., iotype='in', desc='Time to sleep.')

    def execute(self):
        """ Just delays for the requested amount of time. """
        time.sleep(self.delay)


class SimpleCID(Assembly):
    """ Directly executes cases sequentially. """

    def __init__(self):
        """ Create assembly with Sleeper as sole component. """
        super(SimpleCID, self).__init__()
        self.add('sleeper', Sleeper())
        self.add('driver', SimpleCaseIterDriver())
        self.driver.workflow.add(self.sleeper)


class CID(Assembly):
    """ Executes via CaseIteratorDriver. """

    def __init__(self):
        """ Create assembly with Sleeper as sole component. """
        super(CID, self).__init__()
        self.add('sleeper', Sleeper())
        self.add('driver', CaseIteratorDriver())
        self.driver.workflow.add(self.sleeper)
        self.driver.log_level = logging.DEBUG


def main():
    """
    CaseIteratorDriver performance characterization.
    Need curves showing time-to-completion for various model durations
    """

    initial = 0.01
    limit = 20
    results = {}

    if platform.node().startswith('gxterm'):
        # Initialize cluster allocator.
        use_gx(20)
        resource_desc = {'allocator': 'GX'}
    else:
        resource_desc = {}
    max_servers = ResourceAllocationManager.max_servers(resource_desc)
    print 'max servers', max_servers

    model = set_as_top(CID())
    model.driver.reload_model = False
    model.driver.sequential = False

    # Save to an egg to avoid analysis overhead during run_test().
    print '\nInitializing egg module analysis'
    template = Case(inputs=[('sleeper.delay', None, 1)])
    model.driver.iterator = Iterator(template)
    model.driver.recorder = Recorder(model.driver.iterator, 1)
    start = time.time()
    egg_filename, required_distributions, orphan_modules = \
        model.save_to_egg('caseperf', '0')
    et = time.time() - start
    print '    done in %.2f\n' % et
    os.remove(egg_filename)

    results = run_test(model, initial, limit, max_servers)
    record_results(results)


def run_test(model, initial, limit, max_servers):
    """ Run test with the given model. """
    results = []
    duration = initial
    while duration < limit:
        print 'run_test delay %s' % duration
        template = Case(inputs=[('sleeper.delay', None, duration)])
        model.driver.iterator = Iterator(template)
        model.driver.recorder = Recorder(model.driver.iterator, duration)
        start = time.time()
        model.driver.recorder.start = start
        model.run()
        et = time.time() - start
        n_cases = model.driver.recorder.n_cases
        print '    %d cases done in %.2f (%.2f sec/case)' \
              % (n_cases, et, et/n_cases)
        if n_cases < MAX_TRIALS:
            payoff_case = model.driver.recorder.payoff_case
            if payoff_case < n_cases:
                print '    payoff at %d' % payoff_case
            results.append((duration, payoff_case, n_cases, et))
            if payoff_case == 2 or n_cases <= (max_servers + 1):
                print '    (minimum case limit)'
                break
        else:
            print '    no concurrency advantage'
        duration *= 2.

    return results


def record_results(results):
    """ Records results for test. """
    hostname = platform.node()
    dot = hostname.find('.')
    if dot > 0:
        hostname = hostname[:dot]

    with open('%s.csv' % hostname, 'w') as out:
        out.write('duration, payoff_case\n')
        for duration, payoff_case, n_cases, et in results:
            if payoff_case > 0:
                out.write('%.2f, %d\n' % (duration, payoff_case))


def use_gx(n_hosts=55):
    """ Install GX allocator for `n_hosts`. """
    # This has the side-effet of setting credentials.
    for allocator in ResourceAllocationManager.list_allocators():
        if isinstance(allocator, ClusterAllocator):
            return

    machines = []
    python = find_python()
    for i in range(n_hosts):
        machines.append({'hostname':'gx%02d' % i, 'python':python})
    print '\nInitializing GX cluster allocator'
    start = time.time()
    cluster = ClusterAllocator('GX', machines)
    et = time.time() - start
    print '    done in %.2f' % et
    ResourceAllocationManager.insert_allocator(0, cluster)


if __name__ == '__main__':
    main()

