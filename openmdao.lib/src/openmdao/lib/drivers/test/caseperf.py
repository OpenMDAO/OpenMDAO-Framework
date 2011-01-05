"""
CaseIteratorDriver performance analysis.

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

from openmdao.test.cluster import init_cluster

MAX_TRIALS = 1000


class Iterator(HasTraits):
    """ Just keeps returning `case` until told to stop. """

    implements(ICaseIterator)

    def __init__(self, case):
        super(Iterator, self).__init__()
        self.case = case
        self.stop = False

    def get_iter(self):
        return self._next_case()

    def _next_case(self):
        """ Generator which just returns copies of the given case. """
        while not self.stop:
            yield copy.copy(self.case)
        raise StopIteration


class Recorder(HasTraits):
    """ Consumes cases until elapsed time is < #cases * delay. """

    implements(ICaseRecorder)

    def __init__(self, iterator, delay):
        super(Recorder, self).__init__()
        self.iterator = iterator
        self.delay = delay
        self.start = None
        self.n_cases = 0
        self.payoff_case = -1

    def record(self, case):
        """ Check elapsed time and possibly tell the iterator to stop. """
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


class CID(Assembly):
    """ Executes via CaseIteratorDriver. """

    def __init__(self, extra_reqs=None):
        """ Create assembly with Sleeper as sole component. """
        super(CID, self).__init__()
        self.add('sleeper', Sleeper())
        self.add('driver', CaseIteratorDriver())
        self.driver.extra_reqs = extra_reqs
        self.driver.workflow.add('sleeper')
        self.driver.log_level = logging.DEBUG


def main():
    """ CaseIteratorDriver performance characterization. """
    name = platform.node()
    dot = name.find('.')
    if dot > 0:
        name = name[:dot]

    setup_cluster(encrypted=True)
    run_suite({}, name=name)

    allocator_name = setup_cluster(encrypted=False)
    run_suite({'allocator': allocator_name}, name+'-unencrypted')


def setup_cluster(encrypted=True):
    """ Use openmdao.testing.cluster.init_cluster, but fix 'max_load'. """
    name = init_cluster(encrypted, allow_shell=True)
    for allocator in ResourceAllocationManager.list_allocators():
        if allocator.name == 'LocalHost':
            allocator.max_load = 1.
    return name


def run_suite(resource_desc=None, name=None):
    """ Run suite of tests using `resource_desc` and resord under `name`. """
    resource_desc = resource_desc or {}
    name = name or ''
    print '\n%s' % name

    initial = 0.01
    limit = 20
    results = {}

    max_servers = ResourceAllocationManager.max_servers(resource_desc)
    print 'max servers', max_servers

    model = set_as_top(CID())
    model.driver.reload_model = False
    model.driver.sequential = False

    # Save to an egg to avoid analysis overhead during run_test().
    print '\nInitializing egg module analysis'
    template = Case(inputs=[('sleeper.delay', None, 0.01)])
    model.driver.iterator = Iterator(template)
    model.driver.recorder = Recorder(model.driver.iterator, 1000)
    start = time.time()
    egg_filename, required_distributions, orphan_modules = \
        model.save_to_egg('caseperf', '0')
    et = time.time() - start
    print '    done in %.2f' % et
    os.remove(egg_filename)

    print
    results = run_test(model, initial, limit, max_servers)
    record_results(results, name)


def run_test(model, initial, limit, max_servers):
    """ Run test with the given model. """
    results = []
    duration = initial
    while duration < limit:
        print 'run test, delay %s' % duration
        template = Case(inputs=[('sleeper.delay', None, duration)])
        model.driver.iterator = Iterator(template)
        model.driver.recorder = Recorder(model.driver.iterator, duration)
        start = time.time()
        model.driver.recorder.start = start
        model.run()
        et = time.time() - start
        n_cases = model.driver.recorder.n_cases
        if n_cases > 0:
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
        else:
            print '    no successful cases, aborting'
            break

    return results


def record_results(results, name=None):
    """ Records results for test. """
    with open('%s.csv' % name, 'w') as out:
        out.write('duration, payoff_case\n')
        for duration, payoff_case, n_cases, et in results:
            if payoff_case > 0:
                out.write('%.2f, %d\n' % (duration, payoff_case))


if __name__ == '__main__':
    main()

