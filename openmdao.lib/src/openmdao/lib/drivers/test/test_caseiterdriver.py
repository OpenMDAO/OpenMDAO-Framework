"""
Test CaseIteratorDriver.
"""

import logging
import os
import pkg_resources
import re
import sys
import time
import unittest
import nose

import random
import numpy.random as numpy_random

from openmdao.main.api import Assembly, Component, Case, set_as_top
from openmdao.main.interfaces import ICaseIterator
from openmdao.main.eggchecker import check_save_load
from openmdao.main.exceptions import RunStopped
from openmdao.main.resource import ResourceAllocationManager, ClusterAllocator

from openmdao.lib.datatypes.api import Float, Bool, Array, Int, Slot, Str
from openmdao.lib.drivers.caseiterdriver import CaseIteratorDriver
from openmdao.lib.drivers.simplecid import SimpleCaseIterDriver
from openmdao.lib.casehandlers.api import ListCaseRecorder, ListCaseIterator, \
                                          SequenceCaseFilter

from openmdao.test.cluster import init_cluster

from openmdao.util.testutil import assert_raises

# Capture original working directory so we can restore in tearDown().
ORIG_DIR = os.getcwd()

# pylint: disable-msg=E1101

def replace_uuid(msg):
    """ Replace UUID in `msg` with ``UUID``. """
    pattern = '[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}'
    return re.sub(pattern, 'UUID', msg)


def rosen_suzuki(x):
    """ Evaluate polynomial from CONMIN manual. """
    return x[0]**2 - 5.*x[0] + x[1]**2 - 5.*x[1] + \
           2.*x[2]**2 - 21.*x[2] + x[3]**2 + 7.*x[3] + 50


class DrivenComponent(Component):
    """ Just something to be driven and compute results. """

    x = Array([1., 1., 1., 1.], iotype='in')
    y = Array([1., 1., 1., 1.], iotype='in')
    raise_error = Bool(False, iotype='in')
    stop_exec = Bool(False, iotype='in')
    sleep = Float(0., iotype='in')

    rosen_suzuki = Float(0., iotype='out')
    sum_y = Float(0., iotype='out')
    extra = Float(1.5, iotype='out')

    def __init__(self):
        super(DrivenComponent, self).__init__()

    def execute(self):
        """ Compute results from input vector. """
        
        self.extra = 2.5
        
        if self.sleep:
            time.sleep(self.sleep)
        self.rosen_suzuki = rosen_suzuki(self.x)
        self.sum_y = sum(self.y)
        if self.raise_error:
            self.raise_exception('Forced error', RuntimeError)
        if self.stop_exec:
            self.parent.driver.stop()  # Only valid if sequential!


def _get_driver():
    return CaseIteratorDriver()
    #return SimpleCaseIterDriver()


class MyModel(Assembly):
    """ Use CaseIteratorDriver with DrivenComponent. """

    def configure(self):
        self.add('driver', _get_driver())
        self.add('driven', DrivenComponent())
        self.driver.workflow.add('driven')


class Generator(Component):
    """ Generates cases to be evaluated. """

    cases = Slot(ICaseIterator, iotype='out')

    def execute(self):
        """ Generate some cases to be evaluated. """
        cases = []
        for i in range(10):
            inputs = [('driven.x', numpy_random.normal(size=4)),
                      ('driven.y', numpy_random.normal(size=10)),
                      ('driven.raise_error', False),
                      ('driven.stop_exec', False)]
            outputs = ['driven.rosen_suzuki', 'driven.sum_y']
            cases.append(Case(inputs, outputs, label=str(i)))
        self.cases = ListCaseIterator(cases)


class Verifier(Component):
    """ Verifies evaluated cases. """

    cases = Slot(ICaseIterator, iotype='in')

    def execute(self):
        """ Verify evaluated cases. """
        for case in self.cases:
            i = int(case.label)  # Correlation key.
            self._logger.critical('verifying case %d', i)
            assert case.msg is None
            assert case['driven.rosen_suzuki'] == rosen_suzuki(case['driven.x'])
            assert case['driven.sum_y'] == sum(case['driven.y'])
            assert case['driven.extra'] == 2.5


class TracedComponent(Component):
    """ Used to check iteration coordinates. """

    inp = Int(iotype='in')
    itername = Str(iotype='out')

    def execute(self):
        """ Record iteration coordinate. """
        self.itername = self.get_itername()


class TestCase(unittest.TestCase):
    """ Test CaseIteratorDriver. """

    # Need to be in this directory or there are issues with egg loading.
    directory = pkg_resources.resource_filename('openmdao.lib.drivers', 'test')

    def setUp(self):
        random.seed(10)
        numpy_random.seed(10)
        
        os.chdir(self.directory)
        self.model = set_as_top(MyModel())
        self.generate_cases()

    def generate_cases(self, force_errors=False):
        self.cases = []
        for i in range(10):
            raise_error = force_errors and i%4 == 3
            inputs = [('driven.x', numpy_random.normal(size=4)),
                      ('driven.y', numpy_random.normal(size=10)),
                      ('driven.raise_error', raise_error),
                      ('driven.stop_exec', False)]
            outputs = ['driven.rosen_suzuki','driven.sum_y']
            self.cases.append(Case(inputs, outputs, label=str(i)))

    def tearDown(self):
        self.model.pre_delete()
        self.model = None

        # Verify we didn't mess-up working directory.
        end_dir = os.getcwd()
        os.chdir(ORIG_DIR)
        if os.path.realpath(end_dir).lower() != os.path.realpath(self.directory).lower():
            self.fail('Ended in %s, expected %s' % (end_dir, self.directory))

    def test_sequential(self):
        logging.debug('')
        logging.debug('test_sequential')
        self.run_cases(sequential=True)

    def test_sequential_errors(self):
        logging.debug('')
        logging.debug('test_sequential_errors')
        self.generate_cases(force_errors=True)
        self.run_cases(sequential=True, forced_errors=True, retry=False)
        self.run_cases(sequential=True, forced_errors=True, retry=True)
        
    def test_output_errors(self):
        inputs = [('driven.x', numpy_random.normal(size=4)),
                  ('driven.y', numpy_random.normal(size=10)),
                  ('driven.raise_error', False),
                  ('driven.stop_exec', False)]
        outputs = ['driven.rosen_suzuki','driven.foobar']
        self.cases = [Case(inputs, outputs, label='1')]
        self.model.driver.sequential = True
        self.model.driver.iterator = ListCaseIterator(self.cases)
        self.model.driver.recorders = [ListCaseRecorder()]
        self.model.driver.printvars = ['driven.extra']
        self.model.driver.error_policy = 'RETRY'
        self.model.run()
        
    def test_run_stop_step_resume(self):
        logging.debug('')
        logging.debug('test_run_stop_step_resume')

        self.generate_cases()
        stop_case = self.cases[1]  # Stop after 2 cases run.
        stop_case['driven.stop_exec'] = True
        self.model.driver.iterator = ListCaseIterator(self.cases)
        results = ListCaseRecorder()
        self.model.driver.recorders = [results]
        self.model.driver.printvars = ['driven.extra']
        self.model.driver.sequential = True

        try:
            self.model.run()
        except RunStopped:
            self.assertEqual(len(results), 2)
            self.verify_results()
        else:
            self.fail('Expected RunStopped')

        self.model.driver.step()
        self.assertEqual(len(results), 3)
        self.verify_results()

        self.model.driver.step()
        self.assertEqual(len(results), 4)
        self.verify_results()

        self.model.driver.resume()
        self.assertEqual(len(results), len(self.cases))
        self.verify_results()

        try:
            self.model.driver.resume()
        except RuntimeError as exc:
            self.assertEqual(str(exc), 'driver: Run already complete')
        else:
            self.fail('Expected RuntimeError')

    def test_concurrent(self):
        # This can always test using a LocalAllocator (forked processes).
        # It can also use a ClusterAllocator if the environment looks OK.
        logging.debug('')
        logging.debug('test_concurrent')
        init_cluster(encrypted=True, allow_shell=True)
        self.run_cases(sequential=False)

    def test_concurrent_errors(self):
        logging.debug('')
        logging.debug('test_concurrent_errors')
        init_cluster(encrypted=True, allow_shell=True)
        self.generate_cases(force_errors=True)
        self.run_cases(sequential=False, forced_errors=True, retry=False)
        self.run_cases(sequential=False, forced_errors=True, retry=True)

    def test_unencrypted(self):
        logging.debug('')
        logging.debug('test_unencrypted')
        name = init_cluster(encrypted=False, allow_shell=True)
        self.model.driver.extra_resources = {'allocator': name}
        self.run_cases(sequential=False)

    def run_cases(self, sequential, forced_errors=False, retry=True):
        """ Evaluate cases, either sequentially or across multiple servers. """
        self.model.driver.sequential = sequential
        if not sequential:
            # Try to ensure more than one worker is used.
            self.model.driven.sleep = 0.2
        self.model.driver.iterator = ListCaseIterator(self.cases)
        results = ListCaseRecorder()
        self.model.driver.recorders = [results]
        self.model.driver.printvars = ['driven.extra']
        self.model.driver.error_policy = 'RETRY' if retry else 'ABORT'

        if retry:
            self.model.run()
            self.assertEqual(len(results), len(self.cases))
            self.verify_results(forced_errors)
        else:
            try:
                self.model.run()
            except Exception as err:
                err = replace_uuid(str(err))
                startmsg = 'driver: Run aborted: Traceback '
                endmsg = 'driven (UUID.4-1): Forced error'
                self.assertEqual(str(err)[:len(startmsg)], startmsg)
                self.assertEqual(str(err)[-len(endmsg):], endmsg)
            else:
                self.fail("Exception expected")

    def verify_results(self, forced_errors=False):
        """ Verify recorded results match expectations. """
        for case in self.model.driver.recorders[0].cases:
            i = int(case.label)  # Correlation key.
            error_expected = forced_errors and i%4 == 3
            if error_expected:
                expected = 'driven \(UUID.[0-9]+-1\): Forced error'
                msg = replace_uuid(case.msg)
                self.assertTrue(re.match(expected, msg))
            else:
                self.assertEqual(case.msg, None)
                self.assertEqual(case['driven.rosen_suzuki'],
                                 rosen_suzuki(case['driven.x']))
                self.assertEqual(case['driven.sum_y'],
                                 sum(case['driven.y']))
                self.assertEqual(case['driven.extra'],
                                 2.5)

    def test_save_load(self):
        logging.debug('')
        logging.debug('test_save_load')

        self.model.driver.iterator = ListCaseIterator(self.cases)
        results = ListCaseRecorder()
        self.model.driver.printvars = ['driven.extra']
        self.model.driver.recorders = [results]

        # Set local dir in case we're running in a different directory.
        py_dir = self.directory

        # Exercise check_save_load().
        retcode = check_save_load(self.model, py_dir=py_dir)
        self.assertEqual(retcode, 0)

    def test_noinput(self):
        logging.debug('')
        logging.debug('test_noinput')

        # Create cases with missing input 'dc.z'.
        cases = []
        for i in range(2):
            inputs = [('driven.x', numpy_random.normal(size=4)),
                      ('driven.z', numpy_random.normal(size=10))]
            outputs = [('driven.rosen_suzuki', None),
                       ('driven.sum_y', None)]
            cases.append(Case(inputs, outputs))

        self.model.driver.iterator = ListCaseIterator(cases)
        results = ListCaseRecorder()
        self.model.driver.recorders = [results]
        self.model.driver.printvars = ['driven.extra']

        self.model.run()

        self.assertEqual(len(results), len(cases))
        msg = "driver: Exception setting case inputs:" \
              " driven: object has no attribute 'z'"
        for case in results.cases:
            self.assertEqual(case.msg, msg)

    def test_nooutput(self):
        logging.debug('')
        logging.debug('test_nooutput')

        # Create cases with missing output 'dc.sum_z'.
        cases = []
        for i in range(2):
            inputs = [('driven.x', numpy_random.normal(size=4)),
                      ('driven.y', numpy_random.normal(size=10))]
            outputs = [('driven.rosen_suzuki', None),
                       ('driven.sum_z', None)]
            cases.append(Case(inputs, outputs))

        self.model.driver.iterator = ListCaseIterator(cases)
        results = ListCaseRecorder()
        self.model.driver.recorders = [results]
        self.model.driver.printvars = ['driven.extra']
        self.model.driver.error_policy = 'RETRY'

        self.model.run()

        self.assertEqual(len(results), len(cases))
        for case in results.cases:
            expected = "driver: Exception getting case outputs: " \
                       "driven \(UUID.[0-9]+-1\): " \
                       "'DrivenComponent' object has no attribute 'sum_z'"
            msg = replace_uuid(case.msg)
            self.assertTrue(re.match(expected, msg))

    def test_noiterator(self):
        logging.debug('')
        logging.debug('test_noiterator')

        # Check resoponse to no iterator set.
        self.model.driver.recorders = [ListCaseRecorder()]
        self.model.driver.printvars = ['driven.extra']
        try:
            self.model.run()
        except ValueError as exc:
            msg = "driver: iterator has not been set"
            self.assertEqual(str(exc), msg)
        else:
            self.fail('ValueError expected')

    def test_norecorder(self):
        logging.debug('')
        logging.debug('test_norecorder')

        # Check response to no recorder set.
        self.model.driver.iterator = ListCaseIterator([])
        self.model.run()

    def test_noresource(self):
        logging.debug('')
        logging.debug('test_noresource')

        # Check response to unsupported resource.
        self.model.driver.extra_resources = {'allocator': 'LocalHost',
                                             'localhost': False}
        self.model.driver.sequential = False
        self.model.driver.iterator = ListCaseIterator([])
        assert_raises(self, 'self.model.run()', globals(), locals(),
                      RuntimeError,
                      'driver: No servers supporting required resources')

    def test_connections(self):
        logging.debug('')
        logging.debug('test_connections')

        top = Assembly()
        top.add('generator', Generator())
        top.add('cid', CaseIteratorDriver())
        top.add('driven', DrivenComponent())
        top.add('verifier', Verifier())

        top.driver.workflow.add(('generator', 'cid', 'verifier'))
        top.cid.workflow.add('driven')
        top.cid.printvars = ['driven.extra']

        top.connect('generator.cases', 'cid.iterator')
        top.connect('cid.evaluated', 'verifier.cases')

        top.run()

    def test_rerun(self):
        logging.debug('')
        logging.debug('test_rerun')

        self.run_cases(sequential=True)
        orig_cases = self.model.driver.recorders[0].cases
        self.model.driver.iterator = ListCaseIterator(orig_cases)
        rerun_seq = (1, 3, 5, 7, 9)
        self.model.driver.filter = SequenceCaseFilter(rerun_seq)
        rerun = ListCaseRecorder()
        self.model.driver.printvars = ['driven.extra']
        self.model.driver.recorders[0] = rerun
        self.model.run()

        self.assertEqual(len(orig_cases), 10)
        self.assertEqual(len(rerun.cases), len(rerun_seq))
        for i, case in enumerate(rerun.cases):
            self.assertEqual(case, orig_cases[rerun_seq[i]])

    def test_itername(self):
        logging.debug('')
        logging.debug('test_itername')

        top = set_as_top(Assembly())
        top.add('driver', CaseIteratorDriver())
        top.add('comp1', TracedComponent())
        top.add('comp2', TracedComponent())
        top.driver.workflow.add(('comp1', 'comp2'))

        cases = []
        for i in range(3):
            cases.append(Case(label=str(i),
                              inputs=(('comp1.inp', i), ('comp2.inp', i)),
                              outputs=(('comp1.itername', 'comp2.itername'))))
        # Sequential.
        top.driver.iterator = ListCaseIterator(cases)
        top.run()
        self.verify_itername(top.driver.evaluated)

        # Concurrent.
        top.driver.sequential = False
        top.driver.iterator = ListCaseIterator(cases)
        top.run()
        self.verify_itername(top.driver.evaluated)

    def verify_itername(self, cases, subassembly=False):
        # These iternames will have the case's uuid prepended.
        expected = (('1-1', '1-2'),
                    ('2-1', '2-2'),
                    ('3-1', '3-2'))

        for case in cases:
            logging.debug('%s: %r %r', case.label,
                          case['comp1.itername'], case['comp2.itername'])
            i = int(case.label)
            prefix1, dot, iter1 = case['comp1.itername'].partition('.')
            prefix2, dot, iter2 = case['comp2.itername'].partition('.')
            prefix = '1-1' if subassembly else case.uuid
            self.assertEqual(prefix1, prefix)
            self.assertEqual(iter1, expected[i][0])
            self.assertEqual(prefix2, prefix)
            self.assertEqual(iter2, expected[i][1])

    def test_subassembly(self):
        logging.debug('')
        logging.debug('test_subassembly')

        top = set_as_top(Assembly())
        sub = top.add('sub', Assembly())
        sub.force_execute = True
        top.driver.workflow.add('sub')

        sub.add('driver', CaseIteratorDriver())
        sub.add('comp1', TracedComponent())
        sub.add('comp2', TracedComponent())
        sub.driver.workflow.add(('comp1', 'comp2'))

        cases = []
        for i in range(3):
            cases.append(Case(label=str(i),
                              inputs=(('comp1.inp', i), ('comp2.inp', i)),
                              outputs=(('comp1.itername', 'comp2.itername'))))
        # Sequential.
        sub.driver.iterator = ListCaseIterator(cases)
        top.run()
        self.verify_itername(sub.driver.evaluated, subassembly=True)

        # Concurrent.
        sub.driver.sequential = False
        sub.driver.iterator = ListCaseIterator(cases)
        top.run()
        self.verify_itername(sub.driver.evaluated, subassembly=True)


if __name__ == '__main__':
    sys.argv.append('--cover-package=openmdao.lib.drivers')
    sys.argv.append('--cover-erase')
    nose.runmodule()

