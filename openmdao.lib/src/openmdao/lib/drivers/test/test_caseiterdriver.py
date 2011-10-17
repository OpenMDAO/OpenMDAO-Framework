"""
Test CaseIteratorDriver.
"""

import logging
import os
import pkg_resources
import sys
import time
import unittest
import nose

import random
import numpy.random as numpy_random

from openmdao.main.api import Assembly, Component, Case, set_as_top
from openmdao.main.eggchecker import check_save_load
from openmdao.main.exceptions import RunStopped
from openmdao.main.resource import ResourceAllocationManager, ClusterAllocator

from openmdao.lib.datatypes.api import Float, Bool, Array
from openmdao.lib.casehandlers.listcaseiter import ListCaseIterator
from openmdao.lib.drivers.caseiterdriver import CaseIteratorDriver
from openmdao.lib.drivers.simplecid import SimpleCaseIterDriver
from openmdao.lib.casehandlers.listcaserecorder import ListCaseRecorder

from openmdao.test.cluster import init_cluster

from openmdao.util.testutil import assert_raises

# Capture original working directory so we can restore in tearDown().
ORIG_DIR = os.getcwd()

# pylint: disable-msg=E1101


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

    def __init__(self, *args, **kwargs):
        super(DrivenComponent, self).__init__(*args, **kwargs)

    def execute(self):
        """ Compute results from input vector. """
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

    def __init__(self, *args, **kwargs):
        super(MyModel, self).__init__(*args, **kwargs)
        self.add('driver', _get_driver())
        self.add('driven', DrivenComponent())
        self.driver.workflow.add('driven')


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

    def test_run_stop_step_resume(self):
        logging.debug('')
        logging.debug('test_run_stop_step_resume')

        self.generate_cases()
        stop_case = self.cases[1]  # Stop after 2 cases run.
        stop_case['driven.stop_exec'] = True
        self.model.driver.iterator = ListCaseIterator(self.cases)
        results = ListCaseRecorder()
        self.model.driver.recorder = results
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
        self.model.driver.extra_reqs = {'allocator': name}
        self.run_cases(sequential=False)

    def run_cases(self, sequential, forced_errors=False, retry=True):
        """ Evaluate cases, either sequentially or across multiple servers. """
        self.model.driver.sequential = sequential
        if not sequential:
            # Try to ensure more than one worker is used.
            self.model.driven.sleep = 0.2
        self.model.driver.iterator = ListCaseIterator(self.cases)
        results = ListCaseRecorder()
        self.model.driver.recorder = results
        self.model.driver.error_policy = 'RETRY' if retry else 'ABORT'

        if retry:
            self.model.run()
            self.assertEqual(len(results), len(self.cases))
            self.verify_results(forced_errors)
        else:
            try:
                self.model.run()
            except Exception as err:
                startmsg = 'driver: Run aborted: Traceback '
                endmsg = 'driven: Forced error\n'
                self.assertEqual(str(err)[:len(startmsg)], startmsg)
                self.assertEqual(str(err)[-len(endmsg):], endmsg)
            else:
                self.fail("Exception expected")

    def verify_results(self, forced_errors=False):
        """ Verify recorded results match expectations. """
        for case in self.model.driver.recorder.cases:
            i = int(case.label)  # Correlation key.
            error_expected = forced_errors and i%4 == 3
            if error_expected:
                startmsg = 'Traceback '
                endmsg = 'driven: Forced error\n'
                self.assertEqual(case.msg[:len(startmsg)], startmsg)
                self.assertEqual(case.msg[-len(endmsg):], endmsg)
            else:
                self.assertEqual(case.msg, None)
                self.assertEqual(case['driven.rosen_suzuki'],
                                 rosen_suzuki(case['driven.x']))
                self.assertEqual(case['driven.sum_y'],
                                 sum(case['driven.y']))

    def test_save_load(self):
        logging.debug('')
        logging.debug('test_save_load')

        self.model.driver.iterator = ListCaseIterator(self.cases)
        results = ListCaseRecorder()
        self.model.driver.recorder = results

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
        self.model.driver.recorder = results

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
        self.model.driver.recorder = results

        self.model.run()

        self.assertEqual(len(results), len(cases))
        msg = "driver: Exception getting case outputs: " \
            "driven: object has no attribute 'sum_z'"
        for case in results.cases:
            self.assertEqual(case.msg, msg)

    def test_noiterator(self):
        logging.debug('')
        logging.debug('test_noiterator')

        # Check resoponse to no iterator set.
        self.model.driver.recorder = ListCaseRecorder()
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
        self.model.driver.extra_reqs = {'no-such-resource': 0}
        self.model.driver.sequential = False
        self.model.driver.iterator = ListCaseIterator([])
        assert_raises(self, 'self.model.run()', globals(), locals(),
                      RuntimeError,
                      'driver: No servers supporting required resources')


if __name__ == '__main__':
    sys.argv.append('--cover-package=openmdao.drivers')
    sys.argv.append('--cover-erase')
    nose.runmodule()

