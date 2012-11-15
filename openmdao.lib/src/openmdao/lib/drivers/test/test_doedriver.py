"""
Test DOEdriver.
"""

import logging
import nose
import os.path
import pkg_resources
import re
import sys
import unittest

from openmdao.lib.datatypes.api import Event

from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.lib.datatypes.api import Float, Bool
from openmdao.lib.casehandlers.api import SequenceCaseFilter
from openmdao.lib.drivers.doedriver import DOEdriver, NeighborhoodDOEdriver
from openmdao.lib.casehandlers.api import ListCaseRecorder, DumpCaseRecorder
from openmdao.lib.doegenerators.api import OptLatinHypercube, FullFactorial, \
                                           CSVFile

# Capture original working directory so we can restore in tearDown().
ORIG_DIR = os.getcwd()

# pylint: disable-msg=E1101


def replace_uuid(msg):
    """ Replace UUID in `msg` with ``UUID``. """
    pattern = '[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}'
    return re.sub(pattern, 'UUID', msg)


def rosen_suzuki(x0, x1, x2, x3):
    """ Evaluate polynomial from CONMIN manual. """
    return x0**2 - 5.*x0 + x1**2 - 5.*x1 + \
           2.*x2**2 - 21.*x2 + x3**2 + 7.*x3 + 50


class DrivenComponent(Component):
    """ Just something to be driven and compute results. """

    x0 = Float(1., iotype='in')
    y0 = Float(1., iotype='in')  # used just to get ParameterGroup
    x1 = Float(1., iotype='in')
    x2 = Float(1., iotype='in')
    x3 = Float(1., iotype='in')
    err_event = Event()
    stop_exec = Bool(False, iotype='in')
    rosen_suzuki = Float(0., iotype='out')

    def __init__(self):
        super(DrivenComponent, self).__init__()
        self._raise_err = False

    def _err_event_fired(self):
        self._raise_err = True

    def execute(self):
        """ Compute results from input vector. """
        self.rosen_suzuki = rosen_suzuki(self.x0, self.x1, self.x2, self.x3)
        if self._raise_err:
            self.raise_exception('Forced error', RuntimeError)
        if self.stop_exec:
            self.parent.driver.stop()  # Only valid if sequential!


class MyModel(Assembly):
    """ Use DOEdriver with DrivenComponent. """

    def configure(self):
        self.add('driver', DOEdriver())
        self.add('driven', DrivenComponent())
        self.driver.workflow.add('driven')
        self.driver.DOEgenerator = OptLatinHypercube(num_samples=10)
        self.driver.case_outputs = ['driven.rosen_suzuki']
        self.driver.add_parameter(('driven.x0', 'driven.y0'),
                                  low=-10., high=10., scaler=20., adder=10.)
        for name in ['x1', 'x2', 'x3']:
            self.driver.add_parameter("driven.%s" % name,
                                      low=-10., high=10., scaler=20., adder=10.)


class TestCaseDOE(unittest.TestCase):
    """ Test DOEdriver. """

    # Need to be in this directory or there are issues with egg loading.
    directory = pkg_resources.resource_filename('openmdao.lib.drivers', 'test')

    def setUp(self):
        os.chdir(self.directory)
        self.model = set_as_top(MyModel())

    def tearDown(self):
        self.model.pre_delete()
        self.model = None
        if os.path.exists('driver.csv'):
            os.remove('driver.csv')

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
        self.model.driver._call_execute = True
        self.run_cases(sequential=True, forced_errors=True, retry=True)

    def test_sequential_errors_abort(self):
        self.run_cases(sequential=True, forced_errors=True)

    def test_no_parameter(self):
        logging.debug('')
        logging.debug('test_no_parameter')
        try:
            self.model.driver.add_parameter('foobar.blah')
        except AttributeError as err:
            self.assertEqual(str(err), "driver: Can't add parameter"
                             " 'foobar.blah' because it doesn't exist.")

    def test_event_removal(self):
        self.model.driver.add_event('driven.err_event')
        lst = self.model.driver.get_events()
        self.assertEqual(lst, ['driven.err_event'])
        self.model.driver.remove_event('driven.err_event')
        lst = self.model.driver.get_events()
        self.assertEqual(lst, [])

    def test_param_removal(self):
        lst = self.model.driver.list_param_targets()
        self.assertEqual(lst, ['driven.x0', 'driven.y0',
                               'driven.x1', 'driven.x2', 'driven.x3'])
        self.model.driver.remove_parameter('driven.x1')
        lst = self.model.driver.list_param_targets()
        self.assertEqual(lst, ['driven.x0', 'driven.y0',
                               'driven.x2', 'driven.x3'])

    def test_no_event(self):
        logging.debug('')
        logging.debug('test_no_event')
        try:
            self.model.driver.add_event('foobar.blah')
        except AttributeError as err:
            self.assertEqual(str(err), "driver: Can't add event"
                             " 'foobar.blah' because it doesn't exist")
        else:
            self.fail("expected AttributeError")

    def test_nooutput(self):
        logging.debug('')
        logging.debug('test_nooutput')

        results = ListCaseRecorder()
        self.model.driver.recorders = [results]
        self.model.driver.error_policy = 'RETRY'
        self.model.driver.case_outputs.append('driven.sum_z')

        self.model.run()

        self.assertEqual(len(results),
                         self.model.driver.DOEgenerator.num_samples)
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
        self.model.driver.DOEgenerator = None
        try:
            self.model.run()
        except Exception as exc:
            msg = "driver: required plugin 'DOEgenerator' is not present"
            self.assertEqual(str(exc), msg)
        else:
            self.fail('Exception expected')

    def test_norecorder(self):
        logging.debug('')
        logging.debug('test_norecorder')

        self.model.driver.recorders = []
        self.model.run()

    def test_output_error(self):
        class Dummy(Component):
            x = Float(0, iotype="in")
            y = Float(0, iotype="out")
            z = Float(0, iotype="out")

            def execute(self):
                self.y = 10 + self.x

        class Analysis(Assembly):

            def configure(self):
                self.add('d', Dummy())
                self.add('driver', DOEdriver())
                self.driver.DOEgenerator = FullFactorial(2)
                self.driver.recorders = [DumpCaseRecorder()]
                self.driver.add_parameter('d.x', low=0, high=10)
                self.driver.case_outputs = ['d.y', 'd.bad', 'd.z']

        a = Analysis()

        try:
            a.run()
        except Exception as err:
            err = replace_uuid(str(err))
            self.assertTrue(err.startswith('driver: Run aborted: Traceback '))
            self.assertTrue(err.endswith("d (UUID.1-1): 'Dummy' object has no attribute 'bad'"))
        else:
            self.fail("Exception expected")

    def run_cases(self, sequential, forced_errors=False, retry=True):
        # Evaluate cases, either sequentially or across  multiple servers.

        self.model.driver.sequential = sequential
        results = ListCaseRecorder()
        self.model.driver.recorders = [results]
        self.model.driver.error_policy = 'RETRY' if retry else 'ABORT'
        if forced_errors:
            self.model.driver.add_event('driven.err_event')

        if retry:
            self.model.run()
            self.assertEqual(len(results), 10)
            self.verify_results(forced_errors)
        else:
            assert_raises(self, 'self.model.run()', globals(), locals(),
                          RuntimeError, "driver: Run aborted:"
                          " RuntimeError('driven: Forced error',)")

    def test_scaling(self):
        self.model.driver.DOEgenerator = ff = FullFactorial(num_levels=3)
        ff.num_parameters = 4
        for case in self.model.driver._get_cases():
            print case

    def verify_results(self, forced_errors=False):
        # Verify recorded results match expectations.

        for case in self.model.driver.recorders[0].cases:
            if forced_errors:
                expected = 'driven \(UUID.[0-9]+-1\): Forced error'
                msg = replace_uuid(case.msg)
                self.assertTrue(re.match(expected, msg))
            else:
                self.assertEqual(case.msg, None)
                self.assertEqual(case['driven.rosen_suzuki'],
                                 rosen_suzuki(*[case['driven.x%s' % i] for i in range(4)]))

    def test_rerun(self):
        logging.debug('')
        logging.debug('test_rerun')

        self.run_cases(sequential=True)
        orig_cases = self.model.driver.recorders[0].cases

        self.model.driver.DOEgenerator = CSVFile(self.model.driver.doe_filename)
        self.model.driver.record_doe = False
        rerun_seq = (1, 3, 5, 7, 9)
        self.model.driver.case_filter = SequenceCaseFilter(rerun_seq)
        rerun = ListCaseRecorder()
        self.model.driver.recorders[0] = rerun
        self.model.run()

        self.assertEqual(len(orig_cases), 10)
        self.assertEqual(len(rerun.cases), len(rerun_seq))
        for i, case in enumerate(rerun.cases):
            self.assertEqual(case, orig_cases[rerun_seq[i]])


class MyModel2(Assembly):
    """ Use DOEdriver with DrivenComponent. """

    def configure(self):
        self.add('driver', NeighborhoodDOEdriver())
        self.add('driven', DrivenComponent())
        self.driver.workflow.add('driven')
        self.driver.DOEgenerator = OptLatinHypercube(num_samples=10)
        self.driver.case_outputs = ['driven.rosen_suzuki']
        self.driver.add_parameter(('driven.x0', 'driven.y0'),
                                  low=-10., high=10., scaler=20., adder=10.)
        for name in ['x1', 'x2', 'x3']:
            self.driver.add_parameter("driven.%s" % name,
                                      low=-10., high=10., scaler=20., adder=10.)


class TestCaseNeighborhoodDOE(unittest.TestCase):
    """ Test NeighborhoodDOEdriver. """

    # Need to be in this directory or there are issues with egg loading.
    directory = pkg_resources.resource_filename('openmdao.lib.drivers', 'test')

    def setUp(self):
        os.chdir(self.directory)
        self.model = set_as_top(MyModel2())

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
        self.model.driver._call_execute = True
        self.run_cases(sequential=True, forced_errors=True, retry=True)

    def test_sequential_errors_abort(self):
        self.run_cases(sequential=True, forced_errors=True)

    def test_no_parameter(self):
        logging.debug('')
        logging.debug('test_no_parameter')
        try:
            self.model.driver.add_parameter('foobar.blah')
        except AttributeError as err:
            self.assertEqual(str(err), "driver: Can't add parameter"
                             " 'foobar.blah' because it doesn't exist.")

    def test_event_removal(self):
        self.model.driver.add_event('driven.err_event')
        lst = self.model.driver.get_events()
        self.assertEqual(lst, ['driven.err_event'])
        self.model.driver.remove_event('driven.err_event')
        lst = self.model.driver.get_events()
        self.assertEqual(lst, [])

    def test_param_removal(self):
        lst = self.model.driver.list_param_targets()
        self.assertEqual(lst, ['driven.x0', 'driven.y0',
                               'driven.x1', 'driven.x2', 'driven.x3'])
        self.model.driver.remove_parameter('driven.x1')
        lst = self.model.driver.list_param_targets()
        self.assertEqual(lst, ['driven.x0', 'driven.y0',
                               'driven.x2', 'driven.x3'])

    def test_no_event(self):
        logging.debug('')
        logging.debug('test_no_event')
        try:
            self.model.driver.add_event('foobar.blah')
        except AttributeError as err:
            self.assertEqual(str(err), "driver: Can't add event"
                             " 'foobar.blah' because it doesn't exist")
        else:
            self.fail("expected AttributeError")

    def test_nooutput(self):
        logging.debug('')
        logging.debug('test_nooutput')

        results = ListCaseRecorder()
        self.model.driver.recorders = [results]
        self.model.driver.error_policy = 'RETRY'
        self.model.driver.case_outputs.append('driven.sum_z')

        self.model.run()

        self.assertEqual(len(results), 1 + self.model.driver.DOEgenerator.num_samples)
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
        self.model.driver.DOEgenerator = None
        try:
            self.model.run()
        except Exception as exc:
            msg = "driver: required plugin 'DOEgenerator' is not present"
            self.assertEqual(str(exc), msg)
        else:
            self.fail('Exception expected')

    def test_norecorder(self):
        logging.debug('')
        logging.debug('test_norecorder')

        self.model.driver.recorders = []
        self.model.run()

    def test_output_error(self):
        class Dummy(Component):
            x = Float(0, iotype="in")
            y = Float(0, iotype="out")
            z = Float(0, iotype="out")

            def execute(self):
                self.y = 10 + self.x

        class Analysis(Assembly):

            def configure(self):
                self.add('d', Dummy())
                self.add('driver', NeighborhoodDOEdriver())
                self.driver.DOEgenerator = FullFactorial(2)
                self.driver.recorders = [DumpCaseRecorder()]
                self.driver.add_parameter('d.x', low=0, high=10)
                self.driver.case_outputs = ['d.y', 'd.bad', 'd.z']

        a = Analysis()

        try:
            a.run()
        except Exception as err:
            err = replace_uuid(str(err))
            self.assertTrue(err.startswith('driver: Run aborted: Traceback '))
            self.assertTrue(err.endswith("d (UUID.1-1): 'Dummy' object has no attribute 'bad'"))
        else:
            self.fail("Exception expected")

    def run_cases(self, sequential, forced_errors=False, retry=True):
        # Evaluate cases, either sequentially or across  multiple servers.

        self.model.driver.sequential = sequential
        results = ListCaseRecorder()
        self.model.driver.recorders = [results]
        self.model.driver.error_policy = 'RETRY' if retry else 'ABORT'
        if forced_errors:
            self.model.driver.add_event('driven.err_event')

        if retry:
            self.model.run()
            self.assertEqual(len(results), 11)
            self.verify_results(forced_errors)
        else:
            assert_raises(self, 'self.model.run()', globals(), locals(),
                          RuntimeError, "driver: Run aborted:"
                          " RuntimeError('driven: Forced error',)")

    def test_scaling(self):
        self.model.driver.DOEgenerator = ff = FullFactorial(num_levels=3)
        ff.num_parameters = 4
        for case in self.model.driver._get_cases():
            print case

    def verify_results(self, forced_errors=False):
        # Verify recorded results match expectations.

        for case in self.model.driver.recorders[0].cases:
            if forced_errors:
                expected = 'driven \(UUID.[0-9]+-1\): Forced error'
                msg = replace_uuid(case.msg)
                self.assertTrue(re.match(expected, msg))
            else:
                self.assertEqual(case.msg, None)
                self.assertEqual(case['driven.rosen_suzuki'],
                                 rosen_suzuki(*[case['driven.x%s' % i] for i in range(4)]))


if __name__ == "__main__":
    sys.argv.append('--cover-package=openmdao.lib.drivers')
    sys.argv.append('--cover-erase')
    nose.runmodule()
