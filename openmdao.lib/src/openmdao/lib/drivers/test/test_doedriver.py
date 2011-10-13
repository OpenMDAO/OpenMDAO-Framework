"""
Test DOEdriver.
"""

import glob
import logging
import os.path
import pkg_resources
import platform
import sys
import unittest

from openmdao.lib.datatypes.api import Event

from openmdao.main.api import Assembly, Component, Case, set_as_top
from openmdao.main.exceptions import RunStopped
from openmdao.main.resource import ResourceAllocationManager, ClusterAllocator
from openmdao.lib.datatypes.api import Float, Bool, Array
from openmdao.lib.casehandlers.listcaseiter import ListCaseIterator
from openmdao.lib.drivers.doedriver import DOEdriver
from openmdao.lib.casehandlers.listcaserecorder import ListCaseRecorder
from openmdao.lib.doegenerators.optlh import OptLatinHypercube
from openmdao.lib.doegenerators.full_factorial import FullFactorial
from openmdao.main.eggchecker import check_save_load
from openmdao.util.testutil import find_python


# Users who have ssh configured correctly for testing.
SSH_USERS = []

# Capture original working directory so we can restore in tearDown().
ORIG_DIR = os.getcwd()

# pylint: disable-msg=E1101


def rosen_suzuki(x0,x1,x2,x3):
    """ Evaluate polynomial from CONMIN manual. """
    return x0**2 - 5.*x0 + x1**2 - 5.*x1 + \
           2.*x2**2 - 21.*x2 + x3**2 + 7.*x3 + 50

class DrivenComponent(Component):
    """ Just something to be driven and compute results. """

    x0 = Float(1., iotype='in')
    y0 = Float(1., iotype='in') #used just to get ParameterGroup
    x1 = Float(1., iotype='in')
    x2 = Float(1., iotype='in')
    x3 = Float(1., iotype='in')
    err_event = Event()
    stop_exec = Bool(False, iotype='in')
    rosen_suzuki = Float(0., iotype='out')
    
    def __init__(self, *args, **kwargs):
        super(DrivenComponent, self).__init__(*args, **kwargs)
        self._raise_err = False
    
    def _err_event_fired(self):
        self._raise_err = True

    def execute(self):
        """ Compute results from input vector. """
        self.rosen_suzuki = rosen_suzuki(self.x0,self.x1,self.x2,self.x3)
        if self._raise_err:
            self.raise_exception('Forced error', RuntimeError)
        if self.stop_exec:
            self.parent.driver.stop()  # Only valid if sequential!

class MyModel(Assembly):
    """ Use DOEdriver with DrivenComponent. """

    def __init__(self, *args, **kwargs):
        super(MyModel, self).__init__(*args, **kwargs)
        self.add('driver', DOEdriver())
        self.add('driven', DrivenComponent())
        self.driver.workflow.add('driven')
        self.driver.DOEgenerator = OptLatinHypercube(num_samples=10)
        self.driver.case_outputs = ['driven.rosen_suzuki']
        self.driver.add_parameter(('driven.x0','driven.y0'),low=-10.,high=10.,
                                      scaler=20., adder=10.)
        for name in ['x1','x2', 'x3']:
            self.driver.add_parameter("driven.%s"%name,low=-10.,high=10.,
                                      scaler=20., adder=10.)
                                    


class TestCase(unittest.TestCase):
    """ Test DOEdriver. """

    # Need to be in this directory or there are issues with egg loading.
    directory = pkg_resources.resource_filename('openmdao.lib.drivers', 'test')

    def setUp(self):
        os.chdir(self.directory)
        self.model = set_as_top(MyModel())

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
            self.assertEqual(str(err), 
                             "driver: Can't add parameter 'foobar.blah' because it doesn't exist.")
            
    def test_event_removal(self):
        self.model.driver.add_event('driven.err_event')
        lst = self.model.driver.get_events()
        self.assertEqual(lst, ['driven.err_event'])
        self.model.driver.remove_event('driven.err_event')
        lst = self.model.driver.get_events()
        self.assertEqual(lst, [])
        
    def test_param_removal(self):
        lst = self.model.driver.list_param_targets()
        self.assertEqual(lst, ['driven.x0', 'driven.y0', 'driven.x1', 'driven.x2', 'driven.x3'])
        self.model.driver.remove_parameter('driven.x1')
        lst = self.model.driver.list_param_targets()
        self.assertEqual(lst, ['driven.x0', 'driven.y0', 'driven.x2', 'driven.x3'])

    def test_no_event(self):
        logging.debug('')
        logging.debug('test_no_event')
        try:
            self.model.driver.add_event('foobar.blah')
        except AttributeError as err:
            self.assertEqual(str(err), 
                             "driver: Can't add event 'foobar.blah' because it doesn't exist")
        else:
            self.fail("expected AttributeError")

    def test_nooutput(self):
        logging.debug('')
        logging.debug('test_nooutput')

        results = ListCaseRecorder()
        self.model.driver.recorder = results
        self.model.driver.case_outputs.append('driven.sum_z')
        
        self.model.run()

        self.assertEqual(len(results), self.model.driver.DOEgenerator.num_sample_points)
        msg = "driver: Exception getting case outputs: " \
            "driven: object has no attribute 'sum_z'"
        for case in results.cases:
            self.assertEqual(case.msg, msg)

    def test_noiterator(self):
        logging.debug('')
        logging.debug('test_noiterator')

        # Check resoponse to no iterator set.
        self.model.driver.recorder = ListCaseRecorder()
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

        self.model.driver.recorder = None
        self.model.run()

    def run_cases(self, sequential, forced_errors=False, retry=True):
        # Evaluate cases, either sequentially or across  multiple servers.
        
        self.model.driver.sequential = sequential
        results = ListCaseRecorder()
        self.model.driver.recorder = results
        self.model.driver.error_policy = 'RETRY' if retry else 'ABORT'
        if forced_errors:
            self.model.driver.add_event('driven.err_event')

        if retry:
            self.model.run()
            self.assertEqual(len(results), 10)
            self.verify_results(forced_errors)
        else:
            assert_raises(self, 'self.model.run()', globals(), locals(),
                          RuntimeError,
                          "driver: Run aborted: RuntimeError('driven: Forced error',)")

    def test_scaling(self):
        self.model.driver.DOEgenerator = ff = FullFactorial(num_levels=3)
        ff.num_parameters = 4
        for case in self.model.driver._get_cases():
            print case
        
    def verify_results(self, forced_errors=False):
        # Verify recorded results match expectations.
        
        for case in self.model.driver.recorder.cases:
            if forced_errors:
                self.assertEqual(case.msg[:10], 'Traceback ')
                self.assertEqual(case.msg[-21:], 'driven: Forced error\n')
            else:
                self.assertEqual(case.msg, None)
                self.assertEqual(case['driven.rosen_suzuki'],
                                 rosen_suzuki(*[case['driven.x%s'%i] for i in range(4)]))

        

if __name__ == "__main__":
    unittest.main()
