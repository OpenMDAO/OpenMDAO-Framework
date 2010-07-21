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

import numpy.random

from enthought.traits.api import TraitError, Event

from openmdao.main.api import Assembly, Component, Case, set_as_top
from openmdao.main.exceptions import RunStopped
from openmdao.main.resource import ResourceAllocationManager, ClusterAllocator
from openmdao.lib.api import Float, Bool, Array, ListCaseIterator
from openmdao.lib.drivers.doedriver import DOEdriver
from openmdao.lib.caserecorders.listcaserecorder import ListCaseRecorder
from openmdao.lib.doegenerators.optlh import OptLatinHypercube
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
    x1 = Float(1., iotype='in')
    x2 = Float(1., iotype='in')
    x3 = Float(1., iotype='in', low=-11., high=11.)
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
        self.driver.workflow.add(self.driven)
        self.driver.DOEgenerator = OptLatinHypercube(10,4)
        self.driver.case_outputs = ['driven.rosen_suzuki']
        for name in ['x0', 'x1','x2', 'x3']:
            self.driver.add_parameter("driven.%s"%name,low=-10.,high=10.)
                                    


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
        if end_dir.lower() != self.directory.lower():
            self.fail('Ended in %s, expected %s' % (end_dir, self.directory))

    def test_sequential(self):
        logging.debug('')
        logging.debug('test_sequential')
        self.run_cases(sequential=True)

    def test_sequential_errors(self):
        logging.debug('')
        logging.debug('test_sequential_errors')
        self.model.driver._call_execute = True
        self.run_cases(sequential=True, forced_errors=True)
        

    #def test_run_stop_step_resume(self):
        #logging.debug('')
        #logging.debug('test_run_stop_step_resume')

        #self.generate_cases()
        #stop_case = self.cases[1]  # Stop after 2 cases run.
        #stop_case.inputs[3] = ('driven.stop_exec', None, True)
        #self.model.driver.iterator = ListCaseIterator(self.cases)
        #results = []
        #self.model.driver.recorder = results
        #self.model.driver.sequential = True

        #try:
            #self.model.run()
        #except RunStopped:
            #self.assertEqual(len(results), 2)
            #self.verify_results()
        #else:
            #self.fail('Expected RunStopped')

        #self.model.driver.step()
        #self.assertEqual(len(results), 3)
        #self.verify_results()

        #self.model.driver.step()
        #self.assertEqual(len(results), 4)
        #self.verify_results()

        #self.model.driver.resume()
        #self.assertEqual(len(results), len(self.cases))
        #self.verify_results()

        #try:
            #self.model.driver.resume()
        #except RuntimeError as exc:
            #self.assertEqual(str(exc), 'driver: Run already complete')
        #else:
            #self.fail('Expected RuntimeError')

    #def test_concurrent(self):
        ## FIXME: temporarily disable this test on windows because it loops
        ## over a set of tests forever when running under a virtualenv
        #if sys.platform == 'win32':
            #return
        ## This can always test using a LocalAllocator (forked processes).
        ## It can also use a ClusterAllocator if the environment looks OK.
        #logging.debug('')
        #logging.debug('test_concurrent')

        ## Ensure we aren't held up by local host load problems.
        #local = ResourceAllocationManager.get_allocator(0)
        #local.max_load = 10

        #if sys.platform != 'win32':
            ## ssh server not typically available on Windows.
            #machines = []
            #node = platform.node()
            #python = find_python()
            #if node.startswith('gxterm'):
                ## User environment assumed OK on this GRC cluster front-end.
                #for i in range(55):
                    #machines.append({'hostname':'gx%02d' % i, 'python':python})
            #elif self.local_ssh_available():
                #machines.append({'hostname':node, 'python':python})
            #if machines:
                #name = node.replace('.', '_')
                #cluster = ClusterAllocator(name, machines)
                #ResourceAllocationManager.insert_allocator(0, cluster)

        #self.run_cases(sequential=False)
        #self.assertEqual(glob.glob('Sim-*'), [])

        #logging.debug('')
        #logging.debug('test_concurrent_errors')
        ##self.generate_cases(force_errors=True)
        #self.model.driver._call_execute = True
        #self.run_cases(sequential=False, forced_errors=True)
        #self.assertEqual(glob.glob('Sim-*'), [])

    #@staticmethod
    #def local_ssh_available():
        #""" Return True if this user has an authorized key for this machine. """
        #user = os.environ['USER']
## Avoid problems with users who don't have a valid environment.
        #if user not in SSH_USERS:
            #return False
        #home = os.environ['HOME']
        #node = platform.node()
        #keyfile = os.path.join(home, '.ssh', 'authorized_keys')
        #try:
            #with open(keyfile, 'r') as keys:
                #for line in keys:
                    #if line.find(user+'@'+node) > 0:
                        #return True
                #return False
        #except IOError:
            #return False

    def test_no_parameter(self):
        logging.debug('')
        logging.debug('test_no_parameter')
        try:
            self.model.driver.add_parameter('foobar.blah')
        except AttributeError as err:
            self.assertEqual(str(err), 
                             "driver: Can't add parameter 'foobar.blah' because it doesn't exist.")
            
    def test_param_high_low_errs(self):
        self.model.driver.clear_parameters()
        try:
            self.model.driver.add_parameter('driven.x3', low=-20.)
        except ValueError as err:
            self.assertEqual(str(err), "driver: Trying to add parameter 'driven.x3', "
                             "but the lower limit supplied (-20.0) exceeds the built-in "
                             "lower limit (-11.0).")
        else:
            self.fail("expected ValueError")

        try:
            self.model.driver.add_parameter('driven.x3', high=20.)
        except ValueError as err:
            self.assertEqual(str(err), "driver: Trying to add parameter 'driven.x3', "
                             "but the upper limit supplied (20.0) exceeds the built-in "
                             "upper limit (11.0).")
        else:
            self.fail("expected ValueError")

    def test_param_already_added(self):
        try:
            self.model.driver.add_parameter('driven.x3')
        except AttributeError as err:
            self.assertEqual(str(err), "driver: Trying to add parameter 'driven.x3' to driver, "
                             "but it's already there")
        else:
            self.fail("expected AttributeError")
    
    def test_event_removal(self):
        self.model.driver.add_event('driven.err_event')
        lst = self.model.driver.get_events()
        self.assertEqual(lst, ['driven.err_event'])
        self.model.driver.remove_event('driven.err_event')
        lst = self.model.driver.get_events()
        self.assertEqual(lst, [])
        
    def test_param_removal(self):
        lst = self.model.driver.list_parameters()
        self.assertEqual(lst, ['driven.x0','driven.x1','driven.x2','driven.x3'])
        self.model.driver.remove_parameter('driven.x1')
        lst = self.model.driver.list_parameters()
        self.assertEqual(lst, ['driven.x0','driven.x2','driven.x3'])
    
    def test_DOE_param_mismatch(self):
        self.model.driver.remove_parameter('driven.x2')
        results = ListCaseRecorder()
        self.model.driver.recorder = results
        try:
            self.model.run()
        except ValueError as err:
            self.assertEqual(str(err), "driver: number of DOE values (4) != number of parameters (3)")
        else:
            self.fail("expected ValueError")

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
        msg = "driver: Exception getting 'driven.sum_z': " \
            "'DrivenComponent' object has no attribute 'sum_z'"
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
        except TraitError as exc:
            msg = "driver: required plugin 'DOEgenerator' is not present"
            self.assertEqual(str(exc), msg)
        else:
            self.fail('TraitError expected')

    def test_norecorder(self):
        logging.debug('')
        logging.debug('test_norecorder')

        self.model.driver.recorder = None
        try:
            self.model.run()
        except TraitError as exc:
            msg = "driver: required plugin 'recorder' is not present"
            self.assertEqual(str(exc), msg)
        else:
            self.fail('TraitError expected')

    def run_cases(self, sequential, forced_errors=False):
        """ Evaluate cases, either sequentially or across  multiple servers. """
        self.model.driver.sequential = sequential
        results = ListCaseRecorder()
        self.model.driver.recorder = results
        if forced_errors:
            self.model.driver.add_event('driven.err_event')

        self.model.run()

        self.assertEqual(len(results), 10)
        self.verify_results(forced_errors)

    def verify_results(self, forced_errors=False):
        """ Verify recorded results match expectations. """
        for case in self.model.driver.recorder.cases:
            if forced_errors:
                self.assertEqual(case.msg, 'driven: Forced error')
            else:
                self.assertEqual(case.msg, None)
                self.assertEqual(case.outputs[0][2],
                                 rosen_suzuki(*[x[2] for x in case.inputs[:4]]))

if __name__ == "__main__":
    unittest.main()
