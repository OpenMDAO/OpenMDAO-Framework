"""
Test CaseIteratorDriver.
"""

import glob
import logging
import os.path
import pkg_resources
import platform
import sys
import unittest

import numpy.random

from enthought.traits.api import Bool, Float, Array, TraitError

from openmdao.main.api import Assembly, Component, Case, ListCaseIterator, set_as_top
from openmdao.main.exceptions import RunStopped
from openmdao.main.resource import ResourceAllocationManager, ClusterAllocator
from openmdao.lib.drivers.caseiterdriver import CaseIteratorDriver
from openmdao.main.eggchecker import check_save_load
from openmdao.util.testutil import find_python


# Users who have ssh configured correctly for testing.
SSH_USERS = ('setowns1',)

# Capture original working directory so we can restore in tearDown().
ORIG_DIR = os.getcwd()

# pylint: disable-msg=E1101


def rosen_suzuki(x):
    """ Evaluate polynomial from CONMIN manual. """
    return x[0]**2 - 5.*x[0] + x[1]**2 - 5.*x[1] + \
           2.*x[2]**2 - 21.*x[2] + x[3]**2 + 7.*x[3] + 50


class DrivenComponent(Component):
    """ Just something to be driven and compute results. """

    x = Array('d', value=[1., 1., 1., 1.], iotype='in')
    y = Array('d', value=[1., 1., 1., 1.], iotype='in')
    raise_error = Bool(False, iotype='in')
    stop_exec = Bool(False, iotype='in')
    rosen_suzuki = Float(0., iotype='out')
    sum_y = Float(0., iotype='out')
        
    def __init__(self, *args, **kwargs):
        super(DrivenComponent, self).__init__(*args, **kwargs)

    def execute(self):
        """ Compute results from input vector. """
        self.rosen_suzuki = rosen_suzuki(self.x)
        self.sum_y = sum(self.y)
        if self.raise_error:
            self.raise_exception('Forced error', RuntimeError)
        if self.stop_exec:
            self.parent.stop()  # Only valid if sequential!
#FIXME: for some reason the above doesn't call stop() on the driver...
            self.parent._stop = True


class MyModel(Assembly):
    """ Use CaseIteratorDriver with DrivenComponent. """

    def __init__(self, *args, **kwargs):
        super(MyModel, self).__init__(*args, **kwargs)
        self.add_container('driver', CaseIteratorDriver())
        self.driver.add_container('model', DrivenComponent())


class TestCase(unittest.TestCase):
    """ Test CaseIteratorDriver. """

    # Need to be in this directory or there are issues with egg loading.
    directory = pkg_resources.resource_filename('openmdao.lib.drivers', 'test')

    def setUp(self):
        os.chdir(self.directory)
        self.model = set_as_top(MyModel())
        self.generate_cases()

    def generate_cases(self, force_errors=False):
        self.cases = []
        for i in range(10):
            raise_error = force_errors and i%4 == 3
            inputs = [('x', None, numpy.random.normal(size=4)),
                      ('y', None, numpy.random.normal(size=10)),
                      ('raise_error', None, raise_error),
                      ('stop_exec', None, False)]
            outputs = [('rosen_suzuki', None, None),
                       ('sum_y', None, None)]
            self.cases.append(Case(inputs, outputs, ident=i))

    def tearDown(self):
        self.model.pre_delete()
        self.model = None

        # Verify we didn't mess-up working directory.
        end_dir = os.getcwd()
        os.chdir(ORIG_DIR)
        if sys.platform == 'win32':
            end_dir = end_dir.lower()
        if end_dir != self.directory:
            self.fail('Ended in %s, expected %s' % (end_dir, self.directory))

    def test_sequential(self):
        logging.debug('')
        logging.debug('test_sequential')
        self.run_cases(sequential=True)

        logging.debug('')
        logging.debug('test_sequential_errors')
        self.generate_cases(force_errors=True)
        self.model.driver._call_execute = True
        self.run_cases(sequential=True, forced_errors=True)

    def test_run_stop_step_resume(self):
        logging.debug('')
        logging.debug('test_run_stop_step_resume')

        self.generate_cases()
        stop_case = self.cases[1]  # Stop after 2 cases run.
        stop_case.inputs[3] = ('stop_exec', None, True)
        self.model.driver.iterator = ListCaseIterator(self.cases)
        results = []
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

    def test_concurrent(self):
        # This can always test using a LocalAllocator (forked processes).
        # It can also use a ClusterAllocator if the environment looks OK.
        logging.debug('')
        logging.debug('test_concurrent')

        # Ensure we aren't held up by local host load problems.
        local = ResourceAllocationManager.get_allocator(0)
        local.max_load = 10

        if sys.platform != 'win32':
            # ssh server not typically available on Windows.
            machines = []
            node = platform.node()
            python = find_python()
            if node == 'gxterm3':
                # User environment assumed OK on this GRC cluster front-end.
                for i in range(55):
                    machines.append({'hostname':'gx%02d' % i, 'python':python})
            elif self.local_ssh_available():
                machines.append({'hostname':node, 'python':python})
            if machines:
                name = node.replace('.', '_')
                cluster = ClusterAllocator(name, machines)
                ResourceAllocationManager.insert_allocator(0, cluster)

        self.run_cases(sequential=False)
        self.assertEqual(glob.glob('Sim-*'), [])

        logging.debug('')
        logging.debug('test_concurrent_errors')
        self.generate_cases(force_errors=True)
        self.model.driver._call_execute = True
        self.run_cases(sequential=False, forced_errors=True)
        self.assertEqual(glob.glob('Sim-*'), [])

    @staticmethod
    def local_ssh_available():
        """ Return True if this user has an authorized key for this machine. """
        user = os.environ['USER']
# Avoid problems with users who don't have a valid environment.
        if user not in SSH_USERS:
            return False
        home = os.environ['HOME']
        node = platform.node()
        keyfile = os.path.join(home, '.ssh', 'authorized_keys')
        try:
            with open(keyfile, 'r') as keys:
                for line in keys:
                    if line.find(user+'@'+node) > 0:
                        return True
                return False
        except IOError:
            return False

    def run_cases(self, sequential, forced_errors=False):
        """ Evaluate cases, either sequentially or across  multiple servers. """
        self.model.driver.sequential = sequential
        self.model.driver.iterator = ListCaseIterator(self.cases)
        results = []
        self.model.driver.recorder = results

        self.model.run()

        self.assertEqual(len(results), len(self.cases))
        self.verify_results(forced_errors)

    def verify_results(self, forced_errors=False):
        """ Verify recorded results match expectations. """
        for case in self.model.driver.recorder:
            i = case.ident  # Correlation key.
            error_expected = forced_errors and i%4 == 3
            if error_expected:
                if self.model.driver.sequential:
                    self.assertEqual(case.msg, 'driver.model: Forced error')
                else:
                    self.assertEqual(case.msg, 'model: Forced error')
            else:
                self.assertEqual(case.msg, None)
                self.assertEqual(case.outputs[0][2],
                                 rosen_suzuki(case.inputs[0][2]))
                self.assertEqual(case.outputs[1][2],
                                 sum(case.inputs[1][2]))

    def test_save_load(self):
        logging.debug('')
        logging.debug('test_save_load')

        self.model.driver.iterator = ListCaseIterator(self.cases)
        results = []
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
            inputs = [('x', None, numpy.random.normal(size=4)),
                      ('z', None, numpy.random.normal(size=10))]
            outputs = [('rosen_suzuki', None, None),
                       ('sum_y', None, None)]
            cases.append(Case(inputs, outputs))

        self.model.driver.iterator = ListCaseIterator(cases)
        results = []
        self.model.driver.recorder = results

        self.model.run()

        self.assertEqual(len(results), len(cases))
        msg = "driver: Exception setting 'z':" \
              " driver.model: object has no attribute 'z'"
        for case in results:
            self.assertEqual(case.msg, msg)

    def test_nooutput(self):
        logging.debug('')
        logging.debug('test_nooutput')

        # Create cases with missing output 'dc.sum_z'.
        cases = []
        for i in range(2):
            inputs = [('x', None, numpy.random.normal(size=4)),
                      ('y', None, numpy.random.normal(size=10))]
            outputs = [('rosen_suzuki', None, None),
                       ('sum_z', None, None)]
            cases.append(Case(inputs, outputs))

        self.model.driver.iterator = ListCaseIterator(cases)
        results = []
        self.model.driver.recorder = results

        self.model.run()

        self.assertEqual(len(results), len(cases))
        msg = "driver: Exception getting 'sum_z':" \
              " driver.model: object has no attribute 'sum_z'"
        for case in results:
            self.assertEqual(case.msg, msg)

    def test_noiterator(self):
        logging.debug('')
        logging.debug('test_noiterator')

        # Check resoponse to no iterator set.
        self.model.driver.recorder = []
        try:
            self.model.run()
        except TraitError, exc:
            msg = "driver: required plugin 'iterator' is not present"
            self.assertEqual(str(exc), msg)
        else:
            self.fail('TraitError expected')

    def test_norecorder(self):
        logging.debug('')
        logging.debug('test_norecorder')

        # Check resoponse to no recorder set.
        self.model.driver.iterator = ListCaseIterator([])
        try:
            self.model.run()
        except TraitError, exc:
            msg = "driver: required plugin 'recorder' is not present"
            self.assertEqual(str(exc), msg)
        else:
            self.fail('TraitError expected')


if __name__ == "__main__":
    import nose
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()

