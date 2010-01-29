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

from enthought.traits.api import Float, Array, TraitError

from openmdao.main.api import Assembly, Component, Case, ListCaseIterator, set_as_top
from openmdao.main.resource import ResourceAllocationManager, ClusterAllocator
from openmdao.lib.drivers.caseiterdriver import CaseIteratorDriver
from openmdao.main.eggchecker import check_save_load
from openmdao.util.testutil import find_python

# Capture original working directory so we can restore in tearDown().
ORIG_DIR = os.getcwd()

# pylint: disable-msg=E1101


def rosen_suzuki(x):
    """ Evaluate polynomial from CONMIN manual. """
    return x[0]**2 - 5.*x[0] + x[1]**2 - 5.*x[1] + \
           2.*x[2]**2 - 21.*x[2] + x[3]**2 + 7.*x[3] + 50


class DrivenComponent(Component):
    """ Just something to be driven and compute results. """

    x = Array('d', value=[1., 1., 1., 1.], iostatus='in')
    y = Array('d', value=[1., 1., 1., 1.], iostatus='in')
    rosen_suzuki = Float(0., iostatus='out')
    sum_y = Float(0., iostatus='out')
        
    def __init__(self, *args, **kwargs):
        super(DrivenComponent, self).__init__(*args, **kwargs)

    def execute(self):
        """ Compute results from input vector. """
        self.rosen_suzuki = rosen_suzuki(self.x)
        self.sum_y = 0
        for i in range(len(self.y)):
            self.sum_y += self.y[i]


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
        self.cases = []
        for i in range(10):
            inputs = [('x', None, numpy.random.normal(size=4)),
                      ('y', None, numpy.random.normal(size=10))]
            outputs = [('rosen_suzuki', None, None),
                       ('sum_y', None, None)]
            self.cases.append(Case(inputs, outputs))

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

    def test_concurrent(self):
        # This will always test using a LocalAllocator (forked processes).
        # It will also use a ClusterAllocator if the environment looks OK.
        logging.debug('')
        logging.debug('test_concurrent')
        if sys.platform != 'win32':
            # Storm needs firewall changes.
            machines = []
            node = platform.node()
            python = find_python()
            if node == 'gxterm3':
                # User environment assumed OK.
                for i in range(1, 6):
                    machines.append({'hostname':'gx%02d' % i, 'python':python})
            elif self.local_ssh_available():
                machines.append({'hostname':node, 'python':python})
            if machines:
                cluster = ClusterAllocator(machines)
                ResourceAllocationManager.add_allocator(cluster)
        self.run_cases(sequential=False, n_servers=5)
        self.assertEqual(glob.glob('Sim-*'), [])

    @staticmethod
    def local_ssh_available():
        """ Return True if this user has an authorized key for this machine. """
        node = platform.node()
        user = os.environ['USER']
        home = os.environ['HOME']
        try:
            with open(os.path.join(home, '.ssh', 'authorized_keys'), 'r') as keys:
                for line in keys:
                    if line.index(user+'@'+node) > 0:
                        return True
                return False
        except IOError:
            return False

    def run_cases(self, sequential, n_servers=0):
        """ Evaluate cases, either sequentially or across n_servers. """
        self.model.driver.sequential = sequential
        self.model.driver._n_servers = n_servers
        self.model.driver.iterator = ListCaseIterator(self.cases)
        results = []
        self.model.driver.recorder = results

        self.model.run()

        # Verify recorded results match expectations.
        self.assertEqual(len(results), len(self.cases))
        for case in results:
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

