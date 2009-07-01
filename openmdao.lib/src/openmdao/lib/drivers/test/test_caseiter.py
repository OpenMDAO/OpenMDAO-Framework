"""
Test CaseIteratorDriver.
"""

import logging
import pkg_resources
import sys
import unittest

import numpy.random

from openmdao.main import Assembly, Component, Case, ListCaseIterator, \
                          ArrayVariable, Float
from openmdao.main.variable import INPUT, OUTPUT
from openmdao.lib.drivers.caseiterdriver import CaseIteratorDriver

# pylint: disable-msg=E1101


def rosen_suzuki(x):
    """ Evaluate polynomial from CONMIN manual. """
    return x[0]**2 - 5.*x[0] + x[1]**2 - 5.*x[1] + \
           2.*x[2]**2 - 21.*x[2] + x[3]**2 + 7.*x[3] + 50


class DrivenComponent(Component):
    """ Just something to be driven and compute results. """

    def __init__(self, *args, **kwargs):
        super(DrivenComponent, self).__init__(*args, **kwargs)
        ArrayVariable('x', self, INPUT, default=[1., 1., 1., 1.])
        ArrayVariable('y', self, INPUT, default=[1., 1., 1., 1.])
        Float('rosen_suzuki', self, OUTPUT, default=0.)
        Float('sum_y', self, OUTPUT, default=0.)

    def execute(self):
        """ Compute results from input vector. """
        self.rosen_suzuki = rosen_suzuki(self.x)
        self.sum_y = sum(self.y)


class MyModel(Assembly):
    """ Use CaseIteratorDriver with DrivenComponent. """

    def __init__(self, name='CID_TestModel', *args, **kwargs):
        super(MyModel, self).__init__(name, *args, **kwargs)
        cid = CaseIteratorDriver('driver', self)
        cid.model = DrivenComponent('dc', self)


class DriverTestCase(unittest.TestCase):
    """ Test CaseIteratorDriver. """

    def setUp(self):
        self.model = MyModel()

    def tearDown(self):
        self.model.pre_delete()
        self.model = None

    def test_normal(self):
        logging.debug('')
        logging.debug('test_normal')

        cases = []
        for i in range(10):
            inputs = [('dc.x', None, numpy.random.normal(size=4)),
                      ('dc.y', None, numpy.random.normal(size=10))]
            outputs = [('dc.rosen_suzuki', None, None),
                       ('dc.sum_y', None, None)]
            cases.append(Case(inputs, outputs))

        self.model.driver.iterator = ListCaseIterator(cases)
        results = []
        self.model.driver.outerator = results

        self.model.run()

        self.assertEqual(len(results), len(cases))
        for i, case in enumerate(cases):
            self.assertEqual(results[i].msg, None)
            self.assertEqual(results[i].outputs[0][2],
                             rosen_suzuki(case.inputs[0][2]))
            self.assertEqual(results[i].outputs[1][2],
                             sum(case.inputs[1][2]))

    def test_save_load(self):
        logging.debug('')
        logging.debug('test_save_load')

        cases = []
        for i in range(10):
            inputs = [('dc.x', None, numpy.random.normal(size=4)),
                      ('dc.y', None, numpy.random.normal(size=10))]
            outputs = [('dc.rosen_suzuki', None, None),
                       ('dc.sum_y', None, None)]
            cases.append(Case(inputs, outputs))

        self.model.driver.iterator = ListCaseIterator(cases)
        results = []
        self.model.driver.outerator = results

        if sys.platform != 'win32':
            # Set local dir in case we're running in a different directory.
            py_dir = pkg_resources.resource_filename('openmdao.lib.drivers',
                                                     'test')
            retcode = self.model.check_save_load(py_dir=py_dir)
            self.assertEqual(retcode, 0)

    def test_noinput(self):
        logging.debug('')
        logging.debug('test_noinput')

        cases = []
        for i in range(2):
            inputs = [('dc.x', None, numpy.random.normal(size=4)),
                      ('dc.z', None, numpy.random.normal(size=10))]
            outputs = [('dc.rosen_suzuki', None, None),
                       ('dc.sum_y', None, None)]
            cases.append(Case(inputs, outputs))

        self.model.driver.iterator = ListCaseIterator(cases)
        results = []
        self.model.driver.outerator = results

        self.model.run()

        self.assertEqual(len(results), len(cases))
        msg = "CID_TestModel.driver: Exception setting 'dc.z':" \
              " CID_TestModel.dc: object has no attribute 'z'"
        for i, case in enumerate(cases):
            self.assertEqual(results[i].msg, msg)

    def test_nooutput(self):
        logging.debug('')
        logging.debug('test_nooutput')

        cases = []
        for i in range(2):
            inputs = [('dc.x', None, numpy.random.normal(size=4)),
                      ('dc.y', None, numpy.random.normal(size=10))]
            outputs = [('dc.rosen_suzuki', None, None),
                       ('dc.sum_z', None, None)]
            cases.append(Case(inputs, outputs))

        self.model.driver.iterator = ListCaseIterator(cases)
        results = []
        self.model.driver.outerator = results

        self.model.run()

        self.assertEqual(len(results), len(cases))
        msg = "CID_TestModel.driver: Exception getting 'dc.sum_z':" \
              " CID_TestModel.dc: object has no attribute 'sum_z'"
        for i, case in enumerate(cases):
            self.assertEqual(results[i].msg, msg)

    def test_noiterator(self):
        logging.debug('')
        logging.debug('test_noiterator')

        self.model.driver.outerator = []
        try:
            self.model.run()
        except ValueError, exc:
            msg = "CID_TestModel.driver: required plugin 'iterator' is not" \
                  " present"
            self.assertEqual(str(exc), msg)
        else:
            self.fail('ValueError expected')

    def test_noouterator(self):
        logging.debug('')
        logging.debug('test_noouterator')

        self.model.driver.iterator = ListCaseIterator([])
        try:
            self.model.run()
        except ValueError, exc:
            msg = "CID_TestModel.driver: required plugin 'outerator' is not" \
                  " present"
            self.assertEqual(str(exc), msg)
        else:
            self.fail('ValueError expected')


if __name__ == "__main__":
    import nose
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()

