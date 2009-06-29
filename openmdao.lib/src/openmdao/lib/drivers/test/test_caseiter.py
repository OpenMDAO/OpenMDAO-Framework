"""
Test CaseIteratorDriver.
"""

import unittest

import numpy.random

from enthought.traits.api import Float, Array

from openmdao.main.api import Assembly, Component, Case, ListCaseIterator
from openmdao.lib.drivers.caseiterdriver import CaseIteratorDriver

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

    def test_noinput(self):
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
        for i, case in enumerate(cases):
            self.assertEqual(results[i].msg,
                "CID_TestModel.driver: Exception setting 'dc.z': CID_TestModel.dc: object has no attribute 'z'")

    def test_nooutput(self):
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
        for i, case in enumerate(cases):
            self.assertEqual(results[i].msg,
                             "CID_TestModel.driver: Exception getting 'dc.sum_z': CID_TestModel.dc: object has no attribute 'sum_z'")

    def test_noiterator(self):
        self.model.driver.outerator = []
        try:
            self.model.run()
        except TraitError, exc:
            self.assertEqual(str(exc), "CID_TestModel.driver: required plugin 'iterator' is not present")
        else:
            self.fail('TraitError expected')

    def test_noouterator(self):
        self.model.driver.iterator = ListCaseIterator([])
        try:
            self.model.run()
        except TraitError, exc:
            self.assertEqual(str(exc), "CID_TestModel.driver: required plugin 'outerator' is not present")
        else:
            self.fail('TraitError expected')


if __name__ == "__main__":
    unittest.main()

