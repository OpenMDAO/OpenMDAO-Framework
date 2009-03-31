"""
Test CaseIteratorDriver.
"""

import unittest

import numpy.random

from openmdao.main import Assembly, Component, Case, ListCaseIterator, \
                          ArrayVariable, Float
from openmdao.main.variable import INPUT, OUTPUT
from openmdao.lib.drivers.caseiterdriver import CaseIteratorDriver

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


class Model(Assembly):
    """ Use CaseIteratorDriver with DrivenComponent. """

    def __init__(self, name='CID_TestModel', *args, **kwargs):
        super(Model, self).__init__(name, *args, **kwargs)
        CaseIteratorDriver('driver', self)
        self.workflow.add_node(DrivenComponent('dc', parent=self))


class DriverTestCase(unittest.TestCase):
    """ Test CaseIteratorDriver. """

    def setUp(self):
        self.model = Model()

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
        try:
            self.model.run()
        except ValueError, exc:
            self.assertEqual(str(exc), 'CID_TestModel.driver: No iterator plugin')
        else:
            self.fail('ValueError expected')

    def test_noouterator(self):
        self.model.driver.iterator = ListCaseIterator([])
        try:
            self.model.run()
        except ValueError, exc:
            self.assertEqual(str(exc), 'CID_TestModel.driver: No outerator plugin')
        else:
            self.fail('ValueError expected')


if __name__ == "__main__":
    unittest.main()

