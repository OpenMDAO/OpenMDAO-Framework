"""
Test CaseIteratorDriver.
"""

import unittest

import numpy.random

from openmdao.main import Assembly, Component, Case, ListCaseIterator, \
                          ArrayVariable, Float
from openmdao.main.component import RUN_OK, RUN_FAILED
from openmdao.main.variable import INPUT, OUTPUT
from openmdao.lib.drivers.caseiterdriver import CaseIteratorDriver

def rosen_suzuki(x):
    """ Evaluate polynomial from CONMIN manual. """
    return x[0]**2 - 5.*x[0] + x[1]**2 - 5.*x[1] + \
           2.*x[2]**2 - 21.*x[2] + x[3]**2 + 7.*x[3] + 50


class DrivenComponent(Component):
    """ Just something to be driven and compute results. """

    def __init__(self, name, parent):
        super(DrivenComponent, self).__init__(name, parent)
        ArrayVariable('x', self, INPUT, default=[1., 1., 1., 1.])
        ArrayVariable('y', self, INPUT, default=[1., 1., 1., 1.])
        Float('rosen_suzuki', self, OUTPUT, default=0.)
        Float('sum_y', self, OUTPUT, default=0.)

    def execute(self):
        """ Compute results from input vector. """
        self.rosen_suzuki = rosen_suzuki(self.x)
        self.sum_y = sum(self.y)
        return RUN_OK


class Model(Assembly):
    """ Use CaseIteratorDriver with DrivenComponent. """

    def __init__(self, name='Model', parent=None):
        super(Model, self).__init__(name, parent)
        CaseIteratorDriver('driver', self)
        self.workflow.add_node(DrivenComponent('dc', parent=self))


class DriverTestCase(unittest.TestCase):
    """ Test CaseIteratorDriver. """

    def setUp(self):
        self.model = Model()

    def tearDown(self):
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

        status = self.model.run()
        self.assertEqual(status, RUN_OK)

        self.assertEqual(len(results), len(cases))
        for i, case in enumerate(cases):
            self.assertEqual(results[i].status, RUN_OK)
            self.assertEqual(results[i].outputs[0][2],
                             rosen_suzuki(case.inputs[0][2]))
            self.assertEqual(results[i].outputs[1][2],
                             sum(case.inputs[1][2]))

    def test_noinput(self):
        cases = []
        for i in range(10):
            inputs = [('dc.x', None, numpy.random.normal(size=4)),
                      ('dc.z', None, numpy.random.normal(size=10))]
            outputs = [('dc.rosen_suzuki', None, None),
                       ('dc.sum_y', None, None)]
            cases.append(Case(inputs, outputs))

        self.model.driver.iterator = ListCaseIterator(cases)
        results = []
        self.model.driver.outerator = results

        status = self.model.run()
        self.assertEqual(status, RUN_OK)

        self.assertEqual(len(results), len(cases))
        for i, case in enumerate(cases):
            self.assertEqual(results[i].status, RUN_FAILED)
            self.assertEqual(results[i].msg,
                             "Model.driver: Exception setting 'dc.z': Model.dc: object has no attribute 'z'")

    def test_nooutput(self):
        cases = []
        for i in range(10):
            inputs = [('dc.x', None, numpy.random.normal(size=4)),
                      ('dc.y', None, numpy.random.normal(size=10))]
            outputs = [('dc.rosen_suzuki', None, None),
                       ('dc.sum_z', None, None)]
            cases.append(Case(inputs, outputs))

        self.model.driver.iterator = ListCaseIterator(cases)
        results = []
        self.model.driver.outerator = results

        status = self.model.run()
        self.assertEqual(status, RUN_OK)

        self.assertEqual(len(results), len(cases))
        for i, case in enumerate(cases):
            self.assertEqual(results[i].status, RUN_FAILED)
            self.assertEqual(results[i].msg,
                             "Model.driver: Exception getting 'dc.sum_z': Model.dc: object has no attribute 'sum_z'")


if __name__ == "__main__":
    unittest.main()

