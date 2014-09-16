import unittest

import logging

from openmdao.main.api import Assembly, Component, Case
from openmdao.main.datatypes.api import Float
from openmdao.lib.casehandlers.api import ListCaseIterator, ListCaseRecorder
from openmdao.lib.drivers.api import SimpleCaseIterDriver

from openmdao.lib.drivers.test.test_caseiterdriver import Generator, \
                                                          DrivenComponent, \
                                                          Verifier

from openmdao.main.case import CaseTreeNode


class SCIDriver(SimpleCaseIterDriver):

    def __init__(self, max_iterations, comp_name):
        super(SCIDriver, self).__init__()
        self.max_iterations = max_iterations
        self.comp_name = comp_name
        self.iterator = ListCaseIterator([])  # Just to pass config check.

    def execute(self):
        inp = self.comp_name+'.x'
        out = self.comp_name+'.y'
        cases = []
        for i in range(self.max_iterations):
            cases.append(Case(inputs=[(inp, i)], outputs=[out]))
        self.iterator = ListCaseIterator(cases)
        super(SCIDriver, self).execute()


class CaseComponent(Component):

    x = Float(iotype='in')
    y = Float(iotype='out')

    def execute(self):
        self.y = self.x


class TestCase(unittest.TestCase):

    def test_scid(self):
        # Record tree of cases via SimpleCaseIterDriver.
        top = Assembly()
        top.recorders = [ListCaseRecorder()]

        top.add('driver2', SCIDriver(3, 'comp2'))
        top.add('comp2', CaseComponent())
        top.driver2.workflow.add('comp2')

        top.add('driver1', SCIDriver(2, 'comp1'))
        top.add('comp1', CaseComponent())
        top.driver1.workflow.add(['comp1', 'driver2'])

        top.driver.workflow.add('driver1')
        top.run()

        print
        print 'Forest:'
        roots = CaseTreeNode.sort(top.recorders[0].get_iterator())
        for root in roots:
            root.dump(1)

        print
        print 'Iternames:'
        for root in roots:
            for name in root.iternames():
                print '   ', name

        expected = [
            '1',
            '1-1.1',
            '1-1.1-2.1',
            '1-1.1-2.2',
            '1-1.1-2.3',
            '1-1.2',
            '1-1.2-2.1',
            '1-1.2-2.2',
            '1-1.2-2.3'
        ]
        for i, name in enumerate(roots[0].iternames()):
            self.assertEqual(name, expected[i])

    def test_scid2(self):
        logging.debug('')
        logging.debug('test_simplecid')

        top = Assembly()
        top.add('generator', Generator())
        cid = top.add('cid', SimpleCaseIterDriver())
        top.add('driven', DrivenComponent())
        top.add('verifier', Verifier())

        top.driver.workflow.add(('generator', 'cid', 'verifier'))
        cid.workflow.add('driven')
        cid.add_parameter('driven.x')
        cid.add_parameter('driven.y')
        cid.add_response('driven.rosen_suzuki')
        cid.add_response('driven.sum_y')

        top.connect('generator.x', 'cid.case_inputs.driven.x')
        top.connect('generator.y', 'cid.case_inputs.driven.y')

        top.connect('generator.x', 'verifier.x')
        top.connect('generator.y', 'verifier.y')
        top.connect('cid.case_outputs.driven.rosen_suzuki', 'verifier.rosen_suzuki')
        top.connect('cid.case_outputs.driven.sum_y', 'verifier.sum_y')

        top.run()


if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao.lib.drivers')
    sys.argv.append('--cover-erase')
    nose.runmodule()

