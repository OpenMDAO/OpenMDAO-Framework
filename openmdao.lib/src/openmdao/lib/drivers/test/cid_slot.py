"""
Program used to check that instances of a class defined in the main module
can be passed as inputs to a CaseIteratorDriver.
"""

from openmdao.main.api import Assembly, Component
from openmdao.main.case import Case
from openmdao.main.datatypes.slot import Slot
from openmdao.lib.drivers.caseiterdriver import CaseIteratorDriver
from openmdao.lib.casehandlers.api import ListCaseRecorder, ListCaseIterator
from openmdao.main.datatypes.api import Int


class PGrafObject(object):

    def __init__(self, num):
        super(PGrafObject, self).__init__()
        self.num = num


class PGrafComponent(Component):

    num = Int(iotype='in')
    obj = Slot(PGrafObject)
    result = Int(iotype='out')

    def execute(self):
        self.result = self.num + self.obj.num


class PGrafSubComponent(PGrafComponent):

    def execute(self):
        self.result = self.num * self.obj.num


class PGrafAssembly(Assembly):

    def configure(self):
        self.add('driver', CaseIteratorDriver())
        self.add('runner', PGrafSubComponent())
        self.driver.workflow.add('runner')
        self.driver.sequential = False
        # uncomment to keep simulation directories for debugging purposes
        #import os
        #os.environ['OPENMDAO_KEEPDIRS'] = '1'

        cases = []
        for num in range(4):
            cases.append(Case(inputs=[('runner.obj', PGrafObject(num)),
                                      ('runner.num', num)],
                              outputs=['runner.result']))

        self.driver.iterator = ListCaseIterator(cases)
        self.recorders = [ListCaseRecorder()]


if __name__ == '__main__':
    top = PGrafAssembly()
    top.run()

    results = 0
    for case in top.recorders[0].cases:
        print case
        assert case.get_output('runner.result') == case.get_input('runner.num') ** 2
        results += 1
    assert results == 4

