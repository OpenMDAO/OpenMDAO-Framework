"""
Program used to check that instances of a class defined in the main module
can be passed as inputs to a CaseIteratorDriver.
"""

from openmdao.main.api import Assembly, Component
from openmdao.main.datatypes.slot import Slot
from openmdao.lib.drivers.caseiterdriver import CaseIteratorDriver
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
        cid = self.add('driver', CaseIteratorDriver())
        self.add('runner', PGrafSubComponent())
        cid.workflow.add('runner')
        cid.sequential = True #False
        # uncomment to keep simulation directories for debugging purposes
        #import os
        #os.environ['OPENMDAO_KEEPDIRS'] = '1'

        cid.add_parameter('runner.obj')
        cid.add_parameter('runner.num')
        cid.add_response('runner.result')

        cid.case_inputs.runner.obj = [PGrafObject(num) for num in range(4)]
        cid.case_inputs.runner.num = [num for num in range(4)]


def main():
    top = PGrafAssembly()
    top.run()

    inps = top.driver.case_inputs
    outs = top.driver.case_outputs
    results = 0
    for i, result in enumerate(outs.runner.result):
        num = inps.runner.num[i]
        print result, num
        assert result == num ** 2
        results += 1
    assert results == 4


if __name__ == '__main__':
    main()
