
import unittest

from openmdao.main.api import Assembly, Component, Driver, VariableTree, set_as_top, Dataflow
from openmdao.test.rosen_suzuki import Simulation, PreProc, ScalingPreProc, \
                                       PostProc, ScalingPostProc
from openmdao.lib.drivers.slsqpdriver import SLSQPdriver

class Replace2TestCase(unittest.TestCase):

    def test_replace2(self):
        top = set_as_top(Simulation())
        top.replace('preproc', ScalingPreProc())
        top.get_attributes(False)
        top.preproc.get_attributes(False)
        top.replace('postproc', ScalingPostProc())
        top.postproc.get_attributes(False)
        top.replace('driver', SLSQPdriver())
        top.driver.get_attributes(False)
        try:
            top.replace('comp', Assembly())
        except Exception as err:
            self.assertTrue("missing in the replacement object" in str(err))
            self.assertTrue("'x'" in str(err))
            self.assertTrue("'result'" in str(err))
        else:
            self.fail("Exception expected")
        top.driver.get_attributes(False)
        top.comp.get_attributes(False)
        top.get_attributes(False)
        top.get_dataflow()

if __name__ == "__main__": 

    unittest.main()
