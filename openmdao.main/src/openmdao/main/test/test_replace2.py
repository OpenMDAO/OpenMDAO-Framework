
import unittest

from openmdao.main.api import Assembly, set_as_top
from openmdao.test.rosen_suzuki import Simulation, ScalingPreProc, \
                                       ScalingPostProc
from openmdao.lib.drivers.slsqpdriver import SLSQPdriver

class Replace2TestCase(unittest.TestCase):

    def test_replace2(self):
        top = set_as_top(Simulation())
        top.replace('preproc', ScalingPreProc())
        top.replace('postproc', ScalingPostProc())
        top.replace('driver', SLSQPdriver())
        top.replace('comp', Assembly())
        top._setup()

if __name__ == "__main__":

    unittest.main()
