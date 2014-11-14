import unittest

from openmdao.main.api import Assembly, Component
from openmdao.main.datatypes.api import Float, Array
from openmdao.lib.drivers.api import CONMINdriver, BroydenSolver, \
                                     SensitivityDriver, FixedPointIterator

from openmdao.lib.optproblems import sellar



class Dis12Linear(Component):
    """ Linear model of one a sellar model or system. """

    z1 = Float(0., iotype='in')
    z2 = Float(0., iotype='in')
    z_store = Array([0., 0.], iotype='in')

    ssa_F = Array([0.0], iotype='in')
    ssa_G = Array([0.0, 0.0], iotype='in')
    ssa_dF = Array([0.0, 0.0], iotype='in')
    ssa_dG = Array([[0.0, 0.0], [0.0, 0.0]], iotype='in')

    obj = Float(0.0, iotype='out')
    con1 = Float(0.0, iotype='out')
    con2 = Float(0.0, iotype='out')

    def execute(self):

        self.obj = self.ssa_F[0] + self.ssa_dF[0]*(self.z_store[0] - self.z1) + \
                                   self.ssa_dF[1]*(self.z_store[1] - self.z2)
        self.con1 = self.ssa_G[0] + self.ssa_dG[0][0]*(self.z_store[0] - self.z1) + \
                                    self.ssa_dG[0][1]*(self.z_store[1] - self.z2)
        self.con2 = self.ssa_G[1] + self.ssa_dG[1][0]*(self.z_store[0] - self.z1) + \
                                    self.ssa_dG[1][1]*(self.z_store[1] - self.z2)


class SellarBLISS(Assembly):

    z_store = Array([0., 0.], dtype=Float, iotype='in')

    def configure(self):
        """ Creates a new Assembly with this problem

        Optimal Design at (1.9776, 0, 0)

        Optimal Objective = 3.18339"""

        # Disciplines
        self.add('dis12lin', Dis12Linear())
        self.connect('z_store', 'dis12lin.z_store')

        # Global Optimization
        self.add('sysopt', CONMINdriver())
        self.sysopt.add_parameter('z_store[0]', low=-10.0, high=10.0, start=5.0)
        self.sysopt.add_objective('dis12lin.obj')

        self.driver.workflow.add(['sysopt'])
        self.sysopt.workflow.add(['dis12lin'])




class BndryFullSubTestCase(unittest.TestCase):
    
    def test_MDF(self):
        prob = SellarBLISS()
        prob.name = 'top'
        prob.run()
        
        #assert_rel_error(self, 31., top.sens.dF, 0.00001)
        #assert_rel_error(self, 31., top.down.y, 0.00001)


if __name__ == '__main__':
    unittest.main()