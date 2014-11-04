"""
Test the CONMIN optimizer component
     used Golinski's speed reducer problem
"""

import pkg_resources
import sys
import unittest
import numpy

from openmdao.main.datatypes.api import Float, Array

# pylint: disable-msg=F0401,E0611
from openmdao.main.api import Component, Assembly, set_as_top
from openmdao.main.eggchecker import check_save_load
from openmdao.lib.drivers.conmindriver import CONMINdriver
from openmdao.util.testutil import assert_rel_error



class OptGolinskiComponent(Component):
    """ From the University of Buffalo MDO Test Suite Problem 2.4
    EXAMPLE   - Golinski's Speed Reducer Test Problem
                (Minimize the speed reducer weight with
                 7 design variables X(1)-X(7) with upper & lower bounds
                 of a gear box.

         MINIMIZE OBJ = 0.7854*X(1)*X(2)**2 * (3.3333*X(3)**2  +
                        14.9334*X(3) - 43.0934) -
                        1.5079*X(1) * (X(6)**2 + X(7)**2) +
                        7.477 * (X(6)**3 + X(7)**3) +
                        0.7854 * (X(4)*X(6)**2 + X(5) * X(7)**2)

          with variable bounds as:
            Width of gear face X(1)  in cm
            2.6 <= X(1) <= 3.6
            teeth module  X(2)       in cm
            0.7 <= X(2) <= 0.8
            number of pinion teeth X(3)
            17  <= X(3) <= 28
            shaft 1 length between bearings X(4) in cm
            7.3 <= X(4) <= 8.3
            shaft 2 length between bearings X(5) in cm
            7.3 <= X(5) <= 8.3
            diameter of shaft 1 X(6)  in cm
            2.9 <= X(6) <= 3.9
            diameter of shaft 2 X(7)  in cm
            5.0 <= X(7) <= 5.5

         Subject to:

              G(1) = 27.0 / (X(1) * X(2)**2 * X(3) ) .LE. 1.0

              G(2) = 397.5 / ( X(1) * X(2)**2 * X(3)**2 ) .LE. 1.0

              G(3) = 1.93 * X(4)**3 /( X(2) * X(3)* X(6)**4) .LE. 1.0

              G(4) = 1.93 * X(5)**3 /( X(2) * X(3)* X(7)**4) .LE. 1.0

              G(5) = SQRT((745.0 * X(4)/ ( X(2) * X(3))**2 + 16.9 * 1.0e6 ) /
                     (110.0 * X(6)**3)  .LE. 1.0

              G(6) = SQRT((745.0 * X(5)/ ( X(2) * X(3))**2 + 157.5 * 1.0e6 ) /
                     (85.0 * X(7)**3)      .LE. 1.0

              G(7) = (X(2) * X(3) )/40.0   .LE. 1.0

              G(8) = 5.0 * X(2) / X(1)     .LE. 1.0

              G(9) = X(1) / (12.0 *X(2) )  .LE. 1.0

              G(10) = 2.6 / X(1)           .LE. 1.0

              G(11) = X(1) / 3.6           .LE. 1.0

              G(12) = 0.7 / X(2)           .LE. 1.0

              G(13) = X(2) / 0.8           .LE. 1.0

              G(14) = 17.0 / X(3)          .LE. 1.0

              G(15) = X(3) / 28.0          .LE. 1.0

              G(16) = 7.3 / X(4)           .LE. 1.0

              G(17) = X(4) / 8.3           .LE. 1.0

              G(18) = 7.3 / X(5)           .LE. 1.0

              G(19) = X(5) / 8.3           .LE. 1.0

              G(20) = 2.9 / X(6)           .LE. 1.0

              G(21) = X(6) / 3.9           .LE. 1.0

              G(22) = 5.0 / X(7)           .LE. 1.0

              G(23) = X(7) / 5.5           .LE. 1.0

              G(24) = (1.5 * X(6) + 1.9 ) / X(4)   .LE. 1.0

              G(25) = (1.1 * X(7) + 1.9 ) / X(5)   .LE. 1.0

              note: G(10) and G(11) are side constraints of X(1)
                    G(12) and G(13) are side constraints of X(2)
                    G(14) and G(15) are side constraints of X(3)
                    G(16) and G(17) are side constraints of X(4)
                    G(18) and G(19) are side constraints of X(5)
                    G(20) and G(21) are side constraints of X(6)
                    G(22) and G(23) are side constraints of X(7)

    This problem is solved by 2 step process:

    1.  Low Level optimization for X(1), X(6) and X(7)

    2.  High Level (using Conmin) for X(2), X(3), X(4), X(5)

         X = (3.3,0.59,25.0,7.9,7.599999,3.0,5.0999999)
    The optimum design is known to be
         OBJ = 0.2985138e+04
    and the corresponding X-vector is
         X = (3.5,0.7,17.0,7.3,7.3,3.35,5.286518)
    """

    x = Array(iotype='in',dtype=numpy.float)
    result = Float(0., iotype='out')

    # pylint: disable-msg=C0103
    def __init__(self):
        super(OptGolinskiComponent, self).__init__()
        self.x = numpy.array([3.3,0.70,25.0,7.9,7.5999999,3.0,5.09999999],dtype=float)
        # self.x = numpy.array([3.3,0.589999970,25.0,7.9,7.5999999,3.0,5.09999999],dtype=float)
        # self.x = numpy.array([3.5,0.700,17.0,7.3,7.7153201,3.50215,5.2866545],dtype=float)

        self.opt_objective = 0.29851384e+04
        self.opt_design_vars = [3.3,0.7,17.0,7.3,7.3,3.35020,5.2865]

    def execute(self):
        """calculate the new objective value"""
        # print ' Executing objective expression******'
        cf = [0.7854, 3.3333, 14.9334, 43.09340, 1.508, 7.477 ]
        self.result = (cf[0]*self.x[0]*self.x[1]*self.x[1] * (cf[1]*self.x[2]*self.x[2] + cf[2]*self.x[2] - cf[3])
                      - cf[4]*self.x[0]*(self.x[5]*self.x[5]+self.x[6]*self.x[6])
                      + cf[5]*(self.x[5]*self.x[5]*self.x[5]+self.x[6]*self.x[6]*self.x[6])
                      + cf[0]*(self.x[3]*self.x[5]*self.x[5]+self.x[4]*self.x[6]*self.x[6]) )



class GolinskiTestCase(unittest.TestCase):
    """test CONMIN optimizer component"""


    def setUp(self):
        self.top = set_as_top(Assembly())
        self.top.add('driver', CONMINdriver())
        self.top.add('comp', OptGolinskiComponent())
        self.top.driver.workflow.add('comp')
        self.top.driver.iprint = 0
        self.top.driver.itmax = 30


    def tearDown(self):
        self.top = None


#   optimize  x[0] ..............
    def getx0(self, g11, g12):
        x00 = self.top.comp.x[0] = max(27.0 /(g11 * g11 * g12) ,
                                     397.5 /(g11*g11 * g12*g12) ,
                                     5.0 * g11 ,
                                     2.6 )
        return x00


#   optimize  x[5] ..............
    def getx5(self, g11, g12, g13):
        A1 = (((745.0 * g13) /(g11*g12))**2 + 0.169*1.0e08)**0.5
        g21 = (A1 / (1100.0*0.1) )**(1.0/3.0)
        g22 = (1.93 * g13 * g13 * g13 / (g11 * g12))**0.25
        g23 = 2.9

        x05 = max(g21, g22, g23)
        return x05


#   optimize  x[6] ..............
    def getx6(self, g11, g12, g14):
        A2 = (((745.0 * g14) /(g11*g12))**2 + 0.1575*1.0e09)**0.5
        g31 = (A2 /(850.0*0.1))**(1.0/3.0)
        g32 = (1.93*g14*g14*g14 /(g11 * g12))**0.25
        g33 = 5.0
        x06 = max(g31, g32, g33)
        return x06

    def test_opt1(self):
        # Golinski optimization using CONMIN

        self.top.driver.add_objective('comp.result')
        #
        #  maximize x[0] value
        iter  = 1
        self.top.driver.add_parameter('comp.x[1]',.7,.8)
        self.top.driver.add_parameter('comp.x[2]',17.,28.)
        self.top.driver.add_parameter('comp.x[3]',7.3,8.3)
        self.top.driver.add_parameter('comp.x[4]',7.3,8.3)
        #  25 CONSTRAINTS  defined in the problem
        #  reduced to 1 constraint
        self.top.driver.add_constraint('40.0/(comp.x[2] * comp.x[3]) > 1.0')
        while iter < 4:
            # print  'iter     ',iter
            g00 = self.top.comp.x[0]
            g11 = self.top.comp.x[1]
            g12 = self.top.comp.x[2]
            g13 = self.top.comp.x[3]
            g14 = self.top.comp.x[4]
            g15 = self.top.comp.x[5]
            g16 = self.top.comp.x[6]
            # print 'starting initial design variables ****** x0 to x6'
            # print  g00, g11, g12, g13, g14, g15, g16
            self.top.comp.set('x[0]', self.getx0(g11, g12))
            self.top.comp.set('x[5]', self.getx5(g11, g12, g13))
            self.top.comp.set('x[6]', self.getx6(g11, g12, g14))
            # print ' *********************************'
            # print ' *********************************'
            # pylint: disable-msg=C0301

            # print  '  before run values ..x0 - x6...'
            # print 'x0 = ', self.top.comp.x[0]
            # print 'x1 = ', self.top.comp.x[1]
            # print 'x2 = ', self.top.comp.x[2]
            # print 'x3 = ', self.top.comp.x[3]
            # print 'x4 = ', self.top.comp.x[4]
            # print 'x5 = ', self.top.comp.x[5]
            # print 'x6 = ', self.top.comp.x[6]
            self.top.run()
            # print  'New values after CONMIN optimization **********'
            # print 'x0 = ', self.top.comp.x[0]
            # print 'x1 = ', self.top.comp.x[1]
            # print 'x2 = ', self.top.comp.x[2]
            # print 'x3 = ', self.top.comp.x[3]
            # print 'x4 = ', self.top.comp.x[4]
            # print 'x5 = ', self.top.comp.x[5]
            # print 'x6 = ', self.top.comp.x[6]

            # print ' *********************************'
            # print ' *********************************'
            # print 'Obj FUNCTION Val = ', self.top.comp.result
            iter = iter +1

        #print 'Obj FUNCTION Val = ', self.top.comp.result
        # pylint: disable-msg=E1101
        assert_rel_error(self, self.top.comp.opt_objective, \
                               self.top.driver.eval_objective(), 0.01)
        assert_rel_error(self, self.top.comp.opt_design_vars[1], \
                               self.top.comp.x[1], 0.1)
        assert_rel_error(self, self.top.comp.opt_design_vars[2], \
                               self.top.comp.x[2], 0.01)
        assert_rel_error(self, self.top.comp.opt_design_vars[3], \
                               self.top.comp.x[3], 0.01)
        assert_rel_error(self, self.top.comp.opt_design_vars[4], \
                               self.top.comp.x[4], 0.05)


if __name__ == "__main__":
    import nose
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()

