"""
Test the CONMIN optimizer component
     used Golinski's speed reducer problem
"""

import unittest
import numpy

from enthought.traits.api import Float, Array

# pylint: disable-msg=F0401,E0611
from openmdao.main.component import Component
from openmdao.main.assembly import Assembly
from openmdao.lib.drivers.conmindriver import CONMINdriver

# we need to add the ImportFactory to the factorymanager to be 
# able to find plugin modules
import openmdao.main.factorymanager as factorymanager
from openmdao.main.importfactory import ImportFactory
factorymanager.register_factory(ImportFactory())


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
    
    # pylint: disable-msg=C0103
    def __init__(self, name, parent=None, desc=None):
        super(OptGolinskiComponent, self).__init__(name, parent, desc)
        self.x = numpy.array([3.3,0.70,25.0,7.9,7.5999999,3.0,5.09999999],dtype=float)
        # self.x = numpy.array([3.3,0.589999970,25.0,7.9,7.5999999,3.0,5.09999999],dtype=float)
        # self.x = numpy.array([3.5,0.700,17.0,7.3,7.7153201,3.50215,5.2866545],dtype=float)
        self.result = 0.
        Array('x',self,iostatus='in',entry_type=float)
        Float('result',self,iostatus='out')
        
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
        self.top = Assembly('top',None)
        OptGolinskiComponent('comp', self.top)
        CONMINdriver('driver', self.top)
        self.top.driver.iprint = 0
        self.top.driver.maxiters = 30
        

    def tearDown(self):
        self.top = None
        

#   optimize  x[0] ..............
    def getx0(self,g11,g12):
        x00 = self.top.comp.x[0] = max(27.0 /(g11 * g11 * g12) ,
                                     397.5 /(g11*g11 * g12*g12) ,
                                     5.0 * g11 ,
                                     2.6 )
        return x00


#   optimize  x[5] ..............
    def getx5(self,g11,g12,g13):
        A1 = (((745.0 * g13) /(g11*g12))**2 + 0.169*1.0e08)**0.5
        g21 = (A1 / (1100.0*0.1) )**(1.0/3.0)
        g22 = (1.93 * g13 * g13 * g13 / (g11 * g12))**0.25
        g23 = 2.9

        x05 = max(g21, g22, g23)
        return x05


#   optimize  x[6] ..............
    def getx6(self,g11,g12,g14):
        A2 = (((745.0 * g14) /(g11*g12))**2 + 0.1575*1.0e09)**0.5
        g31 = (A2 /(850.0*0.1))**(1.0/3.0)
        g32 = (1.93*g14*g14*g14 /(g11 * g12))**0.25
        g33 = 5.0
        x06 = max(g31,g32,g33) 
        return x06

    def test_opt1(self):
        """Golinski optimization using CONMIN"""
        self.top.driver.objective = 'comp.result'
        #                                
        #  maximize x[0] value
        iter  = 1
        self.top.driver.design_vars = ['comp.x[1]','comp.x[2]',
                                             'comp.x[3]','comp.x[4]']
        self.top.driver.lower_bounds = [0.70, 17.0, 7.300, 7.300]
        self.top.driver.upper_bounds = [0.80, 28.0, 8.300, 8.300]
        #  25 CONSTRAINTS  defined in the problem
        #  reduced to 1 constraint
        self.top.driver.constraints = ['1.0 - 40.0/(comp.x[2] * comp.x[3])']
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
            self.top.comp.set('x', self.getx0(g11,g12), [0])
            self.top.comp.set('x', self.getx5(g11,g12,g13), [5])
            self.top.comp.set('x', self.getx6(g11,g12,g14), [6])
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

        print 'Obj FUNCTION Val = ', self.top.comp.result 
        # pylint: disable-msg=E1101
        self.assertAlmostEqual(self.top.comp.opt_objective, 
                               self.top.driver.objective.refvalue, places=2)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[1], 
                               self.top.comp.x[1], places=1)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[2], 
                               self.top.comp.x[2], places=2)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[3], 
                               self.top.comp.x[3], places=2)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[4], 
                               self.top.comp.x[4], places=1)

        
    def test_bad_objective(self):
        try:
            self.top.driver.objective = 'comp.missing'
        except RuntimeError, err:
            self.assertEqual(str(err), "top.driver.objective: cannot find variable 'comp.missing'")
        else:
            self.fail('RuntimeError expected')


    def test_no_design_vars(self):
        try:
            self.top.run()
        except RuntimeError, err:
            self.assertEqual(str(err), "top.driver.objective: reference is undefined")
        else:
            self.fail('RuntimeError expected')
    
    def test_no_objective(self):
        self.top.driver.design_vars = ['comp.x[1]','comp.x[2]',
                                             'comp.x[3]','comp.x[4]']
        try:
            self.top.run()
        except RuntimeError, err:
            self.assertEqual(str(err), "top.driver.objective: reference is undefined")
        else:
            self.fail('RuntimeError expected')
            
    def test_get_objective(self):
        self.top.driver.objective = 'comp.result'
        self.assertEqual('comp.result', self.top.driver.objective.value)
    
    def test_update_objective(self):
        try:
            x = self.top.driver.objective.refvalue
        except RuntimeError, err:
            self.assertEqual(str(err), "top.driver.objective: reference is undefined")
        else:
            self.fail('RuntimeError expected')
        self.top.driver.objective = 'comp.result'
        self.top.comp.x = numpy.array([0,0,0,0,0,0,0],dtype=float)
        self.top.driver.design_vars = ['comp.x[1]','comp.x[2]',
                                             'comp.x[3]','comp.x[4]']
        self.top.driver.design_vars.refvalue = [0, 0, 0, 0]
        self.assertEqual(self.top.driver.objective.refvalue, 0.)
        
    
    def test_bad_design_vars(self):
        try:
            self.top.driver.design_vars = ['comp_bogus.x[0]','comp.x[1]']
        except RuntimeError, err:
            self.assertEqual(str(err), "top.driver.design_vars: cannot find variable 'comp_bogus.x'")
        else:
            self.fail('RuntimeError expected')
    
    def test_bad_constraint(self):
        try:
            self.top.driver.constraints = ['bogus.flimflam']
        except RuntimeError, err:
            self.assertEqual(str(err), 
                "top.driver.constraints: cannot find variable 'bogus.flimflam'")
        else:
            self.fail('RuntimeError expected')
            
    def test_lower_bounds_mismatch(self):
        self.top.driver.objective = 'comp.result'
        self.top.driver.design_vars = ['comp.x[1]','comp.x[2]']
        try:
            self.top.driver.lower_bounds = [0, 0, 0, 0]
            self.top.run()
        except ValueError, err:
            self.assertEqual(str(err),
                             "top.driver: size of new lower bound array"+
                             " (4) does not match number of design vars (2)")
        else:
            self.fail('ValueError expected')
            
    def test_upper_bounds_mismatch(self):
        self.top.driver.objective = 'comp.result'
        self.top.driver.design_vars = ['comp.x[1]','comp.x[2]']
        try:
            self.top.driver.upper_bounds = [99]
            self.top.run()
        except ValueError, err:
            self.assertEqual(str(err),"top.driver: size of new upper bound array"+
                             " (1) does not match number of design vars (2)")
        else:
            self.fail('ValueError expected')
    
 

if __name__ == "__main__":
    unittest.main()
    #suite = unittest.TestLoader().loadTestsFromTestCase(ContainerTestCase)
    #unittest.TextTestRunner(verbosity=2).run(suite)    




    
    
