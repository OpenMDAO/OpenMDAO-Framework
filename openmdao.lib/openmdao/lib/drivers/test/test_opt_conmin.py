"""
Test the CONMIN optimizer component
"""

import unittest
import numpy

# pylint: disable-msg=F0401,E0611
from openmdao.main.component import Component, RUN_OK
from openmdao.main.assembly import Assembly
from openmdao.main.arrayvar import ArrayVariable
from openmdao.main.variable import INPUT,OUTPUT
from openmdao.main.float import Float
from openmdao.lib.drivers.conmindriver import CONMINdriver

# we need to add the ImportFactory to the factorymanager to be 
# able to find plugin modules
import openmdao.main.factorymanager as factorymanager
from openmdao.main.importfactory import ImportFactory
factorymanager.register_factory(ImportFactory())

class OptRosenSuzukiComponent(Component):
    """ From the CONMIN User's Manual:
    EXAMPLE 1 - CONSTRAINED ROSEN-SUZUKI FUNCTION. NO GRADIENT INFORMATION.
    
         MINIMIZE OBJ = X(1)**2 - 5*X(1) + X(2)**2 - 5*X(2) +
                        2*X(3)**2 - 21*X(3) + X(4)**2 + 7*X(4) + 50
    
         Subject to:
    
              G(1) = X(1)**2 + X(1) + X(2)**2 - X(2) +
                     X(3)**2 + X(3) + X(4)**2 - X(4) - 8   .LE.0
    
              G(2) = X(1)**2 - X(1) + 2*X(2)**2 + X(3)**2 +
                     2*X(4)**2 - X(4) - 10                  .LE.0
    
              G(3) = 2*X(1)**2 + 2*X(1) + X(2)**2 - X(2) +
                     X(3)**2 - X(4) - 5                     .LE.0
                     
    This problem is solved beginning with an initial X-vector of
         X = (1.0, 1.0, 1.0, 1.0)
    The optimum design is known to be
         OBJ = 6.000
    and the corresponding X-vector is
         X = (0.0, 1.0, 2.0, -1.0)
    """
    
    # pylint: disable-msg=C0103
    def __init__(self, name, parent=None, desc=None):
        super(OptRosenSuzukiComponent, self).__init__(name, parent, desc)
        self.x = numpy.array([1.,1.,1.,1.],dtype=float)
        self.result = 0.
        ArrayVariable('x',self,iostatus=INPUT,entry_type=float)
        Float('result',self,iostatus=OUTPUT)
        
        self.opt_objective = 6.
        self.opt_design_vars = [0., 1., 2., -1.]

    def execute(self):
        """calculate the new objective value"""
        self.result = (self.x[0]**2 - 5.*self.x[0] + 
                       self.x[1]**2 - 5.*self.x[1] +
                       2.*self.x[2]**2 - 21.*self.x[2] + 
                       self.x[3]**2 + 7.*self.x[3] + 50)
        return RUN_OK


class CONMINdriverTestCase(unittest.TestCase):
    """test CONMIN optimizer component"""

    def setUp(self):
        self.top = Assembly('top',None)
        comp = OptRosenSuzukiComponent('comp', self.top)
        self.top.workflow.add_node(comp)
        CONMINdriver('driver', self.top)
        self.top.driver.iprint = 0
        self.top.driver.maxiters = 30
        
    def tearDown(self):
        self.top = None
        
    def test_opt1(self):
        """Rosen-Suzuki optimization using CONMIN"""
        self.top.driver.objective = 'comp.result'
        self.top.driver.design_vars = ['comp.x[0]','comp.x[1]',
                                       'comp.x[2]','comp.x[3]']
        self.top.driver.lower_bounds = [-10, -10, -10, -10]
        self.top.driver.upper_bounds = [99,99,99,99]
        
        # pylint: disable-msg=C0301
        self.top.driver.constraints = [
            'comp.x[0]**2+comp.x[0]+comp.x[1]**2-comp.x[1]+comp.x[2]**2+comp.x[2]+comp.x[3]**2-comp.x[3]-8',
            'comp.x[0]**2-comp.x[0]+2*comp.x[1]**2+comp.x[2]**2+2*comp.x[3]**2-comp.x[3]-10',
            '2*comp.x[0]**2+2*comp.x[0]+comp.x[1]**2-comp.x[1]+comp.x[2]**2-comp.x[3]-5']        
        self.top.run()
        # pylint: disable-msg=E1101
        self.assertAlmostEqual(self.top.comp.opt_objective, 
                               self.top.driver.objective_val, places=2)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[0], 
                               self.top.comp.x[0], places=1)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[1], 
                               self.top.comp.x[1], places=2)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[2], 
                               self.top.comp.x[2], places=2)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[3], 
                               self.top.comp.x[3], places=1)

        
    def test_bad_objective(self):
        try:
            self.top.driver.objective = 'comp.missing'
        except RuntimeError, err:
            self.assertEqual(str(err), "top.driver: objective 'comp.missing' is invalid")
        else:
            self.fail('RuntimeError expected')


    def test_no_design_vars(self):
        try:
            self.top.run()
        except RuntimeError, err:
            self.assertEqual(str(err), "top.driver: no design variables specified")
        else:
            self.fail('RuntimeError expected')
    
    def test_no_objective(self):
        self.top.driver.design_vars = ['comp.x[0]','comp.x[1]',
                                       'comp.x[2]','comp.x[3]']
        try:
            self.top.run()
        except RuntimeError, err:
            self.assertEqual(str(err), "top.driver: no objective specified")
        else:
            self.fail('RuntimeError expected')
            
    def test_get_objective(self):
        self.top.driver.objective = 'comp.result'
        self.assertEqual('comp.result', self.top.driver.objective)
    
    def test_update_objective(self):
        try:
            self.top.driver.update_objective_val()
        except RuntimeError, err:
            self.assertEqual(str(err), "top.driver: No objective has been set")
        else:
            self.fail('RuntimeError expected')
        self.assertEqual(self.top.driver.objective_val, 0)
        self.top.comp.result = 99.
        self.top.driver.objective = 'comp.result'
        self.top.driver.update_objective_val()
        self.assertEqual(self.top.driver.objective_val, 99.)
    
    def test_bad_design_vars(self):
        try:
            self.top.driver.design_vars = ['comp_bogus.x[0]','comp.x[1]']
        except RuntimeError, err:
            self.assertEqual(str(err), 
                    "top.driver: design variable 'comp_bogus.x[0]' is invalid")
        else:
            self.fail('RuntimeError expected')
    
    def test_bad_constraint(self):
        try:
            self.top.driver.constraints = ['bogus.flimflam']
        except RuntimeError, err:
            self.assertEqual(str(err), 
                    "top.driver: constraint 'bogus.flimflam' is invalid")
        else:
            self.fail('RuntimeError expected')
            
    def test_lower_bounds_mismatch(self):
        self.top.driver.design_vars = ['comp.x[0]','comp.x[1]']
        try:
            self.top.driver.lower_bounds = [0,0,0,0]
        except ValueError, err:
            self.assertEqual(str(err),
                             "top.driver: size of new lower bound array"+
                             " (4) does not match number of design vars (2)")
        else:
            self.fail('ValueError expected')
            
    def test_upper_bounds_mismatch(self):
        self.top.driver.design_vars = ['comp.x[0]','comp.x[1]']
        try:
            self.top.driver.upper_bounds = [99]
        except ValueError, err:
            self.assertEqual(str(err),"top.driver: size of new upper bound array"+
                             " (1) does not match number of design vars (2)")
        else:
            self.fail('ValueError expected')
    
if __name__ == "__main__":
    unittest.main()
    #suite = unittest.TestLoader().loadTestsFromTestCase(ContainerTestCase)
    #unittest.TextTestRunner(verbosity=2).run(suite)    




    
    
