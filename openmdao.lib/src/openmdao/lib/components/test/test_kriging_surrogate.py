# pylint: disable-msg=C0111,C0103

import unittest
import random

from numpy import array,round,linspace,sin,cos,pi
import numpy.random as numpy_random

from openmdao.lib.components.kriging_surrogate import KrigingSurrogate
from openmdao.lib.caseiterators.listcaseiter import ListCaseIterator
from openmdao.main.case import Case
from openmdao.main.uncertain_distributions import NormalDistribution

class KrigingSurrogateTests(unittest.TestCase):
    
    def setUp(self):
        random.seed(10)
        numpy_random.seed(10)
    
    def test_1d_kriging1(self):
        
        x = array([[0.05], [.25], [0.61], [0.95]])
        y = array([0.738513784857542,-0.210367746201974,-0.489015457891476,12.3033138316612])
        krig1 = KrigingSurrogate(x,y)
        
        self.assertAlmostEqual(0.6275625,krig1.thetas,places=7)
        
    def test_1d_kriging2(self):
        x = array([[0.05], [.25], [0.61], [0.95]])
        y = array([0.738513784857542,-0.210367746201974,-0.489015457891476,12.3033138316612])
        krig1 = KrigingSurrogate()
        krig1.train(x,y)

        self.assertAlmostEqual(0.6275625,krig1.thetas,places=7)
        
    def test_1d_kriging_predictor(self):
        x = array([[0.05], [.25], [0.61], [0.95]])
        y = array([0.738513784857542,-0.210367746201974,-0.489015457891476,12.3033138316612])
        
        krig1 = KrigingSurrogate(x,y)
        new_x = array([0.5])
        pred = krig1.predict(new_x)
        
        self.assertTrue(isinstance(pred,NormalDistribution))
        self.assertAlmostEqual(0.614359992261,pred.sigma,places=7)
        self.assertAlmostEqual(-1.87363787811,pred.mu,places=11)
    
    def test_1d_kriging3(self):
        """Test for least squares solver utilization when ill-conditioned"""
        x = [[case] for case in linspace(0,1,40)]
        y = sin(x).flatten()
        krig1 = KrigingSurrogate(x,y)
        new_x = array([0.5])
        pred = krig1.predict(new_x)
       
        self.assertAlmostEqual(8.7709e-09,pred.sigma,places=7)
        self.assertAlmostEqual(0.479425538688,pred.mu,places=7)
    
    def test_2d_kriging(self):
        def bran(x):
            y = (x[1]-(5.1/(4.*pi**2.))*x[0]**2.+5.*x[0]/pi-6.)**2.+10.*(1.-1./(8.*pi))*cos(x[0])+10.
            return y

        x = array([[-2,0],[-0.5,1.5],[1,3],[8.5,4.5],[-3.5,6],[4,7.5],[-5,9],[5.5,10.5],[10,12],[7,13.5],[2.5,15]])
        y = array([bran(case) for case in x])

        krig1 = KrigingSurrogate(x,y)
        pred = krig1.predict([5,5])
        
        self.assertAlmostEqual(16.0393816869,pred.sigma,places=5)
        self.assertAlmostEqual(15.6772857277,pred.mu,places=5)
        
    def test_get_uncertain_value(self): 
        x = array([[0.05], [.25], [0.61], [0.95]])
        y = array([0.738513784857542,-0.210367746201974,-0.489015457891476,12.3033138316612])
        krig1 = KrigingSurrogate(x,y)
        
        self.assertEqual(krig1.get_uncertain_value(1).mu,NormalDistribution(1,0).mu)
        self.assertEqual(krig1.get_uncertain_value(1).sigma,NormalDistribution(1,0).sigma)
        
    def test_no_training_data(self): 
        krig1 = KrigingSurrogate()
        
        try: 
            krig1.predict([0,1])
        except RuntimeError,err:
            self.assertEqual(str(err),"KrigingSurrogate has not been trained, so no prediction can be made")
        else: 
            self.fail("RuntimeError Expected")
        
    
if __name__ == "__main__":
    unittest.main()        