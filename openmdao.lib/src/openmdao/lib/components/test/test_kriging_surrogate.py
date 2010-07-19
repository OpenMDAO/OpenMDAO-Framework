# pylint: disable-msg=C0111,C0103

import unittest

from numpy import array,round,linspace,sin,cos,random,pi
from openmdao.lib.components.kriging_surrogate import KrigingSurrogate
from openmdao.lib.caseiterators.listcaseiter import ListCaseIterator
from openmdao.main.case import Case
from openmdao.main.uncertain_distributions import NormalDistribution

class KrigingSurrogateTests(unittest.TestCase):
    
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
    """
    def test_2d_kriging(self):
        n = 12
        x1 = linspace(-5,10,n)
        x2 = linspace(0,15,n)
        random.seed(seed=1)
        random.shuffle(x1)
        random.shuffle(x2)

        x = array(zip(x1,x2))
        
        def bran(x):
            y = (x[1]-(5.1/(4.*pi**2.))*x[0]**2.+5.*x[0]/pi-6.)**2.+10.*(1.-1./(8.*pi))*cos(x[0])+10.
            return y
            
        y = [bran(x) for x in x]

        rse = KrigingSurrogate(x,y)
    """
    
if __name__ == "__main__":
    unittest.main()        