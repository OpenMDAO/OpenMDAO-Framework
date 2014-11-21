import unittest
from numpy import array, sin, cos, pi, ones
from openmdao.lib.surrogatemodels.multifi_cokriging_surrogate import MultiFiCoKrigingSurrogate
from openmdao.lib.surrogatemodels.kriging_surrogate import KrigingSurrogate
from openmdao.main.uncertain_distributions import NormalDistribution

class CoKrigingSurrogateTest(unittest.TestCase):

    def test_1d_1fi_cokriging(self):
        # CoKrigingSurrogate with one fidelity could be used as a KrigingSurrogate
        # Same test as for KrigingSurrogate...  well with predicted test value adjustment
        
        x = array([[0.05], [.25], [0.61], [0.95]])
        y = array([0.738513784857542, -0.210367746201974, -0.489015457891476, 12.3033138316612])
        krig1 = MultiFiCoKrigingSurrogate()
        krig1.train(x, y)
        new_x = array([0.5])
        
        pred1 = krig1.predict(x[0])
        self.assertTrue(isinstance(pred1, NormalDistribution))
        self.assertAlmostEqual(y[0] , pred1.mu, places=4)
        self.assertAlmostEqual(.0, pred1.sigma, places=4)

        pred2 = krig1.predict(new_x)
        self.assertTrue(isinstance(pred2, NormalDistribution))
        self.assertAlmostEqual( -2.0279, pred2.mu, places=3)
        self.assertAlmostEqual(1.3408, pred2.sigma, places=3)
        
        # Test with theta setting instead of estimation
        krig2 = MultiFiCoKrigingSurrogate(theta=0.1)
        krig2.train(x, y)
        
        pred1 = krig2.predict(x[0])
        self.assertTrue(isinstance(pred1, NormalDistribution))
        self.assertAlmostEqual(y[0] , pred1.mu, places=4)
        self.assertAlmostEqual(.0, pred1.sigma, places=4)

        pred2 = krig2.predict(new_x)
        self.assertTrue(isinstance(pred2, NormalDistribution))
        self.assertAlmostEqual( -1.2719, pred2.mu, places=3)
        self.assertAlmostEqual(0.0439, pred2.sigma, places=3)

    def test_1d_2fi_cokriging(self):     
        # Example from Forrester: Engineering design via surrogate modelling
        def f_expensive(x):
            return ((x*6-2)**2)*sin((x*6-2)*2)
        def f_cheap(x):
            return 0.5*((x*6-2)**2)*sin((x*6-2)*2)+(x-0.5)*10. - 5
        
        
        x = array([[[0.0], [0.4], [0.6], [1.0]], 
                   [[0.1], [0.2], [0.3], [0.5], [0.7], 
                    [0.8], [0.9], [0.0], [0.4], [0.6], [1.0]]])
        y = array([[f_expensive(v) for v in array(x[0]).ravel()], 
                   [f_cheap(v) for v in array(x[1]).ravel()]])
        
        cokrig = MultiFiCoKrigingSurrogate()
        cokrig.train_multifi(x, y)
        
        new_x = array([0.75])        
        pred = cokrig.predict(new_x)
        self.assertTrue(isinstance(pred, NormalDistribution))
        self.assertAlmostEqual( f_expensive(new_x[0]), pred.mu, delta=0.05)
        self.assertAlmostEqual(0., pred.sigma, delta=0.02)
        
    def test_2d_1fi_cokriging(self):
        # CoKrigingSurrogate with one fidelity could be used as a KrigingSurrogate
        # Same test as for KrigingSurrogate...  well with predicted test value adjustment
        
        def branin(x):
            y = (x[1]-(5.1/(4.*pi**2.))*x[0]**2.+5.*x[0]/pi-6.)**2.+10.*(1.-1./(8.*pi))*cos(x[0])+10.
            return y
        
        x = array([[-2., 0.], [-0.5, 1.5], [1., 3.], [8.5, 4.5], 
                   [-3.5, 6.], [4., 7.5], [-5., 9.], [5.5, 10.5],
                   [10., 12.], [7., 13.5], [2.5, 15.]])
        y = array([branin(case) for case in x])
        krig1 = MultiFiCoKrigingSurrogate()
        krig1.train(x, y)
        
        pred1 = krig1.predict([-2., 0.])
        self.assertAlmostEqual(branin(x[0]), pred1.mu, places=5)
        self.assertAlmostEqual(0., pred1.sigma, places=5)
        
        pred2 = krig1.predict([5., 5.])
        self.assertAlmostEqual(22, pred2.mu, delta=1)        
        self.assertAlmostEqual(13, pred2.sigma, delta=1)
        
        # Test with theta setting instead of estimation
        krig2 = MultiFiCoKrigingSurrogate(theta=[0.1])
        krig1.train(x, y)
        
        pred1 = krig1.predict([-2., 0.])
        self.assertAlmostEqual(branin(x[0]), pred1.mu, places=5)
        self.assertAlmostEqual(0., pred1.sigma, places=5)
        
        pred2 = krig1.predict([5., 5.])
        self.assertAlmostEqual(22, pred2.mu, delta=1)        
        self.assertAlmostEqual(13, pred2.sigma, delta=1)
 
        
    def test_2d_2fi_cokriging(self):
        
        def branin(x):
            x1 = 15*x[0]-5
            x2 = 15*x[1]
            return (x2-(5.1/(4.*pi**2.))*x1**2.+5.*x1/pi-6.)**2.+10.*(1.-1./(8.*pi))*cos(x1)+10.
            
        # Add a linear error
        def branin_low_fidelity(x):
            return branin(x)+30.*x[1] + 10.
        
        x = [[[ 0.13073587,  0.24909577],  # expensive (hifi) doe
              [ 0.91915571,  0.4735261 ],
              [ 0.75830543,  0.13321705],
              [ 0.51760477,  0.34594101],
              [ 0.03531219,  0.77765831],
              [ 0.27249206,  0.5306115 ],
              [ 0.62762489,  0.65778471],
              [ 0.3914706 ,  0.09852519],
              [ 0.86565585,  0.85350002],
              [ 0.40806563,  0.91465314]],
             
             [[ 0.91430235,  0.17029894],  # cheap (lowfi) doe
              [ 0.99329651,  0.76431519],
              [ 0.2012252 ,  0.35006032],
              [ 0.61707854,  0.90210676],
              [ 0.15113004,  0.0133355 ],
              [ 0.07108082,  0.55344447],
              [ 0.4483159 ,  0.52182902],
              [ 0.5926638 ,  0.06595122],
              [ 0.66305449,  0.48579608],
              [ 0.47965045,  0.7407793 ],
              [ 0.13073587,  0.24909577],  # notice hifi doe inclusion
              [ 0.91915571,  0.4735261 ],
              [ 0.75830543,  0.13321705],
              [ 0.51760477,  0.34594101],
              [ 0.03531219,  0.77765831],
              [ 0.27249206,  0.5306115 ],
              [ 0.62762489,  0.65778471],
              [ 0.3914706 ,  0.09852519],
              [ 0.86565585,  0.85350002],
              [ 0.40806563,  0.91465314]]]
        y = array([[branin(case) for case in x[0]],
                   [branin_low_fidelity(case) for case in x[1]]])
        nfi=2
        cokrig = MultiFiCoKrigingSurrogate()
        cokrig.train_multifi(x, y)
        
        pred = cokrig.predict([2./3., 1/3.])
        self.assertAlmostEqual(26, pred.mu, places=0)        
        self.assertAlmostEqual(0.3, pred.sigma, places=0)
        
        # Test with theta setting instead of theta estimation
        cokrig2 = MultiFiCoKrigingSurrogate(theta=0.1)
        cokrig2.train_multifi(x, y)
        
        pred = cokrig2.predict([2./3., 1/3.])
        self.assertAlmostEqual(21.7, pred.mu, places=0)        
        self.assertAlmostEqual(2.29, pred.sigma, places=0)
        
        # Test with theta setting instead of theta estimation
        cokrig2 = MultiFiCoKrigingSurrogate(theta=[0.1, 10])
        cokrig2.train_multifi(x, y)
        
        pred = cokrig2.predict([2./3., 1/3.])
        self.assertAlmostEqual(21.01, pred.mu, places=0)        
        self.assertAlmostEqual(2.29, pred.sigma, places=0)
        
        # Test bad theta setting
        cokrig3 = MultiFiCoKrigingSurrogate(theta=[0.1])
        try:
            cokrig3.train_multifi(x, y)
        except ValueError, err:
            self.assertEqual(str(err),
                "theta must be a list of 2 element(s).")
        else:
            self.fail("ValueError Expected")
    
    
if __name__ == "__main__":
    unittest.main()
        
    