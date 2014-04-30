import unittest
import random

import numpy as np

from openmdao.lib.surrogatemodels.api import ResponseSurface


class ResponseSurfaceTest(unittest.TestCase):
    
    def setUp(self):
        random.seed(10)
        np.random.seed(10)
        
        N = 26
        d = 20
        
        means = .05 * np.random.randn(2, d)
        self.X_train = np.zeros((N, d))     
        self.Y_train = np.zeros(N) 
        for i in range(N):
            if np.random.random() > .5:
                y = 1
            else:
                y = 0
            self.X_train[i, :] = np.random.random(d) + means[y, :]
            self.Y_train[i] = 4*y-2
        
        
    def test_training(self):
  
        lr = ResponseSurface(self.X_train, self.Y_train)
            
        training_reconstruction = [lr.predict(x) for x in self.X_train]
        residual = sum([ x-y for x,y in zip(training_reconstruction,self.Y_train)])
        
        self.assertTrue(residual<1e-5)
        
