"""
Test for single criteria EI example.
"""
import os
import unittest
import random

from numpy import random as numpy_random
from numpy import pi

from pyevolve import Selectors

from openmdao.main.api import set_as_top
from openmdao.examples.singleEI.branin_ei_example import Analysis, Iterator
from openmdao.lib.doegenerators.full_factorial import FullFactorial
from openmdao.util.plot import case_db_to_dict


class EITest(unittest.TestCase):
    """Test to make sure the EI sample problem works as it should"""
    
    def test_EI(self): 
        # pyevolve does some caching that causes failures during our
        # complete unit tests due to stale values in the cache attributes
        # below, so reset them here
        Selectors.GRankSelector.cachePopID = None
        Selectors.GRankSelector.cacheCount = None
        Selectors.GRouletteWheel.cachePopID = None
        Selectors.GRouletteWheel.cacheWheel = None

        random.seed(10)
        numpy_random.seed(10)

        analysis = Analysis()
        set_as_top(analysis)
        analysis.DOE_trainer.DOEgenerator = FullFactorial(2, 2)
        analysis.iterations = 1
        analysis.run()
        # This test looks for the presence of at least one point close to
        # each optimum.
        
        data_EI = case_db_to_dict(os.path.join(analysis._tdir,'retrain.db'),
                                ['branin_meta_model.x',
                                 'branin_meta_model.y'])
        
        true_optima = [(-pi, 12.275), (pi, 2.275), (9.42478, 2.745)]
        num_close_points = [0, 0, 0]
        tol_radius = 0.45
        jj = 0
        xx = data_EI['branin_meta_model.x']
        yy = data_EI['branin_meta_model.y']
        
        for optimum in true_optima:
            for x,y in zip(xx,yy):
                dist = (optimum[0] - x)**2 + (optimum[1] - y)**2
                if dist < tol_radius**2:
                    num_close_points[jj] += 1
            jj += 1
        
        analysis.cleanup()
        self.assertTrue( num_close_points[0] > 0 )
        self.assertTrue( num_close_points[1] > 0 )
        self.assertTrue( num_close_points[2] > 0 )
        
        #self.assertAlmostEqual(3.491477,analysis.EI_driver.next_case[0].inputs[0][2],1)
        #self.assertAlmostEqual(0.29819,analysis.EI_driver.next_case[0].inputs[1][2],1)
        
if __name__=="__main__": #pragma: no cover
    unittest.main()



