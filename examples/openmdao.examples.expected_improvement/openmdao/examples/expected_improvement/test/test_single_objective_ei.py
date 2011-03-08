"""
Test for single criteria EI example.
"""
import os
import unittest
import random
from math import sqrt

from numpy import random as numpy_random
from numpy import pi

from nose import SkipTest

from pyevolve import Selectors

from openmdao.main.api import set_as_top
from openmdao.examples.expected_improvement.single_objective_ei import Analysis
from openmdao.lib.doegenerators.full_factorial import FullFactorial
from openmdao.lib.caserecorders.dbcaserecorder import case_db_to_dict


class SingleObjectiveEITest(unittest.TestCase):
    """Test to make sure the EI sample problem works as it should"""
    
    def test_EI(self): 
        
        raise SkipTest("This test is problematic. May not be a good test")
        
        # pyevolve does some caching that causes failures during our
        # complete unit tests due to stale values in the cache attributes
        # below, so reset them here
        Selectors.GRankSelector.cachePopID = None
        Selectors.GRankSelector.cacheCount = None
        Selectors.GRouletteWheel.cachePopID = None
        Selectors.GRouletteWheel.cacheWheel = None


        analysis = Analysis()
        set_as_top(analysis)
        #analysis.DOE_trainer.DOEgenerator = FullFactorial(num_levels=10)
        
        analysis.run()
        # This test looks for the presence of at least one point close to
        # each optimum.
        
        #print analysis.EI.EI
        #print analysis.branin_meta_model.x
        #print analysis.branin_meta_model.y
        
        points = [(-pi,12.275,.39789),(pi,2.275,.39789),(9.42478,2.745,.39789)]
        errors = []
        for x,y,z in points: 
            analysis.branin_meta_model.x = x
            analysis.branin_meta_model.y = y
            analysis.branin_meta_model.execute()
            
            errors.append((analysis.branin_meta_model.f_xy.mu - z)/z*100)
        avg_error = sum(errors)/float(len(errors))
        print avg_error, errors
        self.assertTrue(avg_error <= 25)
if __name__=="__main__": #pragma: no cover
    unittest.main()



