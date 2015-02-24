
import os
import unittest
from numpy import array

from openmdao.examples.metamodel_tutorial.krig_sin import Simulation
from openmdao.main.api import set_as_top

class TestMetamodelTutorial(unittest.TestCase):

    def setUp(self):
        pass
    
    def tearDown(self):
        for name in ['DOE_Trainer.csv', 'DOE_Validate.csv']:
            if os.path.exists(name):
                os.remove(name)
            
    def test_krig_sin(self):
        sim = set_as_top(Simulation())
        sim.run()

        actual = array(sim.DOE_Validate.case_outputs.sin_verify.f_x)
        predicted = array(sim.DOE_Validate.case_outputs.sin_meta_model.f_x)

        diff = abs(actual - predicted).max()
        self.assertTrue(diff < 0.02)


if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()