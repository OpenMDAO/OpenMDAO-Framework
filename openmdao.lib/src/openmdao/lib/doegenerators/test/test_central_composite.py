"""
Test CentralComposite.
"""

# pylint: disable-msg=C0111,C0103

import unittest

from openmdao.lib.doegenerators.central_composite import CentralComposite

class TestCase(unittest.TestCase):
    
    def setup(self):
        pass
    
    def teardown(self):
        pass
    
    def test_central_composite(self): 
        
        # fcc = CentralComposite(type="Face-Centered", alpha=0.8, center_points=1)
        # fcc.num_parameters = 2
        # cases = [case for case in fcc]
        # self.assertEqual([(0.0,0.0),(0.0,1.0),(1.0,0.0),(1.0,1.0),(0.0,0.5),(0.5,0.0),(0.5,1.0),(1.0,0.5),[0.5,0.5]],cases)

        # ccc = CentralComposite(type="Spherical", alpha=0.8, center_points=1)
        # ccc.num_parameters = 2
        # cases = [case for case in ccc]
        # self.assertEqual([(0,0),(0,1),(1,0),(1,1),(0.5,0.1),(0.1,0.5),(0.9,0.5),(0.5,0.9),[0.5,0.5]],cases)
        
        ccc = CentralComposite(type="Spherical", center_points=1)
        ccc.num_parameters = 2
        cases = [case for case in ccc]
        print cases
        self.assertEqual([(0.146446609406726,0.146446609406726),(0.853553390593274,0.146446609406726),(0.853553390593274,0.853553390593274),(0.146446609406726,0.853553390593274),(0.5,0),(1,0.5),(0.5,1),(0,0.5),[0.5,0.5]],cases)

if __name__ == "__main__":
    unittest.main()
