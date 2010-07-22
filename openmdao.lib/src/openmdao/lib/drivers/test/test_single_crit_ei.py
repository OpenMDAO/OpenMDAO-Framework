# pylint: disable-msg=C0111,C0103

import unittest
from openmdao.main.api import Assembly, Component
from openmdao.lib.doegenerators.full_factorial import FullFactorial
from openmdao.lib.drivers.single_crit_ei import SingleCritEI

class SingleCritEITests(unittest.TestCase):

    def test_add_parameter(self):
        """test for adding parameters and setting alleles
        test that ranges on alleles are correct"""
        
        
        pass
    
    def test_calc_ei(self):
        """tests ei calculation function _calc_ei
        make sure ValueError gets thrown if dividing by zero, etc.
        and make sure ei is the correct value
        """
        pass

    def test_ei1(self):
        """test error message raised if criteria is longer than one"""
        pass

    def test_ei2(self):
        """test that evaluate() returns a normal distribution"""
        pass

    def test_ei3(self):
        """test to see if optimization of EI returns correct value
        and sets next_case"""
        pass

    def test_ei4(self):
        """test error messaged raised if best case not provided"""
        pass

if __name__ == "__main__":
    unittest.main()