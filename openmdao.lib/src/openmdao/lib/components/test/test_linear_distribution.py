import unittest

import numpy as np

from openmdao.lib.components.linear_distribution import LinearDistribution

class TestLinearDist(unittest.TestCase): 
    
    def test_Linear(self): 
        ld = LinearDistribution(3)

        self.assertTrue(all([x==y for x,y in zip(ld.output,np.ones(3))]))

        ld.start = 0
        ld.end = 10

        ld.run()
        self.assertEqual(ld.output[0],0)
        self.assertEqual(ld.output[1],5)
        self.assertEqual(ld.output[2],10)
        self.assertEqual(ld.delta,5)

    def test_offset(self):
        
        ld = LinearDistribution(3)
        ld.start = 0
        ld.end = 10
        ld.offset = 5

        ld.run()
        self.assertEqual(ld.output[0],5)
        self.assertEqual(ld.output[1],10)
        self.assertEqual(ld.output[2],15)
        self.assertEqual(ld.delta,5)        


    def test_units(self):
        ld = LinearDistribution(3,units='m')

        ld.start = 0
        ld.end = 10

        ld.run()
        self.assertEqual(ld.output[0],0)
        self.assertEqual(ld.output[1],5)
        self.assertEqual(ld.output[2],10)
        self.assertEqual(ld.delta,5) 

        meta = ld.get_metadata('start')
        self.assertEqual(meta['units'],'m')

        meta = ld.get_metadata('end')
        self.assertEqual(meta['units'],'m')

        meta = ld.get_metadata('output')
        self.assertEqual(meta['units'],'m')


    def test_bad_units(self):
        try: 
            ld = LinearDistribution(3,units='second')
        except ValueError as err: 
            self.assertEqual(str(err),"Units of 'second' are invalid")
        else: 
            self.fail('ValueError expected')    
            



if __name__ == "__main__": 
    unittest.main()
