"""
Test our array flatteners.
"""
import unittest

from openmdao.main.array_helpers import flatten_slice

class Testcase_flatten_slice(unittest.TestCase):
    """ Test capability to flatten any slice. """

    def test_general_flatten_slice(self):
        
        shape = (5,)
        index = '[1]'
        flat_str, ii = flatten_slice(index, shape, name='ii')
        
        self.assertTrue(flat_str=='ii:ii+1')
        self.assertEqual(ii, 1)
        
        shape = (3, 4)
        index = '[1, 2]'
        flat_str, ii = flatten_slice(index, shape, name='ii')
        
        self.assertTrue(flat_str=='ii:ii+1')
        self.assertEqual(ii, 6)
        
        shape = (9, 7)
        index = '[-1, -1]'
        flat_str, ii = flatten_slice(index, shape, name='ii')
        
        self.assertTrue(flat_str=='ii')
        self.assertEqual(ii, 62)
        
        shape = (4, 7)
        index = '[:, 3]'
        flat_str, ii = flatten_slice(index, shape, name='ii')
        
        self.assertTrue(flat_str=='ii')
        self.assertTrue(set(ii)==set([3, 10, 17, 24]))
        
        shape = (50,)
        index = '[-2]'
        flat_str, ii = flatten_slice(index, shape, name='ii')
        
        self.assertTrue(flat_str=='ii')
        self.assertEqual(ii, 48)
        
        shape = (50,)
        index = '[3:-3:5]'
        flat_str, ii = flatten_slice(index, shape, name='ii')
        
        self.assertTrue(flat_str=='ii')
        self.assertTrue(set(ii)==set([3, 8, 13, 18, 23, 28, 33, 38, 43]))
        
        print flat_str
        print ii

if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()