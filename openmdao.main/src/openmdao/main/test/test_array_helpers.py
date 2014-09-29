"""
Test our array flatteners.
"""
import unittest

from openmdao.main.array_helpers import flatten_slice, get_flattened_index

class TestcaseArrayHelpers(unittest.TestCase):


    def test_general_flatten_slice(self):
        # Test capability to flatten any slice.
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

    def test_get_flattened_index(self):
        self.assertEqual(slice(4,5,None), get_flattened_index(4, (10,)))
        self.assertEqual(slice(9,10,None), get_flattened_index(-1, (10,)))
        self.assertEqual(slice(90,100,1), get_flattened_index(-1, (10,10)))
        try:
            self.assertEqual(0, get_flattened_index(10, (10,10)))
        except IndexError as err:
            # Some versions of numpy have slightly different messages, so as
            # long as it is an index error, we are fine.
            pass
        else:
            self.fail('Should get an Indexerror')

        self.assertEqual(slice(22,23,None), get_flattened_index((2,2), (10,10)))
        self.assertEqual(slice(42,63, 10), get_flattened_index((slice(4,7),2), (10,10)))
        self.assertEqual(slice(40,61,10), get_flattened_index((slice(4,7),0), (10,10)))
        self.assertEqual(slice(4, 11, 2), get_flattened_index(slice(4,11,2), (20,)))
        self.assertEqual(slice(40,50,1),
                          get_flattened_index(slice(4,5,2), (20,10)))

        self.assertEqual(slice(1,2,None), get_flattened_index(1, (5,)))
        self.assertEqual(slice(6,7,None), get_flattened_index([1,2], (3,4)))
        self.assertEqual(slice(62,63,None), get_flattened_index([-1,-1], (9,7)))
        self.assertEqual(slice(3, 25, 7), get_flattened_index([slice(None),3], (4,7)))
        self.assertEqual(slice(48,49,None), get_flattened_index(-2, (50,)))
        self.assertEqual(slice(3, 44, 5), get_flattened_index(slice(3,-3,5), (50,)))

if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()