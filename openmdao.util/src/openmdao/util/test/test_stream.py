"""
Test Stream functions.
"""

import logging
import os.path
import sys
import unittest

import numpy.testing

from openmdao.util.stream import Stream


class TestCase(unittest.TestCase):
    """ Test Stream functions. """

    def setUp(self):
        self.filename = 'test_stream.dat'

    def tearDown(self):
        if os.path.exists(self.filename):
            os.remove(self.filename)

    def test_ints(self):
        logging.debug('')
        logging.debug('test_ints')

        # 'Normal' integers.
        data = numpy.arange(0, 10, dtype=numpy.int32)
        with open(self.filename, 'wb') as out:
            stream = Stream(out, binary=True)
            stream.write_ints(data)
        self.assertEqual(os.path.getsize(self.filename), 40)
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True)
            new_data = stream.read_ints(data.size)
        numpy.testing.assert_array_equal(new_data, data)

        # Byteswapped.
        with open(self.filename, 'wb') as out:
            stream = Stream(out, binary=True, big_endian=True)
            stream.write_ints(data)
        self.assertEqual(os.path.getsize(self.filename), 40)
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True)
            new_data = stream.read_ints(data.size)
        try:
            numpy.testing.assert_array_equal(new_data, data)
        except AssertionError:
            pass
        else:
            self.fail('Expected AssertionError')
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True, big_endian=True)
            new_data = stream.read_ints(data.size)
        numpy.testing.assert_array_equal(new_data, data)

        # Big integers, which are default on some machines.
        data = numpy.arange(0, 10, dtype=numpy.int64)
        with open(self.filename, 'wb') as out:
            stream = Stream(out, binary=True, integer_8=True)
            stream.write_ints(data)
        self.assertEqual(os.path.getsize(self.filename), 80)
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True, integer_8=True)
            new_data = stream.read_ints(data.size)
        numpy.testing.assert_array_equal(new_data, data)

        # Row-major.
        arr2d = data.reshape((5, 2))
        with open(self.filename, 'wb') as out:
            stream = Stream(out, binary=True, integer_8=True)
            stream.write_ints(arr2d)
        self.assertEqual(os.path.getsize(self.filename), 80)
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True, integer_8=True)
            new_data = stream.read_ints(data.size).reshape((5, 2))
        numpy.testing.assert_array_equal(new_data, arr2d)

        # Column-major.
        with open(self.filename, 'wb') as out:
            stream = Stream(out, binary=True, integer_8=True)
            stream.write_ints(arr2d, order='Fortran')
        self.assertEqual(os.path.getsize(self.filename), 80)
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True, integer_8=True)
            new_data = stream.read_ints(data.size).reshape((5, 2))
        try:
            numpy.testing.assert_array_equal(new_data, arr2d)
        except AssertionError:
            pass
        else:
            self.fail('Expected AssertionError')
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True, integer_8=True)
            new_data = stream.read_ints(data.size).reshape((5, 2),
                                                           order='Fortran')

    def test_floats(self):
        logging.debug('')
        logging.debug('test_floats')

        # Single precision.
        data = numpy.arange(0, 10, dtype=numpy.float32)
        with open(self.filename, 'wb') as out:
            stream = Stream(out, binary=True, single_precision=True)
            stream.write_floats(data)
        self.assertEqual(os.path.getsize(self.filename), 40)
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True, single_precision=True)
            new_data = stream.read_floats(data.size)
        numpy.testing.assert_array_equal(new_data, data)

        # Byteswapped.
        with open(self.filename, 'wb') as out:
            stream = Stream(out, binary=True, single_precision=True,
                            big_endian=True)
            stream.write_floats(data)
        self.assertEqual(os.path.getsize(self.filename), 40)
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True, single_precision=True)
            new_data = stream.read_floats(data.size)
        try:
            numpy.testing.assert_array_equal(new_data, data)
        except AssertionError:
            pass
        else:
            self.fail('Expected AssertionError')
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True, single_precision=True,
                            big_endian=True)
            new_data = stream.read_floats(data.size)
        numpy.testing.assert_array_equal(new_data, data)

        # Double precision.
        data = numpy.arange(0, 10, dtype=numpy.float64)
        with open(self.filename, 'wb') as out:
            stream = Stream(out, binary=True)
            stream.write_floats(data)
        self.assertEqual(os.path.getsize(self.filename), 80)
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True)
            new_data = stream.read_floats(data.size)
        numpy.testing.assert_array_equal(new_data, data)

        # Row-major.
        arr2d = data.reshape((5, 2))
        with open(self.filename, 'wb') as out:
            stream = Stream(out, binary=True)
            stream.write_floats(arr2d)
        self.assertEqual(os.path.getsize(self.filename), 80)
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True)
            new_data = stream.read_floats(data.size).reshape((5, 2))
        numpy.testing.assert_array_equal(new_data, arr2d)

        # Column-major.
        with open(self.filename, 'wb') as out:
            stream = Stream(out, binary=True)
            stream.write_floats(arr2d, order='Fortran')
        self.assertEqual(os.path.getsize(self.filename), 80)
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True)
            new_data = stream.read_floats(data.size).reshape((5, 2))
        try:
            numpy.testing.assert_array_equal(new_data, arr2d)
        except AssertionError:
            pass
        else:
            self.fail('Expected AssertionError')
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True)
            new_data = stream.read_floats(data.size).reshape((5, 2),
                                                             order='Fortran')


if __name__ == '__main__':
    import nose
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()

