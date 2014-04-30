"""
Test Stream functions.
"""

import logging
import os.path
import sys
import unittest

import numpy.testing

from openmdao.util.stream import Stream
from openmdao.util.testutil import assert_raises

# These constants are in little-endian form.
UNF_I4 = '\x04\x00\x00\x00' \
         '\x01\x00\x00\x00' \
         '\x04\x00\x00\x00'

UNF_I4A = '\x20\x00\x00\x00' \
          '\x01\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00' \
          '\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00' \
          '\x20\x00\x00\x00'

UNF_I8 = '\x08\x00\x00\x00' \
         '\x01\x00\x00\x00\x00\x00\x00\x00' \
         '\x08\x00\x00\x00'

UNF_I8A = '\x40\x00\x00\x00' \
          '\x01\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00' \
          '\x03\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00' \
          '\x05\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00' \
          '\x07\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00' \
          '\x40\x00\x00\x00'

UNF_R4 = '\x04\x00\x00\x00' \
         '\x00\x00\x80\x3f' \
         '\x04\x00\x00\x00'

UNF_R4A = '\x20\x00\x00\x00' \
          '\x00\x00\x80\x3f\x00\x00\x00\x40\x00\x00\x40\x40\x00\x00\x80\x40' \
          '\x00\x00\xa0\x40\x00\x00\xc0\x40\x00\x00\xe0\x40\x00\x00\x00\x41' \
          '\x20\x00\x00\x00'

UNF_R8 = '\x08\x00\x00\x00' \
         '\x00\x00\x00\x00\x00\x00\xf0\x3f' \
         '\x08\x00\x00\x00'

UNF_R8A = '\x40\x00\x00\x00' \
          '\x00\x00\x00\x00\x00\x00\xf0\x3f\x00\x00\x00\x00\x00\x00\x00\x40' \
          '\x00\x00\x00\x00\x00\x00\x08\x40\x00\x00\x00\x00\x00\x00\x10\x40' \
          '\x00\x00\x00\x00\x00\x00\x14\x40\x00\x00\x00\x00\x00\x00\x18\x40' \
          '\x00\x00\x00\x00\x00\x00\x1c\x40\x00\x00\x00\x00\x00\x00\x20\x40' \
          '\x40\x00\x00\x00'


class TestCase(unittest.TestCase):
    """ Test Stream functions. """

    def setUp(self):
        self.filename = 'test_stream.dat'

    def tearDown(self):
        if os.path.exists(self.filename):
            os.remove(self.filename)

    def test_int32(self):
        logging.debug('')
        logging.debug('test_int32')

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

        # Text scalar.
        with open(self.filename, 'w') as out:
            stream = Stream(out)
            stream.write_int(4, sep=' ')
            stream.write_int(2, full_record=True)
        size = 5 if sys.platform == 'win32' else 4  # CR LF
        self.assertEqual(os.path.getsize(self.filename), size)
        with open(self.filename, 'r') as inp:
            new_data = inp.read()
            self.assertEqual(new_data, '4 2\n')

        # Unformatted scalar.
        with open(self.filename, 'wb') as out:
            stream = Stream(out, binary=True, unformatted=True)
            stream.write_int(1, full_record=True)
        self.assertEqual(os.path.getsize(self.filename), 12)
        with open(self.filename, 'rb') as inp:
            new_data = inp.read()
            self.assertEqual(new_data, UNF_I4)
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True)
            new_data = stream.read_int()
        try:
            self.assertEqual(new_data, 1)
        except AssertionError:
            pass
        else:
            self.fail('Expected AssertionError')
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True, unformatted=True)
            new_data = stream.read_int(full_record=True)
        self.assertEqual(new_data, 1)

        # Unformatted array.
        data = numpy.arange(1, 9, dtype=numpy.int32)
        with open(self.filename, 'wb') as out:
            stream = Stream(out, binary=True, unformatted=True)
            stream.write_ints(data, full_record=True)
        self.assertEqual(os.path.getsize(self.filename), 40)
        with open(self.filename, 'rb') as inp:
            new_data = inp.read()
            self.assertEqual(new_data, UNF_I4A)
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
            stream = Stream(inp, binary=True, unformatted=True)
            new_data = stream.read_ints(data.size, full_record=True)
        numpy.testing.assert_array_equal(new_data, data)

        # Byteswapped.
        swap_endian = sys.byteorder == 'little'
        wrong_endian = not swap_endian
        with open(self.filename, 'wb') as out:
            stream = Stream(out, binary=True, big_endian=swap_endian)
            stream.write_ints(data)
        self.assertEqual(os.path.getsize(self.filename), 32)
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True, big_endian=wrong_endian)
            new_data = stream.read_ints(data.size)
        try:
            numpy.testing.assert_array_equal(new_data, data)
        except AssertionError:
            pass
        else:
            self.fail('Expected AssertionError')
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True, big_endian=swap_endian)
            new_data = stream.read_ints(data.size)
        numpy.testing.assert_array_equal(new_data, data)

        # Write as 8-byte integers.
        with open(self.filename, 'wb') as out:
            stream = Stream(out, binary=True, integer_8=True)
            stream.write_ints(data)
        self.assertEqual(os.path.getsize(self.filename), 64)
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True, integer_8=True)
            new_data = stream.read_ints(data.size)
        numpy.testing.assert_array_equal(new_data, data)

        # Write from list.
        data = list(data)
        with open(self.filename, 'wb') as out:
            stream = Stream(out, binary=True)
            stream.write_ints(data)
        self.assertEqual(os.path.getsize(self.filename), 32)
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True)
            new_data = stream.read_ints(len(data))
        numpy.testing.assert_array_equal(new_data, data)

    def test_int64(self):
        logging.debug('')
        logging.debug('test_int64')

        # Big integers, which are default on some machines.
        data = numpy.arange(1, 9, dtype=numpy.int64)
        with open(self.filename, 'wb') as out:
            stream = Stream(out, binary=True, integer_8=True)
            stream.write_ints(data)
        self.assertEqual(os.path.getsize(self.filename), 64)
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True, integer_8=True)
            new_data = stream.read_ints(data.size)
        numpy.testing.assert_array_equal(new_data, data)

        # Unformatted scalar.
        with open(self.filename, 'wb') as out:
            stream = Stream(out, binary=True, integer_8=True, unformatted=True)
            stream.write_int(1, full_record=True)
        self.assertEqual(os.path.getsize(self.filename), 16)
        with open(self.filename, 'rb') as inp:
            new_data = inp.read()
            self.assertEqual(new_data, UNF_I8)
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True, integer_8=True)
            new_data = stream.read_int()
        try:
            self.assertEqual(new_data, 1)
        except AssertionError:
            pass
        else:
            self.fail('Expected AssertionError')
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True, integer_8=True, unformatted=True)
            new_data = stream.read_int(full_record=True)
        self.assertEqual(new_data, 1)

        # Unformatted array.
        with open(self.filename, 'wb') as out:
            stream = Stream(out, binary=True, integer_8=True, unformatted=True)
            stream.write_ints(data, full_record=True)
        self.assertEqual(os.path.getsize(self.filename), 72)
        with open(self.filename, 'rb') as inp:
            new_data = inp.read()
            self.assertEqual(new_data, UNF_I8A)
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
            stream = Stream(inp, binary=True, integer_8=True, unformatted=True)
            new_data = stream.read_ints(data.size, full_record=True)
        numpy.testing.assert_array_equal(new_data, data)

        # Write as 4-byte integers.
        with open(self.filename, 'wb') as out:
            stream = Stream(out, binary=True)
            stream.write_ints(data)
        self.assertEqual(os.path.getsize(self.filename), 32)
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True)
            new_data = stream.read_ints(data.size)
        numpy.testing.assert_array_equal(new_data, data)

        # Row-major.
        data = numpy.arange(0, 10, dtype=numpy.int64)
        arr2d = data.reshape((5, 2))
        with open(self.filename, 'wb') as out:
            stream = Stream(out, binary=True, integer_8=True)
            stream.write_ints(arr2d)
        self.assertEqual(os.path.getsize(self.filename), 80)
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True, integer_8=True)
            new_data = stream.read_ints((5, 2))
        numpy.testing.assert_array_equal(new_data, arr2d)

        # Row-major text.
        with open(self.filename, 'w') as out:
            stream = Stream(out)
            stream.write_ints(arr2d, linecount=4)
        with open(self.filename, 'r') as inp:
            stream = Stream(inp)
            new_data = stream.read_ints((5, 2), order='Fortran')
        try:
            numpy.testing.assert_array_equal(new_data, arr2d)
        except AssertionError:
            pass
        else:
            self.fail('Expected AssertionError')
        with open(self.filename, 'r') as inp:
            stream = Stream(inp)
            new_data = stream.read_ints((5, 2), order='C')
        numpy.testing.assert_array_equal(new_data, arr2d)

        # Column-major.
        with open(self.filename, 'wb') as out:
            stream = Stream(out, binary=True, integer_8=True)
            stream.write_ints(arr2d, order='Fortran')
        self.assertEqual(os.path.getsize(self.filename), 80)
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True, integer_8=True)
            new_data = stream.read_ints((5, 2))
        try:
            numpy.testing.assert_array_equal(new_data, arr2d)
        except AssertionError:
            pass
        else:
            self.fail('Expected AssertionError')
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True, integer_8=True)
            new_data = stream.read_ints((5, 2), order='Fortran')
        numpy.testing.assert_array_equal(new_data, arr2d)

        # Column-major text.
        with open(self.filename, 'w') as out:
            stream = Stream(out)
            stream.write_ints(arr2d, order='Fortran', linecount=4)
        with open(self.filename, 'r') as inp:
            stream = Stream(inp)
            new_data = stream.read_ints((5, 2))
        try:
            numpy.testing.assert_array_equal(new_data, arr2d)
        except AssertionError:
            pass
        else:
            self.fail('Expected AssertionError')
        with open(self.filename, 'r') as inp:
            stream = Stream(inp)
            new_data = stream.read_ints((5, 2), order='Fortran')
        numpy.testing.assert_array_equal(new_data, arr2d)

        # Illegal-order text.
        with open(self.filename, 'w') as out:
            stream = Stream(out)
            assert_raises(self, "stream.write_ints(arr2d, order='Unknown')",
                          globals(), locals(), ValueError,
                          "order must be 'C' or 'Fortran'")

    def test_float32(self):
        logging.debug('')
        logging.debug('test_float32')

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

        # Text scalar.
        with open(self.filename, 'w') as out:
            stream = Stream(out)
            stream.write_float(4., sep=' ')
            stream.write_float(2., full_record=True)
        size = 5 if sys.platform == 'win32' else 4  # CR LF
        self.assertEqual(os.path.getsize(self.filename), size)
        with open(self.filename, 'r') as inp:
            new_data = inp.read()
            self.assertEqual(new_data, '4 2\n')

        # Unformatted scalar.
        with open(self.filename, 'wb') as out:
            stream = Stream(out, binary=True, single_precision=True,
                            unformatted=True)
            stream.write_float(1., full_record=True)
        self.assertEqual(os.path.getsize(self.filename), 12)
        with open(self.filename, 'rb') as inp:
            new_data = inp.read()
            self.assertEqual(new_data, UNF_R4)
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True, single_precision=True)
            new_data = stream.read_float()
        try:
            self.assertEqual(new_data, 1.)
        except AssertionError:
            pass
        else:
            self.fail('Expected AssertionError')
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True, single_precision=True,
                            unformatted=True)
            new_data = stream.read_float(full_record=True)
        self.assertEqual(new_data, 1.)

        # Unformatted array.
        data = numpy.arange(1, 9, dtype=numpy.float32)
        with open(self.filename, 'wb') as out:
            stream = Stream(out, binary=True, single_precision=True,
                            unformatted=True)
            stream.write_floats(data, full_record=True)
        self.assertEqual(os.path.getsize(self.filename), 40)
        with open(self.filename, 'rb') as inp:
            new_data = inp.read()
            self.assertEqual(new_data, UNF_R4A)
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
                            unformatted=True)
            new_data = stream.read_floats(data.size, full_record=True)
        numpy.testing.assert_array_equal(new_data, data)

        # Byteswapped.
        swap_endian = sys.byteorder == 'little'
        wrong_endian = not swap_endian
        data = numpy.arange(0, 10, dtype=numpy.float32)
        with open(self.filename, 'wb') as out:
            stream = Stream(out, binary=True, single_precision=True,
                            big_endian=swap_endian)
            stream.write_floats(data)
        self.assertEqual(os.path.getsize(self.filename), 40)
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True, single_precision=True,
                            big_endian=wrong_endian)
            new_data = stream.read_floats(data.size)
        try:
            numpy.testing.assert_array_equal(new_data, data)
        except AssertionError:
            pass
        else:
            self.fail('Expected AssertionError')
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True, single_precision=True,
                            big_endian=swap_endian)
            new_data = stream.read_floats(data.size)
        numpy.testing.assert_array_equal(new_data, data)

        # Write as double precision.
        with open(self.filename, 'wb') as out:
            stream = Stream(out, binary=True)
            stream.write_floats(data)
        self.assertEqual(os.path.getsize(self.filename), 80)
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True)
            new_data = stream.read_floats(data.size)
        numpy.testing.assert_array_equal(new_data, data)

        # Write from list.
        data = list(data)
        with open(self.filename, 'wb') as out:
            stream = Stream(out, binary=True, single_precision=True)
            stream.write_floats(data)
        self.assertEqual(os.path.getsize(self.filename), 40)
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True, single_precision=True)
            new_data = stream.read_floats(len(data))
        numpy.testing.assert_array_equal(new_data, data)

    def test_float64(self):
        logging.debug('')
        logging.debug('test_float64')

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

        # Unformatted scalar.
        with open(self.filename, 'wb') as out:
            stream = Stream(out, binary=True, unformatted=True)
            stream.write_float(1., full_record=True)
        self.assertEqual(os.path.getsize(self.filename), 16)
        with open(self.filename, 'rb') as inp:
            new_data = inp.read()
            self.assertEqual(new_data, UNF_R8)
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True)
            new_data = stream.read_float()
        try:
            self.assertEqual(new_data, 1.)
        except AssertionError:
            pass
        else:
            self.fail('Expected AssertionError')
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True, unformatted=True)
            new_data = stream.read_float(full_record=True)
        self.assertEqual(new_data, 1.)

        # Unformatted array.
        data = numpy.arange(1, 9, dtype=numpy.float64)
        with open(self.filename, 'wb') as out:
            stream = Stream(out, binary=True, unformatted=True)
            stream.write_floats(data, full_record=True)
        self.assertEqual(os.path.getsize(self.filename), 72)
        with open(self.filename, 'rb') as inp:
            new_data = inp.read()
            self.assertEqual(new_data, UNF_R8A)
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True)
            new_data = stream.read_floats(data.size)
        try:
            numpy.testing.assert_array_equal(new_data, data)
        except AssertionError:
            pass
        else:
            self.fail('Expected AssertionError')
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True, unformatted=True)
            new_data = stream.read_floats(data.size, full_record=True)
        numpy.testing.assert_array_equal(new_data, data)

        # Write as single precision.
        with open(self.filename, 'wb') as out:
            stream = Stream(out, binary=True, single_precision=True)
            stream.write_floats(data)
        self.assertEqual(os.path.getsize(self.filename), 32)
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True, single_precision=True)
            new_data = stream.read_floats(data.size)
        numpy.testing.assert_array_equal(new_data, data)

        # Row-major.
        data = numpy.arange(0, 10, dtype=numpy.float64)
        arr2d = data.reshape((5, 2))
        with open(self.filename, 'wb') as out:
            stream = Stream(out, binary=True)
            stream.write_floats(arr2d)
        self.assertEqual(os.path.getsize(self.filename), 80)
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True)
            new_data = stream.read_floats((5, 2))
        numpy.testing.assert_array_equal(new_data, arr2d)

        # Column-major.
        with open(self.filename, 'wb') as out:
            stream = Stream(out, binary=True)
            stream.write_floats(arr2d, order='Fortran')
        self.assertEqual(os.path.getsize(self.filename), 80)
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True)
            new_data = stream.read_floats((5, 2))
        try:
            numpy.testing.assert_array_equal(new_data, arr2d)
        except AssertionError:
            pass
        else:
            self.fail('Expected AssertionError')
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True)
            new_data = stream.read_floats((5, 2), order='Fortran')
        numpy.testing.assert_array_equal(new_data, arr2d)

        # Text.
        with open(self.filename, 'w') as out:
            stream = Stream(out)
            stream.write_floats(arr2d, order='Fortran', linecount=4)
        with open(self.filename, 'r') as inp:
            stream = Stream(inp)
            new_data = stream.read_floats((5, 2))
        try:
            numpy.testing.assert_array_equal(new_data, arr2d)
        except AssertionError:
            pass
        else:
            self.fail('Expected AssertionError')
        with open(self.filename, 'r') as inp:
            stream = Stream(inp)
            new_data = stream.read_floats((5, 2), order='Fortran')
        numpy.testing.assert_array_equal(new_data, arr2d)

    def test_misc(self):
        logging.debug('')
        logging.debug('test_misc')

        out = open(self.filename, 'w')
        stream = Stream(out)
        stream.close()

        # Unformatted recordlength errors.
        data = '\x42'+UNF_I4[1:]
        with open(self.filename, 'wb') as out:
            out.write(data)
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True, unformatted=True)
            assert_raises(self, 'stream.read_int(full_record=True)',
                          globals(), locals(), RuntimeError,
                          'unexpected recordlength 66')

        data = UNF_I4[:-1]+'\x42'
        with open(self.filename, 'wb') as out:
            out.write(data)
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True, unformatted=True)
            assert_raises(self, 'stream.read_int(full_record=True)',
                          globals(), locals(), RuntimeError,
                          'mismatched recordlength 1107296260 vs. 4')

        data = '\x42'+UNF_I4A[1:]
        with open(self.filename, 'wb') as out:
            out.write(data)
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True, unformatted=True)
            assert_raises(self, 'stream.read_ints(8, full_record=True)',
                          globals(), locals(), RuntimeError,
                          'unexpected recordlength 66')

        data = UNF_I4A[:-1]+'\x42'
        with open(self.filename, 'wb') as out:
            out.write(data)
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True, unformatted=True)
            assert_raises(self, 'stream.read_ints(8, full_record=True)',
                          globals(), locals(), RuntimeError,
                          'mismatched recordlength 1107296288 vs. 32')

        data = '\x42'+UNF_R8[1:]
        with open(self.filename, 'wb') as out:
            out.write(data)
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True, unformatted=True)
            assert_raises(self, 'stream.read_float(full_record=True)',
                          globals(), locals(), RuntimeError,
                          'unexpected recordlength 66')

        data = UNF_R8[:-1]+'\x42'
        with open(self.filename, 'wb') as out:
            out.write(data)
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True, unformatted=True)
            assert_raises(self, 'stream.read_float(full_record=True)',
                          globals(), locals(), RuntimeError,
                          'mismatched recordlength 1107296264 vs. 8')

        data = '\x42'+UNF_R8A[1:]
        with open(self.filename, 'wb') as out:
            out.write(data)
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True, unformatted=True)
            assert_raises(self, 'stream.read_floats(8, full_record=True)',
                          globals(), locals(), RuntimeError,
                          'unexpected recordlength 66')

        data = UNF_R8A[:-1]+'\x42'
        with open(self.filename, 'wb') as out:
            out.write(data)
        with open(self.filename, 'rb') as inp:
            stream = Stream(inp, binary=True, unformatted=True)
            assert_raises(self, 'stream.read_floats(8, full_record=True)',
                          globals(), locals(), RuntimeError,
                          'mismatched recordlength 1107296320 vs. 64')


if __name__ == '__main__':
    import nose
    sys.argv.append('--cover-package=openmdao.util')
    sys.argv.append('--cover-erase')
    nose.runmodule()

