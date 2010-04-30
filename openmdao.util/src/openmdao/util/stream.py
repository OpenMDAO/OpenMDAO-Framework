import logging
import struct

import numpy

_SZ_INT = 4
_SZ_LONG = 8
_SZ_FLOAT = 4
_SZ_DOUBLE = 8


class Stream(object):
    """
    Wrapper of standard Python :class:`file` object.
    Supports reading/writing int and float arrays in various formats.

    - `file_obj` is a file object opened for reading or writing.
    - If `binary`, the data is in binary, not text, form.
    - If `big_endian`, the data bytes are in 'big-endian' order. \
      Only meaningful if `binary`.
    - If `single_precision`, floating-point data is 32 bits, not 64. \
      Only meaningful if `binary`.
    - If `integer_8`, integer data is 64 bits, not 32. \
      Only meaningful if `binary`.

    """
    def __init__(self, file_obj, binary=False, big_endian=False,
                 single_precision=False, integer_8=False):
        self.file = file_obj
        self.binary = binary
        if binary:
            self.big_endian = big_endian
            self.single_precision = single_precision
            self.integer_8 = integer_8
        else:
            # Ensure sanity.
            self.big_endian = False
            self.single_precision = False
            self.integer_8 = False

    def read_int(self):
        """ Returns next integer. """
        data = self.read_ints(1)[0]
        return data

    def read_ints(self, count):
        """ Returns next `count` integers as a :mod:`numpy` array. """
        sep = '' if self.binary else ' '
        dtype = numpy.int64 if self.integer_8 else numpy.int32
        data = numpy.fromfile(self.file, dtype=dtype, count=count, sep=sep)
        if self.big_endian:
            data.byteswap(True)
        return data

    def read_float(self):
        """ Returns next float. """
        data = self.read_floats(1)[0]
        return data

    def read_floats(self, count):
        """ Returns next `count` floats as a :mod:`numpy` array. """
        sep = '' if self.binary else ' '
        dtype = numpy.float32 if self.single_precision else numpy.float64
        data = numpy.fromfile(self.file, dtype=dtype, count=count, sep=sep)
        if self.big_endian:
            data.byteswap(True)
        return data

    def write_int(self, value):
        """ Writes an integer. """
        if self.binary:
            fmt = '>' if self.big_endian else '<'
            fmt += 'q' if self.integer_8 else 'i'
            self.file.write(struct.pack(fmt, value))
        else:
            self.file.write('%d ' % value)

    def write_ints(self, data, order='C'):
        """
        Writes :mod:`numpy` integer array `data`.  If `order` is 'C', the data
        is written in row-major order.  If `order` is 'Fortran', the data is
        written in column-major order.
        """
        if self.binary:
# TODO: capability to write as different size than stored.
            if self.big_endian:
                data.byteswap(True)
            self.file.write(data.tostring(order=order))
            if self.big_endian:
                data.byteswap(True)
        else:
            data.tofile(self.file, sep=' ')

    def write_float(self, value):
        """ Writes a float. """
        if self.binary:
            fmt = '>' if self.big_endian else '<'
            fmt += 'f' if self.single_precision else 'd'
            self.file.write(struct.pack(fmt, value))
        else:
            self.file.write('%g ' % value)

    def write_floats(self, data, order='C'):
        """
        Writes :mod:`numpy` float array `data`.  If `order` is 'C', the data
        is written in row-major order.  If `order` is 'Fortran', the data is
        written in column-major order.
        """
        if self.binary:
# TODO: capability to write as different size than stored.
            if self.big_endian:
                data.byteswap(True)
            self.file.write(data.tostring(order=order))
            if self.big_endian:
                data.byteswap(True)
        else:
            data.tofile(self.file, sep=' ')

