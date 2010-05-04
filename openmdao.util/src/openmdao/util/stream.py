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

    def close(self):
        """ Close underlying file. """
        return self.file.close()


    ######## Input Operations ########


    def read_int(self):
        """ Returns next integer. """
        data = self.read_ints(1)[0]
        return data

    def read_ints(self, shape, order='C'):
        """ Returns integers as a :mod:`numpy` array of `shape`. """
        reshape = False
        count = 1
        try:
            for size in shape:
                count *= size
            reshape = True
        except TypeError:
            count = shape

        sep = '' if self.binary else ' '
        dtype = numpy.int64 if self.integer_8 else numpy.int32
        data = numpy.fromfile(self.file, dtype=dtype, count=count, sep=sep)
        if self.big_endian:
            data.byteswap(True)

        return data.reshape(shape, order=order) if reshape else data

    def read_float(self):
        """ Returns next float. """
        data = self.read_floats(1)[0]
        return data

    def read_floats(self, shape, order='C'):
        """ Returns floats as a :mod:`numpy` array of `shape`. """
        reshape = False
        count = 1
        try:
            for size in shape:
                count *= size
            reshape = True
        except TypeError:
            count = shape

        sep = '' if self.binary else ' '
        dtype = numpy.float32 if self.single_precision else numpy.float64
        data = numpy.fromfile(self.file, dtype=dtype, count=count, sep=sep)
        if self.big_endian:
            data.byteswap(True)

        return data.reshape(shape, order=order) if reshape else data


    ######## Output Operations ########


    def write_int(self, value, sep=' ', fmt='%s'):
        """ Writes an integer. """
        if self.binary:
            fmt = '>' if self.big_endian else '<'
            fmt += 'q' if self.integer_8 else 'i'
            self.file.write(struct.pack(fmt, value))
        else:
            self.file.write(fmt % value)
            self.file.write(sep)

    def write_ints(self, data, order='C', sep=' ', fmt='%s', linecount=0):
        """
        Writes :mod:`numpy` integer array `data`.  If `order` is 'C', the data
        is written in row-major order.  If `order` is 'Fortran', the data is
        written in column-major order. If `linecount` is > zero, then at most
        `linecount` values are written per line.
        """
        if self.binary:
            arr = data
            if self.integer_8:
                if data.itemsize != _SZ_LONG:
                    arr = numpy.array(data, dtype=numpy.int64)
            elif data.itemsize != _SZ_INT:
                arr = numpy.array(data, dtype=numpy.int32)

            if self.big_endian:
                arr.byteswap(True)
            self.file.write(arr.tostring(order=order))
            if self.big_endian:
                arr.byteswap(True)
        else:
            self.write_array(data, order, sep, fmt, linecount)

    def write_float(self, value, sep=' ', fmt='%s'):
        """ Writes a float. """
        if self.binary:
            fmt = '>' if self.big_endian else '<'
            fmt += 'f' if self.single_precision else 'd'
            self.file.write(struct.pack(fmt, value))
        else:
            self.file.write(fmt % value)
            self.file.write(sep)

    def write_floats(self, data, order='C', sep=' ', fmt='%s', linecount=0):
        """
        Writes :mod:`numpy` float array `data`.  If `order` is 'C', the data
        is written in row-major order.  If `order` is 'Fortran', the data is
        written in column-major order. If `linecount` is > zero, then at most
        `linecount` values are written per line.
        """
        if self.binary:
            arr = data
            if self.single_precision:
                if data.itemsize != _SZ_FLOAT:
                    arr = numpy.array(data, dtype=numpy.float32)
            elif data.itemsize != _SZ_DOUBLE:
                arr = numpy.array(data, dtype=numpy.float64)

            if self.big_endian:
                data.byteswap(True)
            self.file.write(data.tostring(order=order))
            if self.big_endian:
                data.byteswap(True)
        else:
            self.write_array(data, order, sep, fmt, linecount)

    def write_array(self, data, order='C', sep=' ', fmt='%s', linecount=0):
        """
        Writes :mod:`numpy` array `data` as text. If `order` is 'C', the data
        is written in row-major order. If `order` is 'Fortran', the data is
        written in column-major order.  If `linecount` is > zero, then at
        most `linecount` values are written per line.
        """
        shape = data.shape
        indices = [0 for i in shape]
        item = data.item

        if order == 'C':
            # Row-major order.
            sequence = range(len(shape))
            sequence.reverse()
        elif order == 'Fortran':
            # Column-major order.
            sequence = range(len(shape))
        else:
            raise ValueError("order must be 'C' or 'Fortran'")

        count = 0
        while True:
            self.file.write(sep)
            self.file.write(fmt % item(*indices))
            if linecount > 0:
                count += 1
                if count >= linecount:
                    self.file.write('\n')
                    count = 0

            for i in sequence:
                indices[i] += 1
                if indices[i] >= shape[i]:
                    indices[i] = 0
                else:
                    break
            else:
                break

