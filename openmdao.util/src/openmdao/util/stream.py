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
    - If `unformatted`, the data is surrounded by Fortran record length \
      markers.  Only meaningful if `binary`.
    - If `recordmark_8`, the record length markers are 64 bits, not 32. \
      Only meaningful if `unformatted`.

    """
    def __init__(self, file_obj, binary=False, big_endian=False,
                 single_precision=False, integer_8=False,
                 unformatted=False, recordmark_8=False):
        self.file = file_obj
        self.binary = binary
        if binary:
            self.big_endian = big_endian
            self.single_precision = single_precision
            self.integer_8 = integer_8
            self.unformatted = unformatted
            self.recordmark_8 = recordmark_8
        else:
            # Ensure sanity.
            self.big_endian = False
            self.single_precision = False
            self.integer_8 = False
            self.unformatted = False
            self.recordmark_8 = False

    def reclen_ints(self, count):
        """ Returns record length for `count` ints. """
        if self.integer_8:
            return _SZ_LONG * count
        else:
            return _SZ_INT * count

    def reclen_floats(self, count):
        """ Returns record length for `count` floats. """
        if self.single_precision:
            return _SZ_FLOAT * count
        else:
            return _SZ_DOUBLE * count


    ######## Input Operations ########


    def read_int(self, full_record=False):
        """
        Returns next integer. If `full_record`, then read surrounding
        recordmarks. Only meaningful if `unformatted`.
        """
        if full_record and self.unformatted:
            reclen = self.read_recordmark()
            if reclen != self.reclen_ints(1):
                logging.warning('unexpected recordlength %d', reclen)

        data = self.read_ints(1)[0]

        if full_record and self.unformatted:
            reclen2 = self.read_recordmark()
            if reclen2 != reclen:
                logging.warning('mismatched recordlength %d vs. %d',
                                reclen2, reclen)
        return data

    def read_ints(self, shape, order='C', full_record=False):
        """
        Returns integers as a :mod:`numpy` array of `shape`. If `full_record`,
        then read surrounding recordmarks. Only meaningful if `unformatted`.
        """
        reshape = False
        count = 1
        try:
            for size in shape:
                count *= size
            reshape = True
        except TypeError:
            count = shape

        if full_record and self.unformatted:
            reclen = self.read_recordmark()
            if reclen != self.reclen_ints(count):
                logging.warning('unexpected recordlength %d', reclen)

        sep = '' if self.binary else ' '
        dtype = numpy.int64 if self.integer_8 else numpy.int32
        data = numpy.fromfile(self.file, dtype=dtype, count=count, sep=sep)
        if self.big_endian:
            data.byteswap(True)

        if full_record and self.unformatted:
            reclen2 = self.read_recordmark()
            if reclen2 != reclen:
                logging.warning('mismatched recordlength %d vs. %d',
                                reclen2, reclen)

        return data.reshape(shape, order=order) if reshape else data

    def read_float(self, full_record=False):
        """
        Returns next float. If `full_record`, then read surrounding
        recordmarks. Only meaningful if `unformatted`.
        """
        if full_record and self.unformatted:
            reclen = self.read_recordmark()
            if reclen != self.reclen_floats(1):
                logging.warning('unexpected recordlength %d', reclen)

        data = self.read_floats(1)[0]

        if full_record and self.unformatted:
            reclen2 = self.read_recordmark()
            if reclen2 != reclen:
                logging.warning('mismatched recordlength %d vs. %d',
                                reclen2, reclen)
        return data

    def read_floats(self, shape, order='C', full_record=False):
        """
        Returns floats as a :mod:`numpy` array of `shape`. If `full_record`,
        then read surrounding recordmarks. Only meaningful if `unformatted`.
        """
        reshape = False
        count = 1
        try:
            for size in shape:
                count *= size
            reshape = True
        except TypeError:
            count = shape

        if full_record and self.unformatted:
            reclen = self.read_recordmark()
            if reclen != self.reclen_floats(count):
                logging.warning('unexpected recordlength %d', reclen)

        sep = '' if self.binary else ' '
        dtype = numpy.float32 if self.single_precision else numpy.float64
        data = numpy.fromfile(self.file, dtype=dtype, count=count, sep=sep)
        if self.big_endian:
            data.byteswap(True)

        if full_record and self.unformatted:
            reclen2 = self.read_recordmark()
            if reclen2 != reclen:
                logging.warning('mismatched recordlength %d vs. %d',
                                reclen2, reclen)

        return data.reshape(shape, order=order) if reshape else data

    def read_recordmark(self):
        """ Returns value of next recordmark. """
        fmt = '>' if self.big_endian else '<'
        fmt += 'q' if self.recordmark_8 else 'i'
        size = _SZ_LONG if self.recordmark_8 else _SZ_INT
        return struct.unpack(fmt, self.file.read(size))[0]


    ######## Output Operations ########


    def write_int(self, value, sep=' ', fmt='%s', full_record=False):
        """
        Writes an integer. If `full_record`, then write surrounding
        recordmarks. Only meaningful if `unformatted`.
        """
        if self.binary:
            if full_record and self.unformatted:
                self.write_recordmark(self.reclen_ints(1))

            fmt = '>' if self.big_endian else '<'
            fmt += 'q' if self.integer_8 else 'i'
            self.file.write(struct.pack(fmt, value))

            if full_record and self.unformatted:
                self.write_recordmark(self.reclen_ints(1))
        else:
            self.file.write(fmt % value)
            if full_record:
                self.file.write('\n')
            else:
                self.file.write(sep)

    def write_ints(self, data, order='C', sep=' ', fmt='%s', linecount=0,
                   full_record=False):
        """
        Writes :mod:`numpy` integer array `data`. If `order` is 'C', the data
        is written in row-major order. If `order` is 'Fortran', the data is
        written in column-major order. If `linecount` is > zero, then at most
        `linecount` values are written per line. If `full_record`, then write
        surrounding recordmarks. Only meaningful if `unformatted`.
        """
        if self.binary:
            if full_record and self.unformatted:
                self.write_recordmark(self.reclen_ints(data.size))

            arr = data
            if self.integer_8:
                if data.itemsize != _SZ_LONG:
                    arr = numpy.array(data, dtype=numpy.int64)
            elif data.itemsize != _SZ_INT:
                arr = numpy.array(data, dtype=numpy.int32)

            if self.big_endian:
                arr.byteswap(True)
            try:
                self.file.write(arr.tostring(order=order))
            finally:
                if self.big_endian:
                    arr.byteswap(True)

            if full_record and self.unformatted:
                self.write_recordmark(self.reclen_ints(data.size))
        else:
            self.write_array(data, order, sep, fmt, linecount)

    def write_float(self, value, sep=' ', fmt='%s', full_record=False):
        """
        Writes a float. If `full_record`, then write surrounding
        recordmarks. Only meaningful if `unformatted`.
        """
        if self.binary:
            if full_record and self.unformatted:
                self.write_recordmark(self.reclen_ints(1))

            fmt = '>' if self.big_endian else '<'
            fmt += 'f' if self.single_precision else 'd'
            self.file.write(struct.pack(fmt, value))

            if full_record and self.unformatted:
                self.write_recordmark(self.reclen_ints(1))
        else:
            self.file.write(fmt % value)
            if full_record:
                self.file.write('\n')
            else:
                self.file.write(sep)

    def write_floats(self, data, order='C', sep=' ', fmt='%s', linecount=0,
                     full_record=False):
        """
        Writes :mod:`numpy` float array `data`. If `order` is 'C', the data
        is written in row-major order. If `order` is 'Fortran', the data is
        written in column-major order. If `linecount` is > zero, then at most
        `linecount` values are written per line. If `full_record`, then write
        surrounding recordmarks. Only meaningful if `unformatted`.
        """
        if self.binary:
            if full_record and self.unformatted:
                self.write_recordmark(self.reclen_floats(data.size))

            arr = data
            if self.single_precision:
                if data.itemsize != _SZ_FLOAT:
                    arr = numpy.array(data, dtype=numpy.float32)
            elif data.itemsize != _SZ_DOUBLE:
                arr = numpy.array(data, dtype=numpy.float64)

            if self.big_endian:
                arr.byteswap(True)
            try:
                self.file.write(arr.tostring(order=order))
            finally:
                if self.big_endian:
                    arr.byteswap(True)

            if full_record and self.unformatted:
                self.write_recordmark(self.reclen_floats(data.size))
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
        _item = data.item
        _write = self.file.write

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
            _write(sep)
            _write(fmt % _item(*indices))
            count += 1
            if linecount > 0:
                if count >= linecount:
                    _write('\n')
                    count = 0

            for i in sequence:
                indices[i] += 1
                if indices[i] >= shape[i]:
                    indices[i] = 0
                else:
                    break
            else:
                break

        if count:
            _write('\n')

    def write_recordmark(self, length):
        """ Writes recordmark for `length` record. """
        fmt = '>' if self.big_endian else '<'
        fmt += 'q' if self.recordmark_8 else 'i'
        self.file.write(struct.pack(fmt, length))

