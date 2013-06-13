import sys
import traceback
from io import StringIO
from multiprocessing import Process

import zmq
from zmq.eventloop import ioloop
from zmq.eventloop.zmqstream import ZMQStream


class OutStream(object):
    """ A file-like object that publishes the stream to a 0MQ PUB socket.
        (Borrowed from IPython, but stripped down a bit...)
    """

    def __init__(self, pub_socket, name):
        self.pub_socket = pub_socket
        self.name = name
        self._new_buffer()

    def close(self):
        self.pub_socket = None

    def flush(self):
        if self.pub_socket is None:
            raise ValueError(u'I/O operation on closed file')
        else:
            data = self._buffer.getvalue()
            if data:
                self.pub_socket.send_unicode(data)
                if hasattr(self.pub_socket, 'flush'):
                    # socket itself has flush (presumably ZMQStream)
                    self.pub_socket.flush()
                self._buffer.close()
                self._new_buffer()

    def isatty(self):
        return False

    def next(self):
        raise IOError('Read not supported on a write only stream.')

    def read(self, size=-1):
        raise IOError('Read not supported on a write only stream.')

    def readline(self, size=-1):
        raise IOError('Read not supported on a write only stream.')

    def write(self, string):
        if self.pub_socket is None:
            raise ValueError('I/O operation on closed file')
        else:
            # Make sure that we're handling unicode
            if not isinstance(string, unicode):
                enc = sys.getdefaultencoding()
                string = string.decode(enc, 'replace')
            self._buffer.write(string)
            self.flush()

    def writelines(self, sequence):
        if self.pub_socket is None:
            raise ValueError('I/O operation on closed file')
        else:
            for string in sequence:
                self.write(string)

    def _new_buffer(self):
        self._buffer = StringIO()


class OutStreamRedirector(Process):
    ''' Listen for output on the given port and dump it to a file.
    '''

    def __init__(self, name, addr, filename='sys.stdout'):
        super(OutStreamRedirector, self).__init__()
        self.name = name
        self.addr = addr
        self.filename = filename

    def run(self):
        if self.filename == 'sys.stdout':
            self.file = sys.stdout
        elif self.filename == 'sys.stderr':
            self.file = sys.stderr
        else:
            self.file = open(self.filename, 'a+b')

        ioloop.install()
        loop = ioloop.IOLoop.instance()

        stream = None
        try:
            context = zmq.Context()
            socket = context.socket(zmq.SUB)
            socket.connect(self.addr)
            socket.setsockopt(zmq.SUBSCRIBE, '')
            stream = ZMQStream(socket)
        except Exception, err:
            print self.name, 'error getting outstream:', err
            exc_type, exc_value, exc_traceback = sys.exc_info()
            traceback.print_exception(exc_type, exc_value, exc_traceback)
            traceback.print_tb(exc_traceback, limit=30)
            if stream and not stream.closed():
                stream.close()
        else:
            stream.on_recv(self._write_message)
            loop.start()

    def _write_message(self, msg):
        try:
            print >> self.file, self.name, '>>', msg
        except Exception, err:
            print 'Error writing to file:', err

    def terminate(self):
        super(OutStreamRedirector, self).terminate()
