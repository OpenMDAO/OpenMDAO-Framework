import sys
import time
from io import StringIO

# largely borrowed from IPython

class OutStream(object):
    """A file like object that publishes the stream to a 0MQ PUB socket."""

    # The time interval between automatic flushes, in seconds.
    flush_interval = 0.05
    topic=None

    def __init__(self, pub_socket, name):
        self.pub_socket = pub_socket
        self.name = name
        self.parent_header = {}
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
            current_time = time.time()
            if self._start <= 0:
                self._start = current_time
            elif current_time - self._start > self.flush_interval:
                self.flush()

    def writelines(self, sequence):
        if self.pub_socket is None:
            raise ValueError('I/O operation on closed file')
        else:
            for string in sequence:
                self.write(string)

    def _new_buffer(self):
        self._buffer = StringIO()
        self._start = -1

        

def try_it():
    import zmq
    from zmq.eventloop import ioloop
    from zmq.eventloop.zmqstream import ZMQStream
    ioloop.install()
    
    context = zmq.Context()
    loop = ioloop.IOLoop.instance()

    out_url = "tcp://127.0.0.1:5656"
    
    # set up publisher
    try:
        cout_socket = context.socket(zmq.PUB)
        print "binding output to %s" % out_url
        cout_socket.bind(out_url)
        sys.stdout = OutStream(cout_socket,'cout')
    except Exception, err:
        print err

    # set up subscriber
    def _write_message(msg):
        print  >> sys.stderr, 'output>>',msg
        
    stream = None
    try:
        socket = context.socket(zmq.SUB)
        print  >> sys.stderr, 'listening for output on',out_url
        socket.connect(out_url)
        socket.setsockopt(zmq.SUBSCRIBE, '')
        stream = ZMQStream(socket)
    except Exception, err:
        print  >> sys.stderr, '    error getting outstream:',err
        if stream and not stream.closed():
            stream.close()
    else:
        stream.on_recv(_write_message)
    
    
    def say_hello():
        print "hello"

    timer = ioloop.PeriodicCallback(say_hello, 1000)
    timer.start()
    try:
        loop.start()
    except KeyboardInterrupt:
        print ' Interrupted'       
        
if __name__ == '__main__':
    try_it()