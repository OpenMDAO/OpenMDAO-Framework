import sys, os, traceback
import time
from io import StringIO
from multiprocessing import Process

import zmq
from zmq.eventloop import ioloop
from zmq.eventloop.zmqstream import ZMQStream

from openmdao.util.network import get_unused_ip_port

from random import randrange

debug = True
def DEBUG(msg):
    if debug:
        print '<<<'+str(os.getpid())+'>>> OutStreamServer --',msg

class OutStream(object):
    """ A file like object that publishes the stream to a 0MQ PUB socket.
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
    ''' listen for output on the given port and dump it to a file
    '''
    def __init__(self,name,addr,filename='sys.stdout'):
        super(OutStreamRedirector, self).__init__()
        DEBUG(name+'..............')
        print addr,'-->',filename
        self.name = name
        self.addr = addr
        self.filename = filename

    def run(self):
        if self.filename == 'sys.stdout':
            self.file = sys.stdout
        elif self.filename == 'sys.stderr':
            self.file = sys.stderr
        else:
            self.file = open(self.filename,'a+b')
            print 'set file to',self.file
            
        ioloop.install()
        loop = ioloop.IOLoop.instance()
        
        stream = None
        try:
            context = zmq.Context()
            socket = context.socket(zmq.SUB)
            print self.name,'listening for output on',self.addr
            socket.connect(self.addr)
            socket.setsockopt(zmq.SUBSCRIBE, '')
            stream = ZMQStream(socket)
        except Exception, err:
            print self.name,'error getting outstream:',err
            exc_type, exc_value, exc_traceback = sys.exc_info()
            traceback.print_exception(exc_type, exc_value, exc_traceback)
            traceback.print_tb(exc_traceback, limit=30)   
            if stream and not stream.closed():
                stream.close()
        else:
            stream.on_recv(self._write_message)
            loop.start()
    
    def _write_message(self,msg):
        try:
            print >> self.file, self.name,'>>',msg
        except Exception, err:
            print 'Error writing to file:',err

    def terminate(self):
        DEBUG(self.name+'shutting down .........')
        super(OutStreamRedirector, self).terminate()


def try_outstream():
    ioloop.install()    
    loop = ioloop.IOLoop.instance()
    context = zmq.Context()

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

def try_outstream_redirector():
    DEBUG('try_outstream_redirector ..............')
    ioloop.install()
    loop = ioloop.IOLoop.instance()
    context = zmq.Context()

    out_url = "tcp://127.0.0.1:%i" % get_unused_ip_port()
    
    # set up subscriber
    sub = OutStreamRedirector('TestRedirector',out_url,'testfil.out')
    print 'TestRedirector has been created...'
    sub.start()
    print 'TestRedirector has been started...'
    
    # set up publisher
    sysout = sys.stdout
    try:
        cout_socket = context.socket(zmq.PUB)
        print "binding output to %s" % out_url
        cout_socket.bind(out_url)
        sys.stdout = OutStream(cout_socket,'cout')
    except Exception, err:
        print err
    
    # create timer to feed the publisher
    names =  ['billy', 'tommy', 'johnny', 'susie', 'mary', 'bobby']
    def say_hello():
        print 'hello',names[randrange(len(names))]
    hello_timer = ioloop.PeriodicCallback(say_hello, 1000)
    hello_timer.start()

    # create timer to shut down after 10 seconds
    def all_done():
        sys.stdout = sysout
        loop.stop()
        sub.terminate()
        print '\nThanks for playing!'
    done_timer = ioloop.DelayedCallback(all_done, 10000)
    done_timer.start()

    # kick it off
    try:
        loop.start()
    except KeyboardInterrupt:
        print ' Interrupted'
    

if __name__ == '__main__':
    #try_outstream()
    try_outstream_redirector()