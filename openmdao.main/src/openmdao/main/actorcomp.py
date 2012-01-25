
import zmq
from zmq.eventloop import ioloop, zmqstream

import time
import threading

class Actor(object):
    def __init__(self, context, name, url=None, sleep=1):
        if url is None:
            url = 'inproc://%s' % name
        self._context = context
        self._name = name
        self._sleep = sleep
        
        self._repsock = ctx.socket(zmq.REP)
        self._repsock.bind(url)
        self._repstream = zmqstream.ZMQStream(self._repsock)
        self._repstream.on_recv(self.handle_req)
        
        self._pubsock = ctx.socket(zmq.PUB)
        self._pubsock.bind('inproc://%s_pub_' % name)
        self._pubstream = zmqstream.ZMQStream(self._pubsock)
        
    def handle_req(self, msg):
        print 'received %s' % msg
    
    def execute(self):
        self._pubsock.send_multipart("[('a',1.0), ('b',2.0)]")
    
if __name__ == '__main__':
    ioloop.install()
    
    loop = ioloop.IOLoop.instance()
    
    ctx = zmq.Context()
    
    Actor(ctx, 'foo', 3)
    Actor(ctx, 'bar', 0.5)
    Actor(ctx, 'baz')
    
    loop.start()
    
