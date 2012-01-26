
import pickle

import zmq
from zmq.eventloop import ioloop, zmqstream

import time
import threading

from openmdao.test.execcomp import ExecComp

class ActorCompWrapper(object):
    def __init__(self, context, comp, url=None):
        if url is None:
            url = 'inproc://%s' % name
        self._context = context
        self._comp = comp
        
        self._repsock = ctx.socket(zmq.REP)
        self._repsock.bind(url)
        self._repstream = zmqstream.ZMQStream(self._repsock)
        self._repstream.on_recv(self.handle_req)
        
        self._pubsock = ctx.socket(zmq.PUB)
        self._pubsock.bind('inproc://%s_pub_' % comp.get_pathname())
        self._pubstream = zmqstream.ZMQStream(self._pubsock)
        
    def handle_req(self, msg):
        parts = pickle.loads(msg[0])
        print 'received %s' % parts
        funct = getattr(self._comp, parts[0])
        ret = funct(*parts[1], **parts[2])
        self._repstream.send_pyobj(ret)
    
    def execute(self):
        self._pubsock.send_multipart("[('a',1.0), ('b',2.0)]")
    
if __name__ == '__main__':
    loop = ioloop.IOLoop.instance()
    
    ctx = zmq.Context()
    
    comp =ExecComp(exprs=['z=x+y'])
    comp.name = 'foo'
    ActorCompWrapper(ctx, comp, url='tcp://127.0.0.1:5555')
    
    loop.start()
    
