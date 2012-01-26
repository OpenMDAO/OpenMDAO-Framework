
import pickle

import zmq
from zmq.eventloop import ioloop, zmqstream

import time
import threading

from openmdao.test.execcomp import ExecComp

class ActorCompWrapper(object):
    def __init__(self, context, comp, rep_url=None, pub_url=None):
        self._context = context
        self._comp = comp
        
        if rep_url is None:
            rep_url = 'inproc://%s' % comp.comp.get_pathname()
        self._rep_url = rep_url
        self._repsock = ctx.socket(zmq.REP)
        self._repsock.bind(rep_url)
        self._repstream = zmqstream.ZMQStream(self._repsock)
        self._repstream.on_recv(self.handle_req)
        
        if pub_url is None:
            pub_url = 'inproc://%s_pub_' % comp.get_pathname()
        self._pub_url = pub_url
        self._pubsock = ctx.socket(zmq.PUB)
        self._pubsock.bind(pub_url)
        self._pubstream = zmqstream.ZMQStream(self._pubsock)
        
    def handle_req(self, msg):
        parts = pickle.loads(msg[0])
        print 'received %s' % parts
        funct = getattr(self._comp, parts[0])
        ret = funct(*parts[1], **parts[2])
        if parts[0] == 'run' or parts[0] == 'execute':
            pname = self._comp.get_pathname()
            for var in self._comp._publish_vars:
                print 'publishing var %s' % var
                self._pubstream.send_multipart(['.'.join([pname,var]), 
                                                pickle.dumps(getattr(self._comp, var), -1)])
        self._repstream.send_pyobj(ret)
    
    
if __name__ == '__main__':
    loop = ioloop.IOLoop.instance()
    
    ctx = zmq.Context()
    
    comp =ExecComp(exprs=['z=x+y'])
    comp.name = 'foo'
    comp.x = 1
    comp.y = 2
    ActorCompWrapper(ctx, comp, rep_url='tcp://127.0.0.1:5555', 
                     pub_url='tcp://127.0.0.1:5556')
    
    loop.start()
    
