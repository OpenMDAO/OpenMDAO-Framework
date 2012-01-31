import sys
import traceback
import cPickle as pickle

import zmq
from zmq.eventloop import ioloop, zmqstream

import time
import threading

from openmdao.test.execcomp import ExecComp
from openmdao.main.api import Assembly, set_as_top
from openmdao.main.container import deep_getattr

def msg_split(frames):
    """Take a list of message frames and split it into routing frames and payload
    frames.  
    
    The format of the frames passed in is assumed to be: [rframe1, ..., '', plframe1, ... ]
    It is assumed that there will be zero or more routing frames, and empty frame, and
    one or more payload frames.
    
    Returns a tuple of (routing_frames, payload_frames)
    """
    rframes = []
    for i,f in enumerate(frames):
        if f:
            rframes.append(f)
        else:
            break
    return (rframes, frames[i+1:])

def encode(msg):
    return pickle.dumps(msg, -1)

def decode(msg):
    return pickle.loads(msg)



class ZmqCompWrapper(object):
    def __init__(self, context, comp, rep_url=None, decoder=None, encoder=None):
        self._context = context
        self._comp = comp
        
        #self._worker_cmds = { '\x01': self.on_ready,
                      #'\x03': self.on_reply,
                      #'\x04': self.on_heartbeat,
                      #'\x05': self.on_disconnect,
                      #}

        if decoder is None:
            decoder = decode
        self._decoder = decoder
        
        if encoder is None:
            encoder = encode
        self._encoder = encoder
        
        if rep_url is None:
            rep_url = 'inproc://%s_rep' % comp.comp.get_pathname()
        self._rep_url = rep_url
        repsock = context.socket(zmq.REP)
        repsock.bind(rep_url)
        self._repstream = zmqstream.ZMQStream(repsock)
        self._repstream.on_recv(self.handle_req)
        
    def handle_req(self, msg):
        parts = self._decoder(msg[0])
        print 'received %s' % parts
        try:
            funct = deep_getattr(self._comp, parts[0])
            ret = funct(*parts[1], **parts[2])
        except Exception as err:
            exc_type, exc_value, exc_traceback = sys.exc_info()
            ret = traceback.format_exc(exc_traceback)
        print 'returning %s' % ret
        self._repstream.send_multipart([self._encoder(ret)])
        
    @staticmethod
    def serve(top, context=None, rep_url='tcp://*:5555', pub_url='tcp://*:5556'):
        loop = ioloop.IOLoop.instance()
        if context is None:
            context = zmq.Context()
        actor = ZmqCompWrapper(context, top, rep_url)
        
        # initialize the publisher
        from openmdao.main.publisher import Publisher
        pub = Publisher.init(context, pub_url)
        
        loop.start()
    
if __name__ == '__main__':
    
    asm = set_as_top(Assembly())
    comp1 = asm.add("comp1", ExecComp(exprs=['z=x+y']))
    comp2 = asm.add("comp2", ExecComp(exprs=['z=x+y']))
    comp3 = asm.add("comp3", ExecComp(exprs=['z=x+y']))
    
    asm.connect("comp1.z", "comp3.x")
    asm.connect("comp2.z", "comp3.y")
    
    asm.driver.workflow.add(['comp1','comp2','comp3'])
    
    comp1.register_published_vars(["x","y","z"])
    comp2.register_published_vars(["x","y","z"])
    comp3.register_published_vars(["x","y","z"])
    
    ZmqCompWrapper.serve(asm)
    
