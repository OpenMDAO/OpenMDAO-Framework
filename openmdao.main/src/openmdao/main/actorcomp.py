import sys
import traceback
import pickle

import zmq
from zmq.eventloop import ioloop, zmqstream

import time
import threading

from openmdao.test.execcomp import ExecComp
from openmdao.main.api import Assembly, set_as_top

class ActorCompWrapper(object):
    def __init__(self, context, comp, rep_url=None):
        self._context = context
        self._comp = comp
        
        if rep_url is None:
            rep_url = 'inproc://%s_rep' % comp.comp.get_pathname()
        self._rep_url = rep_url
        repsock = ctx.socket(zmq.REP)
        repsock.bind(rep_url)
        self._repstream = zmqstream.ZMQStream(repsock)
        self._repstream.on_recv(self.handle_req)
        
    def handle_req(self, msg):
        parts = pickle.loads(msg[0])
        print 'received %s' % parts
        for i,p in enumerate(parts):
            if p=='None':
                parts[i] = None
        try:
            funct = getattr(self._comp, parts[0])
            ret = funct(*parts[1], **parts[2])
        except Exception as err:
            exc_type, exc_value, exc_traceback = sys.exc_info()
            ret = traceback.format_exc(exc_traceback)
        print 'ran funct'
        self._repstream.send_pyobj(ret)
    
if __name__ == '__main__':
    loop = ioloop.IOLoop.instance()
    
    ctx = zmq.Context()
    
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
    
    ActorCompWrapper(ctx, asm, 
                     rep_url='tcp://*:5555') #,pub_url='tcp://*:5556')
    
    # initialize the publisher
    from openmdao.main.publisher import init
    pub = init(ctx, 'tcp://*:5556')
    
    loop.start()
    
