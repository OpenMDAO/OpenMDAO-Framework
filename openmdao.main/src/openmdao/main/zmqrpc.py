
import sys
import traceback
import optparse
import pprint
from functools import partial

import zmq

from zmqcomp import encode, decode

class ZMQ_RPC(object):
    def __init__(self, url, context=None):
        if context is None:
            context = zmq.Context()
            
        # Socket to talk to command sockets
        self._cmdsock = context.socket(zmq.REQ)
        self._cmdsock.connect(url)
        
    def __getattr__(self, name):
        f = partial(self.invoke, name)
        setattr(self, name, f)
        return f
    
    def invoke(self, fname, *args, **kwargs):
        self._cmdsock.send(encode([fname, args, kwargs]))
        return decode(self._cmdsock.recv_multipart()[0])
    
    def close(self):
        self._cmdsock.close()

def main(args):
    parser = optparse.OptionParser()
    parser.add_option("-u", "--url", action="store", type="string", dest='url', 
                      help="url of command socket", default='tcp://*:5555')

    (options, args) = parser.parse_args(args)
    
    proxy = ZMQ_RPC(options.url, zmq.Context())
    
    # now do some remote commands
    proxy.set('comp1.x', 3.0)
    proxy.set('comp2.x', 7.0)
    proxy.set('comp1.x', 9.0)
    proxy.set('comp2.x', 3.0)
    proxy.run()
    
    proxy.close()

if __name__ == "__main__":
    main(sys.argv[1:])
    