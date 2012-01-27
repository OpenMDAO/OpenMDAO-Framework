
import sys
import traceback
import optparse
import pprint
from functools import partial

import zmq

class ZMQ_RPC(object):
    def __init__(self, url, context=None):
        if context is None:
            context = zmq.Context()
            
        # Socket to talk to command sockets
        self._cmdsock = context.socket(zmq.REQ)
        self._cmdsock.connect(url)
        
    def __getattr__(self, name):
        return partial(self.invoke, name)
    
    def invoke(self, fname, *args, **kwargs):
        self._cmdsock.send_pyobj([fname, args, kwargs])
        return self._cmdsock.recv_pyobj()
    
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
    