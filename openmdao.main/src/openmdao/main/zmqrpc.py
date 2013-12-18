
import sys
import traceback
import optparse
import pprint
from functools import partial

from zmqcomp import encode, decode

class ZMQ_RPC(object):
    def __init__(self, url, context=None):
        if url.startswith('ws'):
            import websocket
            # use websockets
        else:
            import zmq

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
                      help="url of command socket", default='tcp://localhost:5555')

    (options, args) = parser.parse_args(args)

    proxy = ZMQ_RPC(options.url)

    # now do some remote commands
    proxy.register_published_vars(['paraboloid.x', 'paraboloid.y', 'paraboloid.f_xy'])
    proxy.run()
    print proxy.get('paraboloid.x')
    print proxy.get('paraboloid.y')
    print proxy.get('paraboloid.f_xy')

    proxy.close()

if __name__ == "__main__":
    main(sys.argv[1:])
