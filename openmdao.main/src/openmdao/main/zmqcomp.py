import sys
import traceback
import cPickle as pickle

import time
import threading

import optparse

import zmq
from zmq.eventloop import ioloop, zmqstream

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
    def serve(top, context=None, wsroute=None, port=8888,
              rep_url='tcp://*:5555', pub_url='inproc:_pub_'):

        if wsroute:
            import tornado
            from tornado import web, websocket
            
            class CompWebSocket(websocket.WebSocketHandler):
                def initialize(self, context=None, rep_url=None, pub_url=None):
                    repsock = context.socket(zmq.REP)
                    repsock.connect(rep_url)
                    self._repstream = zmqstream.ZMQStream(repsock)
                    subsock = context.socket(zmq.SUB)
                    subsock.connect(url)
                    substream = zmqstream.ZMQStream(subsock)
                    substream.setsockopt(zmq.SUBSCRIBE, '')
                    substream.on_recv(self.on_sub)
                    
                def open(self):
                    print "WebSocket opened"
            
                def on_message(self, message):
                    # received an REQ message
                    self._repstream.send(message)
            
                def on_close(self):
                    print "WebSocket closed"
                    
                def on_sub(self, msg):
                    print "on_sub: %s" % msg
                    self.write_message(msg)
                
            application = web.Application([(wsroute, CompWebSocket, {'context':context,
                                                                     'rep_url':rep_url,
                                                                     'pub_url':pub_url})])
            application.listen(port)

        loop = ioloop.IOLoop.instance()
        if context is None:
            context = zmq.Context()
        actor = ZmqCompWrapper(context, top, rep_url)
        
        # initialize the publisher
        from openmdao.main.publisher import Publisher
        pub = Publisher.init(context, pub_url)
        
        try:
            loop.start()
        except KeyboardInterrupt:
            print ' Interrupted'
    
def main(args=None):
    if args is None:
        args = sys.argv[1:]

    parser = optparse.OptionParser()
    parser.add_option("-u", "--url", action="store", type="string", dest='url', 
                      help="url of command socket", default='tcp://*:5555')
    parser.add_option("-c", "--class", action="store", type="string", dest='classpath', 
                      help="module path to component class")
    parser.add_option("-p", "--publish", action="append", type="string", dest='published', 
                      help="specify a variable to publish", default=[])
    parser.add_option("-w", "--websocket", action="store", type="string", dest='wsroute', 
                      help="route to websocket")

    (options, args) = parser.parse_args(args)
    
    if options.classpath is None:
        print "you must specify the module path to a class or factory function"
        parser.print_help()
        sys.exit(-1)
        
    if options.wsroute:
        ioloop.install() # must call this before importing any tornado stuff

    parts = options.classpath.split('.')
    modpath = '.'.join(parts[:-1])
    __import__(modpath)
    
    try:
        mod = sys.modules[modpath]
        ctor = getattr(mod, parts[-1])
    except KeyError, AttributeError:
        print "can't locate %s" % options.classpath
        sys.exit(-1)
        
    top = set_as_top(ctor())
    top.register_published_vars(options.published)
    
    ZmqCompWrapper.serve(top, rep_url=options.url, wsroute=options.wsroute)
    

if __name__ == '__main__':
    main()
    
