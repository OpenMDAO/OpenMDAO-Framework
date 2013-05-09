import sys
import os
import traceback
import cPickle as pickle
import StringIO

import time
import threading
import logging

import optparse

import zmq
from zmq.eventloop import ioloop, zmqstream

from openmdao.main.api import set_as_top
from openmdao.main.container import deep_getattr
from openmdao.util.debug import DEBUG, debug


def msg_split(frames):
    """Take a list of message frames and split it into routing frames and payload
    frames.  
    
    The format of the frames passed in is assumed to be: [rframe1, ..., '', plframe1, ... ]
    It is assumed that there will be zero or more routing frames, an empty frame, and
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
        if debug: 
            DEBUG('received %s' % parts)
        try:
            funct = deep_getattr(self._comp, parts[0])
            ret = funct(*parts[1], **parts[2])
        except Exception:
            ret = traceback.format_exc()
            logging.exception('handle_req %s %s %s',
                              parts[0], parts[1], parts[2])
        if debug:
            DEBUG('returning %s' % ret)
        try:
            self._repstream.send_multipart([self._encoder(ret)])
        except Exception:
            print "Error handling request: %s: %s" % (msg, traceback.format_exc())
        
    @staticmethod
    def serve(top, context=None, wspub=None, wscmd=None, port=8888,
              rep_url='tcp://*:5555', pub_url='inproc://_pub_'):

        if context is None:
            context = zmq.Context()

        loop = ioloop.IOLoop.instance()
        ZmqCompWrapper(context, top, rep_url)
        
        # initialize the publisher
        from openmdao.main.publisher import Publisher
        Publisher.init(context, pub_url)
            
        if wspub or wscmd:
            from openmdao.main.zmqws import CmdWebSocketHandler, PubWebSocketHandler
            from tornado import web
            handlers = []
            if wspub:
                handlers.append((wspub, PubWebSocketHandler, {'context':context,
                                                              'pub_url':pub_url}))
            if wscmd:
                handlers.append((wscmd, CmdWebSocketHandler, {'context':context,
                                                              'rep_url':rep_url}))
            application = web.Application(handlers)
            application.listen(port)

        try:
            loop.start()
        except KeyboardInterrupt:
            print ' Interrupted'
    
def main(args=None):
    if args is None:
        args = sys.argv[1:]

    parser = optparse.OptionParser()
    parser.add_option("--repurl", action="store", type="string", dest='repurl', 
                      help="url of REP socket", default='tcp://*:5555')
    parser.add_option("--puburl", action="store", type="string", dest='puburl', 
                      help="url of PUB socket", default='tcp://*:5556')
    parser.add_option("-c", "--class", action="store", type="string", dest='classpath', 
                      help="module path to class of top level component")
    parser.add_option("-p", "--publish", action="append", type="string", dest='published', 
                      help="specify a variable to publish", default=[])
    parser.add_option("--wspub", action="store", type="string", dest='wspub', 
                      help="route to pub websocket")
    parser.add_option("--wscmd", action="store", type="string", dest='wscmd', 
                      help="route to cmd websocket")

    (options, args) = parser.parse_args(args)
    
    if options.classpath is None:
        print "you must specify the module path to a class or factory function"
        parser.print_help()
        sys.exit(-1)
        
    if options.wspub or options.wscmd:
        ioloop.install() # must call this before importing any tornado stuff

    parts = options.classpath.split('.')
    modpath = '.'.join(parts[:-1])
    __import__(modpath)
    
    try:
        mod = sys.modules[modpath]
        ctor = getattr(mod, parts[-1])
    except (KeyError, AttributeError):
        print "can't locate %s" % options.classpath
        sys.exit(-1)
        
    top = set_as_top(ctor())
    top.register_published_vars(options.published)
    
    ZmqCompWrapper.serve(top, rep_url=options.repurl, pub_url=options.puburl,
                         wspub=options.wspub, wscmd=options.wscmd)
    

if __name__ == '__main__':
    main()
    
