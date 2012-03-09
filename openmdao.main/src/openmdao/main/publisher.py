
import sys
import pprint

from threading import RLock

#import json
import pickle

try:
    import zmq
    from zmq.eventloop import ioloop, zmqstream
except ImportError:
    zmq = None
    

class Publisher(object):

    __publisher = None

    def __init__(self, context, url, use_stream=True):
        # Socket to talk to pub socket
        sock = context.socket(zmq.PUB)
        sock.bind(url)
        if use_stream:
            self._sender = zmqstream.ZMQStream(sock)
        else:
            self._sender = sock
        self._lock = RLock()
    
    def publish(self, topic, value):
        with self._lock:
            #print 'publishing %s' % topic
            self._sender.send_multipart([topic, pickle.dumps(value, -1)])
    
    def publish_list(self, items):
        with self._lock:
            for topic, value in items:
                #print 'publishing list %s' % topic
                self._sender.send_multipart([topic, pickle.dumps(value, -1)])
                
    @staticmethod
    def get_instance():
        return Publisher.__publisher
    
    @staticmethod
    def init(context, url, use_stream=True):
        if Publisher.__publisher is not None:
            raise RuntimeError("publisher already exists")
        Publisher.__publisher = Publisher(context, url, use_stream)
        return Publisher.__publisher

    