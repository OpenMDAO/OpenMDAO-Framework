
from threading import RLock

import jsonpickle
#import pickle

try:
    import zmq
    from zmq.eventloop import zmqstream
except ImportError:
    zmq = None


class Publisher(object):

    __publisher = None
    __enabled = True

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
        if Publisher.__enabled:
            if isinstance(topic, unicode):
                # in case someone snuck in a unicode name
                topic = topic.encode('utf-8', errors='backslashreplace')
            with self._lock:
                try:
                    self._sender.send_multipart([
                        topic,
                        jsonpickle.encode(value)
                    ])
                    if hasattr(self._sender, 'flush'):
                        self._sender.flush()
                except Exception, err:
                    print 'Publisher - Error publishing message %s: %s, %s' % \
                          (topic, value, err)

    def publish_list(self, items):
        if Publisher.__enabled:
            with self._lock:
                try:
                    for topic, value in items:
                        if isinstance(topic, unicode):
                            # in case someone snuck in a unicode name
                            topic = topic.encode('utf-8',
                                                 errors='backslashreplace')
                        self._sender.send_multipart([
                            topic,
                            jsonpickle.encode(value)
                        ])
                    if hasattr(self._sender, 'flush'):
                        self._sender.flush()
                except Exception, err:
                    print 'Publisher - Error publishing list %s, %s' % \
                          (topic, err)

    @staticmethod
    def get_instance():
        return Publisher.__publisher

    @staticmethod
    def init(context, url, use_stream=True):
        if Publisher.__publisher is not None:
            raise RuntimeError("publisher already exists")
        Publisher.__publisher = Publisher(context, url, use_stream)
        return Publisher.__publisher

    @staticmethod
    def enable():
        Publisher.__enabled = True

    @staticmethod
    def disable():
        Publisher.__enabled = False
