import sys
import StringIO
import traceback

from threading import RLock

try:
    import simplejson as json
except ImportError:
    import json

try:
    import zmq
    from zmq.eventloop import zmqstream
except ImportError:
    zmq = None


class Publisher(object):

    __publisher = None
    __enabled = True
    silent = False
    
    _sender_types = [] # classes for sending complex binary reps of objects


    def __init__(self, context, url, use_stream=True):
        # Socket to talk to pub socket
        sock = context.socket(zmq.PUB)
        sock.bind(url)
        if use_stream:
            self._sender = zmqstream.ZMQStream(sock)
        else:
            self._sender = sock
        self._lock = RLock()

    def publish(self, topic, value, lock=True, binary=False):
        if Publisher.__enabled:
            try:
                if lock:
                    self._lock.acquire()
                if binary:
                    if not isinstance(value, bytes):
                        raise TypeError("published binary value must be of type 'bytes'")
                    self._sender.send_multipart([topic.encode('utf-8'), value])
                else:
                    try:
                        msg = json.dumps([topic.encode('utf-8'), value])
                    except TypeError:
                        msg = json.dumps([topic.encode('utf-8'), str(type(value))])
                    self._sender.send_multipart([msg])
                if hasattr(self._sender, 'flush'):
                    self._sender.flush()
            except Exception, err:
                strio = StringIO.StringIO()
                traceback.print_exc(file=strio)
                print 'Publisher - Error publishing message %s: %s, %s' % \
                      (topic, value, strio.getvalue())
            finally:
                if lock:
                    self._lock.release()

    def publish_list(self, items):
        if Publisher.__enabled:
            with self._lock:
                for topic, value in items:
                    self.publish(topic, value, lock=False)

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


def publish(topic, msg, binary=False):
    try:
        Publisher.get_instance().publish(topic, msg, binary=binary)
    except AttributeError:
        if not Publisher.silent:
            raise RuntimeError("Publisher has not been initialized")
        
        
