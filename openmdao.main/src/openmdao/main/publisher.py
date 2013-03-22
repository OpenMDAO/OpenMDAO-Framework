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

    def __init__(self, context, url, use_stream=True):
        # Socket to talk to pub socket
        sock = context.socket(zmq.PUB)
        sock.bind(url)
        if use_stream:
            self._sender = zmqstream.ZMQStream(sock)
        else:
            self._sender = sock
        self._lock = RLock()

    def publish(self, topic, value, lock=True):
        if Publisher.__enabled:
            try:
                if lock:
                    self._lock.acquire()
                try:
                    value = json.dumps(value)
                except TypeError:
                    value = json.dumps(str(type(value)))
                self._sender.send_multipart([topic.encode('utf-8'), value])
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

    def publish_binary(self, topic, value):
        if Publisher.__enabled:
            with self._lock:
                try:
                    self._sender.send_multipart([topic, value])
                    if hasattr(self._sender, 'flush'):
                        self._sender.flush()
                except Exception, err:
                    strio = StringIO.StringIO()
                    traceback.print_exc(file=strio)
                    print 'Publisher - Error publishing binary message %s: %s, %s' % \
                          (topic, value, strio.getvalue())

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


def publish(topic, msg):
    try:
        Publisher.get_instance().publish(topic, msg)
    except AttributeError:
        if not Publisher.silent:
            raise RuntimeError("Publisher has not been initialized")
