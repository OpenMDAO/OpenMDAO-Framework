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

from pkg_resources import working_set
from openmdao.util.log import logger
from openmdao.main.variable import json_default
from pyV3D import WV_Wrapper

_lock = RLock()
_binpub_types = None  # classes for sending complex binary reps of objects
_binpubs = {}  # [count, sender] corresponding to specific topics


class Pub_WV_Wrapper(WV_Wrapper):
    """A wrapper for the wv library that is used by a Sender to
    send updates to the Publisher.
    """

    def __init__(self, name):
        super(Pub_WV_Wrapper, self).__init__()
        self.objname = name

    def send(self, first=False):
        self.prepare_for_sends()

        if first:
            self.send_GPrim(self, 1, self.send_binary_data)  # send init packet
            self.send_GPrim(self, -1, self.send_binary_data)  # send initial suite of GPrims
        else:
            self.send_GPrim(self, -1, self.send_binary_data)  # send initial suite of GPrims

        self.finish_sends()

    def send_binary_data(self, wsi, buf, ibuf):
        """This is called multiple times during the sending of a
        set of graphics primitives.
        """
        try:
            publish(self.objname, buf, binary=True)
        except Exception:
            logger.error(traceback.format_exc())
            return -1
        return 0


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

    def publish(self, topic, value, lock=True, binary=False):
        global _binpubs
        if Publisher.__enabled:
            try:
                if lock:
                    _lock.acquire()
                if binary:
                    if not isinstance(value, bytes):
                        raise TypeError("published binary value must be of type 'bytes'")
                    logger.debug("sending binary value for topic %s" % topic)
                    self._sender.send_multipart([topic.encode('utf-8'), value])
                elif topic in _binpubs:
                    # if a binary publisher exists for this topic, use that to
                    # publish the value. It will call publish again (possibly multiple times)
                    # with binary=True
                    logger.debug("sending value via binpub for topic %s" % topic)
                    try:
                        _binpubs[topic][1].send(value)
                    except Exception:
                        logger.error("ERROR: %s" % traceback.format_exc())
                else:
                    msg = json.dumps([topic.encode('utf-8'), value], default=json_default)
                    self._sender.send_multipart([msg])
                if hasattr(self._sender, 'flush'):
                    self._sender.flush()
            except Exception:
                print 'Publisher - Error publishing message %s: %s, %s' % \
                      (topic, value, traceback.format_exc())
            finally:
                if lock:
                    _lock.release()

    def publish_list(self, items):
        if Publisher.__enabled:
            with _lock:
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

    @staticmethod
    def register(topic, obj):
        """Associates a given topic with a binary publisher based on the corresponding object type.
        If no binpub type exists for that object type, nothing happens.
        """
        global _binpubs, _binpub_types
        sender = None
        if _binpub_types is None:
            load_binpubs()

        with _lock:
            if topic in _binpubs:
                logger.debug("found topic %s in _binpubs" % topic)
                _binpubs[topic][0] += 1
            else:
                # see if a sender is registered for this object type
                for sender_type in _binpub_types:
                    if sender_type.supports(obj):
                        logger.debug("creating a sender for topic: %s" % topic)
                        try:
                            sender = sender_type(Pub_WV_Wrapper(topic))
                        except Exception:
                            logger.error(traceback.format_exc())
                        _binpubs[topic] = [1, sender]
                        break

        if sender is not None:
            sender.send(obj, first=True)

    @staticmethod
    def unregister(topic):
        """Removes an association between a 'sender' and a topic."""
        global _binpubs
        with _lock:
            if topic in _binpubs:
                logger.debug("Publisher unregistering topic %s" % topic)
                if _binpubs[topic][0] <= 1:
                    del _binpubs[topic]
                else:
                    _binpubs[topic][0] -= 1


def publish(topic, msg, binary=False):
    try:
        Publisher.get_instance().publish(topic, msg, binary=binary)
    except AttributeError:
        if not Publisher.silent:
            raise RuntimeError("Publisher has not been initialized")


def load_binpubs():
    """Loads all binpubs entry points."""
    global _binpub_types
    logger.debug("loading binpubs")

    if _binpub_types is None:
        _binpub_types = []

        # find all of the installed binpubs
        for ep in working_set.iter_entry_points('openmdao.binpub'):
            try:
                klass = ep.load()
            except Exception as err:
                logger.error("Entry point %s failed to load: %s" % (str(ep).split()[0], err))
            else:
                logger.debug("adding binpub entry point: %s" % str(ep).split()[0])
                with _lock:
                    _binpub_types.append(klass)
