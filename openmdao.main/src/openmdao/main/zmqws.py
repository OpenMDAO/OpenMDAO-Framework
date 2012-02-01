

import tornado
from tornado import web, websocket

class PubWebSocketHandler(websocket.WebSocketHandler):
    def initialize(self, context, pub_url, topic=''):
        self._pub_url = pub_url
        self._substream = None
        self._topic = ''
        
    def open(self):
        print "WebSocket opened"
        subsock = context.socket(zmq.SUB)
        subsock.connect(self._pub_url)
        self._substream = zmqstream.ZMQStream(subsock)
        
        if isinstance(self._topic, basestring):
            topics = [self._topic]
        else:
            topics = self._topic
        for topic in topics:
            self._substream.setsockopt(zmq.SUBSCRIBE, topic)
        self._substream.on_recv(self.on_sub)

    def on_close(self):
        print "WebSocket closed"
        if self._substream:
            self._substream.close()
        
    def on_message(self, message):
        raise ValueError("PUB websocket received an incoming message: %s" % message)

    def on_sub(self, msg):
        print "on_sub: %s" % msg
        self.write_message(msg)


class CompWebSocketHandler(PubWebSocketHandler):
    def initialize(self, context, rep_url, pub_url, topic=''):
        super(CompWebSocketHandler, self).initialize(context, pub_url, topic)
        self._rep_url = rep_url
        self._repstream = None
        
    def open(self):
        print "WebSocket opened"
        super(CompWebSocketHandler, self).open()
        repsock = context.socket(zmq.REP)
        repsock.connect(self._rep_url)
        self._repstream = zmqstream.ZMQStream(repsock)

    def on_close(self):
        print "WebSocket closed"
        if self._repstream:
            self._repstream.close()
        super(CompWebSocketHandler, self).on_close()
        
    def on_message(self, message):
        # received an REQ message
        self._repstream.send(message)
