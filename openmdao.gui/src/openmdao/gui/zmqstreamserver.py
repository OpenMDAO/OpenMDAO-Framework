import sys
import os
import traceback
import subprocess

from optparse import OptionParser
    
import zmq
from zmq.eventloop import ioloop
from zmq.eventloop.zmqstream import ZMQStream

from tornado import httpserver, web, websocket

debug = True
NAME_SIZE = 256  # this must agree with NAME_SIZE in Model.js

def DEBUG(msg):
    if debug:
        print '<<<' + str(os.getpid()) + '>>> ZMQStreamServer --', msg
        sys.stdout.flush()


def make_unicode(content):
    if type(content) == str:
        # Ignore errors even if the string is not proper UTF-8 or has
        # broken marker bytes.
        # Python built-in function unicode() can do this.
        content = unicode(content, "utf-8", errors="ignore")
    else:
        # Assume the content object has proper __unicode__() method
        content = unicode(content)
    return content


class ZMQStreamHandler(websocket.WebSocketHandler):
    ''' A handler that forwards output from a ZMQStream to a WebSocket.
    '''
    
    def initialize(self, addr):
        self.addr = addr

    def open(self):
        stream = None
        try:
            context = zmq.Context()
            socket = context.socket(zmq.SUB)
            socket.connect(self.addr)
            socket.setsockopt(zmq.SUBSCRIBE, '')
            stream = ZMQStream(socket)
        except Exception, err:
            exc_type, exc_value, exc_traceback = sys.exc_info()
            print 'ZMQStreamHandler ERROR getting ZMQ stream:', err
            traceback.print_exception(exc_type, exc_value, exc_traceback)
            if stream and not stream.closed():
                stream.close()
        else:
            stream.on_recv(self._write_message)

    def _write_message(self, message):
        if len(message) == 1:  # assume message[0] is some json object
            binary = False
            try:
                message = message[0]
            except Exception as err:
                exc_type, exc_value, exc_traceback = sys.exc_info
                print 'ZMQStreamHandler ERROR converting message to unicode:', str(message), err
                traceback.print_exception(exc_type, exc_value, exc_traceback)
                return

        elif len(message) == 2:  # it's a msg of the form [topic, binary_value]
            binary = True
            try:
                if len(message[0]) > NAME_SIZE:
                    raise RuntimeError("topic field of message is longer than %d characters" % NAME_SIZE)
                if not isinstance(message[1], bytes):
                    raise TypeError("message value must be of type 'bytes', not type '%s'" %
                                    str(type(message[1])))
                #padded = bytes(message[0])+(NAME_SIZE-len(message[0]))*b'\0'  # 0 padded object name in bytes
                message = message[0].ljust(NAME_SIZE, '\0') + message[1]  # FIXME: message is copied here
            except Exception as err:
                exc_type, exc_value, exc_traceback = sys.exc_info()
                print 'ZMQStreamHandler ERROR:', str(message), err
                traceback.print_exception(exc_type, exc_value, exc_traceback)
                return

        # write message to websocket
        self.write_message(message, binary=binary)

    def on_message(self, message):
        pass

    def on_close(self):
        pass


class ZMQStreamApp(web.Application):
    ''' A web application that serves a ZMQStream over a WebSocket.
    '''

    def __init__(self, zmqstream_addr, websocket_url):
        handlers = [
            (websocket_url, ZMQStreamHandler, dict(addr=zmqstream_addr))
        ]
        settings = {
            'login_url': '/login',
            'debug': True,
        }
        super(ZMQStreamApp, self).__init__(handlers, **settings)


class ZMQStreamServer(object):
    ''' Runs an http server that serves a ZMQStream over a WebSocket.
    '''

    def __init__(self, options):
        self.options = options
        self.web_app = ZMQStreamApp(options.addr, options.url)
        self.http_server = httpserver.HTTPServer(self.web_app)

    def serve(self):
        ''' Start server listening on port & start the ioloop.
        '''
        DEBUG('serve %s' % self.options.port)
        try:
            if (self.options.external):
                self.http_server.listen(self.options.port)
            else:
                self.http_server.listen(self.options.port, address='localhost')
        except Exception as exc:
            print '<<<%s>>> ZMQStreamServer -- listen on %s failed: %s' \
                  % (os.getpid(), self.options.port, exc)
            sys.exit(1)

        try:
            ioloop.IOLoop.instance().start()
        except KeyboardInterrupt:
            DEBUG('interrupt received, shutting down.')

    @staticmethod
    def get_options_parser():
        ''' Create a parser for command-line arguments.
        '''
        parser = OptionParser()
        parser.add_option("-z", "--zmqstream",
                          dest="addr", default=0,
                          help="the address of the zmqstream")
        parser.add_option("-p", "--port",
                          dest="port", type="int", default=0,
                          help="the port to run websocket server on")
        parser.add_option("-u", "--url",
                          dest="url",
                          help="the url to expose for the websocket")
        parser.add_option("-x", "--external",
                          dest="external", action="store_true",
                          help="allow access to the server from external clients")
        return parser

    @staticmethod
    def spawn_process(zmq_url, ws_port, ws_url='/', external=False):
        ''' Run zmqstreamserver in its own process, mapping a zmq
            stream to a websocket.

            args:
            zmq_url     the url of the ZMQStream
            ws_port     the port to serve the WebSocket on
            ws_url      the url to map to the WebSocket
        '''
        file_path = os.path.abspath(__file__)
        cmd = ['python', file_path,
               '-z', str(zmq_url),
               '-p', str(ws_port),
               '-u', str(ws_url)]
        if external:
            cmd.append('-x')
        return subprocess.Popen(cmd)


def main():
    ''' Process command line arguments, create server, and start it up.
    '''
    # make sure to install zmq ioloop before creating any tornado objects
    ioloop.install()

    # create the server and kick it off
    parser = ZMQStreamServer.get_options_parser()
    (options, args) = parser.parse_args()
    server = ZMQStreamServer(options)
    server.serve()


if __name__ == '__main__':
    # dont run main() if this is a forked windows process
    if sys.modules['__main__'].__file__ == __file__:
        main()
