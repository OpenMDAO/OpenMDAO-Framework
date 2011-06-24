"""
Display ModelCenter AnalysisServer protocol exchanges, based on the
protocol described in:
http://www.phoenix-int.com/~AnalysisServer/commands/index.html
"""

import optparse
import SocketServer
import socket
import sys
import telnetlib

import stream


class _Sniffer(SocketServer.TCPServer):
    """ Display traffic between client & server. """

    def __init__(self, client_host, client_port, server_host, server_port):
        SocketServer.TCPServer.__init__(self, (client_host, client_port),
                                        _Handler)
        self.server_host = server_host
        self.server_port = server_port

    def finish_request(self, request, client_address):
        """ Override to track active clients. """
        host, port = client_address
        print 'Connection from %s:%s' % (host, port)
        try:
            SocketServer.TCPServer.finish_request(self, request, client_address)
        finally:
            print 'Disconnect %s:%s' % (host, port)


class _Handler(SocketServer.BaseRequestHandler):
    """ Handles requests from a single client. """

    def handle(self):
        """ Get request, forward, receive reply, forward. """
        client_stream = stream.Stream(self.request, debug=True)
        raw = False

        server_conn = telnetlib.Telnet(self.server.server_host,
                                       self.server.server_port)
        server_stream = stream.Stream(server_conn.sock, debug=True)

        reply = server_stream.recv_reply()
        client_stream.send_reply(reply)

        req = None
        while req != 'quit':
            try:
                if raw:
                    req, req_id, background = client_stream.recv_request()
                    server_stream.send_request(req, req_id, background)

                    reply_id = None
                    while reply_id != req_id:
                        reply, reply_id, fmt = server_stream.recv_reply()
                        client_stream.send_reply(reply, reply_id, fmt)
                else:
                    request = client_stream.recv_request()
                    server_stream.send_request(request)
                    if request == 'setMode raw':
                        raw = True
                        client_stream.raw = True
                        server_stream.raw = True
                    else:
                        reply = server_stream.recv_reply()
                        client_stream.send_reply(reply)

                sys.stdout.flush()
                sys.stderr.flush()
            except EOFError:
                break


def main():
    """
    Display requests from client_port and replies from server_port.

    Usage: python sniffer.py [--client-host=address][--client-port=number][--server-host=address][--server-port=number]

    --client-host: string
        IPv4 address or hostname. Default is ``socket.gethostname()``.

    --client-port: int
        Client port (default 1834).

    --server-host: string
        IPv4 address or hostname. Default is ``socket.gethostname()``.

    --server-port: int
        Server port (default 1835).
    """
    parser = optparse.OptionParser()
    parser.add_option('--client-host', action='store', type='string',
                      default=socket.gethostname(),
                      help='host for client to connect to')
    parser.add_option('--client-port', action='store', type='int',
                      default=server.DEFAULT_PORT-1,
                      help='port for client to connect to')
    parser.add_option('--server-host', action='store', type='string',
                      default=socket.gethostname(),
                      help='host for server')
    parser.add_option('--server-port', action='store', type='int',
                      default=server.DEFAULT_PORT,
                      help='port for server')

    options, arguments = parser.parse_args()
    if arguments:
        parser.print_help()
        sys.exit(1)

    print 'Sniffing between %s:%d and %s:%d' \
          % (options.client_host, options.client_port,
             options.server_host, options.server_port)
    sniffer = _Sniffer(options.client_host, options.client_port,
                       options.server_host, options.server_port)
    try:
        sniffer.serve_forever()
    except KeyboardInterrupt:
        pass


if __name__ == '__main__':
    main()

