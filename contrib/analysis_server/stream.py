import re
import socket
import sys


class Stream(object):
    """
    Stream abstraction on top of socket, supporting AnalysisServer protocol.
    Inspired by telnetlib, but drops 'cooking' of data.
    `sock` is the socket to wrap. If `debug` is True then message data is
    shown on stdout.
    """

    def __init__(self, sock, debug=False):
        if debug:  # pragma no cover
            print 'Stream', sock.getsockname(), sock.getpeername()
        self._sock = sock
        self._peer = '%s:%s' % sock.getpeername()
        self._recv_buffer = ''
        self._raw = False
        self._debug = debug

        # _expect() patterns, compiled and retained as needed.
        self._request_id = ['^setID [0-9]*\n']
        self._request_len = ['^bg\n', '^cmdLen=[0-9]*\n']
        self._reply_id = ['^[0-9]*\r\n']
        self._reply_len = ['^[0-9]*\r\n']
        self._formats = ['^format: string\r\n',
                         '^format: error\r\n',
                         '^format: PHXIcon\r\n']
        self._cooked_request = ['^.*\n']
        self._cooked_reply = ['\r\n>$', '^>$']

    def _get_raw(self):
        """ Return True if in 'raw' mode. """
        return self._raw

    def _set_raw(self, value):
        """ Set 'raw' mode. """
        if not self._raw and value:
            self._raw = True
        else:
            raise ValueError("Can only transition from 'cooked' to 'raw'")

    raw = property(_get_raw, _set_raw, doc="True if in 'raw' mode")

    def send_request(self, request, request_id=None, background=False):
        """
        Send `request` to server.
        If in 'raw' mode use `request_id` and `background`.
        """
        if self._raw:
            if self._debug:  # pragma no cover
                text, zero, rest = request.partition('\x00')
                if zero is not None:
                    print '\nREQUEST to %s: id=%d, bg=%s, req=%r <+binary...>' \
                          % (self._peer, request_id, background, text)
                else:
                    print '\nREQUEST to %s: id=%d, bg=%s, request=%r' \
                          % (self._peer, request_id, background, request)
            req = ['setID %s\n' % request_id]
            if background:
                req.append('bg\n')
            length = len(request)
            req.append('cmdLen=%d\n' % length)
            if length <= 32:  # The value 32 is not critical here.
                req.append(request)
                self._send(''.join(req))
            else:
                self._send(''.join(req))
                self._send(request)
        else:
            if self._debug:  # pragma no cover
                print '\nREQUEST to %s: request=%r' % (self._peer, request)
            self._send('%s\r\n' % request)

    def recv_request(self):
        """ Receive request from client. """
        if self._debug:  # pragma no cover
            print '\nREQUEST from %s:' % self._peer
        if self._raw:
            info = self._expect(self._request_id)
            args = info[2].split()
            request_id = int(args[1])
            if self._debug:  # pragma no cover
                print '    request_id', request_id

            info = self._expect(self._request_len)
            if info[2].strip() == 'bg':
                background = True
                if self._debug:  # pragma no cover
                    print '    background'
                info = self._expect(self._request_len)
            else:
                background = False
            args = info[2].split('=')
            length = int(args[1])
            if self._debug:  # pragma no cover
                print '    length', length

            request = self._recv(length)
            if self._debug:  # pragma no cover
                text, zero, rest = request.partition('\x00')
                if zero is not None:
                    print '    req %r <+binary...>' % text
                else:
                    print '    request %r' % request
            return (request, request_id, background)
        else:
            info = self._expect(self._cooked_request)
            request = info[2].strip()
            if self._debug:  # pragma no cover
                print '    request %r' % request
            return request

    def send_reply(self, reply, reply_id=None, format='string'):
        """
        Send `reply` to client.
        If in 'raw' mode use `reply_id` and `format`.
        """
        if self._raw:
            if self._debug:  # pragma no cover
                text, zero, rest = reply.partition('\x00')
                if zero is not None:
                    print '\nREPLY to %s: id=%d, format=%s, reply=%r <+binary...>' \
                          % (self._peer, reply_id, format, text)
                else:
                    print '\nREPLY to %s: id=%d, format=%s, reply=%r' \
                          % (self._peer, reply_id, format, reply)
            length = len(reply)
            msg = '%d\r\nformat: %s\r\n%d\r\n' % (reply_id, format, length)
            if length <= 32:  # The value 32 is not critical here.
                msg += reply
                self._send(msg)
            else:
                self._send(msg)
                self._send(reply)
        else:
            if self._debug:  # pragma no cover
                print '\nREPLY to %s: reply=%r' % (self._peer, reply)
            if reply:
                reply = reply.replace('\n', '\r\n')
                if reply.endswith('\n>'):
                    self._send(reply)
                else:
                    self._send('%s\r\n>' % reply)
            else:
                self._send('>')

    def recv_reply(self):
        """ Receive reply from server. """
        if self._debug:  # pragma no cover
            print '\nREPLY from %s:' % self._peer
        if self._raw:
            info = self._expect(self._reply_id)
            reply_id = int(info[2])
            if self._debug:  # pragma no cover
                print '    reply_id', reply_id

            info = self._expect(self._formats)
            args = info[2].split()
            format = args[1].strip()
            if self._debug:  # pragma no cover
                print '    format %r' % format

            info = self._expect(self._reply_len)
            length = int(info[2])
            if self._debug:  # pragma no cover
                print '    length', length

            reply = self._recv(length)
            if self._debug:  # pragma no cover
                text, zero, rest = reply.partition('\x00')
                if zero is not None:
                    print '    reply %r <+binary...>' % text
                else:
                    print '    reply %r' % reply
            return (reply, reply_id, format)
        else:
            info = self._expect(self._cooked_reply)
            reply = info[2]
            reply = reply.replace('\r\n', '\n')
            if self._debug:  # pragma no cover
                print '    reply %r' % reply
            return reply

    def _send(self, data):
        """ Send `data`. """
#        if self._debug:  # pragma no cover
#            print '    send %s %r' % (self._peer, data)
        length = len(data)
        start = 0
        chunk = 1 << 17  # 128KB, chunking allows for send/recv overlap.
        while start < length:
            end = start + chunk
            self._sock.sendall(data[start:end])
            start = end

    def _expect(self, patterns):
        """
        Wait for one or more patterns to match.
        Return (index, match_obj, data).
        """
#        if self._debug:  # pragma no cover
#            print '    _expect: _recv_buffer %r' % self._recv_buffer
        indices = range(len(patterns))
        for i in indices:
            if not hasattr(patterns[i], 'search'):
                patterns[i] = re.compile(patterns[i])

        while True:
            for i in indices:
                pattern = patterns[i]
                match_obj = pattern.search(self._recv_buffer)
                if match_obj is not None:
                    end = match_obj.end()
                    data = self._recv_buffer[:end]
                    self._recv_buffer = self._recv_buffer[end:]
                    return (i, match_obj, data)
            self._receive()

    def _recv(self, length):
        """ Return next `length` bytes. """
#        if self._debug:  # pragma no cover
#            print '    recv: %d _recv_buffer %r (%d)' \
#                  % (length, self._recv_buffer, len(self._recv_buffer))
        while len(self._recv_buffer) < length:
            self._receive()
        data = self._recv_buffer[:length]
        self._recv_buffer = self._recv_buffer[length:]
        return data

    def _receive(self):
        """ Receive more data. """
#        if self._debug:  # pragma no cover
#            print '    _receive'
        try:
            data = self._sock.recv(4096)
        except socket.error as exc:  # pragma no cover
            if sys.platform == 'win32':
                if exc.errno == 10053 or exc.errno == 10054:
                    raise EOFError('Connection to %s closed' % self._peer)
            raise
        if data:
#            if self._debug:  # pragma no cover
#                print '       %r' % data
            self._recv_buffer += data
        else:
            raise EOFError('Connection to %s closed' % self._peer)

