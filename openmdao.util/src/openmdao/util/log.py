"""
This is just a wrapper for the logging module.
Messages can be routed to the console via enable_console().
If the file logger.cfg exists, it can be used to configure logging.
See the Python documentation for `logging.config` for details.
The example below is equivalent to calling enable_console():

.. parsed-literal::

    [loggers]
    keys=root

    [handlers]
    keys=consoleHandler

    [formatters]
    keys=consoleFormatter

    [logger_root]
    level=DEBUG
    handlers=consoleHandler

    [handler_consoleHandler]
    class=StreamHandler
    level=DEBUG
    formatter=consoleFormatter
    args=(sys.stderr,)

    [formatter_consoleFormatter]
    format=%(levelname)s %(name)s: %(message)s

"""

#public symbols
__all__ = ['logger', 'getLogger', 'enable_console', 'disable_console',
           'TRACER', 'enable_trace', 'disable_trace',
           'Logger', 'NullLogger',
           'LOG_DEBUG', 'LOG_INFO', 'LOG_WARNING', 'LOG_ERROR', 'LOG_CRITICAL']

import atexit
import logging.config
import logging.handlers
import os
import os.path
import select
import socket
import SocketServer
import sys
import threading
import datetime

from cPickle import loads
from struct import unpack

LOG_DEBUG    = logging.DEBUG
LOG_INFO     = logging.INFO
LOG_WARNING  = logging.WARNING
LOG_ERROR    = logging.ERROR
LOG_CRITICAL = logging.CRITICAL

LOG_DEBUG2 = logging.DEBUG - 2  # Protocol debug, etc.
LOG_DEBUG3 = logging.DEBUG - 3  # Even lower-level stuff.

# Compress level names.
logging.addLevelName(logging.NOTSET,   'N')
logging.addLevelName(logging.DEBUG,    'D')
logging.addLevelName(logging.INFO,     'I')
logging.addLevelName(logging.WARNING,  'W')
logging.addLevelName(logging.ERROR,    'E')
logging.addLevelName(logging.CRITICAL, 'C')

logging.addLevelName(LOG_DEBUG2, 'D2')
logging.addLevelName(LOG_DEBUG3, 'D3')


def _configure_root():
    """ Configure root logger with a rotating file handler. """

    filename = os.environ.get('OPENMDAO_LOGFILE', 'openmdao_log.txt')
    # Ensure we can write to the log file.
    try:
        with open(filename, 'w'):
            pass
    except IOError:
        filename = 'openmdao_log_%d.txt' % os.getpid()

    msg_fmt = '%(asctime)s %(levelname)s %(name)s: %(message)s'
    date_fmt = '%b %d %H:%M:%S'
    formatter = logging.Formatter(msg_fmt, date_fmt)

    handler = logging.handlers.RotatingFileHandler(filename)
    handler.setFormatter(formatter)
    handler.setLevel(0)  # Handle anything sent to us.

    logging.getLogger().addHandler(handler)


# If the root logger hasn't been configured, do it now.
logging._acquireLock()
try:
    logger = logging.getLogger()
    if not logger.handlers:
        _configure_root()
finally:
    logging._releaseLock()


# If a logging config file exists, use it.
if os.path.exists('logging.cfg'):
    logging.config.fileConfig('logging.cfg')


def getLogger(name):
    """ Return the named logger. """
    return logging.getLogger(name)


# Optional handler which writes messages to sys.stderr
CONSOLE = None

def enable_console(level=logging.WARNING):
    """ Configure logging to receive log messages at the console. """
    global CONSOLE
    if CONSOLE is None:
        # define a Handler which writes messages to sys.stderr
        CONSOLE = logging.StreamHandler()
        # set a format which is simpler for console use
        formatter = logging.Formatter('%(levelname)s %(name)s: %(message)s')
        # tell the handler to use this format
        CONSOLE.setFormatter(formatter)
    CONSOLE.setLevel(level)
    logger.addHandler(CONSOLE)

def disable_console():
    """ Stop receiving log messages at the console. """
    logger.removeHandler(CONSOLE)

if int(os.environ.get('OPENMDAO_ENABLE_CONSOLE', '0')):
    enable_console()


# Optional logger for iteration coordinates.
TRACER = None

def enable_trace(stream=None):
    """
    Enable iteration tracing.

    stream: file or string
        File object or filename for trace output.
        Only used on first enable, default ``sys.stderr``.
    """
    global TRACER
    if TRACER is None:
        TRACER = logging.getLogger('TRACER')
        TRACER.propagate = False
        formatter = logging.Formatter('%(message)s')
        if stream is None:
            stream = sys.stderr
        elif isinstance(stream, basestring):
            stream = open(stream, 'w')
        handler = logging.StreamHandler(stream)
        handler.setFormatter(formatter)
        TRACER.addHandler(handler)
    TRACER.setLevel(logging.DEBUG)

def disable_trace():
    """ Disable iteration tracing. """
    if TRACER is not None:
        TRACER.setLevel(logging.CRITICAL)

if int(os.environ.get('OPENMDAO_ENABLE_TRACE', '0')):
    enable_trace()


class Logger(object):
    """ Pickle-able logger. Mostly a pass-through to a real logger."""

    def __init__(self, name, level=None):
        self._name = name
        self._logger = logging.getLogger(name)
        self._level = None
        if level is None:
            self.level = self._logger.getEffectiveLevel()
        else:
            self.level = level

    def __eq__(self, other):
        try:
            if self._name == other._name and self._logger == other._logger and \
               self._level == other._level:
                return True
        except AttributeError:
            pass
        return False

    def __ne__(self, other):
        return not self.__eq__(other)

    def __getstate__(self):
        """ Return dict representing this Logger's state. """
        state = self.__dict__.copy()
        state['_logger'] = None  # Contains an unpickleable lock.
        return state

    def __setstate__(self, state):
        """ Restore this Logger's state. """
        self.__dict__ = state
        self._logger = logging.getLogger(self._name)
        self.level = self._level

    def _get_level(self):
        """ Return our level. """
        return self._level

    def _set_level(self, level):
        """ Set our level. """
        self._level = level
        self._logger.setLevel(level)

    level = property(_get_level, _set_level, doc='Logging message level.')

    def rename(self, name):
        """ Change name reported in log. """
        self._name = name
        self._logger = logging.getLogger(name)
        self._logger.setLevel(self._level)

    def debug(self, msg, *args, **kwargs):
        """ Log a debug message. """
        self._logger.debug(msg, *args, **kwargs)

    def info(self, msg, *args, **kwargs):
        """ Log an information message. """
        self._logger.info(msg, *args, **kwargs)

    def warning(self, msg, *args, **kwargs):
        """ Log a warning message. """
        self._logger.warning(msg, *args, **kwargs)

    def error(self, msg, *args, **kwargs):
        """ Log an error message. """
        self._logger.error(msg, *args, **kwargs)

    def exception(self, msg, *args, **kwargs):
        """ Log an exception. """
        self._logger.exception(msg, *args, **kwargs)

    def critical(self, msg, *args, **kwargs):
        """ Log a critical message. """
        self._logger.critical(msg, *args, **kwargs)

    def log(self, level, msg, *args, **kwargs):
        """ Log a message at a specified level. """
        self._logger.log(level, msg, *args, **kwargs)


class NullLogger(object):
    """
    Can be useful when no logger has been supplied to a routine.
    It produces no output.
    """

    def debug(self, msg, *args, **kwargs):
        """ Log a debug message. """
        pass

    def info(self, msg, *args, **kwargs):
        """ Log an information message. """
        pass

    def warning(self, msg, *args, **kwargs):
        """ Log a warning message. """
        pass

    def error(self, msg, *args, **kwargs):
        """ Log an error message. """
        pass

    def exception(self, msg, *args, **kwargs):
        """ Log an exception. """
        pass

    def critical(self, msg, *args, **kwargs):
        """ Log a critical message. """
        pass

    def log(self, level, msg, *args, **kwargs):
        """ Log a message at a specified level. """
        pass


# Remote logging support.
# Note that a process fork will 'inherit' existing global data.
# To avoid child processes from using parent process data,
# the process' PID is used in dictionary keys.

_REMOTE_HANDLERS = {}  # Logging handler(s) installed by the process.


# Called by the remote process.
def install_remote_handler(host, port, prefix=None):  # pragma no cover
    """
    Installs a handler for logging to `host` on `port` with `prefix`.
    Returns True if connecting to the remote host was successful.

    host: string
        Host to send log requests to.

    port: int
        Port on `host` to send log requests to.

    prefix: string
        Added to the log record for use on `host`.
        The default prefix is ``pid@hostname``.
    """
    if prefix is None:
        prefix = '%s@%s' % (os.getpid(), socket.gethostname())

    try:
        sock = socket.create_connection((host, port))
    except Exception as exc:
        logging.error("Can't connect to logging server at %s:%s: %s",
                      host, port, exc)
        return False
    sock.close()

    handler = _RemoteHandler(host, port, prefix)
    root = logging.getLogger()
    root.addHandler(handler)

    my_pid = os.getpid()
    if my_pid not in _REMOTE_HANDLERS:
        # Remove any handlers from our parent process due to a fork.
        for pid, handlers in _REMOTE_HANDLERS.items():
            for handler in handlers:
                try:
                    root.removeHandler(handler)
                    handler.close()
                except KeyError:  # Apparently it's not there anymore.
                    pass
                except Exception as exc:
                    logging.warning("Can't remove inherited remote log handler:"
                                    " %s", str(exc) or repr(exc))
            del _REMOTE_HANDLERS[pid]
        _REMOTE_HANDLERS[my_pid] = []
    _REMOTE_HANDLERS[my_pid].append(handler)

    return True


# Called by the remote process.
def remove_remote_handlers():  # pragma no cover
    """ Removes all installed remote handlers. """
    root = logging.getLogger()
    for pid, handlers in _REMOTE_HANDLERS.items():
        if pid == os.getpid():
            for handler in handlers:
                root.removeHandler(handler)
                handler.close()
        del _REMOTE_HANDLERS[pid]


# Used by the remote process.
class _RemoteHandler(logging.handlers.SocketHandler):  # pragma no cover
    """ Handler which adds a ``prefix`` attribute to the log record. """

    def __init__(self, host, port, prefix):
        self.prefix = prefix
        logging.handlers.SocketHandler.__init__(self, host, port)

    def handle(self, record):
        """
        Add ``prefix`` attribute to `record` and format locally
        to avoid problems with object types which may not be handled
        well at the remote end.
        """
        record.prefix = self.prefix
        record.msg = record.getMessage()
        record.args = None
        logging.handlers.SocketHandler.handle(self, record)


def logging_port(server_host, client_host):
    """
    Return port to use to send log messages to `server_host`.
    Registers `client_host` as a legal host to receive from.
    """
    return _LogListener.server_port(server_host, client_host)


class _LogListener(object):
    """ Maintains :class:`_LogServer` threads. """

    _lock = threading.Lock()  # server_port() may be called by multiple threads.
    _listeners = {}           # _LogListeners keyed by hostname.

    def __init__(self, hostname):
        self._server = None
        self._port = None
        self._started = threading.Event()  # Signals listener has started.
        self._thread = threading.Thread(name=hostname+'-log-listener',
                                        target=self._listen, args=(hostname,))
        self._thread.daemon = True
        self._thread.start()
        self._started.wait(10)
        if self._port is None:
            raise RuntimeError('Timeout starting thread for %s' % hostname)

    @property
    def port(self):
        """ Port server is listening on. """
        return self._port

    def _listen(self, hostname):
        """ Listen for log messages. """
        self._server = _LogServer((hostname, 0), _LogHandler)
        self._port = self._server.server_address[1]
        logging.info('Listening for log messages at %s:%s',
                     hostname, self._port)
        atexit.register(self._stop, os.getpid())
        self._started.set()
        self._server.service_loop()

    def _stop(self, pid):
        """ Stop listening. """
        if os.getpid() == pid and self._thread.is_alive():
            self._server.stop()
            self._thread.join(2)

    @staticmethod
    def server_port(server_host, client_host):
        """
        Return port to use to send log messages to `server_host`.
        Registers `client_host` as a legal host to receive from.
        """
        with _LogListener._lock:
            # Ensure server thread running.
            listener = _LogListener._listeners.get(server_host, None)
            if listener is None:
                listener = _LogListener(server_host)
                _LogListener._listeners[server_host] = listener
            port = listener.port

            # Ensure client in allowed hosts.
            _LogServer.add_client(client_host)

        return port


class _LogServer(SocketServer.ThreadingTCPServer):
    """ Server for remote logging requests. """

    # Default is False, which would keep this process running until
    # all remote connections are closed. That could be problematic.
    daemon_threads = True

    _hosts = set()  # Hosts we allow connections from.

    @staticmethod
    def add_client(hostname):
        """ Add `hostname` as a legal client host. """
        client_addr = socket.gethostbyname(hostname)
        _LogServer._hosts.add(client_addr)
        # Some machines register 127.0.1.1 and then use 127.0.0.1 for localhost.
        if client_addr.startswith('127.') and \
           '127.0.0.1' not in _LogServer._hosts:
            _LogServer._hosts.add('127.0.0.1')

    def verify_request(self, request, client_address):
        """
        Returns True if the client at `client_address` is on a legal host.

        request: string
            Request message.

        client_address: ``(host, port)``
            Source of client request.
        """
        host, port = client_address
        if host in self._hosts:
            return True
        logging.warning('Rejecting connection from %s: allowed hosts %s',
                        host, self._hosts)
        return False

    def service_loop(self):
        """ Loop waiting for connections. """
        self._is_shut_down = threading.Event()
        self._shutdown_request = False
        fd = self.socket.fileno()
        try:
            while not self._shutdown_request:
                rd, wr, ex = select.select([fd], [], [], 0.5)
                if fd in rd:
                    self._handle_request_noblock()
        finally:
            self._is_shut_down.set()

    def stop(self):
        """ Stop server loop. """
        self._shutdown_request = True
        self._is_shut_down.wait(2)


class _LogHandler(SocketServer.StreamRequestHandler):
    """ Handler for a stream of logging requests. """

    def handle(self):
        """ Handle log requests until connection closed. """
        # An initial 'unused' connection will be made by the client to see
        # if it can connect. We reduce logging noise by ignoring these.
        makeLogRecord = logging.makeLogRecord
        logger = logging.getLogger('remote')
        conn = self.connection
        recv = conn.recv
        peer = None

        while True:
            try:
                data = recv(4)
            except Exception:
                return  # Typically [Errno 10054] on Windows.
            if len(data) < 4:
                break

            if peer is None:
                host, port = conn.getpeername()
                try:
                    host, aliases, addrs = socket.gethostbyaddr(host)
                except Exception as exc:
                    logger.debug('gethostbyaddr(%s) failed: %s',
                                 host, str(exc) or repr(exc))
                peer = '%s:%s' % (host, port)
                logger.info('New logging connection from %s', peer)

            slen = unpack('>L', data)[0]
            data = recv(slen)
            slen -= len(data)
            msg = data
            while slen:
                data = recv(slen)
                slen -= len(data)
                msg = ''.join((msg, data))

            try:
                obj = loads(msg)
                record = makeLogRecord(obj)
            except Exception as exc:
                logger.exception("Can't process log request from %s: %s",
                                 peer, exc)
            else:
                prefix = record.prefix
                name = record.name
                if name != prefix:
                    record.name = '[%s] %s' % (prefix, name)
                else:
                    record.name = '[%s]' % prefix
                logger.handle(record)

        conn.close()
        if peer is not None:
            logger.info('Logging connection from %s closed', peer)
