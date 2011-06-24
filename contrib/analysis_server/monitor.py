"""
Support for heartbeat and file monitoring.
"""

import os
import threading


class BaseMonitor(object):
    """
    Base class for monitors. `name` is the name for the monitor thread,
    `delay` is the delay in seconds between calls to :meth:`poll`.
    Updates are sent by :meth:`send` via `send_reply` using `monitor_id`.
    If `immediate` is True, then :meth:`poll` will be called before the
    first delay.
    """

    def __init__(self, name, delay, monitor_id, send_reply, immediate=True):
        assert delay > 0, 'delay %r must be > 0' % delay
        assert callable(send_reply), \
               'send_reply %r must be callable' % send_reply
        self._name = name
        self._delay = delay
        self._monitor_id = monitor_id
        self._send_reply = send_reply
        self._immediate = immediate
        self._stop = threading.Event()
        self._thread = None

    def start(self):
        """ Start the monitor. May be called only once. """
        if self._thread is None:
            self._thread = threading.Thread(name=self._name, target=self._run)
            self._thread.daemon = True
            self._thread.start()
        else:
            raise RuntimeError('start() may only be called once.')

    def stop(self):
        """ Stop the monitor. """
        self._stop.set()

    def _run(self):
        """ Run the monitor. """
        if self._immediate:
            self.poll()

        while not self._stop.is_set():
            self._stop.wait(self._delay)
            if not self._stop.is_set():
                self.poll()
        try:
            self.shutdown()
        except Exception:  # pragma no cover
            pass  # Probably interpreter shutdown issues.

    def poll(self):
        """
        Called periodically to poll the state of the monitored item.
        Must be overridden.
        """
        raise NotImplementedError('BaseMonitor.poll()')

    def shutdown(self):
        """ Called when polling stops. """
        pass

    def send(self, msg):
        """ Send `msg` reply. """
        try:
            self._send_reply(msg, self._monitor_id)
        except Exception:  # pragma no cover
            pass  # Probably interpreter shutdown issues.


class Heartbeat(BaseMonitor):
    """
    Sends an 'HB' reply every 60 seconds using `monitor_id` and `send_reply`.
    """

    def __init__(self, monitor_id, send_reply):
        super(Heartbeat, self).__init__('Heartbeat', 60,
                                        monitor_id, send_reply, False)
    def poll(self):
        """ Send a heartbeat reply. """
        self.send('HB')


class FileMonitor(BaseMonitor):
    """
    Sends file updates at most once per second. The file is accessed using
    `server`, `path`, and `mode`. Replies are sent using `monitor_id`
    and `send_reply`.
    """

    def __init__(self, server, path, mode, monitor_id, send_reply):
        super(FileMonitor, self).__init__(path, 1, monitor_id, send_reply)
        self._server = server
        self._path = path
        self._mode = mode
        self._file = None
        self._size = 0

    def poll(self):
        """ Check for an increase in file size and send updated file data. """
        if self._file is None:
            try:
                if self._server is None:  # Used when testing.
                    self._file = open(self._path, self._mode)
                else:  # pragma no cover
                    self._file = self._server.open(self._path, self._mode)
            except Exception as exc:
                self.send("Can't open %r: %r" % (self._path, exc))
                self.stop()
                return
            self._size = 0

        if self._server is None:  # Used when testing.
            info = os.stat(self._path)
        else:  # pragma no cover
            info = self._server.stat(self._path)
        size = info.st_size

        if size > self._size:
            data = self._file.read(size - self._size)
            self.send(data)
            self._size = size

    def shutdown(self):
        """ Close file. """
        if self._file is not None:
            self._file.close()
            self._file = None

