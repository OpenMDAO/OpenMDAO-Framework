"""
This is just a wrapper for the logging module.
Messages can be routed to the console via enable_console().
If the file ``logger.cfg`` exists, it can be used to configure logging.
See the Python documentation for ``logging.config`` for details.
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
           'Logger', 'NullLogger',
           'LOG_DEBUG', 'LOG_INFO', 'LOG_WARNING', 'LOG_ERROR', 'LOG_CRITICAL']


import logging
import logging.config
import os.path


LOG_DEBUG    = logging.DEBUG
LOG_INFO     = logging.INFO
LOG_WARNING  = logging.WARNING
LOG_ERROR    = logging.ERROR
LOG_CRITICAL = logging.CRITICAL

# Ensure we can write to the log file.
_filename = 'openmdao_log.txt'
try:
    _tmplog = open(_filename, 'w')
except IOError:
    _filename = 'openmdao_log_%d.txt' % os.getpid()
else:
    _tmplog.close()

# Allow everything through, typical UNIX-ish timestamp, typical log format.
logging.basicConfig(level=logging.WARNING,
                    datefmt='%b %d %H:%M:%S',
                    format='%(asctime)s %(levelname)s %(name)s: %(message)s',
                    filename=_filename,
                    filemode='w')

# Compress level names.
logging.addLevelName(logging.NOTSET,   'N')
logging.addLevelName(logging.DEBUG,    'D')
logging.addLevelName(logging.INFO,     'I')
logging.addLevelName(logging.WARNING,  'W')
logging.addLevelName(logging.ERROR,    'E')
logging.addLevelName(logging.CRITICAL, 'C')

logger = logging.getLogger('')

# Optional handler which writes messages to sys.stderr
CONSOLE = None

# If a logging config file exists, use it.
if os.path.exists('logging.cfg'):
    logging.config.fileConfig('logging.cfg')


def getLogger(name):
    """ Return the named logger. """
    return logging.getLogger(name)
    

def enable_console():
    """ Configure logging to receive log messages at the console. """
    global CONSOLE
    if CONSOLE is None:
        # define a Handler which writes messages to sys.stderr
        CONSOLE = logging.StreamHandler()
        CONSOLE.setLevel(logging.DEBUG)
        # set a format which is simpler for console use
        formatter = logging.Formatter('%(levelname)s %(name)s: %(message)s')
        # tell the handler to use this format
        CONSOLE.setFormatter(formatter)
    logger.addHandler(CONSOLE)

def disable_console():
    """ Stop receiving log messages at the console. """
    logger.removeHandler(CONSOLE)


if int(os.environ.get('OPENMDAO_ENABLE_CONSOLE', '0')):
    enable_console()


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
            if self._name == other._name and self._logger == other._logger and self._level == other._level:
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

