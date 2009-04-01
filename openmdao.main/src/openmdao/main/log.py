"""
This is just a wrapper for the logging module.
"""

#public symbols
__all__ = ['logger', 'getLogger', 'Logger',
           'LOG_DEBUG', 'LOG_INFO', 'LOG_WARNING', 'LOG_ERROR', 'LOG_CRITICAL']

__version__ = '0.1'


import logging

LOG_DEBUG    = logging.DEBUG
LOG_INFO     = logging.INFO
LOG_WARNING  = logging.WARNING
LOG_ERROR    = logging.ERROR
LOG_CRITICAL = logging.CRITICAL

# Root logger level (normally WARNING).
logging.getLogger().setLevel(logging.DEBUG)

# Allow everything through, typical UNIX-ish timestamp, typical log format.
logging.basicConfig(level=logging.NOTSET,
                    datefmt='%b %d %H:%M:%S',
                    format='%(asctime)s %(levelname)s %(name)s: %(message)s',
                    filename='openmdao_log.txt',
                    filemode='w')

# Compress level names.
logging.addLevelName(logging.NOTSET,   'N')
logging.addLevelName(logging.DEBUG,    'D')
logging.addLevelName(logging.INFO,     'I')
logging.addLevelName(logging.WARNING,  'W')
logging.addLevelName(logging.ERROR,    'E')
logging.addLevelName(logging.CRITICAL, 'C')

logger = logging.getLogger('')

# define a Handler which writes INFO messages or higher to the sys.stderr
console = logging.StreamHandler()
console.setLevel(logging.DEBUG)
# set a format which is simpler for console use
#formatter = logging.Formatter('%(name)-12s: %(levelname)-8s %(message)s')
formatter = logging.Formatter('%(levelname)-6s %(message)s')
# tell the handler to use this format
console.setFormatter(formatter)
# add the handler to the root logger
#logger.addHandler(console)


def getLogger(name):
    return logging.getLogger(name)
    

class Logger(object):
    """ Pickle-able logger. Mostly a pass-through to real logger."""

    def __init__(self, name, level=None):
        self._name = name
        self._logger = logging.getLogger(name)
        self._level = None
        if level is None:
            self.level = self._logger.getEffectiveLevel()
        else:
            self.level = level

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

    def critical(self, msg, *args, **kwargs):
        """ Log a critical message. """
        self._logger.critical(msg, *args, **kwargs)

    def log(self, level, msg, *args, **kwargs):
        """ Log a message at a specified level. """
        self._logger.log(level, msg, *args, **kwargs)
    
