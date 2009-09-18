
__all__ = ('EggObserver',)


class EggObserver(object):
    """
    Provides a convenient API for calling an observer of egg operations.
    `observer` will be called with:

    - ('analyze', filename, -1, -1) during module analysis.
    - ('add', filename, file_fraction, byte_fraction) while writing files.
    - ('copy', filename, file_fraction, byte_fraction) while copying files.
    - ('extract', filename, file_fraction, byte_fraction) while extracting files.
    - ('complete', egg_name, 1, 1) when complete.
    - ('except', message, -1, -1) when an exception occurs.
    """

    def __init__(self, observer, logger):
        assert observer is None or callable(observer)
        self.observer = observer
        self.logger = logger

    def analyze(self, path):
        """ Observe analysis of file. May cause an abort."""
        self.logger.debug("    analyzing '%s'", path)
        if self.observer is not None:
            proceed = True
            try:
                proceed = self.observer('analyze', path, -1, -1)
            except Exception, exc:
                self.logger.debug('Exception calling observer: %s', exc)
            else:
                if not proceed:
                    raise RuntimeError('Aborted by observer.')

    def add(self, path, file_fraction, byte_fraction):
        """ Observe add of file. May cause an abort."""
        self.logger.debug("    adding '%s'", path)
        if self.observer is not None:
            proceed = True
            try:
                proceed = self.observer('add', path, file_fraction,
                                        byte_fraction)
            except Exception, exc:
                self.logger.debug('Exception calling observer: %s', exc)
            else:
                if not proceed:
                    raise RuntimeError('Aborted by observer.')

    def copy(self, path, file_fraction, byte_fraction):
        """ Observe copy of file. May cause an abort."""
#        import time
#        time.sleep(1)
        self.logger.debug("    copying '%s'", path)
        if self.observer is not None:
            proceed = True
            try:
                proceed = self.observer('copy', path, file_fraction,
                                        byte_fraction)
            except Exception, exc:
                self.logger.debug('Exception calling observer: %s', exc)
            else:
                if not proceed:
                    raise RuntimeError('Aborted by observer.')

    def extract(self, path, file_fraction, byte_fraction):
        """ Observe extraction of file. May cause an abort."""
#        import time
#        time.sleep(1)
        self.logger.debug("    extracting '%s'", path)
        if self.observer is not None:
            proceed = True
            try:
                proceed = self.observer('extract', path, file_fraction,
                                        byte_fraction)
            except Exception, exc:
                self.logger.debug('Exception calling observer: %s', exc)
            else:
                if not proceed:
                    raise RuntimeError('Aborted by observer.')

    def complete(self, path):
        """ Observations complete. """
        if self.observer is not None:
            try:
                self.observer('complete', path, 1, 1)
            except Exception, exc:
                self.logger.debug('Exception calling observer: %s', exc)

    def exception(self, msg):
        """ Observe exception. """
        if self.observer is not None:
            try:
                self.observer('except', msg, -1, -1)
            except Exception, exc:
                self.logger.debug('Exception calling observer: %s', exc)

