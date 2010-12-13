"""
Exercise logging functions.
"""

import logging
import sys
import unittest

from openmdao.util.log import enable_console, disable_console, \
                              Logger, NullLogger


class TestCase(unittest.TestCase):
    """ Exercise logging functions. """

    def test_console(self):
        logging.debug('')
        logging.debug('test_console')

        enable_console()
        disable_console()

    def test_logger(self):
        logging.debug('')
        logging.debug('test_logger')

        logger = Logger('lut', logging.NOTSET)
        logger.debug('debug message')
        logger.info('info message')
        logger.warning('warning message')
        logger.error('error message')
        logger.critical('critical message')
        logger.log(1, 'logged at level 1')

    def test_null_logger(self):
        logging.debug('')
        logging.debug('test_null_logger')

        logger = NullLogger()
        logger.debug('debug message')
        logger.info('info message')
        logger.warning('warning message')
        logger.error('error message')
        logger.critical('critical message')
        logger.log(1, 'logged at level 1')


if __name__ == '__main__':
    import nose
    sys.argv.append('--cover-package=openmdao.util')
    sys.argv.append('--cover-erase')
    nose.runmodule()

