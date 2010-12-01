"""
Test Egg Observer functions.
"""

import logging
import sys
import unittest
import nose

from openmdao.util.eggobserver import EggObserver
from openmdao.util.testutil import assert_raises


class Observer(object):
    """ Observes egg operations. """

    def __init__(self):
        self.abort = False
        self.throw_exception = False

    def observe(self, operation, *args):
        """ Note `operation`. """
        if self.throw_exception:
            raise UserWarning('oops')
        return not self.abort


class TestCase(unittest.TestCase):
    """ Test Egg Observer functions. """

    def setUp(self):
        """ Invoked before each test. """
        self.observer = Observer()
        self.egg_observer = EggObserver(self.observer.observe,
                                        logging.getLogger('EO'))

    def tearDown(self):
        """ Invoked after each test. """
        pass

    def test_normal(self):
        logging.debug('')
        logging.debug('test_normal')

        # Note: this is not a valid ordering of calls.
        self.egg_observer.analyze('xyzzy')
        self.egg_observer.add('file1', 0, 0)
        self.egg_observer.copy('file2', 0.3, 0.5)
        self.egg_observer.extract('file3', 0.6, 0.75)
        self.egg_observer.complete('xyzzy')
        self.egg_observer.exception('oops')

    def test_exception(self):
        logging.debug('')
        logging.debug('test_exception')

        self.observer.throw_exception = True

        # Note: this is not a valid ordering of calls.
        self.egg_observer.analyze('xyzzy')
        self.egg_observer.add('file1', 0, 0)
        self.egg_observer.copy('file2', 0.3, 0.5)
        self.egg_observer.extract('file3', 0.6, 0.75)
        self.egg_observer.complete('xyzzy')
        self.egg_observer.exception('oops')

    def test_abort(self):
        logging.debug('')
        logging.debug('test_abort')

        self.observer.abort = True

        # Note: this is not a valid ordering of calls.
        assert_raises(self, "self.egg_observer.analyze('xyzzy')",
                      globals(), locals(), RuntimeError, 'Aborted by observer.')
        assert_raises(self, "self.egg_observer.add('file1', 0, 0)",
                      globals(), locals(), RuntimeError, 'Aborted by observer.')
        assert_raises(self, "self.egg_observer.copy('file2', 0.3, 0.5)",
                      globals(), locals(), RuntimeError, 'Aborted by observer.')
        assert_raises(self, "self.egg_observer.extract('file3', 0.6, 0.75)",
                      globals(), locals(), RuntimeError, 'Aborted by observer.')
        self.egg_observer.complete('xyzzy')
        self.egg_observer.exception('oops')


if __name__ == '__main__':
    sys.argv.append('--cover-package=openmdao.util.eggobserver')
    sys.argv.append('--cover-erase')
    nose.runmodule()

