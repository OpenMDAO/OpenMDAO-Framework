"""
Test WorkerPool functions.
"""

import logging
import Queue
import sys
import time
import unittest
import nose

from openmdao.util.wrkpool import WorkerPool


class TestCase(unittest.TestCase):
    """ Test WorkerPool functions. """

    def setUp(self):
        """ Invoked before each test. """
        self.reply_q = Queue.Queue()
        self.total = 0

    def tearDown(self):
        """ Invoked after each test. """
        self.reply_q = None

    def add(self, value):
        """ Dummy task for a worker to do. """
        self.total += value
        return -value

    def test_basic(self):
        logging.debug('')
        logging.debug('test_basic')

        worker_q = WorkerPool.get()
        worker_q.put((self.add, (1,), {}, self.reply_q))

        done_q, retval, exc, trace = self.reply_q.get()
        self.assertEqual(done_q, worker_q)
        self.assertEqual(retval, -1)
        self.assertEqual(exc, None)
        self.assertEqual(trace, None)
        self.assertEqual(self.total, 1)

        WorkerPool.release(worker_q)
        WorkerPool.cleanup()

    def test_exception(self):
        logging.debug('')
        logging.debug('test_exception')

        tail = "TypeError: unsupported operand type(s) for +=: 'int' and 'NoneType'\n"
        worker_q = WorkerPool.get()
        worker_q.put((self.add, (None,), {}, self.reply_q))

        done_q, retval, exc, trace = self.reply_q.get()
        self.assertEqual(done_q, worker_q)
        self.assertEqual(retval, None)
        self.assertEqual(type(exc), TypeError)
        self.assertTrue(trace.endswith(tail))
        self.assertEqual(self.total, 0)

        WorkerPool.release(worker_q)
        WorkerPool.cleanup()

    def test_join_timeout(self):
        logging.debug('')
        logging.debug('test_join_timeout')

        worker_q = WorkerPool.get()
        worker_q.put((time.sleep, (3,), {}, self.reply_q))
        WorkerPool.cleanup()

    def test_never_released(self):
        logging.debug('')
        logging.debug('test_never_released')

        worker_q = WorkerPool.get()
        worker_q.put((self.add, (1,), {}, self.reply_q))
        WorkerPool.cleanup()


if __name__ == '__main__':
    sys.argv.append('--cover-package=openmdao.util.wrkpool')
    sys.argv.append('--cover-erase')
    nose.runmodule()

