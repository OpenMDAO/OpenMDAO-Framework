import atexit
import logging
import Queue
import threading
import traceback


class WorkerPool(object):
    """ Pool of worker threads; grows as necessary. """

    _lock = threading.Lock()
    _pool = None  # Singleton.

    def __init__(self):
        self._idle = []     # Queues of idle workers.
        self._workers = {}  # Maps queue to worker.
        atexit.register(self.cleanup)

    @staticmethod
    def get_instance():
        """ Return singleton instance. """
        with WorkerPool._lock:
            if WorkerPool._pool is None:
                WorkerPool._pool = WorkerPool()
            return WorkerPool._pool

    @staticmethod
    def cleanup():
        """ Cleanup resources (worker threads). """
        WorkerPool.get_instance()._cleanup()

    def _cleanup(self):
        """ Cleanup resources (worker threads). """
        with self._lock:
            for queue in self._workers:
                queue.put((None, None, None, None))
                self._workers[queue].join(1)
                if self._workers[queue].is_alive():
                    logging.debug('WorkerPool: worker join timed-out.')
                try:
                    self._idle.remove(queue)
                except ValueError:
                    pass  # Never released due to some other issue...
            self._idle = []
            self._workers = {}

    @staticmethod
    def get(one_shot=False):
        """
        Get a worker queue from the pool. Work requests should be of the form:

        ``(callable, *args, **kwargs, reply_queue)``

        Work replies are of the form:

        ``(queue, retval, exc, traceback)``

        one_shot: bool
            If True, the worker will self-release after processing one request.
        """
        return WorkerPool.get_instance()._get(one_shot)

    def _get(self, one_shot):
        """ Get a worker queue from the pool. """
        with self._lock:
            try:
                return self._idle.pop()
            except IndexError:
                queue = Queue.Queue()
                worker = threading.Thread(target=self._service_loop,
                                          args=(queue, one_shot))
                worker.daemon = True
                worker.start()
                self._workers[queue] = worker
                return queue

    @staticmethod
    def release(queue):
        """
        Release a worker queue back to the pool.

        queue: Queue
            Worker queue previously obtained from :meth:`get`.
        """
        return WorkerPool.get_instance()._release(queue)

    def _release(self, queue):
        """ Release a worker queue back to the pool. """
        with self._lock:
            self._idle.append(queue)

    def _service_loop(self, request_q, one_shot):
        """ Get (callable, args, kwargs) from request_q and queue result. """
        while True:
            callable, args, kwargs, reply_q = request_q.get()
            if callable is None:
                request_q.task_done()
                return  # Shutdown.

            exc = None
            trace = None
            retval = None
            try:
                retval = callable(*args, **kwargs)
            except Exception as exc:
                # Sometimes we have issues at shutdown.
                try:
                    trace = traceback.format_exc()
                except Exception:  #pragma no cover
                    return

            request_q.task_done()
            if reply_q is not None:
                reply_q.put((request_q, retval, exc, trace))

            if one_shot:
                self._release(request_q)

