
#public symbols
__all__ = ["ProjDirFactory"]


from openmdao.main.factory import Factory
from openmdao.util.log import logger
import fnmatch

from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler, PatternMatchingEventHandler


class PyWatcher(FileSystemEventHandler):

    #def __init__(self):
        #super(PyWatcher, self).__init__(patterns=['*.py'], ignore_directories=True, 
                                        #case_sensitive=True)

    def on_modified(self, event):
        if not event.is_directory and fnmatch.fnmatch(event.src_path, '*.py'):
            print 'MODIFIED: ', event.event_type, event.is_directory, event.src_path
    
    def on_deleted(self, event):
        if not event.is_directory and fnmatch.fnmatch(event.src_path, '*.py'):
            print 'DELETED: ', event.event_type, event.is_directory, event.src_path
    
    def on_moved(self, event):
        if not event.is_directory and event.src_path is not None and fnmatch.fnmatch(event.src_path, '*.py'):
            print 'MOVED: ', event.event_type, event.is_directory, event.src_path
   
class ProjDirFactory(Factory):
    """
    """
    def __init__(self):
        super(ProjDirFactory, self).__init__()

    def create(self, typ, version=None, server=None, 
               res_desc=None, **ctor_args):
        """
        """
        pass

    def get_available_types(self, groups=None):
        """
        """
        pass


if __name__ == '__main__':
    import time
    event_handler = PyWatcher()
    observer = Observer()
    observer.schedule(event_handler, path='.', recursive=True)
    observer.start()
    try:
        while True:
            time.sleep(.1)
    except KeyboardInterrupt:
        observer.stop()
    observer.join()