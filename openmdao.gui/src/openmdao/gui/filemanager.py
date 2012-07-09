import os
import os.path
import shutil
import tempfile
import zipfile

from openmdao.gui.util import filedict
from openmdao.main.publisher import Publisher

from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler


class FilesPublisher(FileSystemEventHandler):
    ''' publishes file collection when ANY file system event occurs
    '''

    def __init__(self, files):
        self.files = files

    def dispatch(self, event):
        ''' just publish the updated file collection
        '''
        self.files.publish_files()


class FileManager(object):
    ''' Object that keeps track of a collection of files (i.e. a directory)
        and optionally publishes an update when the collection is modified
    '''

    def __init__(self, name, path=None, publish_updates=False):
        self.name = name

        self.orig_dir = os.getcwd()
        if path:
            self.root_dir = path
        else:
            self.root_dir = tempfile.mkdtemp(self.name)
        if os.path.exists(self.root_dir):
            shutil.rmtree(self.root_dir)
        os.mkdir(self.root_dir)
        os.chdir(self.root_dir)

        self.publish_updates = publish_updates
        self.publisher = None

        if self.publish_updates:
            self.observer = Observer()
            self.observer.daemon = True
            self.observer.schedule(FilesPublisher(self),
                                   path=self.root_dir,
                                   recursive=True)
            self.observer.start()
        else:
            self.observer = None

    def publish_files(self):
        ''' publish the current file collection
        '''
        if not self.publisher:
            try:
                self.publisher = Publisher.get_instance()
            except Exception, err:
                print 'Error getting publisher:', err
                self.publisher = None

        if self.publisher:
            self.publisher.publish(self.name, self.get_files())

    def getcwd(self):
        ''' return the current working directory
        '''
        return os.getcwd()

    def cleanup(self):
        ''' Stop observer and cleanup the file directory.
        '''
        if self.observer:
            self.observer.unschedule_all()
            self.observer.stop()
            self.observer.join()
        os.chdir(self.orig_dir)
        if os.path.exists(self.root_dir):
            try:
                shutil.rmtree(self.root_dir)
            except Exception, err:
                print 'Filemanager: Error cleaning up file directory', err

    def get_files(self):
        ''' get a nested dictionary of files in the working directory
        '''
        cwd = os.getcwd()
        return filedict(cwd, root=cwd)

    def get_file(self, filename):
        ''' get contents of file in working directory
            returns None if file was not found
        '''
        filepath = os.getcwd()+'/'+str(filename)
        if os.path.exists(filepath):
            contents=open(filepath, 'rb').read()
            return contents
        else:
            return None

    def ensure_dir(self, dirname):
        ''' create directory in working directory
            (does nothing if directory already exists)
        '''
        try:
            dirpath = os.getcwd()+'/'+str(dirname)
            if not os.path.isdir(dirpath):
                os.makedirs(dirpath)
            return str(True)
        except Exception, err:
            return str(err)

    def write_file(self, filename, contents):
        ''' write contents to file in working directory
        '''
        try:
            filepath = os.getcwd()+'/'+str(filename)
            fout = open(filepath, 'wb')
            fout.write(contents)
            fout.close()
            return True
        except Exception, err:
            return err

    def add_file(self, filename, contents):
        ''' add file to working directory
            if it's a zip file, unzip it
        '''
        self.write_file(filename, contents)
        if zipfile.is_zipfile(filename):
            userdir = os.getcwd()
            zfile = zipfile.ZipFile(filename, "r")
            zfile.printdir()
            for fname in zfile.namelist():
                if fname.endswith('/'):
                    dirname = userdir+'/'+fname
                    if not os.path.exists(dirname):
                        os.makedirs(dirname)
            for fname in zfile.namelist():
                if not fname.endswith('/'):
                    data = zfile.read(fname)
                    fname = userdir+'/'+fname
                    fname = fname.replace('\\', '/')
                    fout = open(fname, "wb")
                    fout.write(data)
                    fout.close()
            zfile.close()
            os.remove(filename)

    def delete_file(self, filename):
        ''' delete file in working directory
            returns False if file was not found, otherwise returns True
        '''
        filepath = os.getcwd()+'/'+str(filename)
        if os.path.exists(filepath):
            if os.path.isdir(filepath):
                os.rmdir(filepath)
            else:
                os.remove(filepath)
            return True
        else:
            return False
