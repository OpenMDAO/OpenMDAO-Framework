"""
A simple python distribution server that borrows heavily from Ian Bicking's tutorial at
http://pythonpaste.org/webob/file-example.html.

Usage: python findlinksrv.py --dir=<top level dir> --log=<my log file> --port=<port>

dir can have any structure. The server will recursively search for any python
distributions under the top directory.

The server will automatically handle the addition of new distributions to the 
distribution directory.
    
MD5 checksums are calculated and added to each file URL.

This server does NOT function as a package index. It is intended to be
specified in the 'find-links' list in a buildout config file or as a -f option
to easy_install or pip install. By using it in this way the Python Package
Index can still be used as a fallback when a given package is not found in
this server.

"""

import mimetypes
import os.path
import logging
from webob import Request, Response
from paste import httpserver
from optparse import OptionParser
import platform
from StringIO import StringIO
import wsgiref.util
import hashlib
import fnmatch
from threading import Lock


def find_files(pats, startdir):
    """Return a list of files (using a generator) that match
    the given list of glob patterns. Walks an entire directory structure.
    """
    match = fnmatch.fnmatch
    join = os.path.join
    for path, dirlist, filelist in os.walk(startdir):
        for name in filelist:
            for pat in pats:
                if match(name, pat):
                    yield join(path, name)

class FileApp(object):
    def __init__(self, filename):
        self.filename = filename
    def __call__(self, environ, start_response):
        res = make_response(self.filename)
        return res(environ, start_response)

class FileIterable(object):
    def __init__(self, filename, start=None, stop=None):
        self.filename = filename
        self.start = start
        self.stop = stop
    def __iter__(self):
        return FileIterator(self.filename, self.start, self.stop)
    def app_iter_range(self, start, stop):
        return self.__class__(self.filename, start, stop)
         
class FileIterator(object):
    chunk_size = 4096
    def __init__(self, filename, start, stop):
        self.filename = filename
        self.fileobj = open(self.filename, 'rb')
        if start:
            self.fileobj.seek(start)
        if stop is not None:
            self.length = stop - start
        else:
            self.length = None
    def __iter__(self):
        return self
    def next(self):
        if self.length is not None and self.length <= 0:
            raise StopIteration
        chunk = self.fileobj.read(self.chunk_size)
        if not chunk:
            raise StopIteration
        if self.length is not None:
            self.length -= len(chunk)
            if self.length < 0:
                # Chop off the extra:
                chunk = chunk[:self.length]
        return chunk


def get_mimetype(filename):
    typ, encoding = mimetypes.guess_type(filename)
    # We'll ignore encoding, even though we shouldn't really
    return typ or 'application/octet-stream'


def make_response(filename):
    res = Response(content_type=get_mimetype(filename),
                   conditional_response=True)
    res.app_iter = FileIterable(filename)
    res.content_length = os.path.getsize(filename)
    res.last_modified = os.path.getmtime(filename)
    res.etag = '%s-%s-%s' % (os.path.getmtime(filename),
                             os.path.getsize(filename), hash(filename))
    return res


def file_md5(fpath):
    """Return the MD5 digest for the given file"""
    try:
        f = open(fpath,'rb')
        m = hashlib.md5()
        while True:
            s = f.read(4096)
            if not s:
                break
            m.update(s)
        return m.hexdigest()
    finally:
        f.close()


class DistServer(object):
    def __init__(self, topdir):
        self.topdir = os.path.abspath(topdir)
        self.file_cache = {}
        self.lock = Lock()
        
    def top_dir_response(self, environ, start_response):
        """When the server gets a '/' request, return a page listing all of
        the distributions in this server.
        """
        start_response('200 OK', [('Content-Type', 'text/html')])
        out = StringIO()
        out.write('<html>\n<body>\n')
        self.lock.acquire()
        file_cache = self.file_cache
        try:
            flist = sorted(find_files(["*.egg","*.tar.gz"], self.topdir), key=str.lower)
            for f in flist:
                checksum = file_md5(f)
                basef = os.path.basename(f)
                dirname = wsgiref.util.request_uri(environ)
                lpath = os.path.join(dirname, basef)
                file_cache[basef] = f
                out.write('<li><a href="%s#md5=%s">%s</a>\n'%(lpath, checksum, basef))
        finally:
            self.lock.release()
        out.write('</ul>\n</body>\n</html>')
        return [out.getvalue()]

    def file_response(self, environ, start_response, abspath):
        """Download the requested file."""
        app = FileApp(abspath)
        return app(environ, start_response)
        
    def __call__(self, environ, start_response):
        logging.info(environ['REMOTE_ADDR']+': <-- '+
                     wsgiref.util.request_uri(environ))
                     
        pth = environ['PATH_INFO']
        
        if pth == '/':
            return self.top_dir_response(environ, start_response)
        else:
            fpath = pth.strip('/')
            self.lock.acquire()
            try:
                cached_fname = self.file_cache.get(fpath, '')
                if os.path.isfile(cached_fname):
                    logging.info('serving file: '+cached_fname)
                    return self.file_response(environ, start_response, cached_fname)
                elif cached_fname != '':
                    del self.file_cache[fpath]
            finally:
                self.lock.release()
        
            # should only find one file to match pth, so return immediately
            # when found
            for f in find_files([pth.strip('/')], self.topdir):
                self.lock.acquire()
                self.file_cache[fpath] = f
                self.lock.release()
                logging.info('serving file: '+f)
                return self.file_response(environ, start_response, f)
            else:
                start_response('404 Not Found', [('Content-Type', 'text/html')])
                return ['file '+pth+' could not be found']



def setup_logger(fname):

    logging.basicConfig(level=logging.INFO,
                        format='%(asctime)s %(name)s %(levelname)s %(message)s',
                        datefmt='%m-%d %H:%M',
                        filename=fname,
                        filemode='a')
                        
    logging.getLogger('').setLevel(logging.INFO)


def check_distdir(name):
    if name is None or os.path.isdir(name) is False:
        raise RuntimeError(str(name)+' is not a directory')

if __name__ == '__main__': # pragma no cover
    parser = OptionParser()
    parser.add_option("","--log", action="store", type="string", dest="log",
                      help="a file to log results to")
    parser.add_option("","--dir", action="store", type="string", dest="distdir",
                      help="the directory where distributions are kept",
                        default=".")
    parser.add_option("", "--port", action="store", type="int", dest="port", default=8000,
                      help="the port that the server will listen to")
    (options, args) = parser.parse_args()
    
    distdir = os.path.abspath(options.distdir)
    
    if not os.path.isdir(distdir):
        print >> sys.stderr, distdir,'is not an accessible directory'
        parser.print_help()
        sys.exit(-1)
    
    if options.log:
        setup_logger(options.log)
        
    app = DistServer(distdir)
    
    httpserver.serve(app, host=platform.uname()[1], port=options.port)


