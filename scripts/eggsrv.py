"""
A simple egg server that borrows heavily from Ian Bicking's tutorial at
http://pythonpaste.org/webob/file-example.html.

Usage: python eggsrv.py --eggdir=<top level egg dir> --log=<my log file> --port=<port>

eggdir is expected to have the following sort of structure:

top
    package1_dir
        package1-1.0-py2.5-win32.egg
        package1-1.0-py2.5-linux-x86_64.egg
        package1-1.0.tar.gz
        
    package2_dir
        package2-0.4-py2.4.egg
        
      ...
    
The egg server will automatically handle the addition of new distributions to the 
egg directory structure, provided that they are structured as shown above.
    
MD5 checksums are calculated and added to each file URL.

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


logger = None

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


class EggServer(object):
    def __init__(self, topdir):
        self.topdir = os.path.abspath(topdir)
        
    def top_dir_response(self, environ, start_response):
        """When the server gets a '/' request, return a page listing all of
        the dirs in the egg directory.
        """
        start_response('200 OK', [('Content-Type', 'text/html')])
        out = StringIO()
        out.write('<html>\n<body>\n<h1>Package Index</h1>\n<ul>\n')
        dirs = os.listdir(self.topdir)
        dirs.sort(key=str.lower)
        for d in dirs:
            full = os.path.join(self.topdir,d)
            if os.path.isdir(full):
                out.write('<li><a href="%s">%s</a>\n'%(d,d))
        out.write('</ul>\n</body>\n</html>')
        return [out.getvalue()]
        
    def dist_dir_response(self, environ, start_response, abspath):
        """When the server gets a request for a directory, return a page
        listing each distribution file in the directory, along with its md5 digest.
        """
        dpath = environ['PATH_INFO'].strip('/')
        start_response('200 OK', [('Content-Type', 'text/html')])
        out = StringIO()
        out.write('<html>\n<body>\n<h1>%s Distributions</h1>\n<ul>\n'%(dpath,))
        for f in os.listdir(abspath):
            fpath = os.path.join(abspath,f)
            # TODO: add pkg_resources code here to actually find all of the distributions
            #       so that files that aren't distributions won't show up
            if os.path.isfile(fpath):
                checksum = file_md5(fpath)
                lpath = os.path.join(wsgiref.util.request_uri(environ), f)
                out.write('<li><a href="%s#md5=%s">%s</a>\n'%(lpath, checksum, f))
        out.write('</ul>\n</body>\n</html>')
        return [out.getvalue()]
        
    def file_response(self, environ, start_response, abspath):
        """Download the requested file."""
        app = FileApp(abspath)
        return app(environ, start_response)
        
    def __call__(self, environ, start_response):
        global logger
        if logger:
            logger.info(environ['REMOTE_ADDR']+': <-- '+
                         wsgiref.util.request_uri(environ))
                     
        pth = environ['PATH_INFO']
        abspath = os.path.join(self.topdir, pth.strip('/'))
        
        if pth == '/':
            return self.top_dir_response(environ, start_response)
        elif os.path.isdir(abspath):
            return self.dist_dir_response(environ, start_response, abspath)
        elif os.path.isfile(abspath):
            return self.file_response(environ, start_response, abspath)
        else:
            start_response('404 Not Found', [('Content-Type', 'text/html')])
            return ['file '+abspath+' could not be found']



def setup_logger(fname):

    logging.basicConfig(level=logging.INFO,
                        format='%(asctime)s %(name)s %(levelname)s %(message)s',
                        datefmt='%m-%d %H:%M',
                        filename=fname,
                        filemode='a')
                        
    return logging.getLogger('')


def check_eggdir(name):
    if name is None or os.path.isdir(name) is False:
        raise RuntimeError(str(name)+' is not a directory')
     
    dirs = [d for d in os.listdir(name) if os.path.isdir(os.path.join(name, d))]
    for d in dirs:
        for f in os.listdir(os.path.join(name, d)):
            if d != f[0:len(d)]:
                raise RuntimeError('directory '+name+' does not have the correct format '+
                                   d+' != '+f[0:len(d)])
                                       

if __name__ == '__main__':
    parser = OptionParser()
    parser.add_option("--log", action="store", type="string", dest="log",
                      help="a file to log results to")
    parser.add_option("--eggdir", action="store", type="string", dest="eggdir",
                      help="the directory where distributions are kept. The"+
                        " format of the directory must be a top level directory "+
                        "containing one directory for each package, with each package "+
                        "directory containing all distributions for that package",
                        default=".")
    parser.add_option("--port", action="store", type="int", dest="port", default=31001,
                      help="the port that the egg server will listen to")
    (options, args) = parser.parse_args()
    
    eggdir = os.path.abspath(options.eggdir)
    
    check_eggdir(eggdir)
    
    if not os.path.isdir(eggdir):
        print >> sys.stderr, eggdir,'is not an accessible directory'
        parser.print_help()
        sys.exit(-1)
    
    if options.log:
        logger = setup_logger(options.log)
        
    app = EggServer(eggdir)
    
    httpserver.serve(app, host=platform.uname()[1], port=options.port)


