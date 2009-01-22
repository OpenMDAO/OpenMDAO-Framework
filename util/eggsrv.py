"""
A simple egg server based on Ian Bicking's tutorial at
http://pythonpaste.org/webob/file-example.html
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


port = 31001

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


class EggServer(object):
    def __init__(self, topdir):
        self.topdir = os.path.abspath(topdir)
        
    def top_dir_response(self, environ, start_response):
        start_response('200 OK', [('Content-Type', 'text/html')])
        out = StringIO()
        out.write('<html>\n<body>\n<h1>Package Index</h1>\n<ul>\n')
        for d in os.listdir(self.topdir):
            full = os.path.join(self.topdir,d)
            if os.path.isdir(full):
                out.write('<li><a href="%s">%s</a>\n'%(d,d))
        out.write('</ul>\n</body>\n</html>')
        return [out.getvalue()]
        
    def dist_dir_response(self, environ, start_response, abspath):
        dpath = environ['PATH_INFO'].strip('/')
        start_response('200 OK', [('Content-Type', 'text/html')])
        out = StringIO()
        out.write('<html>\n<body>\n<h1>%s Distributions</h1>\n<ul>\n'%(dpath,))
        for f in os.listdir(abspath):
            fpath = os.path.join(abspath,f)
            if os.path.isfile(fpath) and (fpath.endswith('.tar.gz') or fpath.endswith('.egg')):
                lpath = os.path.join(wsgiref.util.request_uri(environ), f)
                out.write('<li><a href="%s">%s</a>\n'%(lpath, f))
        out.write('</ul>\n</body>\n</html>')
        return [out.getvalue()]
        
    def file_response(self, environ, start_response, abspath):
        print 'requesting file',wsgiref.util.request_uri(environ)
        app = FileApp(abspath)
        return app(environ, start_response)
        
    def __call__(self, environ, start_response):
        pth = environ['PATH_INFO']
        logger.info('from '+environ['REMOTE_ADDR']+' request for file '+pth)
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


if __name__ == '__main__':
    parser = OptionParser()
    parser.add_option("","--log", action="store", type="string", dest="log",
                      help="specify a file to log results to")
    parser.add_option("-e","--eggdir", action="store", type="string", dest="eggdir",
                      help="specify the directory where distributions are kept", default=".")

    (options, args) = parser.parse_args()
    
    eggdir = os.path.abspath(options.eggdir)
    
    if not os.path.isdir(eggdir):
        print >> sys.stderr, eggdir,'is not an accessible directory'
        parser.print_help()
        sys.exit(-1)
        
    if options.log:
        logger = setup_logger(options.log)
        
    app = EggServer(eggdir)
    
    httpserver.serve(app, host=platform.uname()[1], port=port)


