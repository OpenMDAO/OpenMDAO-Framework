
import sys
import os
import shutil
import urllib2
import subprocess
import tempfile
from optparse import OptionParser

from fabric.api import run, env, local, put, cd, get, settings
from fabric.state import connections

from openmdao.devtools.utils import push_and_run


def _test_remote(fname, pyversion='python', keep=False, 
                 branch=None, args=()):
    loctstfile = os.path.join(os.path.dirname(__file__), 'loctst.py')
    remoteargs = ['-f', fname, '--pyversion=%s' % pyversion]
    if keep:
        remoteargs.append('--keep')
    remoteargs.extend(args)
    push_and_run(loctstfile, 
                 remotepath=os.path.basename(loctstfile),
                 args=remoteargs)


if __name__ == '__main__':
    parser = OptionParser()
    parser.add_option("-f","--file", action="store", type='string', 
                      dest='fname',
                      help="URL of a go-openmdao.py file")
    parser.add_option("--pyversion", action="store", type='string', 
                      dest='pyversion',
                      default="python", 
                      help="python version to use, e.g., 'python2.6'")
    parser.add_option("-k","--keep", action="store_true", dest='keep',
                      help="don't delete temporary build directory")
    parser.add_option("--host", action='append', dest='hosts', default=[],
                      metavar='HOST',
                      help="add a host to test the release on")

    (options, args) = parser.parse_args(sys.argv[1:])
    
    if options.hosts is None:
        hosts = [h.strip() for h 
                   in os.environ.get('OPENMDAO_TEST_HOSTS','').split(':')]
    
    if options.hosts is None:
        print "you must supply host(s) to test the release on"
        sys.exit(-1)
        
    try:
        # TODO: run these concurrently
        for host in hosts:
            with settings(host_string=host):
                print "testing on host %s" % host
                _test_remote(options.fname, pyversion=options.pyversion,
                             keep=options.keep, args=args)
    finally:
        # ensure that all network connections are closed
        # TODO: once we move to Fabric 0.9.4, just use disconnect_all() function
        for key in connections.keys():
            connections[key].close()
            del connections[key]
            
