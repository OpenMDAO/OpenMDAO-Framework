
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


def _testremote(site_url, version, args):
    loctstfile = os.path.join(os.path.dirname(__file__), 'loctstrelease.py')
    push_and_run(loctstfile, 
                 remotepath=os.path.basename(loctstfile),
                 args=['--site=%s'%site_url, '--version=%s'%version]+args)


if __name__ == '__main__':
    parser = OptionParser()
    parser.add_option("-s", "--site", type="string", dest='site', 
                      default='http://openmdao.org',
                      help="URL where release files are located. "
                           "This can be just a directory in the file system as well.")
    parser.add_option("-v", action="store", type='string', dest='version', 
                      help="version to test", default='latest')
    parser.add_option("--host", action='append', dest='hosts', default=[],
                      metavar='HOST',
                      help="add a host url to test the release on")

    (options, args) = parser.parse_args(sys.argv[1:])
    
    
    if options.hosts:
        hosts = options.hosts
    else:
        hosts = ['storm.grc.nasa.gov', 'torpedo.grc.nasa.gov', 'viper.grc.nasa.gov']
        
    try:
        # TODO: run these concurrently
        for host in hosts:
            with settings(host_string=host):
                print "testing on host %s" % host
                _testremote(options.site, options.version, args=args)
    finally:
        # ensure that all network connections are closed
        # TODO: once we move to Fabric 0.9.4, just use disconnect_all() function
        for key in connections.keys():
            connections[key].close()
            del connections[key]
