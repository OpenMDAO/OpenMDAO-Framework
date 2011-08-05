import sys
import os
import shutil
import urllib2
import atexit
from optparse import OptionParser

import tempfile
import tarfile

from fabric.api import run, env, local, put, cd, get, settings, prompt, hide, hosts
from fabric.state import connections

import paramiko.util
paramiko.util.log_to_file('paramiko.log')

from openmdao.devtools.utils import put_dir, remote_check_setuptools, \
                                    remote_tmpdir, remote_build, \
                                    remote_listdir, rm_remote_tree, fabric_cleanup

    
def main(argv=None):
    if argv is None:
        argv = sys.argv[1:]

    startdir=os.getcwd()
    
    parser = OptionParser()
    parser.add_option("--host", action="append", dest="hosts", default=[],
                      help="add url of a non-Windows host to build the package on")
    parser.add_option("--winhost", action="append", dest="winhosts", default=[],
                      help="add url of a Windows host to build the package on")
    parser.add_option("-d", "--dest", action="store", type='string', dest="dest",
                      help="destination directory where built package will go")
    parser.add_option("-s", "--src", action="store", type='string', dest="src",
                      help="source directory where package is located")  
    parser.add_option("-b", "--buildtype", action="store", type='string', 
                      dest="btype", default='build -f bdist_egg',
                      help="type of distribution to build")
    parser.add_option("--pyversion", action="store", type='string', 
                      dest="pyversion", default='2.6',
                      help="version of python to use (default='2.6')")
    
    (options, args) = parser.parse_args(argv)
    
    if not options.src:
        print "You must define a source directory"
        parser.print_help()
        sys.exit(-1)
    if not options.dest:
        options.dest = os.getcwd()

    for host in options.hosts:
        with settings(host_string=host):
            remote_build(options.src, options.dest, build_type=options.btype,
                         pyversion=options.pyversion)
    for host in options.winhosts:
        with settings(host_string=host, shell='cmd /C'):
            remote_build(options.src, options.dest, build_type=options.btype,
                         pyversion=options.pyversion)
    
if __name__ == '__main__':
    atexit.register(fabric_cleanup, True)
    main()

    