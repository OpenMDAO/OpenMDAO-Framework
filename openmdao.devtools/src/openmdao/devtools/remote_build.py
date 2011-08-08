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
                                    remote_tmpdir, \
                                    remote_listdir, rm_remote_tree, fabric_cleanup

    
def remote_build(srcdir, destdir, build_type='build -f bdist_egg',
                 pyversion=None):
    """Take the python distribution in the given directory, tar it up,
    ship it over to host, build it, and bring it back, placing it
    in the specified destination directory.
    """
    locals_to_remove = []
    srcdir = os.path.abspath(srcdir)
    if pyversion is None:
        pyversion = "%d.%d" % (sys.version_info[0], sys.version_info[1])
    remotedir = put_dir(srcdir)
    whichpy = 'python%s' % pyversion
    pkgname = os.path.basename(srcdir)
    pkgdir = os.path.join(remotedir, pkgname)

    with cd(pkgdir):
        remtmp = remote_tmpdir()
        run("%s setup.py %s -d %s" % (whichpy, build_type, remtmp))
            
    pkg = remote_listdir(remtmp)[0]  # should only have one file in directory
    pkgpath = os.path.join(destdir, pkg)
    
    get(os.path.join(remtmp, pkg), pkgpath)
    
    for name in locals_to_remove:
        os.remove(name)

    rm_remote_tree(remtmp)
    rm_remote_tree(remotedir)
    
    return pkgpath

def main(argv=None):
    if argv is None:
        argv = sys.argv[1:]

    startdir=os.getcwd()
    
    parser = OptionParser()
    parser.add_option("--host", action="append", dest="hosts", default=[],
                      help="add url of a non-Windows host to build the package on")
    parser.add_option("-d", "--dest", action="store", type='string', dest="dest",
                      help="destination directory where built package will go")
    parser.add_option("-s", "--src", action="store", type='string', dest="src",
                      help="source directory where package is located")  
    parser.add_option("-b", "--buildtype", action="store", type='string', 
                      dest="btype", default='build -f bdist_egg',
                      help="type of distribution to build")
    parser.add_option("--py", action="store", type='string', 
                      dest="whichpy", default='python',
                      help="which python to use (default='python'")
    
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
                         pyversion=options.whichpy)
    
if __name__ == '__main__':
    atexit.register(fabric_cleanup, True)
    main()

    