import sys
import os
import shutil
import urllib2
import atexit

import tempfile
import tarfile

from fabric.api import run, env, local, put, cd, get, settings, prompt, hide, hosts
from fabric.state import connections

import paramiko.util

from openmdao.devtools.utils import put_dir, remote_check_setuptools, \
                                    remote_tmpdir, \
                                    remote_listdir, rm_remote_tree, fabric_cleanup

from openmdao.devtools.remote_cfg import CfgOptionParser, process_options, \
                                         run_host_processes
    
def remote_build(srcdir=None, destdir=None, build_type='build -f bdist_egg',
                 py=None, remote_dir=None, debug=False, **kwargs):
    """Take the python distribution in the given directory, tar it up,
    ship it over to host, build it, and bring it back, placing it
    in the specified destination directory.
    """
    if py is None:
        py = "python%d.%d" % (sys.version_info[0], sys.version_info[1])
    remotedir = put_dir(srcdir, dest=os.path.join(remote_dir,
                                                  os.path.basename(srcdir)))
    pkgname = os.path.basename(srcdir)
    pkgdir = os.path.join(remotedir, pkgname)

    locdistbld = os.path.join(os.path.dirname(__file__), 'locdistbld.py')
    remotebuilder = os.path.join(pkgdir, 'locdistbld.py')
    put(locdistbld, remotebuilder)
    remtmp = remote_tmpdir()
    
    run('%s %s -s %s -d %s -b "%s"' % (py, 
                                       remotebuilder,
                                       pkgdir,
                                       remtmp,
                                       build_type))
            
    pkg = remote_listdir(remtmp)[0]  # should only have one file in directory
    pkgpath = os.path.join(destdir, pkg)
    
    print 'retrieving built distribution %s' % pkgpath
    get(os.path.join(remtmp, pkg), pkgpath)
    
    if debug:
        print 'removing %s' % remtmp
    rm_remote_tree(remtmp)
    if debug:
        print 'removing %s' % remotedir
    rm_remote_tree(remotedir)
    
    return pkgpath

def main(argv=None):
    if argv is None:
        argv = sys.argv[1:]

    startdir=os.getcwd()
    
    parser = CfgOptionParser()
    parser.add_option("-d", "--dest", action="store", type='string', 
                      dest="dest", default='.',
                      help="destination directory where built package will go")
    parser.add_option("-s", "--src", action="store", type='string', dest="src",
                      help="source directory where package is located")  
    parser.add_option("-b", "--buildtype", action="store", type='string', 
                      dest="btype", default='build -f bdist_egg',
                      help="type of distribution to build")
    parser.add_option("--py", action="store", type='string', 
                      dest="py", 
                      help="which python to use (default='python'")
    parser.add_option("-k","--keep", action="store_true", dest='keep',
                      help="if there are build failures, don't delete "
                           "the temporary build directory "
                           "or terminate the remote instance if testing on EC2.")

    (options, args) = parser.parse_args(argv)
    
    config, conn, image_hosts = process_options(options)
    
    if not options.src:
        print "You must define a source directory"
        parser.print_help()
        sys.exit(-1)

    funct_kwargs = { 'keep': options.keep,
                     'srcdir': os.path.abspath(os.path.expanduser(options.src)),
                     'destdir': os.path.abspath(os.path.expanduser(options.dest)), 
                     'build_type': 'build -f bdist_egg',
                     'py': options.py,
                     'remote_dir': options.remotedir,
                     }
    run_host_processes(config, conn, image_hosts, options, 
                       funct=remote_build, funct_kwargs=funct_kwargs)
    
    
if __name__ == '__main__':
    atexit.register(fabric_cleanup, True)
    paramiko.util.log_to_file('paramiko.log')
    main()

    