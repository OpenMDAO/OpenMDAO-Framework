import sys
import os
import urllib2
import atexit

import tempfile
import tarfile

from argparse import ArgumentParser

from openmdao.devtools.utils import put_dir, remote_check_setuptools, \
                                    remote_tmpdir, \
                                    remote_listdir, rm_remote_tree, fabric_cleanup
from openmdao.devtools.remote_cfg import add_config_options, process_options, \
                                         run_host_processes, get_tmp_user_dir, \
                                         print_host_codes
from openmdao.util.fileutil import cleanup
    
def remote_build(srcdirs=(), destdir=None, build_type='bdist_egg',
                 py=None, remote_dir=None, debug=False, **kwargs):
    """Take the Python distribution in the given directory, tar it up,
    ship it over to host, build it, and bring it back, placing it
    in the specified destination directory.
    """
    from fabric.api import run, put, get

    locdistbld = os.path.join(os.path.dirname(__file__), 'locdistbld.py')
    pkgs = []
    if remote_dir is None:
        remote_dir = get_tmp_user_dir()
        clean_remotedir = True
    else:
        clean_remotedir = False

    for srcdir in srcdirs:
        put_dir(srcdir, os.path.join(remote_dir,
                                     os.path.basename(srcdir)))
        pkgname = os.path.basename(srcdir)
        pkgdir = os.path.join(remote_dir, pkgname)
        remotebuilder = os.path.join(pkgdir, 'locdistbld.py')
        put(locdistbld, remotebuilder)
        remtmp = remote_tmpdir()
        
        result = run('%s %s -s %s -d %s -b "%s"' % (py, 
                                                    remotebuilder,
                                                    pkgdir,
                                                    remtmp,
                                                    build_type))
        
        #print result
                
        pkg = remote_listdir(remtmp)[0]  # should only have one file in directory
        pkgpath = os.path.join(destdir, pkg)
        
        print 'retrieving built distribution %s' % pkgpath
        get(os.path.join(remtmp, pkg), pkgpath)
        
        pkgs.append(pkgpath)
        
        if debug:
            print 'removing %s' % remtmp
        rm_remote_tree(remtmp)
        
    if clean_remotedir:
        if debug:
            print 'removing %s' % remote_dir
        rm_remote_tree(remote_dir)
    
    return 0

def main(argv=None):
    import paramiko.util

    atexit.register(fabric_cleanup, True)
    paramiko.util.log_to_file('paramiko.log')
    if argv is None:
        argv = sys.argv[1:]

    parser = add_config_options(ArgumentParser())
    
    parser.add_argument("-d", "--dest", action="store", type=str, 
                        dest="dest", default='.',
                        help="destination directory where built package will go")
    parser.add_argument("-s", "--src", action="append", type=str, 
                        dest="srcs", default=[],
                        help="source directory where package is located")  
    parser.add_argument("-b", "--buildtype", action="store", type=str, 
                        dest="btype", default='bdist_egg',
                        help="type of distribution to build")
    parser.add_argument("--py", action="store", type=str, 
                        dest="py", default='python',
                        help="which python to use (default='python'")
    parser.add_argument("-k","--keep", action="store_true", dest='keep',
                        help="if there are build failures, don't delete "
                           "the temporary build directory "
                           "or terminate the remote instance if testing on EC2.")

    options = parser.parse_args()
    
    config, conn, image_hosts = process_options(options)
    
    if not options.srcs:
        print "You must specify one or more source directories"
        parser.print_help()
        sys.exit(-1)
        
    options.srcs = [os.path.abspath(os.path.expanduser(s)) for s in options.srcs]

    funct_kwargs = { 'keep': options.keep,
                     'srcdirs': options.srcs,
                     'destdir': os.path.abspath(os.path.expanduser(options.dest)), 
                     'build_type': options.btype,
                     'py': options.py,
                     }
    
    retval = run_host_processes(config, conn, image_hosts, options, 
                                funct=remote_build, 
                                funct_kwargs=funct_kwargs,
                                done_functs=[print_host_codes])
    
    if retval == 0:
        cleanup('paramiko.log')

    sys.exit(retval)
    
if __name__ == '__main__':
    main()

    
