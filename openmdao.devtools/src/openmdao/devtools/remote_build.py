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

from openmdao.devtools.utils import put_dir, check_setuptools, remote_tmpdir, \
                                    remote_listdir, rm_remote_tree, fabric_cleanup

def remote_build(distdir, destdir, build_type='build -f bdist_egg',
                 pyversion=None):
    """Take the python distribution in the given directory, tar it up,
    ship it over to host, build it, and bring it back, placing it
    in the specified destination directory.
    """
    locals_to_remove = []
    distdir = os.path.abspath(distdir)
    if pyversion is None:
        pyversion = sys.version_info[:2]
    remotedir = put_dir(distdir)
    whichpy = 'python%s' % pyversion
    pkgname = os.path.basename(distdir)
    if check_setuptools(whichpy):
        has_setuptools = True
    else:
        has_setuptools = False
        print "Remote host has no setuptools."
        if not os.path.isfile('ez_setup.py'):
            print "Attempting to download ez_setup.py and bootstrap setuptools at the remote location"
            resp = urllib2.urlopen('http://peak.telecommunity.com/dist/ez_setup.py')
            with open('ez_setup.py', 'wb') as easyf:
                shutil.copyfileobj(resp.fp, easyf)
            locals_to_remove.append('ez_setup.py')
    
    pkgdir = os.path.join(remotedir, pkgname)
    if not has_setuptools:
        put('ez_setup.py', os.path.join(pkgdir, 'ez_setup.py'))
        with open('_catfile', 'wb') as catf:
            catf.write("from ez_setup import use_setuptools\n")
            catf.write("use_setuptools(download_delay=0)\n\n")
        locals_to_remove.append('_catfile')
        put('_catfile', os.path.join(pkgdir, '_catfile'))
        remote_cat(os.path.join(pkgdir, '_catfile'), 
                   os.path.join(pkgdir, 'setup.py'), 
                   os.path.join(pkgdir, '_newsetup.py'))

    with cd(pkgdir):
        remtmp = remote_tmpdir()
        if not has_setuptools:
            run("%s _newsetup.py %s -d %s" % (whichpy, build_type, remtmp))
        else:
            run("%s setup.py %s -d %s" % (whichpy, build_type, remtmp))
            
    pkg = remote_listdir(remtmp)[0]  # should only have one file in directory
    
    get(os.path.join(remtmp, pkg), pkg)
    
    for name in locals_to_remove:
        os.remove(name)

    rm_remote_tree(remtmp)
    rm_remote_tree(remotedir)

    
def main(argv=None):
    if argv is None:
        argv = sys.argv[1:]

    startdir=os.getcwd()
    
    parser = OptionParser()
    parser.add_option("--host", action="append", dest="hosts",
                   help="add url of a non-Windows host to build the package on", default=[])
    parser.add_option("--winhost", action="append", dest="winhosts",
                   help="add url of a Windows host to build the package on", default=[])
    parser.add_option("-d", "--dest", action="store", type='string', dest="dest",
                   help="destination directory where built package will go")
    parser.add_option("-s", "--src", action="store", type='string', dest="src",
                   help="source directory where package is located")  
    parser.add_option("-b", "--buildtype", action="store", type='string', dest="btype",
                   help="type of distribution to build",
                   default='build -f bdist_egg')  
    parser.add_option("--pyversion", action="store", type='string', dest="pyversion",
                   help="version of python to use to build package (default='2.6')",
                   default='2.6')
    
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

    