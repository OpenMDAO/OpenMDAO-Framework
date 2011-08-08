"""
This module builds a binary distribution from the specified source directory.
"""

import sys
import os
import shutil
import urllib2
import subprocess
from optparse import OptionParser


def has_setuptools():
    try:
        import setuptools
    except ImportError:
        return False
    return True


def make_new_setupfile(setupfile):
    """If setuptools is not installed, make a new setup file that will
    bootstrap and use setuptools. The new file will be in the same location
    as setupfile and will have '_new_' prepended to the name.
    """
    setupfile = os.path.abspath(setupfile)
    newsetupfile = os.path.join(os.path.dirname(setupfile),
                                '_new_'+os.path.basename(setupfile))
    
    startdir = os.getcwd()
    
    os.chdir(os.path.dirname(setupfile))
    
    try:
        print "setuptools is not installed."
        if not os.path.isfile('ez_setup.py'):
            print "Attempting to download ez_setup.py"
            resp = urllib2.urlopen('http://peak.telecommunity.com/dist/ez_setup.py')
            with open('ez_setup.py', 'wb') as easyf:
                shutil.copyfileobj(resp.fp, easyf)
            print 'successfully downloaded ez_setup.py'

        print "Attempting to update %s to import from ez_setup" % setupfile
        
        if not os.path.isfile(setupfile):
            raise IOError("can't find setup file '%s'" % setupfile)
        
        setupf = open(setupfile, 'r')
        setup_contents = setupf.read()
        setupf.close()
        
        with open(newsetupfile, 'wb') as newf:
            newf.write("from ez_setup import use_setuptools\n")
            newf.write("use_setuptools(download_delay=0)\n\n")
            newf.write(setup_contents)
    finally:
        os.chdir(startdir)
        
    return newsetupfile


def build_dist(srcdir, destdir='.', pyversion='python', 
               build_type='build -f bdist_egg'):
    """
    Builds a distribution using the specified source directory and places
    it in the specified destination directory.
    
    srcdir: str
        source directory for the distribution to be built
        
    destdir: str
        directory where the built distribution file will be placed

    pyversion: str
        The version of python, e.g., 'python2.6' or 'python2.7', to be
        used to create the distribution. Only
        major version numbers should be used, i.e., use 'python2.6'
        rather than 'python2.6.5'.

    build_type: str
        The type of distribution to be built.  Default is 'build -f bdist_egg'.
    """
    startdir = os.getcwd()
    destdir = os.path.abspath(os.path.expanduser(destdir))
    srcdir = os.path.abspath(os.path.expanduser(srcdir))
    
    setupname = os.path.join(srcdir, 'setup.py')
    if not has_setuptools():
        setupname = make_new_setupfile(setupname)

    dirfiles = set(os.listdir(destdir))
    
    print "building distribution in %s" % srcdir
    
    # FIXME: fabric barfs when running this remotely due to some unicode
    # output that it can't handle, so we just save the output to 
    # a file instead 
    out = open('build.out', 'wb')
    
    cmd = [pyversion,
           os.path.basename(setupname),
        ]
    cmd.extend(build_type.split(' '))

    os.chdir(srcdir)
    
    try:
        p = subprocess.Popen(cmd, 
                             stdout=out, stderr=subprocess.STDOUT,
                             shell=True)
        p.wait()
    finally:
        out.close()
        os.chdir(startdir)
        
    newfiles = set(os.listdir(destdir)) - dirfiles
    if len(newfiles) != 1:
        raise RuntimeError("found multiple new files %s in destination directory" % 
                           list(newfiles))
    if p.returncode != 0:
        raise RuntimeError("problem building distribution in %s. (return code = %s)" %
                           (srcdir, p.returncode))
    
    distfile = os.path.join(destdir, newfiles.pop())
    print 'new distribution file is %s' % distfile
    return distfile

if __name__ == '__main__':
    parser = OptionParser(usage="%prog [OPTIONS]")
    parser.add_option("-s","--src", action="store", type='string', 
                      dest='srcdir',
                      help="name of directory where the distrib source files are located")
    parser.add_option("-d","--dest", action="store", type='string', 
                      dest='destdir', default='.',
                      help="name of directory where the build distrib will be placed")
    parser.add_option("--pyversion", action="store", type='string', 
                      dest='pyversion',
                      default="python", 
                      help="python version to use, e.g., 'python2.6'")
    parser.add_option("-b","--bldtype", action="store", type='string', 
                      dest='buildtype', default='build -f bdist_egg',
                      help="setup.py build command. Default is 'build -f bdist_egg'")

    (options, args) = parser.parse_args(sys.argv[1:])
    
    retcode = -1
        
    startdir = os.getcwd()
    
    if not options.srcdir:
        print "you must supply a source directory"
        parser.print_help()
        sys.exit(retcode)
    
    srcdir = os.path.abspath(os.path.expanduser(options.srcdir))
    destdir = os.path.abspath(os.path.expanduser(options.destdir))
    
    if not os.path.exists(srcdir):
        print "source directory %s not found" % srcdir
        sys.exit(retcode)
        
    if '.' in options.pyversion:
        parts = options.pyversion.split('.')
        if len(parts) > 2:
            print "For python version, use only major.minor version numbers, ",
            print "e.g., 'python2.6' not 'python2.6.5'"
            sys.exit(retcode)

    try:
        distfile = build_dist(srcdir, destdir, options.pyversion, 
                              options.buildtype)
    finally:
        os.chdir(startdir)

        