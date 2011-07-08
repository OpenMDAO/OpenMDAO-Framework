"""
This module contains everything necessary to install, activate, and test
an OpenMDAO release environment.
"""

import sys
import os
import shutil
import urllib2
import subprocess
import tempfile
from optparse import OptionParser


def get_file(url):
    """Makes a copy of the specified file in the current directory.
    The file may be available via a local filename or a URL.
    """
    fname = os.path.basename(url)
    if script_url.startswith('http'):
        resp = urllib2.urlopen(script_url)
        gofile = open(fname, 'wb')
        shutil.copyfileobj(resp.fp, gofile)
        gofile.close()
    else: # release in local file system
        shutil.copy(url, fname)


def install_release(site_url, version, pyversion):
    """
    Installs an OpenMDAO release in the current directory.
    
    site_url: str
        The go-openmdao.py file will be copied from here. This
        can either be a URL or a local directory name.
        
    version: str
        The version of OpenMDAO to be tested.
        
    pyversion: str
        The version of python, e.g., '2.6' or '2.7', to be
        used to create the OpenMDAO virtual environment. Only
        major version numbers should be used, i.e., use '2.6'
        rather then '2.6.5'.
    
    Returns the relative name of the newly built release directory.
    """
    get_file('%s/downloads/%s/go-openmdao.py' % (site_url, version))
    
    dirfiles = set(os.listdir('.'))
    
    try:
        print "building openmdao environment"
        subprocess.check_call([pyversion, 'go-openmdao.py'])
        newfiles = set(os.listdir('.')) - dirfiles
        if len(newfiles) != 1:
            raise RuntimeError("didn't expect %s in build directory" % 
                               list(newfiles))
        releasedir = newfiles.pop()
    finally:
        os.chdir(startdir)

    return releasedir
    

def activate_and_test(envdir, testargs=()):
    """"
    Runs the test suite on an OpenMDAO virtual environment located
    in the specified directory.
    
    Returns the return code of the process that runs the test suite.
    """
    if sys.platform.startswith('win'):
        devbindir = 'Scripts'
        command = 'activate.bat && openmdao_test %s' % ' '.join(testargs)
    else:
        devbindir = 'bin'
        command = '. ./activate && openmdao_test %s' % ' '.join(testargs)
        
    # activate the environment and run tests
    devbinpath = os.path.join(envdir, devbindir)
    os.chdir(devbinpath)
    print("running tests from %s" % devbinpath)
    env = os.environ.copy()
    for name in ['VIRTUAL_ENV','_OLD_VIRTUAL_PATH','_OLD_VIRTUAL_PROMPT']:
        if name in env: 
            del env[name]
    proc = subprocess.Popen(command, shell = True, cwd = os.getcwd(), env=env)
    proc.wait()
    return proc.returncode
    

if __name__ == '__main__':
    parser = OptionParser(usage="%prog [OPTIONS] testargs")
    parser.add_option("-s", "--site", type="string", dest='site', 
                      default='http://openmdao.org',
                      help="URL where release files are located. "
                           "This can be just a directory in the file system as well.")
    parser.add_option("-v", "--version", action="store", type='string', dest='version', 
                      help="version to test", default='latest')
    parser.add_option("--pyversion", action="store", type='string', dest='pyversion',
                      default="", help="python version to use")
    parser.add_option("-k","--keep", action="store_true", dest='keep',
                      help="don't delete temporary build directory")

    (options, args) = parser.parse_args(sys.argv[1:])
    
    startdir = os.getcwd()
    tmpdir = tempfile.mkdtemp()
    os.chdir(tmpdir)
    
    retcode = -1
    try:
        reldir = install_release(options.site, options.version, 
                               pyversion="python%s"%options.pyversion)
        retcode = activate_and_test(os.path.join(tmpdir,reldir),
                                    testargs=args)
        print 'return code from test was %s' % retcode

    finally:
        os.chdir(startdir)
        if options.keep:
            print "keeping temp dir %s" % tmpdir
        else:
            print "removing temp dir %s" % tmpdir
            shutil.rmtree(tmpdir)

    sys.exit(retcode)
    