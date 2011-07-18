"""
This module contains everything necessary to install, activate, and test
an OpenMDAO release or development environment.
"""

import sys
import os
import shutil
import urllib2
import subprocess
import tempfile
import tarfile
from optparse import OptionParser


def get_file(url):
    """Copies the specified file into the current directory if the
    file is remote.  Otherwise, just returns the path that it's given.
    """
    fname = os.path.basename(url)
    if script_url.startswith('http'):
        resp = urllib2.urlopen(script_url)
        gofile = open(fname, 'wb')
        shutil.copyfileobj(resp.fp, gofile)
        gofile.close()
        return fname
    else: # file is in local file system
        if not os.path.isfile(url):
            print "Can't find file '%s'" % url
            sys.exit(-1)
        return url


def install_release(url, pyversion):
    """
    Installs an OpenMDAO release in the current directory.
    
    url: str
        The url of the go-openmdao.py file.
        
    pyversion: str
        The version of python, e.g., 'python2.6' or 'python2.7', to be
        used to create the OpenMDAO virtual environment. Only
        major version numbers should be used, i.e., use 'python2.6'
        rather than 'python2.6.5'.
    
    Returns the relative name of the newly built release directory.
    """
    gofile = get_file(url)
    
    if os.path.basename(gofile) != 'go-openmdao.py':
        print "Name of OpenMDAO bootstrapping script must be 'go-openmdao.py',",
        print " not '%s'" % os.path.basename(gofile)
        sys.exit(-1)
    
    dirfiles = set(os.listdir('.'))
    
    try:
        # parse pathname to find dists dir
        parts = os.path.split(url)
        version = parts[-2]
        parts = parts[:-3] + ['dists']
        command = [pyversion, gofile, 
                   '--disturl=%s' % os.path.join(parts)]
        
        print "building openmdao version %s environment [%s]" % (version, 
                                                                 ' '.join(command))
        subprocess.check_call(command)
        
        newfiles = set(os.listdir('.')) - dirfiles
        if len(newfiles) != 1:
            raise RuntimeError("didn't expect %s in build directory" % 
                               list(newfiles))
        releasedir = newfiles.pop()
    finally:
        os.chdir(startdir)

    return releasedir
    

def install_dev_env(url, pyversion, branch=None):
    """
    Installs an OpenMDAO dev environment given an OpenMDAO source
    tree.
    
    url: str
        URL of tarfile or git repo containing an OpenMDAO source tree.  May be
        a local file path or an actual URL.

    pyversion: str
        The version of python, e.g., 'python2.6' or 'python2.7', to be
        used to create the OpenMDAO virtual environment. Only
        major version numbers should be used, i.e., use 'python2.6'
        rather than 'python2.6.5'.

    branch: str
        For git repos, branch name must be supplied.
    """
    startdir = os.getcwd()
    
    # make sure we don't clobber an existing repo
    if os.path.exists('OpenMDAO-Framework'):
        print "Directory OpenMDAO-Framework already exists"
        sys.exit(-1)

    dirfiles = set(os.listdir('.'))
    
    if url.endswith('.git'): # clone the git repo
        if branch is None:
            print "You must supply a branch name for a git repo"
            sys.exit(-1)

        cmd = ["git", "clone", url]
        print "cloning git repo at %s" % url
        subprocess.check_call(cmd)
        
        base = os.path.basename(url)
        if base == '.git':
            treedir = os.path.dirname(url)
        else:
            treedir = os.path.splitext()[0]
        os.chdir(treedir)
        try:
            subprocess.check_call(['git','checkout',options.branch])
        finally:
            os.chdir(startdir)
    elif url.endswith('.tar.gz') or url.endswith('.tar'):
        tarpath = get_file(url)
        tar = tarfile.open(tarpath)
        tar.extractall()
        tar.close()
    else:
        print "url '%s' does not end in '.git' or '.tar.gz' or '.tar'" % url
        sys.exit(-1)
    
    newfiles = set(os.listdir('.')) - dirfiles
    if len(newfiles) != 1:
        raise RuntimeError("didn't expect %s in build directory" % 
                           list(newfiles))
    treedir = newfiles.pop()
        
    print "building openmdao development environment in %s" % treedir
    
    os.chdir(treedir)
    
    try:
        subprocess.check_call([pyversion, 'go-openmdao-dev.py'])
    finally:
        os.chdir(startdir)

    return os.path.join(treedir, 'devenv')
    

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
    parser = OptionParser(usage="%prog [OPTIONS] file_url testargs")
    parser.add_option("--pyversion", action="store", type='string', 
                      dest='pyversion',
                      default="python", 
                      help="python version to use, e.g., 'python2.6'")
    parser.add_option("-k","--keep", action="store_true", dest='keep',
                      help="don't delete temporary build directory")
    parser.add_option("-b","--branch", action="store", type='string', 
                      dest='branch',
                      help="if file_url is a git repo, supply branch name here")

    (options, args) = parser.parse_args(sys.argv[1:])
    
    startdir = os.getcwd()
    tmpdir = tempfile.mkdtemp()
    os.chdir(tmpdir)
    
    retcode = -1
    
    if len(args) == 0:
        parser.print_help()
        print "\nYou must supply a tarfile name or a python file name"
        sys.exit(retcode)
        
    fname = args[0]
    
    if fname.endswith('.tar.gz') or fname.endswith('.tar') or fname.endswith('.git'):
        test_type = 'dev'
    elif fname.endswith('.py'):
        test_type = 'release'
    else:
        parser.print_help()
        print "\nFirst arg must be either a tar.gz file or a python file"
        sys.exit(retcode)
        
    if '.' in options.pyversion:
        parts = options.pyversion.split('.')
        if len(parts) > 2:
            print "For python version, use only major.minor version numbers, e.g., ",
            print "'python2.6' not 'python2.6.5'"
            sys.exit(-1)

    try:
        if test_type == 'release':
            envdir = install_release(fname, pyversion=options.pyversion)
        else: # dev test
            envdir = install_dev_env(fname, pyversion=options.pyversion,
                                     branch=options.branch)
            
        retcode = activate_and_test(os.path.join(tmpdir, envdir),
                                    testargs=args[1:])
        print 'return code from test was %s' % retcode

    finally:
        os.chdir(startdir)
        if options.keep:
            print "keeping temp dir %s" % tmpdir
        else:
            print "removing temp dir %s" % tmpdir
            shutil.rmtree(tmpdir)

    sys.exit(retcode)
    