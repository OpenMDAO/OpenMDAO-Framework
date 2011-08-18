import sys
import os
import shutil
import subprocess
import codecs
import subprocess
import fnmatch
import getpass
import datetime
import urllib2
import tarfile


def get_file(url):
    """Copies the specified file into the current directory, whether
    the file is local or remote.
    """
    fname = os.path.basename(url)
    if url.startswith('http'):
        resp = urllib2.urlopen(url)
        gofile = open(fname, 'wb')
        shutil.copyfileobj(resp.fp, gofile)
        gofile.close()
    else: # file is in local file system
        if not os.path.isfile(url):
            print "Can't find file '%s'" % url
            sys.exit(-1)
        if os.path.dirname(url) != os.getcwd():
            shutil.copy(url, fname)
    return fname

def _run_gofile(startdir, gopath, pyversion, args=()):
    retcode = -1
    godir, gofile = os.path.split(gopath)
    os.chdir(godir)
    
    outname = 'build.out'
    f = open(outname, 'wb')
    try:
        p = subprocess.Popen('%s %s %s' % (pyversion, gofile, 
                                           ' '.join(args)), 
                             stdout=f, stderr=subprocess.STDOUT,
                             shell=True)
        p.wait()
        retcode = p.returncode
    finally:
        f.close()
        # in some cases there are some unicode characters in the
        # output which cause fabric to barf, so strip out unicode
        # before returning
        with codecs.open(outname, 'rt', encoding='ascii', errors='ignore') as f:
            for line in f:
                print line,
        os.chdir(startdir)
    return retcode

def _run_sub(outname, cmd, env=None):
    f = open(outname, 'wb')
    
    try:
        p = subprocess.Popen(cmd, stdout=f, stderr=subprocess.STDOUT,
                             shell=True, env=env)
        p.wait()
    finally:
        f.close()
        # in some cases there are some unicode characters in the
        # output which cause fabric to barf, so strip out unicode
        # before returning
        with codecs.open(outname, 'rt', encoding='ascii', errors='ignore') as f:
            for line in f:
                print line,
    return p.returncode
        

def build_and_test(fname=None, workdir='.', pyversion='python', keep=False, 
                   branch=None, testargs=()):
    """Builds OpenMDAO, either a dev build or a release build, and runs
    the test suite on it.
    """
    if fname is None:
        raise RuntimeError("build_and_test: missing arg 'fname'")

    if not fname.startswith('http'):
        fname = os.path.abspath(fname)
    
    workdir = os.path.abspath(workdir)
    startdir = os.getcwd()
    
    if fname.endswith('.py'):
        build_type = 'release'
    else:
        build_type = 'dev'
        
    args = ['-f', fname]
        
    args.append('--pyversion=%s' % pyversion)
    if branch:
        args.append('--branch=%s' % branch)
        
    expectedfiles = set(['build.out', os.path.basename(fname)])
    
    os.chdir(workdir)
    
    print 'building...'
    
    try:
        if build_type == 'release':
            envdir, retcode = install_release(fname, pyversion=options.pyversion)
        else: # dev test
            envdir, retcode = install_dev_env(fname, pyversion=options.pyversion,
                                              branch=options.branch)
    finally:
        os.chdir(workdir)

    print "build return code =", retcode
    if retcode != 0:
        sys.exit(retcode)
    
    print '\ntesting...'

    try:
        retcode = activate_and_test(envdir, testargs)
    finally:
        os.chdir(startdir)
    
    return retcode


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
    
    Returns the name of the newly built release directory.
    """
    gofile = get_file(url)
    
    if os.path.basename(gofile) != 'go-openmdao.py':
        print "Name of OpenMDAO bootstrapping script must be 'go-openmdao.py',",
        print " not '%s'" % os.path.basename(gofile)
        sys.exit(-1)
    
    # parse pathname to find dists dir
    parts = url.split(os.sep)
    parts = parts[:-3] + ['dists']
    dpath = os.path.join(*parts)
    if url.startswith(os.sep):
        dpath = os.sep+dpath
    args = []
    if os.path.isdir(dpath): 
        args.append('--disturl=%s' % dpath)
    
    print "building openmdao environment [%s]" % ' '.join(args)
    
    startdir = os.getcwd()
    
    dirfiles = set(os.listdir('.'))
    
    retcode = _run_gofile(startdir, os.path.join(startdir, gofile), 
                          pyversion, args)
    
    newfiles = set(os.listdir('.')) - dirfiles - set(['build.out'])
    if len(newfiles) != 1:
        raise RuntimeError("didn't expect %s in build directory" % 
                           list(newfiles))
    releasedir = os.path.join(startdir, newfiles.pop())

    return (releasedir, retcode)
    

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

    if url.endswith('.git'): # clone the git repo
        if branch is None:
            print "You must supply a branch name for a git repo"
            sys.exit(-1)

        dirfiles = set(os.listdir('.'))
    
        print "cloning git repo at %s" % url
        subprocess.check_call(["git", "clone", url])
        
        base = os.path.basename(url)
        if base == '.git':
            treedir = os.path.dirname(url)
        else:
            treedir = os.path.splitext(os.path.basename(url))[0]
        treedir = os.path.abspath(treedir)
        os.chdir(treedir)
        try:
            subprocess.check_call(['git','checkout',options.branch])
        finally:
            os.chdir(startdir)
    elif url.endswith('.tar.gz') or url.endswith('.tar'):
        tarpath = get_file(url)
        dirfiles = set(os.listdir('.'))
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
    
    gopath = os.path.join(treedir, 'go-openmdao-dev.py')
    
    retcode = _run_gofile(startdir, gopath, pyversion)
            
    envdir = os.path.join(treedir, 'devenv')
    print 'new openmdao environment built in %s' % envdir
    
    return (envdir, retcode)
    

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
    return _run_sub('test.out', command, env=env)
    

if __name__ == '__main__':
    from optparse import OptionParser
    
    parser = OptionParser(usage="%prog [OPTIONS]")
    parser.add_option("--pyversion", action="store", type='string', 
                      dest='pyversion', default="python", 
                      help="python version to use, e.g., 'python2.6'")
    parser.add_option("--branch", action="store", type='string', 
                      dest='branch',
                      help="if file_url is a git repo, supply branch name here")
    parser.add_option("-f","--file", action="store", type='string', 
                      dest='fname',
                      help="pathname or URL of a git repo, tar file, or go-openmdao.py file")
    parser.add_option("-d","--dir", action="store", type='string', 
                      dest='directory', default='.',
                      help="name of a directory the build will be created")

    (options, args) = parser.parse_args(sys.argv[1:])
    
    sys.exit(build_and_test(fname=options.fname, workdir=options.directory,
                            pyversion=options.pyversion, 
                            branch=options.branch, testargs=args))
    
