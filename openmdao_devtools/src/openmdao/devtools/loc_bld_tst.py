import codecs
import os
import shlex
import shutil
import subprocess
import sys
import tarfile
import time
import urllib2


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


def _run_gofile(startdir, gopath, args=()):
    retcode = -1
    godir, gofile = os.path.split(gopath)
    os.chdir(godir)

    outname = 'build.out'
    f = open(outname, 'wb')
    py = sys.executable.replace('\\','/')
    envdict = os.environ.copy()

    # if we're running from inside of a virtualenv, we need to
    # remove VIRTUAL_ENV from the environment before spawning the
    # subprocess, else the build will end up inside of devenv
    # instead of where it's supposed to be
    if 'VIRTUAL_ENV' in envdict:
        del envdict['VIRTUAL_ENV']
    try:
        p = subprocess.Popen('%s %s %s' % (py, gofile, ' '.join(args)),
                             stdout=f, stderr=subprocess.STDOUT,
                             shell=True, env=envdict)
        _wait(p)
        retcode = p.returncode
    finally:
        f.close()
        # in some cases there are some unicode characters in the
        # output which cause fabric to barf, so strip out unicode
        # before returning
        if sys.platform.startswith('win'):
            mode = 'r'
        else:
            mode = 'rt'
        with codecs.open(outname, mode, encoding='ascii', errors='ignore') as f:
            for line in f:
                print line,
        sys.stdout.flush()
        os.chdir(startdir)
    return retcode


def _run_sub(outname, cmd, env=None):
    f = open(outname, 'wb')
    try:
        p = subprocess.Popen(cmd, stdout=f, stderr=subprocess.STDOUT,
                             shell=True, env=env)
        _wait(p)
    finally:
        f.close()
        # in some cases there are some unicode characters in the
        # output which cause fabric to barf, so strip out unicode
        # before returning
        if sys.platform.startswith('win'):
            mode = 'r'
        else:
            mode = 'rt'
        with codecs.open(outname, mode, encoding='ascii', errors='ignore') as f:
            for line in f:
                print line,
        sys.stdout.flush()
    return p.returncode


def _wait(p):
    """ To avoid firewall inactivity timeouts, print while waiting. """
    # Using 'keepalive' options in fabric or paramiko did not fix this problem.
    print 'process launched...'
    sys.stdout.flush()
    start = time.time()
    while p.returncode is None:
        if p.poll() is not None:
            break
        else:
            time.sleep(10)
            if time.time() - start >= 10 * 60:
                print 'waiting for process to finish...'
                sys.stdout.flush()
                start = time.time()


def build_and_test(fname=None, workdir='.', keep=False,
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

    if branch:
        args.append('--branch=%s' % branch)

    os.chdir(workdir)

    print 'building...'
    sys.stdout.flush()

    try:
        if build_type == 'release':
            envdir, retcode = install_release(fname)
        else: # dev test
            envdir, retcode = install_dev_env(fname, branch=options.branch)
    finally:
        os.chdir(workdir)

    print "build return code =", retcode
    sys.stdout.flush()
    if retcode != 0:
        sys.exit(retcode)

    if build_type == 'release':
        for arg in testargs:
            if not arg.startswith('-'):
                break
        else:
            if '--small' not in testargs and '--all' not in testargs:
                testargs.append('--all') # otherwise release test runs small set by default

    print '\ntesting  (testargs=%s) ...' % testargs
    sys.stdout.flush()

    try:
        retcode = activate_and_test(envdir, testargs)
        print "test return code =", retcode
    finally:
        sys.stdout.flush()
        os.chdir(startdir)

    return retcode


def install_release(url):
    """
    Installs an OpenMDAO release in the current directory.

    url: str
        The url of the go-openmdao-???.py file.

    Returns the name of the newly built release directory.
    """
    gofile = get_file(url)

    if not os.path.basename(gofile).startswith('go-openmdao-'):
        print "Name of OpenMDAO bootstrapping script must be 'go-openmdao-<version>.py',",
        print " not '%s'" % os.path.basename(gofile)
        sys.exit(-1)

    # parse pathname to find dists dir
    dn = os.path.dirname
    dpath = os.path.join(dn(dn(dn(url))), 'dists')
    args = []
    if os.path.isdir(dpath):
        args.append('--testurl=%s' % dpath)

    print "building openmdao environment [%s]" % ' '.join(args)

    startdir = os.getcwd()

    dirfiles = set(os.listdir('.'))

    retcode = _run_gofile(startdir, os.path.join(startdir, gofile), args)

    newfiles = set(os.listdir('.')) - dirfiles - set(['build.out', 'openmdao_log.txt'])
    if len(newfiles) != 1:
        raise RuntimeError("didn't expect %s in build directory" %
                           list(newfiles))
    releasedir = os.path.join(startdir, newfiles.pop())

    return (releasedir, retcode)


def install_dev_env(url, branch=None):
    """
    Installs an OpenMDAO dev environment given an OpenMDAO source
    tree.

    url: str
        URL of tarfile or Git repo containing an OpenMDAO source tree.  May be
        a local file path or an actual URL.

    branch: str
        For Git repos, branch name must be supplied.
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
            subprocess.check_call(['git', 'checkout', options.branch])
        finally:
            os.chdir(startdir)
    elif url.endswith('.tar.gz') or url.endswith('.tar'):
        tarpath = get_file(url)
        dirfiles = set(os.listdir('.'))
        tar = tarfile.open(tarpath)
        tar.extractall()
        tar.close()
    else:
        raise RuntimeError("url '%s' does not end in"
                           " '.git' or '.tar.gz' or '.tar'" % url)

    newfiles = set(os.listdir('.')) - dirfiles
    if len(newfiles) != 1:
        raise RuntimeError("didn't expect %s in build directory" %
                           list(newfiles))
    treedir = newfiles.pop()

    print "building openmdao development environment in %s" % treedir

    gopath = os.path.join(treedir, 'go-openmdao-dev.py')

    retcode = _run_gofile(startdir, gopath)

    envdir = os.path.join(treedir, 'devenv')
    print 'new openmdao environment built in %s' % envdir

    return (envdir, retcode)


def activate_and_test(envdir, testargs=()):
    """
    Runs the test suite on an OpenMDAO virtual environment located
    in the specified directory.

    Returns the return code of the process that runs the test suite.
    """
    if sys.platform.startswith('win'):
        devbindir = 'Scripts'
        command = 'activate.bat && openmdao test %s' % ' '.join(testargs)
    else:
        devbindir = 'bin'
        command = '. ./activate && openmdao test %s 2>&1 | tee ../../test.out' \
                  % ' '.join(testargs)

    # activate the environment and run tests
    devbinpath = os.path.join(envdir, devbindir)
    os.chdir(devbinpath)
    print "running tests from %s" % devbinpath
    env = os.environ.copy()
    for name in ['VIRTUAL_ENV', '_OLD_VIRTUAL_PATH', '_OLD_VIRTUAL_PROMPT']:
        if name in env:
            del env[name]
    print "command = ", command
    return _run_sub('test.out', command, env=env)


if __name__ == '__main__':
    from optparse import OptionParser

    parser = OptionParser(usage="%prog [OPTIONS]")
    parser.add_option("--branch", action="store", type='string',
                      dest='branch',
                      help="if file_url is a git repo, supply branch name here")
    parser.add_option("-f", "--file", action="store", type='string',
                      dest='fname',
                      help="pathname or URL of a git repo, tar file,"
                           " or go-openmdao-<version>.py file")
    parser.add_option("-d", "--dir", action="store", type='string',
                      dest='directory', default='.',
                      help="name of a directory the build will be created in")
    parser.add_option("--testargs", action="store", type='string',
                      dest='testargs', default='',
                      help="args to pass to openmdao test")

    # Handle quoting problem that happens on Windows (at least).
    # (--testargs="-v --gui" gets split into: '--testargs="-v', '--gui', '"')
    args = []
    i = 1
    while i < len(sys.argv):
        arg = sys.argv[i]
        i += 1
        while arg.count('"') == 1 and i < len(sys.argv):
            next_arg = sys.argv[i]
            i += 1
            arg += ' '+next_arg
        args.append(arg)

    (options, args) = parser.parse_args(args)

    sys.exit(build_and_test(fname=options.fname, workdir=options.directory,
                            branch=options.branch,
                            testargs=shlex.split(options.testargs)))

