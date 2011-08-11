import sys
import os
import shutil
import urllib2
import subprocess
import tarfile
import codecs
from optparse import OptionParser


def get_file(url):
    """Copies the specified file into the current directory if the
    file is remote.  Otherwise, just returns the path that it's given.
    """
    fname = os.path.basename(url)
    if url.startswith('http'):
        resp = urllib2.urlopen(url)
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
    
    Returns the name of the newly built release directory.
    """
    gofile = get_file(url)
    
    if os.path.basename(gofile) != 'go-openmdao.py':
        print "Name of OpenMDAO bootstrapping script must be 'go-openmdao.py',",
        print " not '%s'" % os.path.basename(gofile)
        sys.exit(-1)
    
    # parse pathname to find dists dir
    parts = list(os.path.split(url))
    parts = parts[:-3] + ['dists']
    dpath = os.path.join(parts)
    command = [pyversion, gofile]
    if os.path.isdir(dpath): 
        command.append('--disturl=%s' % os.path.join(parts))
    
    print "building openmdao environment [%s]" % ' '.join(command)
    
    startdir = os.getcwd()
    _run_gofile(startdir, os.path.join(startdir, gofile), pyversion)
    
    newfiles = set(os.listdir('.')) - dirfiles
    if len(newfiles) != 1:
        raise RuntimeError("didn't expect %s in build directory" % 
                           list(newfiles))
    releasedir = os.path.join(startdir, newfiles.pop())

    return (releasedir, p.returncode)
    
def _run_gofile(stardir, gopath, pyversion):
    godir, gofile = os.path.split(gopath)
    os.chdir(godir)
    
    # in some cases there are some unicode characters in the
    # build output which cause fabric to barf, so strip out unicode
    # by writing to a file, replacing unicode chars with '?'
    f = codecs.open('build.out', 'wb', 
                    encoding='ascii', errors='replace')
    
    try:
        p = subprocess.Popen('%s %s' % (pyversion, gofile), 
                             stdout=f, stderr=subprocess.STDOUT,
                             shell=True)
        p.wait()
    finally:
        f.close()
        with open('build.out', 'r') as f:
            print f.read()
        os.chdir(startdir)

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
            treedir = os.path.splitext(os.path.basename(url))[0]
        treedir = os.path.abspath(treedir)
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
    
    gopath = os.path.join(treedir, 'go-openmdao-dev.py')
    
    _run_gofile(startdir, gopath, pyversion)
            
    envdir = os.path.join(treedir, 'devenv')
    print 'new openmdao environment built in %s' % envdir
    
    return (envdir, p.returncode)
    

if __name__ == '__main__':
    parser = OptionParser(usage="%prog [OPTIONS]")
    parser.add_option("--pyversion", action="store", type='string', 
                      dest='pyversion',
                      default="python", 
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
    
    retcode = -1
    if options.fname is None:
        parser.print_help()
        print "\nYou must supply the URL or pathname of a tarfile, git repo, or a go-openmdao.py file"
        sys.exit(retcode)
        
    startdir = os.getcwd()
    tmpdir = os.path.abspath(os.path.expanduser(options.directory))
    if not os.path.exists(tmpdir):
        os.mkdir(tmpdir)
    os.chdir(tmpdir)
    
    fname = options.fname
    
    if fname.endswith('.tar.gz') or fname.endswith('.tar') or fname.endswith('.git'):
        test_type = 'dev'
    elif fname.endswith('.py'):
        test_type = 'release'
    else:
        parser.print_help()
        print "\nfilename must end in '.tar.gz', '.tar', or '.git'"
        sys.exit(retcode)
        
    if '.' in options.pyversion:
        parts = options.pyversion.split('.')
        if len(parts) > 2:
            print "For python version, use only major.minor version numbers, e.g., ",
            print "'python2.6' not 'python2.6.5'"
            sys.exit(retcode)

    try:
        if test_type == 'release':
            envdir, retcode = install_release(fname, pyversion=options.pyversion)
        else: # dev test
            envdir, retcode = install_dev_env(fname, pyversion=options.pyversion,
                                              branch=options.branch)
    finally:
        os.chdir(startdir)

    if retcode != 0:
        print "problem during build of environment (return code = %s)" % retcode

    sys.exit(retcode)
