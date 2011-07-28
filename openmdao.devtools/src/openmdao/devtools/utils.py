import sys
import os
import shutil
import urllib2
from optparse import OptionParser
from subprocess import Popen, STDOUT, PIPE, check_call
import socket
import paramiko

import tempfile
import tarfile

from fabric.api import run, local, env, put, cd, prompt, hide, show, get, settings
from fabric.state import connections

from openmdao.util.fileutil import find_up

class VersionError(RuntimeError):
    pass

def fabric_cleanup(debug=False):
    """close all active fabric connections"""
    for key in connections.keys():
        if debug:
            print 'closing connection %s' % key
        connections[key].close()
        del connections[key]

def check_openmdao_version(release_dir, version):
    """Checks to see if the specified version of openmdao already exists on
    the current active host.
    """
    # TODO: make this smarter.  maybe have it grab the latest version via pkg_resources
    # or packaging and see if it's the same or newer then the specified version
    with settings(hide('running', 'stdout')):
        result = run('ls %s/downloads' % release_dir)
    lst = [x.strip() for x in result.split('\n')]
    if version in lst:
        raise VersionError('Version %s already exists. Please specify a different version' % version)
    return version


def get_openmdao_version(release_dir, version=None):
    if version is not None:
        try:
            version = check_openmdao_version(release_dir, version)
        except VersionError, err:
            print str(err),'\n'
            version = None
       
    if version is None:
        version = prompt('Enter version id:', 
                         validate=lambda ver: check_openmdao_version(release_dir, ver))
    return version


def push_and_run(fpath, remotepath=None, runner=None, args=()):
    """Puts the given file onto the current active host and executes it there"""
    if not os.path.isfile(fpath):
        raise IOError("can't find file %s" % fpath)
    if remotepath is None:
        remotepath = fpath
    put(fpath, remotepath)
    if not runner:
        if fpath.endswith('.py'):
            runner = 'python'
        else:
            runner = ''
            
    retval = run("%s %s %s" % (runner, remotepath, ' '.join(args)))
    
    return retval.return_code

def tar_dir(dirpath, archive_name, destdir):
    """Tar up the given directory and put in in the specified destination
    directory.
    """
    dirpath = os.path.abspath(dirpath)
    destdir = os.path.abspath(destdir)
    startdir = os.getcwd()
    os.chdir(os.path.dirname(dirpath))
    tarpath = os.path.abspath(os.path.join(destdir,'%s.tar.gz' % archive_name))
    try:
        archive = tarfile.open(tarpath, 'w:gz')
        archive.add(os.path.basename(dirpath))
        archive.close()
    finally:
        os.chdir(startdir)
    return tarpath

def ssh_test(host, port=22, timeout=3):
    """Returns true if we can connect to the host via ssh."""
    # Set the timeout
    original_timeout = socket.getdefaulttimeout()
    socket.setdefaulttimeout(timeout)
    try:
        transport = paramiko.Transport((host, port))
        transport.close()
        return True
    except:
        pass
    finally:
        socket.setdefaulttimeout(original_timeout)
    return False

def get_platform():
    """Returns the platform string of the current active host."""
    with settings(hide('running', 'stderr'), warn_only=True):
        return run('python -c "import sys; print sys.platform"')

def check_setuptools(py):
    """Return True if setuptools is installed on the remote host"""
    with settings(hide('everything'), warn_only=True):
        return run('%s -c "import setuptools; print setuptools.__version__"' % py)
        
def py_cmd(cmds):
    """Given a list of python statements, returns a string containing
    the 'python -c' command that will execute those statements. 
    """
    for cmd in cmds:
        if '"' in cmd:
            raise ValueError("use single quotes for strings in commands")
    return 'python -c "' + ';'.join(cmds) + '"'

def remote_untar(tarfile):
    """Use internal python tar package to untar a file in the current remote
    directory instead of assuming that tar exists on the remote machine.
    """
    cmds = [ "import tarfile",
             "tar = tarfile.open('%s')" % tarfile,
             "tar.extractall()",
             "tar.close()",
             ]
    run(py_cmd(cmds))
    
def remote_cat(f1, f2, out):
    """Use python to concatenate two files remotely."""
    cmds = [ "f1=open('%s', 'rb')" % f1,
             "f2=open('%s', 'rb')" % f2,
             "out=open('%s', 'wb')" % out,
             "out.write(f1.read())",
             "out.write(f2.read())",
             "out.close()"
        ]
    run(py_cmd(cmds))
    
def get_plat_spec_cmds():
    plat = get_platform()
    if plat.startswith('win'):
        mover = 'move'
        remover = 'del'
        lister = 'dir /B'
    else:
        mover = 'mv'
        remover = 'rm -f'
        lister = 'ls -1'
    return mover, remover, lister

def remote_tmpdir():
    """Create and return the name of a temporary directory at the remote
    location.
    """
    with settings(show('stdout')):
        return run('python -c "import tempfile; print tempfile.mkdtemp()"')

def rm_remote_tree(pathname):
    """Delete the directory at the remote location."""
    run("""python -c "import shutil; shutil.rmtree('%s')" """ % pathname)
    
def list_remote_dir(dirname):
    return run("""python -c "import os; print os.listdir('%s')" """ % dirname)
            


def put_untar(local_path, remote_dir=None, renames=()):
    """Put the given tarfile on the current active host and untar it in the
    specified place. If remote_dir is not specified, a temp directory will 
    be created and used. If renames is not empty, it should contain tuples
    of the form (oldname, newname), and each file or dir named oldname will
    be renamed to newname.  oldname and newname should be relative to the
    top of the untarred archive.
    
    Returns the remote directory where the file was untarred.
    """
    tarname = os.path.basename(local_path)
    mover, remover, lister = get_plat_spec_cmds()
    
    if remote_dir is None:
        remote_dir = remote_tmpdir()

    abstarname = os.path.join(remote_dir, tarname)
    put(local_path, abstarname)
    with cd(remote_dir):
        remote_untar(tarname)
        for oldname, newname in renames:
            run('%s %s %s' % (mover, oldname.strip(['/','\\']), 
                              newname.strip(['/','\\'])))
    run('%s %s' % (remover, abstarname))
    return remote_dir


def put_dir(src, dest=None, renames=()):
    """Tar the specified directory, upload it to the current active
    host, untar it, and perform renaming if necessary.
    
    Returns the remote directory where the directory was untarred.
    """
    tmpdir = tempfile.mkdtemp()
    tarpath = tar_dir(src, os.path.basename(src), tmpdir)
    remote_dir = dest
    if dest is not None:
        remote_dir = os.path.dirname(dest)
    remotedir = put_untar(tarpath, remote_dir=remote_dir, renames=renames)
    shutil.rmtree(tmpdir)
    return remotedir
    
    
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
    whichpy = 'python%d.%d' % pyversion
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
            
    pkg = list_remote_dir(remtmp).strip()  # should only have one file in directory
    pkg = pkg[2:len(pkg)-2]
    
    get(os.path.join(remtmp, pkg), pkg)
    
    for name in locals_to_remove:
        os.remove(name)

    rm_remote_tree(remtmp)
    rm_remote_tree(remotedir)
    
def rsync_dirs(dest, host, dirs=('downloads','dists'),
               doit=os.system):
    """Use rsync to sync the specified directories on the specified host
    with the corresponding directories in the specified destination directory.
    
    This requires ssh access without a password to the host.
    """
    for dname in dirs:
        doit('rsync -arvzt --delete %s:%s %s' % (host, dname, dest))

        

#
# Git related utilities
#

def repo_top():
    """Return the top level directory in the current git repository"""
    # apparently --show-toplevel doesn't work until git 1.7 :(
    #p = Popen('git rev-parse --show-toplevel', 
              #stdout=PIPE, stderr=STDOUT, env=os.environ, shell=True)
    #return p.communicate()[0].strip()
    d = find_up('.git')
    if d is None:
        return d
    return os.path.dirname(d)
    

def get_git_log_info(fmt):
    try:
        p = Popen('git log -1 --format=format:"%s"' % fmt, 
                  stdout=PIPE, stderr=STDOUT, env=os.environ, shell=True)
        out = p.communicate()[0]
        ret = p.returncode
    except:
        return ''
    else:
        return out.strip()

def get_git_branch():
    p = Popen('git branch', 
              stdout=PIPE, stderr=STDOUT, env=os.environ, shell=True)
    brlist = [b.strip() for b in p.communicate()[0].split('\n')]
    for b in brlist:
        if b.startswith('*'):
            return b[2:]
    return ''

def get_git_branches():
    p = Popen('git branch', 
              stdout=PIPE, stderr=STDOUT, env=os.environ, shell=True)
    return [b.strip(' *') for b in p.communicate()[0].split('\n')]


def make_git_archive(tarfilename, prefix='testbranch/'):
    """Make a tar file of the entire current repository."""
    startdir = os.getcwd()
    os.chdir(repo_top())
    try:
        check_call(['git', 'archive', '-o',
                    tarfilename, '--prefix=%s' % prefix, 'HEAD'])
    finally:
        os.chdir(startdir)

