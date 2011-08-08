import sys
import os
import time
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
from fabric.network import connect

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
    """Puts the given file onto the current active host and 
    executes it there.
    """
    if not os.path.isfile(fpath):
        raise IOError("can't find file %s" % fpath)
    if remotepath is None:
        remotepath = os.path.basename(fpath)
    put(fpath, remotepath)
    if not runner:
        if fpath.endswith('.py'):
            runner = 'python'
        else:
            runner = ''

    print 'cd-ing to %s' % os.path.dirname(remotepath)
    with cd(os.path.dirname(remotepath)):
        cmd = "%s %s %s" % (runner, os.path.basename(remotepath), 
                            ' '.join(args))
        print 'running %s' % cmd
        return run(cmd)

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

def connection_test(user, host, port=22, max_tries=10, debug=False):
    tries = 0
    while True:
        try:
            connection = connect(user, host, port)
        except SystemExit as err:
            if 'Timed out' in str(err):
                if debug:
                    print 'connection to %s timed out. trying again...' % host
                tries += 1
                if tries > max_tries:
                    raise RuntimeError("failed to connect to %s@%s after %d tries" %
                                       (user, host, tries))
            else:
                raise
    return connection


# this is a modified version of fabric.network.connect that will
# retry a number of times in the case of timeouts or 'no session'
# errors
def fab_connect(user, host, port=22, max_tries=10, sleep=10, debug=False):
    """
    Create and return a new SSHClient instance connected to given host.
    """
    client = paramiko.SSHClient()

    # Load known host keys (e.g. ~/.ssh/known_hosts) unless user says not to.
    if not env.disable_known_hosts:
        client.load_system_host_keys()
    # Unless user specified not to, accept/add new, unknown host keys
    if not env.reject_unknown_hosts:
        client.set_missing_host_key_policy(paramiko.AutoAddPolicy())

    password = env.password
    tries = 0

    # Loop until successful connect (keep prompting for new password)
    while tries < max_tries:
        
        # Attempt connection
        try:
            client.connect(
                hostname=host,
                port=int(port),
                username=user,
                password=password,
                key_filename=env.key_filename,
                timeout=10,
                allow_agent=not env.no_agent,
                look_for_keys=not env.no_keys
            )
            return client
        except paramiko.SSHException as e:
            if 'No existing session' in str(e):
                tries += 1
                if debug:
                    print "connection attempt %d for host %s failed: %s" % (tries, host, str(e))
            else:
                raise
        # Handle timeouts
        except socket.timeout as e:
            tries += 1
            if debug:
                print "connection attempt %d for host %s failed: %s" % (tries, host, str(e))

        time.sleep(sleep)
        
    raise RuntimeError("failed to connect to host %s after %d tries" %
                       (host, tries))

def remote_py_cmd(cmds, remote_dir='.', py='python'):
    """Given a list of python statements, creates a _cmd_.py file, pushes
    it to the remote host, and runs it, returning the result of 'run'.
    """
    cmdname = '_cmd_.py'
    f = open(cmdname, 'w')
    for cmd in cmds:
        f.write("%s\n" % cmd)
    f.close()
    remote_cmd = os.path.join(remote_dir, cmdname)
    put(cmdname, remote_cmd)
    return run('%s %s' % (py, remote_cmd))

def remote_get_platform():
    """Returns the platform string of the current active host."""
    with settings(hide('running', 'stderr'), warn_only=True):
        return remote_py_cmd(["import sys", "print sys.platform"])

def remote_check_setuptools(py):
    """Return True if setuptools is installed on the remote host"""
    with settings(hide('everything'), warn_only=True):
        return remote_py_cmd(["import setuptools"],
                             py).succeeded
    
def remote_check_pywin32(py):
    """Return True if pywin32 is installed on the remote host"""
    with settings(hide('everything'), warn_only=True):
        return remote_py_cmd(["import win32api",
                              "import win32security"
                              "import ntsecuritycon"],
                             py=py).succeeded

def remote_untar(tarfile, remote_dir=''):
    """Use internal python tar package to untar a file in the current remote
    directory instead of assuming that tar exists on the remote machine.
    """
    cmds = [ "import tarfile",
             "tar = tarfile.open('%s')" % tarfile,
             "tar.extractall()",
             "tar.close()",
             ]
    return remote_py_cmd(cmds, remote_dir=remote_dir)
    
def get_plat_spec_cmds():
    plat = remote_get_platform()
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
        return remote_py_cmd(["import tempfile", 
                              "print tempfile.mkdtemp()"])

def remote_mkdir(path):
    """Create a remote directory with the given name. If it already exists,
    just return with no error.
    """
    return remote_py_cmd(["import os",
                          "if not os.path.exists('%s'):" % path,
                          "    os.makedirs('%s')" % path])
    
def remote_listdir(path):
    """Return a list of files found in the given remote directory."""
    with settings(show('stdout')):
        s = remote_py_cmd(["import os",
                           "print os.listdir('%s')" % path])
    s = s.strip()[1:-1]
    return [part.strip("'") for part in s.split(', ')]

def rm_remote_tree(pathname):
    """Delete the directory at the remote location."""
    return remote_py_cmd(["import shutil",
                          "shutil.rmtree('%s')" % pathname])
    

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
        remote_untar(tarname, remote_dir=remote_dir)
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

