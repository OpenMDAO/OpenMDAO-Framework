import glob
import logging
import os.path
import shutil
import socket
from subprocess import Popen, STDOUT, PIPE, check_call
import tarfile
import tempfile
import time
import cProfile

try:
    import paramiko
    from fabric.api import run, env, put, get, cd, prompt, hide, show, settings
    from fabric.state import connections
except ImportError as err:
    run = None
    connections = {}
    logging.warn("In %s: %r", __file__, err)

from openmdao.util.fileutil import find_up, onerror
from openmdao.util.decorators import stub_if_missing_deps


class VersionError(RuntimeError):
    pass


@stub_if_missing_deps('fabric')
def fabric_cleanup(debug=False):
    """Close all active fabric connections."""
    for key in connections.keys():
        try:
            if debug:
                print 'closing connection %s' % key
            connections[key].close()
            del connections[key]
        except Exception as err:
            print str(err)


@stub_if_missing_deps('fabric')
def check_openmdao_version(release_dir, version):
    """Checks to see if the specified version of openmdao already exists on
    the current active host.
    """
    # TODO: make this smarter.  maybe have it grab the latest version via
    # pkg_resources or packaging and see if it's the same or newer then the
    # specified version
    with settings(hide('running', 'stdout')):
        result = run('ls %s/downloads' % release_dir)
    lst = [x.strip() for x in result.split('\n')]
    if version in lst:
        raise VersionError('Version %s already exists. Please specify a'
                           ' different version' % version)
    return version


def get_openmdao_version(release_dir, version=None):
    if version is not None:
        try:
            version = check_openmdao_version(release_dir, version)
        except VersionError, err:
            print str(err), '\n'
            version = None

    if version is None:
        version = prompt('Enter version id:',
                         validate=lambda ver: check_openmdao_version(release_dir, ver))
    return version


@stub_if_missing_deps('fabric')
def push_and_run(fpaths, remotedir, runner=None, args=()):
    """Puts the given files onto the current active host in the specified
    remote directory and executes the first file specified.
    """
    for fpath in fpaths:
        put(fpath, os.path.join(remotedir, os.path.basename(fpath)))

    if runner is None:
        runner = 'python' if fpaths[0].endswith('.py') else ''

    print 'cd-ing to %s' % remotedir
    cmd = '%s %s %s' % (runner, os.path.basename(fpaths[0]),
                        ' '.join(args))
    with cd(remotedir):
        print 'running:', cmd
        return run(cmd)


def tar_dir(dirpath, archive_name, destdir):
    """Tar up the given directory and put it in the specified destination
    directory.
    """
    dirpath = os.path.abspath(dirpath)
    destdir = os.path.abspath(destdir)
    startdir = os.getcwd()
    os.chdir(os.path.dirname(dirpath))
    tarpath = os.path.abspath(os.path.join(destdir, '%s.tar.gz' % archive_name))
    try:
        archive = tarfile.open(tarpath, 'w:gz')
        archive.add(os.path.basename(dirpath))
        archive.close()
    finally:
        os.chdir(startdir)
    return tarpath


@stub_if_missing_deps('paramiko')
def ssh_test(host, port=22, timeout=3):
    """Returns True if we can connect to the host via ssh."""
    # Set the timeout
    original_timeout = socket.getdefaulttimeout()
    socket.setdefaulttimeout(timeout)
    try:
        transport = paramiko.Transport((host, port))
        transport.close()
        return True
    except Exception as exc:
        print '    caught exception:', exc
    finally:
        socket.setdefaulttimeout(original_timeout)
    return False


# this is a modified version of fabric.network.connect that will
# retry a number of times in the case of timeouts or 'no session'
# errors
@stub_if_missing_deps('paramiko')
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

    # Loop until successful connect
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
        #except (paramiko.SSHException, socket.timeout) as e:
        except Exception as e:
            tries += 1
            print "connection attempt %d for host %s failed: %s" \
                  % (tries, host, str(e))

        time.sleep(sleep)

    raise RuntimeError("failed to connect to host %s after %d tries" %
                       (host, tries))


@stub_if_missing_deps('fabric')
def remote_py_cmd(cmds, py='python', remote_dir=None):
    """Given a list of Python statements, creates a self-deleting _cmd_.py
    file, pushes it to the remote host, and runs it, returning the result of
    'run'.
    """
    cmdfname = '_cmd_.py'
    with open(cmdfname, 'w') as f:
        f.write("import os\n")
        for cmd in cmds:
            f.write("%s\n" % cmd)
        f.write("os.remove(__file__)\n")  # make file delete itself when it runs
    if remote_dir is not None:
        remotecmd = os.path.join(remote_dir, cmdfname).replace('\\', '/')
    else:
        remotecmd = cmdfname
    # apparently put/get ignore the cd() context manager, but run doesn't  :(
    put(cmdfname, remotecmd)
    try:
        retval = run('%s %s' % (py, cmdfname))
    except SystemExit as err:  # error on remote side calls abort :|
        retval = None
    else:
        err = None
    if (err or retval.failed):
        #print the stuff
        print "Enjoy the contents of _cmd_.py below!"
        with open(cmdfname, 'r') as f:
            for i,line in enumerate(f):
                print "%d: %s" % (i+1, line),
    os.remove(cmdfname)  # remove local version
    return retval

@stub_if_missing_deps('fabric')
def remote_get_platform():
    """Returns the platform string of the current active host."""
    with settings(hide('running', 'stderr'), warn_only=True):
        return remote_py_cmd(["import sys", "print sys.platform"])


@stub_if_missing_deps('fabric')
def remote_check_setuptools(py):
    """Return True if setuptools is installed on the remote host."""
    with settings(hide('everything'), warn_only=True):
        return remote_py_cmd(["import setuptools"],
                             py).succeeded


@stub_if_missing_deps('fabric')
def remote_check_pywin32(py):
    """Return True if pywin32 is installed on the remote host."""
    with settings(hide('everything'), warn_only=True):
        return remote_py_cmd(["import win32api",
                              "import win32security"
                              "import ntsecuritycon"],
                             py=py).succeeded


@stub_if_missing_deps('fabric')
def remote_untar(tarfile, remote_dir=None, delete=True):
    """Use internal Python tar package to untar a file in the current remote
    directory instead of assuming that tar exists on the remote machine.
    """
    tarfile = tarfile.replace('\\', '/')
    cmds = [
        "import tarfile",
        "tar = tarfile.open('%s')" % tarfile,
        "tar.extractall()",
        "tar.close()",
    ]
    if delete:
        cmds.extend(['import os', 'os.remove("%s")' % tarfile])
    return remote_py_cmd(cmds, remote_dir=remote_dir)


@stub_if_missing_deps('fabric')
def remote_tmpdir():
    """Create and return the name of a temporary directory at the remote
    location.
    """
    with settings(show('stdout')):
        return remote_py_cmd(['import tempfile',
                              'print tempfile.mkdtemp()'])


def remote_mkdir(path):
    """Create a remote directory with the given name. If it already exists,
    just return with no error.
    """
    return remote_py_cmd(["import os",
                          "if not os.path.exists('%s'):" % path.replace('\\', '/'),
                          "    os.makedirs('%s')" % path.replace('\\', '/')])


@stub_if_missing_deps('fabric')
def remote_listdir(path):
    """Return a list of files found in the given remote directory."""
    with settings(show('stdout')):
        s = remote_py_cmd(["import os",
                           "print os.listdir('%s')" % path.replace('\\', '/')])
    s = s.strip()[1:-1]
    return [part.strip("'") for part in s.split(', ')]


def rm_remote_tree(pathname):
    """Delete the directory at the remote location."""
    return remote_py_cmd(["import shutil",
                          "shutil.rmtree('%s')" % pathname.replace('\\', '/')])


def put_untar(local_path, remote_dir):
    """Put the given tarfile on the current active host and untar it in the
    specified place.
    """
    tarname = os.path.basename(local_path)

    remote_mkdir(remote_dir)

    abstarname = os.path.join(remote_dir, tarname)
    put(local_path, abstarname)

    with cd(remote_dir):
        remote_untar(tarname, remote_dir, delete=True)


def put_dir(src, dest):
    """Tar the src directory, upload it to the current active
    host, untar it, and perform renaming if necessary.

    src: str
        directory to be copied to remote host

    dest: str
        pathname of directory on remote host
    """
    tmpdir = tempfile.mkdtemp()
    tarpath = tar_dir(src, os.path.basename(src), tmpdir)
    remote_dir = os.path.dirname(dest)
    put_untar(tarpath, remote_dir)
    shutil.rmtree(tmpdir, onerror=onerror)


def rsync_dirs(dest, host, dirs=('downloads', 'dists'),
               doit=os.system):
    """Use rsync to sync the specified directories on the specified host
    with the corresponding directories in the specified destination directory.

    This requires ssh access without a password to the host.
    """
    for dname in dirs:
        doit('rsync -arvzt --delete %s:%s %s' % (host, dname, dest))


@stub_if_missing_deps('fabric')
def retrieve_docs(remote_dir):
    """Retrieve a tar file of the docs built on a remote machine."""
    cmds = [
        "import tarfile",
        "import os",
        "remote_dir = os.path.expanduser('%s')" % remote_dir,
        "for fname in os.listdir(remote_dir):",
        "    if '-OpenMDAO-Framework-' in fname and not fname.endswith('.gz'):",
        "        break",
        "else:",
        "    raise RuntimeError('install dir not found in %s' % remote_dir)",
        "tardir = os.path.join(remote_dir, fname, 'docs', '_build', 'html')",
        "tar = tarfile.open(os.path.join(remote_dir, 'html.tar.gz'), mode='w:gz')",
        "tar.add(tardir, arcname='html')",
        "tar.close()",
    ]

    remote_py_cmd(cmds)
    get(os.path.join(remote_dir, 'html.tar.gz'), 'html.tar.gz')

#
# Git related utilities
#

def repo_top():
    """Return the top level directory in the current Git repository."""
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


def make_git_archive(tarfilename, prefix=''):
    """Make a tar file of the entire current repository."""
    startdir = os.getcwd()
    os.chdir(repo_top())
    try:
        check_call(['git', 'archive', '-o',
                    tarfilename, '--prefix=%s' % prefix, 'HEAD'])
    finally:
        os.chdir(startdir)

def do_cprofile(func):
    '''
    Decorator for profiling a function using the `cProfile`
    '''

    def profiled_func(*args, **kwargs):
        profile = cProfile.Profile()
        try:
            profile.enable()
            result = func(*args, **kwargs)
            profile.disable()
            return result
        finally:
            profile.print_stats()
    return profiled_func

def do_line_profile(follow=[]):
    """
    Decorator for profiling a function line by line
    using `line_profiler`. To use the decorator, `line_profiler`
    must be installed manually, as it is not required
    by openmdao.devtools.

    You can obtain `line_profiler` at http://pypi.python.org/pypi/line_profiler

    If `line_profiler` cannot be imported, fallback to
    profiling using `cProfile`
    """

    try:
        from line_profiler import LineProfiler

        def inner(func):
            def profiled_func(*args, **kwargs):
                try:
                    profiler = LineProfiler()
                    profiler.add_function(func)
                    for f in follow:
                        profiler.add_function(f)
                    profiler.enable_by_count()
                    return func(*args, **kwargs)
                finally:
                    profiler.print_stats()
            return profiled_func
        return inner
    except ImportError:
        print "Could not import 'line_profiler'. Falling back to profiling using 'cProfile'."
        return do_cprofile
