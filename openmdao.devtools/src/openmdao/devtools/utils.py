import sys
import os
import shutil
import urllib2
from optparse import OptionParser

from fabric.api import run, env, local, put, cd, prompt, hide, hosts, get, settings
from fabric.state import connections

import tempfile
import tarfile


def check_openmdao_version(version, home='~'):
    """Checks to see if the specified version of openmdao already exists on
    the current active host.
    """
    # TODO: make this smarter.  maybe have it grab the latest version via pkg_resources
    # or packaging and see if it's the same or newer then the specified version
    with settings(hide('running', 'stdout'), host_string=host):
        result = run('ls %s/downloads' % home)
    lst = [x.strip() for x in result.split('\n')]
    if version in lst:
        raise _VersionError('Version %s already exists. Please specify a different version' % version)
    return version

def push_and_run(fpath, remotepath=None, runner=None, args=()):
    """Puts the given file onto the current active host and executes it there"""
    if not os.path.isfile(fpath):
        raise IOError("can't find file %s" % fpath)
    if remotepath is None:
        remotepath = fpath
    put(fpath, remotepath)
    if runner is None and fpath.endswith('.py'):
        runner = 'python'
    run("%s %s %s" % (runner, remotepath, ' '.join(args)))
    

def tar_dir(dirpath, archive_name, destdir):
    """Tar up the given directory and put in in the specified destination
    directory.
    """
    startdir = os.getcwd()
    os.chdir(os.path.dirname(dirpath))
    try:
        archive = tarfile.open(os.path.join(destdir,'%s.tar.gz' % archive_name), 'w:gz')
        archive.add(os.path.basename(dirpath))
        archive.close()
    finally:
        os.chdir(startdir)

def get_platform():
    """Returns the platform string of the current active host."""
    with hide('running', 'stdout'):
        return run('python -c "import sys; print sys.platform"')

def check_setuptools(py):
    """Return True if setuptools is installed on the remote host"""
    with settings(hide('everything'), warn_only=True):
        return run('%s -c "import setuptools; print setuptools.__version__"' % py)

def host_call(host, func, *args, **kwargs):
    """Calls the given function inside of a 'with' block that
    sets the host.
    """
    with settings(host_string=host):
        func(*args, **kwargs)
        
def py_cmd(cmds):
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
    """Create a temporary directory at the remote location."""
    return run('python -c "import tempfile; print tempfile.mkdtemp()"').strip()

def rm_remote_tree(pathname):
    """Delete the directory at the remote location."""
    run("""python -c "import shutil; shutil.rmtree('%s')" """ % pathname)
    
def list_remote_dir(dirname):
    return run("""python -c "import os; print os.listdir('%s')" """ % dirname)
            

def setup_files_area(dest, dirs=('downloads','dists'),
                     production_host='openmdao@web103.webfaction.com'):
    """Set up the specified directory as an OpenMDAO release area.
    This requires ssh access without a password to the host.
    """
    # first, make sure we're in sync with the webfaction server
    for dname in dirs:
        print 'syncing %s dir with production server (%s)...' % (dname,production_host)
        run('rsync -arvzt --delete %s:%s %s' % (production_host, dname, dest))


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

    print os.path.isfile(local_path)
    
    abstarname = os.path.join(remote_dir, tarname)
    put(local_path, abstarname)
    with cd(remote_dir):
        remote_untar(tarname)
        for oldname, newname in renames:
            run('%s %s %s' % (mover, oldname.strip(['/','\\']), 
                              newname.strip(['/','\\'])))
    run('%s %s' % (remover, abstarname))
    return remote_dir


def put_dir(dirpath, archive_name=None, remote_tmp=None):
    """Tar the specified directory, upload it to the current active
    host, untar it, and perform renaming if necessary.
    
    Returns the remote directory where the directory was untarred.
    """
    if archive_name is None:
        archive_name = os.path.basename(dirpath)
    tmpdir = tempfile.mkdtemp()
    tar_dir(dirpath, archive_name, tmpdir)
    tarpath = os.path.join(tmpdir, "%s.tar.gz" % archive_name)
    remotedir = put_untar(tarpath)
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
