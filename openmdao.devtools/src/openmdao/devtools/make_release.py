
import sys
import os
import StringIO
import shutil
import urllib2
import subprocess
from optparse import OptionParser
from subprocess import Popen, STDOUT, PIPE, check_call
from socket import gethostname

from fabric.api import run, env, local, put, cd, prompt, hide, hosts, get, settings
from fabric.state import connections

import paramiko.util
import tempfile
import fnmatch
import tarfile

paramiko.util.log_to_file('paramiko.log')

REAL_URL = 'http://openmdao.org'
TEST_URL = 'http://torpedo.grc.nasa.gov:31004'

class _VersionError(RuntimeError):
    pass

        
def _check_version(version, home):
    with settings(hide('running', 'stdout'), host_string=host):
        result = run('ls %s/downloads' % home)
    lst = [x.strip() for x in result.split('\n')]
    if version in lst:
        raise _VersionError('Version %s already exists. Please specify a different version' % version)
    return version


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

def _release(host, version=None, is_local=True, home=None, url=TEST_URL):
    """Creates source distributions, docs, binary eggs, and install script for 
    the current openmdao namespace packages and puts them on a local test server.  After
    tests have passed, uploads them to <home>/dists, and updates the index.html file there.
    """
    print("host is %s" % host)
    #if version is not None:
#        try:
#            version = _check_version(version, home)
#        except _VersionError, err:
#            print str(err),'\n'
#            version = None
       
    if version is None:
        version = prompt('Enter version id:', validate=lambda ver: _check_version(ver,home))

    dist_dir = os.path.dirname(os.path.dirname(__file__))
    scripts_dir = os.path.join(dist_dir, 'scripts')
    tmpdir = tempfile.mkdtemp()
    startdir = os.getcwd()
    with settings(host_string=host):    
        try:
            # build the release distrib (docs are built as part of this)
            if is_local:
                teststr = '--test'
            else:
                teststr = ''
            local(sys.executable+' '+ os.path.join(scripts_dir,'mkrelease.py')+
                  ' --version=%s %s -d %s' % (version, teststr, tmpdir), capture=False)
        
            # tar up the docs so we can upload them to the test server
            tar_dir(os.path.join(tmpdir, '_build','html'), 'docs', tmpdir)
        
            run('mkdir %s/downloads/%s' % (home, version))
            run('chmod 755 %s/downloads/%s' % (home, version))
        
            # push new distribs up to the testserver
            for f in os.listdir(tmpdir):
                if f.startswith('openmdao_src'): 
                    # upload the repo source tar
                    put(os.path.join(tmpdir,f), '%s/downloads/%s/%s' % (home, version, f), 
                        mode=0644)
                elif f.endswith('.tar.gz') and f != 'docs.tar.gz':
                    put(os.path.join(tmpdir,f), '%s/dists/%s' % (home, f), mode=0644)
                elif f.endswith('.egg'):
                    put(os.path.join(tmpdir,f), '%s/dists/%s' % (home, f), mode=0644)
        
            # for now, put the go-openmdao script up to the test server without the version
            # id in the name
            put(os.path.join(tmpdir, 'go-openmdao-%s.py' % version), 
                '%s/downloads/%s/go-openmdao.py' % (home, version),
                mode=0755)

            ## put the docs on the test server and untar them
            #put_tar(os.path.join(tmpdir,'docs.tar.gz'), 
                    #'%s/downloads/%s' % (home, version), renames=['html','docs'])

            put(os.path.join(scripts_dir,'mkdlversionindex.py'), 
                '%s/downloads/%s/mkdlversionindex.py' % (home, version))
        
            # update the index.html for the version download directory on the test server
            with cd('%s/downloads/%s' % (home, version)):
                run('python2.6 mkdlversionindex.py %s' % url)
            # update the index.html for the dists directory on the test server
            with cd('%s/dists' % home):
                run('python2.6 mkegglistindex.py %s' % url)

            run('rm -f %s/downloads/latest' % home)
            run('ln -s %s/downloads/%s %s/downloads/latest' % (home, version, home))
            
            # update the index.html for the downloads directory on the test server
            with cd('%s/downloads' % home):
                run('python2.6 mkdownloadindex.py %s' % url)
           
        finally:
            shutil.rmtree(tmpdir)

def _find_top_dir():
    path = os.getcwd()
    while path:
        if '.git' in os.listdir(path):
            return path
        path = os.path.dirname(path)
    raise RuntimeError("Can't find top dir of repository starting at %s" % os.getcwd())

#build release and put on webfaction - will no longer be used in this form
@hosts('openmdao@web103.webfaction.com')
def release(version=None):
    if sys.platform != 'win32':
        raise RuntimeError("OpenMDAO releases should be built on Windows so Windows binary distributions can be built")
    try:
        for host in hosts:
            _release(version, is_local=False, home='~')
    finally:
        for key in connections.keys():
            connections[key].close()
            del connections[key]
 
@hosts('torpedo.grc.nasa.gov')
def localrelease(version=None):
    if sys.platform != 'win32':
        raise RuntimeError("OpenMDAO releases should be built on Windows so Windows binary distributions can be built")
    try:
        for host in hosts:
            # first, make sure we're in sync with the webfaction server - don't need to do any more probably
            #print 'syncing downloads dir...'
            #run('rsync -arvzt --delete openmdao@web103.webfaction.com:downloads /OpenMDAO/release_test')
            #print 'syncing dists dir...'
            #run('rsync -arvzt --delete openmdao@web103.webfaction.com:dists /OpenMDAO/release_test')
            print 'creating release...'
            # REAL ONE _release(version, is_local=True, home='/OpenMDAO/release_test', url=TEST_URL)
            _release(version, is_local=True, home='/OpenMDAO/release_test', url=TEST_URL)
    finally:
        for key in connections.keys():
            connections[key].close()
            del connections[key]

if __name__ == '__main__':
    parser = OptionParser()
    parser.add_option("--host", action='append', dest='host', 
                      default='openmdao@web103.webfaction.com',
                      metavar='HOST',
                      help="set the host URL")

    (options, args) = parser.parse_args(sys.argv[1:])
    
    #if sys.platform != 'win32':
    #    raise RuntimeError("OpenMDAO releases should be built on Windows so Windows binary distributions can be built")
    hosts=["torpedo.grc.nasa.gov"]
###NEED TO CHANGE OpenMDAO/release_test TO SOMETHING ELSE FOR TESTING PURPOSES - MAYBE ADD A TEST SWITCH!!!!!
    try:
        for host in hosts:
            # first, make sure we're in sync with the webfaction server - don't need to do any more probably
            #print 'syncing downloads dir...'
            #run('rsync -arvzt --delete openmdao@web103.webfaction.com:downloads /OpenMDAO/release_test')
            #print 'syncing dists dir...'
            #run('rsync -arvzt --delete openmdao@web103.webfaction.com:dists /OpenMDAO/release_test')
            print 'creating release...'
            #READ ONE_release(version=None, is_local=True, home='/OpenMDAO/dev/ckrenek/scripts2', url=TEST_URL)
            _release(hosts, version=None, is_local=True, home='/OpenMDAO/dev/ckrenek/scripts2', url=TEST_URL)
    finally:
        for key in connections.keys():
            connections[key].close()
            del connections[key]
