
from fabric.api import run, env, local, put, cd, prompt, hide, hosts, get
import sys
import os
import tempfile
import shutil
import fnmatch
import tarfile
import urllib2
import subprocess
from subprocess import Popen, STDOUT, PIPE, check_call
from socket import gethostname
import paramiko.util

paramiko.util.log_to_file('paramiko.log')

REAL_URL = 'http://openmdao.org'
TEST_URL = 'http://torpedo.grc.nasa.gov:31004'

class _VersionError(RuntimeError):
    pass

        
def _check_version(version, home):
    with hide('running', 'stdout'):
        result = run('ls %s/downloads' % home)
    lst = [x.strip() for x in result.split('\n')]
    if version in lst:
        raise _VersionError('Version %s already exists. Please specify a different version' % version)
    return version


def _release(version, is_local, home, url=REAL_URL):
    """Creates source distributions, docs, binary eggs, and install script for 
    the current openmdao namespace packages, uploads them to <home>/dists, 
    and updates the index.html file there.
    """
    if version is not None:
        try:
            version = _check_version(version, home)
        except _VersionError, err:
            print str(err),'\n'
            version = None
        
    if version is None:
        version = prompt('Enter version id:', validate=lambda ver: _check_version(ver,home))

    dist_dir = os.path.dirname(os.path.dirname(__file__))
    scripts_dir = os.path.join(dist_dir, 'scripts')
    doc_dir = os.path.join(dist_dir, 'docs')
    util_dir = os.path.join(dist_dir,'openmdao.util','src','openmdao','util')
    tmpdir = tempfile.mkdtemp()
    startdir = os.getcwd()
    try:
        # build the release distrib (docs are built as part of this)
        if is_local:
            teststr = '--test'
        else:
            teststr = ''
        local(sys.executable+' '+ os.path.join(scripts_dir,'mkrelease.py')+
              ' --version=%s %s -d %s' % (version, teststr, tmpdir), capture=False)
        
        # tar up the docs so we can upload them to the server
        os.chdir(os.path.join(tmpdir, '_build'))
        try:
            archive = tarfile.open(os.path.join(tmpdir,'docs.tar.gz'), 'w:gz')
            archive.add('html')
            archive.close()
        finally:
            os.chdir(startdir)
        
        run('mkdir %s/downloads/%s' % (home, version))
        run('chmod 755 %s/downloads/%s' % (home, version))
        
        # push new distribs up to the server
        for f in os.listdir(tmpdir):
            if f.startswith('openmdao_src'): 
                # upload the repo source tar
                put(os.path.join(tmpdir,f), '%s/downloads/%s/%s' % (home, version, f), 
                    mode=0644)
            elif f.endswith('.tar.gz') and f != 'docs.tar.gz':
                put(os.path.join(tmpdir,f), '%s/dists/%s' % (home, f), mode=0644)
            elif f.endswith('.egg'):
                put(os.path.join(tmpdir,f), '%s/dists/%s' % (home, f), mode=0644)
        
        # for now, put the go-openmdao script up without the version
        # id in the name
        put(os.path.join(tmpdir, 'go-openmdao-%s.py' % version), 
            '%s/downloads/%s/go-openmdao.py' % (home, version),
            mode=0755)

        # put the docs on the server and untar them
        put(os.path.join(tmpdir,'docs.tar.gz'), '%s/downloads/%s/docs.tar.gz' % (home, version)) 
        with cd('%s/downloads/%s' % (home, version)):
            run('tar xzf docs.tar.gz')
            run('mv html docs')
            run('rm -f docs.tar.gz')

        put(os.path.join(scripts_dir,'mkdlversionindex.py'), 
            '%s/downloads/%s/mkdlversionindex.py' % (home, version))
        
        # update the index.html for the version download directory on the server
        with cd('%s/downloads/%s' % (home, version)):
            run('python2.6 mkdlversionindex.py %s' % url)

        # update the index.html for the dists directory on the server
        with cd('%s/dists' % home):
            run('python2.6 mkegglistindex.py %s' % url)

        run('rm -f %s/downloads/latest' % home)
        run('ln -s %s/downloads/%s %s/downloads/latest' % (home, version, home))
            
        # update the index.html for the downloads directory on the server
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

                
@hosts('openmdao@web103.webfaction.com')
def release(version=None):
    if sys.platform != 'win32':
        raise RuntimeError("OpenMDAO releases should be built on Windows so Windows binary distributions can be built")
    _release(version, is_local=False, home='~')
    

@hosts('torpedo.grc.nasa.gov')
def localrelease(version=None):
    # first, make sure we're in sync with the webfaction server
    print 'syncing downloads dir...'
    run('rsync -arvzt --delete openmdao@web103.webfaction.com:downloads /OpenMDAO/release_test')
    print 'syncing dists dir...'
    run('rsync -arvzt --delete openmdao@web103.webfaction.com:dists /OpenMDAO/release_test')
    print 'creating release...'
    _release(version, is_local=True, home='/OpenMDAO/release_test', url=TEST_URL)



#Keith's function to update the dev docs - not on trunk yet?
def _update_dev_docs():
    startdir = os.getcwd()
    try:
        # tar up the docs so we can upload them to the server
        topdir = _find_top_dir()
        devtools_dir = os.path.join(topdir,'openmdao.devtools',
                                    'src','openmdao','devtools')
        check_call([sys.executable, os.path.join(devtools_dir,'build_docs.py')])        

        try:
            archive = tarfile.open(os.path.join(topdir,'docs','docs.tar.gz'), 'w:gz')
            os.chdir(os.path.join(topdir, 'docs', '_build'))	
            archive.add('html')
            archive.close()
        finally:
            os.chdir(startdir)

        # put the docs on the server and untar them
        put(os.path.join(topdir,'docs', 'docs.tar.gz'), '~/downloads/dev_docs/docs.tar.gz') 
        with cd('~/downloads/dev_docs'):
            run('tar xzf docs.tar.gz')
            run('mv html/* ~/downloads/dev_docs')
            run('rm -rf html')
            run('rm -f docs.tar.gz')
 
    finally:
        cmd="rm -rf " + os.path.join(topdir,'docs','docs.tar.gz')
        local(cmd)
   
@hosts('openmdao@web103.webfaction.com')
def update_dev_docs():
    _update_dev_docs()
 
