
# This is a fabric file that constructs an openmdao release and
# deploys it to the openmdao.org site.
#
# Usage: fab release    (this will prompt you for a version id)
#     OR
#        fab release:version_id
#

from fabric.api import run, env, local, put, cd, prompt, hide
import sys
import os
from os.path import join,dirname,normpath
import tempfile
import shutil

env.hosts = ['openmdao@web103.webfaction.com']

class VersionError(RuntimeError):
    pass

def _check_version(version):
    with hide('running', 'stdout'):
        result = run('ls ~/downloads')
    lst = result.split(' ')
    if version in lst:
        raise VersionError('Version %s already exists. Please specify a different version' % version)
    return version


def release(version=None):
    """Creates source distributions of the current openmdao namespace packages,
    uploads them to openmdao.org/dists, and updates the index.html file there.
    """
    if version is not None:
        try:
            version = _check_version(version)
        except VersionError, err:
            print str(err),'\n'
            version = None
        
    if version is None:
        version = prompt('Enter version id:', validate=_check_version)

    dist_dir = normpath(join(dirname(__file__),'..'))
    scripts_dir = join(dist_dir, 'scripts')
    doc_dir = join(dist_dir, 'docs')
    util_dir = join(dist_dir,'openmdao.util','src','openmdao','util')
    tmpdir = tempfile.mkdtemp()
    startdir = os.getcwd()
    try:
        # build the release distrib (docs are built as part of this)
        local(sys.executable+' '+ join(scripts_dir,'mkrelease.py')+
              ' --version=%s -d %s' % (version, tmpdir), capture=False)
        
        # tar up the docs so we can upload them to the server
        os.chdir(join(tmpdir, '_build'))
        try:
            local('tar czf %s %s' % (join(tmpdir,'docs.tar.gz'), 
                                     'html'),
                  capture=False)
        finally:
            os.chdir(startdir)
        
        # push new distribs up to the server
        put(join(tmpdir,'openmdao.*.tar.gz'), '~/dists', 
            mode=0644)
        put(join(tmpdir,'openmdao-*.tar.gz'), '~/dists',
            mode=0644)
        
        run('mkdir ~/downloads/%s' % version)
        run('chmod 755 ~/downloads/%s' % version)
        
        # update the 'latest' link to point to the most recent version directory
        run('rm -f ~/downloads/latest')
        run('ln -s ~/downloads/%s ~/downloads/latest' % version)
        
        # upload the repo source tar
        put(join(tmpdir, 'openmdao_src*.gz'), 
            '~/downloads/%s' % version, 
            mode=0644)
        
        # for now, put the go-openmdao script up without the version
        # id in the name
        put(join(tmpdir, 'go-openmdao*.py'), 
            '~/downloads/%s/go-openmdao.py' % version,
            mode=0755)

        # put the docs on the server and untar them
        put(join(tmpdir,'docs.tar.gz'), '~/downloads/%s' % version) 
        with cd('~/downloads/%s' % version):
            run('tar xzf docs.tar.gz')
            run('mv html docs')
            run('rm -f docs.tar.gz')

        # FIXME: just generate the index.html locally and upload it instead
        # of having a bunch of mkdlversionindex.py files lying around on the
        # server
        put(join(scripts_dir,'mkdlversionindex.py'), 
            '~/downloads/%s' % version)
        
        # update the index.html for the version download directory on the server
        with cd('~/downloads/%s' % version):
            run('python2.6 mkdlversionindex.py')

        # update the index.html for the dists directory on the server
        with cd('~/dists'):
            run('python2.6 mkegglistindex.py')

        # update the index.html for the downloads directory on the server
        with cd('~/downloads'):
            run('python2.6 mkdownloadindex.py')

        
    finally:
        shutil.rmtree(tmpdir)


def showpackages():
    """Lists the openmdao.org/dists directory"""
    run('ls ~/dists')

