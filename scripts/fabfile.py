
# This is a fabric file that constructs an openmdao release and
# deploys it to the openmdao.org site.
#
# Usage: fab release    (this will prompt you for a version id)
#     OR
#        fab release:version_id
#

from fabric.api import run, env, local, put, cd, prompt, hide
import sys
import os.path
import tempfile
import shutil

env.hosts = ['openmdao@web103.webfaction.com']

class VersionError(RuntimeError):
    pass

def _check_version(version):
    with hide('running', 'stdout'):
        result = run('ls ~/dists')
    fname = 'openmdao.main-%s.tar.gz' % version
    if fname in result:
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

    tmpdir = tempfile.mkdtemp()
    try:
        # build the release distribs
        local(sys.executable+' '+
              os.path.join(os.path.dirname(__file__),'mkrelease.py')+
              ' --version=%s -d %s' % (version, tmpdir), capture=False)
        
        # push new distribs up to the server
        put(os.path.join(tmpdir,'openmdao.*.tar.gz'), '~/dists', 
            mode=0644)
        put(os.path.join(tmpdir,'openmdao-*.tar.gz'), '~/dists',
            mode=0644)
        
        run('mkdir ~/downloads/%s' % version)
        run('chmod 755 ~/downloads/%s' % version)
        
        # update the 'latest' link to point to the most recent version directory
        run('rm -f ~/downloads/latest')
        run('ln -s ~/downloads/%s ~/downloads/latest' % version)
        
        put(os.path.join(tmpdir, 'openmdao_src*.gz'), 
            '~/downloads/%s' % version, 
            mode=0644)
        put(os.path.join(tmpdir, 'go-openmdao-%s.py' % version), 
            '~/downloads/%s' % version,
            mode=0755)
        
        # update the index.html for the dists directory on the server
        with cd('~/dists'):
            run('python2.6 mkegglistindex.py')
        
    finally:
        shutil.rmtree(tmpdir)


def showpackages():
    """Lists the openmdao.org/dists directory"""
    run('ls ~/dists')

