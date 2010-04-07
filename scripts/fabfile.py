
# This is a fabric file that constructs an openmdao release and
# deploys it to the openmdao.org site.

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
        result = run('ls ~/dists/openmdao*')
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
        prompt('Enter version id:', validate=_check_version)

    tmpdir = tempfile.mkdtemp()
    try:
        local(sys.executable+' '+
              os.path.join(os.path.dirname(__file__),'mkrelease.py')+
              ' --version=%s -d %s' % (version, tmpdir), capture=False)
        
        put(os.path.join(tmpdir,'*'), '~/dists')
        with cd('~/dists'):
            run('python2.6 mkegglistindex.py')
    finally:
        shutil.rmtree(tmpdir)


def showpackages():
    """Lists the openmdao.org/dists directory"""
    run('ls ~/dists')

