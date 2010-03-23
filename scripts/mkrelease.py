
#
# The beginning of an automated release script for OpenMDAO.
# Right now, it just builds sdists of all of the openmdao namespace
# packages and puts them in a specified destination directory.
#
import sys, os
import shutil
import logging
from pkg_resources import working_set, Requirement, Environment, Distribution
from subprocess import Popen, STDOUT, PIPE

from openmdao.util.dumpdistmeta import get_metadata

def _build_dist(build_type, destdir):
    cmd = '%s setup.py %s -d %s' % (sys.executable, build_type, destdir)
    p = Popen(cmd, stdout=PIPE, stderr=STDOUT, env=os.environ, shell=True)
    out = p.communicate()[0]
    ret = p.returncode
    if ret != 0:
        logging.error(out)
        raise RuntimeError(
             'error while building egg in %s (return code=%d): %s'
              % (os.getcwd(), ret, out))

def _build_dev_egg(projdir, destdir):
    """Build an sdist out of a develop egg."""
    env = Environment()
    os.chdir(projdir)
    # clean up any old builds
    if os.path.exists('build'):
        shutil.rmtree('build')
    _build_dist('sdist', destdir)
    if os.path.exists('build'):
        shutil.rmtree('build')

def _find_top_dir():
    path = os.getcwd()
    while path:
        if '.bzr' in os.listdir(path):
            return path
        path = os.path.dirname(path)
    raise RuntimeError("Can't find top dir of repository starting at %s" % os.getcwd())

def main():
    openmdao_packages = { 'openmdao.main': '', 
                          'openmdao.lib': '', 
                          'openmdao.util': '', 
                          'openmdao.recipes': '',
                          'openmdao.test': '', 
                          'openmdao.examples.simple': 'examples',
                          'openmdao.examples.bar3simulation': 'examples',
                          'openmdao.examples.enginedesign': 'examples',
                          }
    try:
        destdir = sys.argv[1]
    except IndexError:
        print 'Usage: mkrelease.py <destination_directory>'
        sys.exit(-1)
    topdir = _find_top_dir()
    destdir = os.path.abspath(destdir)
    if not os.path.exists(destdir):
        os.makedirs(destdir)

    reqs = [Requirement.parse(x) for x in openmdao_packages]
    dists = working_set.resolve(reqs)
    startdir = os.getcwd()
    try:
        for d in dists:
            if d.project_name in openmdao_packages:
                pdir = os.path.join(topdir, 
                                    openmdao_packages[d.project_name], 
                                    d.project_name)
                print 'building %s' % d.project_name
                _build_dev_egg(pdir, destdir)
    finally:
        os.chdir(startdir)
    
    sdists = os.listdir(destdir)
    for dist in dists:
        for sdist in sdists:
            if sdist.startswith(dist.project_name):
                pdir = os.path.join(topdir, 
                                    openmdao_packages[dist.project_name], 
                                    dist.project_name)
                if not dist.location.startswith(pdir):
                    version = get_metadata(os.path.join(destdir, sdist))['version']
                    if version == dist.version:
                        print "removing %s because version (%s) hasn't changed" % \
                              (sdist, version)
                        os.remove(os.path.join(destdir, sdist))
                    else:
                        print "version of %s has changed from %s to %s" % \
                              (dist.project_name, dist.version, version)

if __name__ == '__main__':
    main()