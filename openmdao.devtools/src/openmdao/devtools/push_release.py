
import sys
import os
import shutil
import urllib2
from optparse import OptionParser

from fabric.api import run, env, local, put, cd, prompt, hide, hosts, get, settings
from fabric.state import connections

import tempfile
import tarfile

from openmdao.devtools.utils import get_openmdao_version, put_dir, VersionError

#import paramiko.util
#paramiko.util.log_to_file('paramiko.log')

PRODUCTION_URL = 'openmdao@web103.webfaction.com'
#REAL_URL = 'http://openmdao.org'
#TEST_URL = 'http://torpedo.grc.nasa.gov:31004'

def push_release(release_dir, server_url):
    """Take a directory containing release files (openmdao package distributions,
    install scripts, etc., and push it up to the specified server.
    """
    pass

def _release(host, version=None, home=None, url=TEST_URL):
    """Creates source distributions, docs, binary eggs, and install script for 
    the current openmdao namespace packages and puts them on a local test server.  After
    tests have passed, uploads them to <home>/dists, and updates the index.html file there.
    """
    print("host is %s" % host)
    version = get_openmdao_version(home, version)

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


def create_mirror(destdir, url=PRODUCTION_URL):
    """Create a local mirror in the specified destination directory of the
    downloads and dists directories on the specified web site.
    """
    os.system('rsync -arvzt --delete %s:downloads %s' % (PRODUCTION_URL, destdir))
    os.system('rsync -arvzt --delete %s:dists %s' % (PRODUCTION_URL, destdir))


def main():
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

if __name__ == '__main__':
    main()