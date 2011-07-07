
import sys
import os
import shutil
import urllib2
import subprocess
import tempfile
from optparse import OptionParser

from fabric.api import run, env, local, put, cd, get, settings
from fabric.state import connections

#testing
#import paramiko.util
#paramiko.util.log_to_file('paramiko.log')


def _get_release_script(site_url, version):
    """Grabs the specified openmdao release installer script from the specified 
    url, so it can be tested on our dev platforms. The script will be placed in
    the current directory.
    """
    script_url =  '%s/downloads/%s/go-openmdao.py' % (site_url, version)

    if script_url.startswith('http'):
        resp = urllib2.urlopen(script_url)
        gofile = open('go-openmdao.py', 'wb')
        shutil.copyfileobj(resp.fp, gofile)
        gofile.close()
    else: # release in local file system
        shutil.copy(script_url, 'go-openmdao.py')


def _testlocally(site_url, version, pyversion='python', keep=False):
    """"Copies the go-openmdao.py file from the release area, runs it, and
    runs the test suite on it.
    
    If keep is True, the temporary directory where the release is built will
    not be deleted. 
    """
    testdir = tempfile.mkdtemp()
    startdir = os.getcwd()
    os.chdir(testdir)

    if sys.platform.startswith('win'):
        devbindir = 'Scripts'
        activate = ['activate.bat']
        #removeit = "rmdir /s /q"
    else:
        devbindir = 'bin'
        activate = ['source', 'activate']
        
    print 'getting go-openmdao.py file...'
    _get_release_script(site_url, version)
    
    try:
        subprocess.check_call([pyversion, 'go-openmdao.py'])
        dirs = os.listdir(testdir)
        dirs.remove('go-openmdao.py')
        releasedir = dirs[0]
        
        # activate the environment and run tests
        os.chdir(os.path.join(testdir, releasedir, devbindir))
        print("Please wait while the environment is activated and the tests are run")
        env = os.environ.copy()
        for name in ['VIRTUAL_ENV','_OLD_VIRTUAL_PATH','_OLD_VIRTUAL_PROMPT']:
            if name in env: 
                del env[name]
        subprocess.check_call(activate + ['&&',
                               'echo',
                               'environment activated, please wait while tests run',
                               '&&',
                               'openmdao_test', '-x'], env=env)
        print('Tests completed on %s' % host)
    finally:
        os.chdir(startdir)

def _testremote(site_url, version, host):
    """"Copies the go-openmdao.py file to each production platform and builds 
    and tests on each one
    """
    #get go-openmdao.py from the web and put in current dir on the local host
    print 'getting go-openmdao.py file...'
    _get_release_script(site_url, version)

    with settings(host_string=host):
        devbindir='bin'
        pyversion="python2.6"
        removeit="rm -rf"
        #remove any previous testrelease dirs on remote unix or linux host
        run('%s releasetest' % removeit)
        #make new releasetest dir on remote host
        run('mkdir releasetest')
        #Copy go-openmdao.py to releasetest directory on remote host
        put('go-openmdao.py', 'releasetest/go-openmdao.py')  
        with cd('releasetest'):
            #build the environment and put it in a directory called testrelease
            run('%s go-openmdao.py testrelease' % pyversion)
            #change to testrelease/bin or testrelease\Scripts (on windows), activate the envronment, and run tests
            with cd('testrelease/bin'):
                print("Please wait while the environment is activated and the tests are run")
                run('source activate && echo $PATH && echo environment activated, please wait while tests run && openmdao_test -x')
                print('Tests completed on %s' % host)  

def main(argv=None):
    if argv is None:
        argv = sys.argv[1:]
        
    parser = OptionParser()
    parser.add_option("-s", "--site", type="string", dest='site', 
                      default='http://openmdao.org',
                      help="URL where release files are located. "
                           "This can be just a directory in the file system as well.")
    parser.add_option("-v", action="store", type='string', dest='version', 
                      help="version to test", default='latest')
    parser.add_option("--host", action='append', dest='hosts', default=[],
                      metavar='HOST',
                      help="add a host url to test the release on")

    (options, args) = parser.parse_args(argv)
    
    
    if options.hosts:
        hosts = options.hosts
    else:
        hosts = ['storm.grc.nasa.gov', 'torpedo.grc.nasa.gov', 'viper.grc.nasa.gov']
        
    print 'testing on hosts: %s' % hosts
    try:
        # TODO: run these concurrently
        for host in hosts:
            _testrelease(options.site, options.version, host)
    finally:
        # ensure that all network connections are closed
        # TODO: once we move to Fabric 0.9.4, just use disconnect_all() function
        for key in connections.keys():
            connections[key].close()
            del connections[key]

if __name__ == '__main__':
    main()
