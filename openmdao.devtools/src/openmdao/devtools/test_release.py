
import sys
import os
import shutil
import urllib2
import subprocess
from optparse import OptionParser

from fabric.api import run, env, local, put, cd, get, settings
from fabric.state import connections

import paramiko.util

paramiko.util.log_to_file('paramiko.log')

#----------------------------------------------------------------------------------
# Part of script needed to test releases
# This will only be run from storm, since the release script is always run from storm
# Run this from the scripts directory
def _getrelease(site_url, version):
    """Grabs the specified openmdao release installer script from the specified 
    website, so it can be tested on our dev platforms. The script will be placed in
    the current directory.
    """
    script_url =  '%s/downloads/%s/go-openmdao.py' % (site_url, version)

    try:  
        resp = urllib2.urlopen(script_url)
    except IOError, e:
        if hasattr(e, 'reason'):
            print 'We failed to reach the server'
            print 'Reason: ', e.reason
        if hasattr(e, 'code'):
            print 'We failed to reach a server'
            print 'Error code: ', e.code
        sys.exit()
    else:
        gofile = open('go-openmdao.py', 'wb')
        shutil.copyfileobj(resp.fp, gofile)
        gofile.close()
        #print resp.code
        #print resp.headers["content-type"]


def _testrelease(site_url, version, host):
    """"Copies the go-openmdao.py file to each production platform and builds 
    and tests on each one
    """
    #get go-openmdao.py from the web and put in current dir on the local host
    print 'getting go-openmdao.py file...'
    _getrelease(site_url, version)

    with settings(host_string=host):
        winplatforms=["storm.grc.nasa.gov"]  #Is remote host storm?
        if host in winplatforms:
            # If running windows tests, do it locally on storm 
            # (because it can't ssh to itself)
            devbindir='Scripts'
            pyversion="python"
            removeit="""rmdir /s /q"""
            if os.path.isdir('releasetest'):
                shutil.rmtree('releasetest')
            subprocess.check_call(['mkdir', 'releasetest'])
            shutil.copy('go-openmdao.py', os.path.join('releasetest', 'go-openmdao.py'))
            startdir = os.getcwd()
            os.chdir('releasetest')
            try:
                subprocess.check_call([pyversion, 'go-openmdao.py', 'testrelease'])
                # change to testrelease\Scripts (on windows), activate the environment, 
                # and run tests
                os.chdir(os.path.join('testrelease', devbindir))
                print("Please wait while the environment is activated and the tests are run")
                subprocess.check_call('activate && echo environment activated, please wait while tests run && openmdao_test -x')
                print('Tests completed on %s' % host)
            finally:
                os.chdir(startdir)
        else:
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
    parser.add_option("-l", action="store_true", dest='local', 
                      help="test a local release")
    parser.add_option("-v", action="store", type='string', dest='version', 
                      help="version to test", default='latest')
    parser.add_option("--host", action='append', dest='hosts', default=[],
                      metavar='HOST',
                      help="add a host url to test the release on")

    (options, args) = parser.parse_args(argv)
    
    if options.local:
        site_url = 'http://torpedo.grc.nasa.gov:31004'
    else:
        site_url = 'http://openmdao.org'

    if options.hosts:
        hosts = options.hosts
    else:
        hosts = ['torpedo.grc.nasa.gov', 'storm.grc.nasa.gov', 'viper.grc.nasa.gov']
        
    # ensure that all network connections are closed
    # TODO: once we move to Fabric 0.9.4, just use disconnect_all() function
    try:
        for host in hosts:
            _testrelease(site_url, options.version, host)
    finally:
        for key in connections.keys():
            connections[key].close()
            del connections[key]

if __name__ == '__main__':
    main()