#You must run from the top level of your branch!
#tar up branch
#copy it over to each platform in user dir
#build environment, activate, and run tests
#send data back
#delete temp directory on each platform

#create master python script that will call createtar, transferbranch, and testbranch?

#works on torpedo now, still failing a few tests on viper - maybe an activate issue?

#from fabric.api import run, env, local, put, cd, prompt, hide, hosts
from fabric.api import *
import sys
import os
import tempfile
import shutil
import fnmatch
import urllib
import tarfile

def _getrelease():
    """Grabs the latest openmdao release from the website, go-openmdao.py, so it can be tested on our dev platforms
    """
    run('cd ~')
    homedir=run('pwd')
    print homedir    
    run('rm -rf testRelease')
    run('mkdir testRelease')
    testdir=os.path.join(homedir, 'testRelease')
    print testdir
    #run('mkdir testRelease')
    #with cd('~/testRelease'):
    fname, msg=urlretrieve('http://openmdao.org/downloads/latest/go-openmdao.py', os.path.join(testdir, 'go-openmdao.py'))
    #fname, msg=urllib.urlretrieve('http://openmdao.org/downloads/latest/go-openmdao.py', 'go-openmdao.py')
    print fname
#    print msg

def _testbranch():
    """Builds and runs tests on a branch on all our development platforms
    You must run from the top level of your branch.
    """
    with show('debug'):
	branchdir=os.getcwd()
	print('original branch dir on localhost is is %s' % branchdir)
	#homedir=os.environ['HOME']
	#homedir=run('env|grep HOME')	
	homedir=run('echo %HOME%')
	print('home directory is %s' % homedir)
	sys.exit()
	branchname=os.path.basename('%s' % branchdir)
	#testbranchname=os.path.join(homedir, 'testbranch')
	#print('test branch name is %s' % testbranchname)
	#sys.exit()
	#if os.path.exists(testbranchname):
	    #print('yes it exists')
	run('rm -rf testbranch')

	#run('mkdir %s' % testbranchname)
        put(os.path.join(branchdir,'testbranch.tar.gz'), '~/testbranch.tar.gz') 
        #print('changing into branch %s' % branchname)
	run('tar xvf testbranch.tar.gz')
	with cd('~/testbranch'):
	    print('hopefully, we are now in ~/testbranch')
	    run('pwd')
	    run('mkdir .bzr')
	    #sys.exit()
            run('python2.6 go-openmdao-dev.py')
            with cd('devenv/bin'):
	        run('pwd')
	        #print('changing into hopefully, devenv/bin')
	        run('pwd && source activate && echo $PATH && echo environment activated, please wait while tests run && openmdao_test')
                #run('source activate')
                #run('./openmdao_test')
	        print('Tests completed')
    
def createtar2():
    """Creates a tar file of the versioned files in the current branch on the current host
    This is the hard way - not ideal
    """
    with show('debug'):
        branchdir=os.getcwd()
	print branchdir
	with cd('%s' % branchdir):
            local('rm -f testbranch.tar.gz')
	    local('bzr ls -RV > filelist')
            archbranch=tarfile.open(os.path.join(branchdir, 'testbranch.tar.gz'), 'w:gz')
	    local('chmod 775 testbranch.tar.gz')
            f=open('filelist', 'r')
            for name in f:
                print name,
		#if line is file
                archbranch.add(name.rstrip()),
            f.close()
            archbranch.close()

def createtar():
    """Creates a tar file by doing a bzr export in the current branch on the current host
    """
    with show('debug'):
        branchdir=os.getcwd()
	#print branchdir
	with cd('%s' % branchdir):
            local('bzr export testbranch.tar.gz')
	
def tryit():
    #print('trying to connect to storm')
    print('executing on %s as %s' % (env.host, env.user))
    run('env | grep PATH')
    #run('dir')

def getrelease():
    _getrelease()

#@hosts('viper.grc.nasa.gov')    
def testbranch():
    _testbranch()
    
def movefile():
    branchdir=os.getcwd()
    put('fabfile.py', 'fabfile.py') 
