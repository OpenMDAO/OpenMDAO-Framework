
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
        if '.bzr' in os.listdir(path):
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
 











#ALL FUNCTIONALITY BELOW THIS POINT HAS BEEN MOVED TO STANDALONE FILES!!!!!  
#-------------------------------------------------------------------------------------    
#Local developer script to build and run tests on a branch on each development platform
#FUNCTIONALITY MOVED TO TESTBRANCH.PY
def _testbranch():
    """Builds and runs tests on a branch on all our development platforms
    You can run from anywhere in the branch, but recommend running from branchroot/scripts dir.
    """
    startdir=os.getcwd()
    branchdir=local('bzr root').strip()    
    #print("starting directory is %s" % startdir)
    #print("branch root directory is %s" % branchdir)
    remotehost = env.host.split('.')[0]
    tarfilename=remotehost+"testbranch.tar.gz"
    print("tarfilename is %s" % tarfilename)    
    #export the current branch to a tarfile
    os.chdir(branchdir)  #change to top dir of branch
    #local('bzr export testbranch.tar.gz')
    local("bzr export %s --root=testbranch" % tarfilename)
    paramiko_log="paramiko.log." + remotehost
    paramiko.util.log_to_file(paramiko_log)  
      
    winplatforms=["storm.grc.nasa.gov"]  #list of windows platforms to remote into
    if env.host in winplatforms:     #if we are remoting into a windows host
        devbindir='devenv\Scripts'
        unpacktar="7z x" 
        pyversion="python"   #for some reason, on storm the python2.6 alias doesn't work on storm
        removeit="""rmdir /s /q"""
        env.shell="cmd /C"
        user=env.user
        # env.user="ndc\\"+env.user   #need to preface username with ndc\\ to get into storm
    else:
        devbindir='devenv/bin'
        unpacktar="tar xvf"
        pyversion="python2.6"
        removeit="rm -rf"
        env.shell="/bin/bash -l -c"

    #Copy exported branch tarfile to desired test platform in user's root dir
    #filetocopy = os.path.join(branchdir, 'testbranch.tar.gz')
    filetocopy = os.path.join(branchdir, tarfilename)
    if env.host not in winplatforms:     #if we are not remoting into a windows host
        #remove any previous testbranches on remote host
        run('%s testbranch' % removeit)  
        #copy exported branch tartile to test platform in user's root dir 
        put(filetocopy, 'testbranch.tar.gz')
        #unpack the tarfile
        run('%s testbranch.tar.gz' % unpacktar)  
        with cd('testbranch'):
            #Make a .bzr directory to fool go-openmdao-dev.py into thinking this is a real repository
            run('mkdir .bzr')   
            #build it
            run('%s go-openmdao-dev.py' % pyversion)
            #change to devenv/bin, activate the envronment, and run tests
            with cd(devbindir):
                print("Please wait while the environment is activated and the tests are run")
                run('source activate && echo $PATH && echo environment activated, please wait while tests run && openmdao_test -xv')
                print('Tests completed on %s' % env.host)
         
    else:  #we're remoting into windows (storm)
        #remove any previous testbranches on remote host
        run("""if exist testbranch/nul rmdir /s /q testbranch""")
        run("""if exist testbranch.tar del testbranch.tar""")
        #copy exported branch tartile to test platform (storm) in user's root dir        
        filedestination = user + """@storm.grc.nasa.gov:testbranch.tar.gz"""
        local('scp %s %s' % (filetocopy, filedestination))  
        #unpack the tarfile
        run("call 7z.exe x testbranch.tar.gz")
        run("call 7z.exe x testbranch.tar")
        run("""call python testbranch\go-openmdao-dev.py""")
        teststeps="""chdir testbranch\devenv\Scripts 
            call activate.bat
            set PYTHON_EGG_CACHE=C:\Users\\%USERNAME%\\testbranch
            echo "environment activated, please wait while tests run"
            openmdao_test.exe -xv"""
        #need to export teststeps to batch file
        with open('winteststeps.bat', 'w') as f:
            f.write(teststeps)
        f.close()
        #Then copy the newly generated batch file to windows platform (storm)
        filetocopy = os.path.join(branchdir, 'winteststeps.bat')
        filedestination = user + """@storm.grc.nasa.gov:winteststeps.bat"""
        local('scp %s %s' % (filetocopy, filedestination)) 
        #change to devenv\Scripts, activate the envronment, and run tests
        run('call winteststeps.bat')
        print('Tests completed on %s' % env.host)


@hosts('storm.grc.nasa.gov', 'torpedo.grc.nasa.gov', 'viper.grc.nasa.gov')
def testbranch():
    _testbranch()
    
testbranch.__test__ = False

#------------------------------------------------------------------------------------------------------
#Part of script needed to test releases
#This will only be run from storm, since the release script is always run from storm
#Run this from the scripts directory - THIS FUNCTIONALITY HAS BEEN MOVED TO test_release.py!
def _getrelease(releaseurl):
    """Grabs the latest openmdao release from the website, go-openmdao.py, so it can be tested on our dev platforms
    """
    startdir=os.getcwd()
    print('starting dir is %s' % startdir)
    
    try:  
        resp = urllib2.urlopen(releaseurl)
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

def _testrelease(releaseurl):
    """"Copies the go-openmdao.py file to each production platform and builds and tests on each one
    """
    startdir=os.getcwd()
    #get go-openmdao.py from web and put in startdir on the local host
    _getrelease(releaseurl)
    #@runs_once(_getrelease())

    winplatforms=["storm.grc.nasa.gov"]  #Is remote host storm?
    if env.host in winplatforms:
        #If running windows tests, do it locally on storm 
        devbindir='Scripts'
        pyversion="python"
        removeit=removeit="""rmdir /s /q"""
        if os.path.isdir('releasetest'):
            shutil.rmtree('releasetest')
        local('mkdir releasetest')
        shutil.copy('go-openmdao.py', os.path.join('releasetest', 'go-openmdao.py'))  
        with cd('releasetest'):
            local('%s go-openmdao.py testrelease' % pyversion)
            #change to testrelease\Scripts (on windows), activate the environment, and run tests
            with cd(os.path.join('testrelease', devbindir)):
                print("Please wait while the environment is activated and the tests are run")
                local('activate && echo environment activated, please wait while tests run && openmdao_test -x')
                print('Tests completed on %s' % env.host)
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
                print('Tests completed on %s' % env.host)  

#Do not need to run this separately since testrelease calls it - just here for debugging purposes
def getrelease(releaseurl='%s/downloads/latest/go-openmdao.py' % REAL_URL):
    _getrelease(releaseurl)

@hosts('torpedo.grc.nasa.gov', 'viper.grc.nasa.gov', 'storm.grc.nasa.gov')
def testrelease(releaseurl='%s/downloads/latest/go-openmdao.py' % REAL_URL):
    if sys.platform != 'win32':
        raise RuntimeError("OpenMDAO releases should be tested from Windows since that's where releases are created by config mgr.")
    _testrelease(releaseurl)

testrelease.__test__ = False

# release testing on the local mirror (torpedo)
@hosts('torpedo.grc.nasa.gov', 'viper.grc.nasa.gov', 'storm.grc.nasa.gov')
def testlocalrelease(releaseurl='%s/downloads/latest/go-openmdao.py' % TEST_URL):
    if sys.platform != 'win32':
        raise RuntimeError("OpenMDAO releases should be tested from Windows since that's where releases are created by config mgr.")

testlocalrelease.__test__ = False

#Do not need to run this separately since testlocalrelease calls it - just here for debugging purposes
def getlocalrelease(releaseurl='%s/downloads/latest/go-openmdao.py' % TEST_URL):
    _getrelease(releaseurl)

