#testbranch script (formerly in fabfile.py)

import sys
import os
import shutil
import subprocess
from optparse import OptionParser
from fabric.api import run, env, local, put, cd, get, settings, prompt, hide, hosts
from fabric.state import connections
import paramiko.util
from socket import gethostname

paramiko.util.log_to_file('paramiko.log')

def _make_archive(tarfilename):
    #branchdir=local('git rev-parse --show-toplevel').strip()
    #export the current branch to a tarfile
    local("git archive -o %s --prefix=testbranch/ HEAD" % tarfilename)
    

def _testbranch(hostname, tarfilename):
    """Builds and runs tests on a branch on a specified host platform. You can
    run from anywhere in the branch, but recommend running from
    branchroot/scripts dir.
    """
    print('running tests on %s' % hostname)
    remotehost = hostname.split(".")[0]
    branchdir=local('git rev-parse --show-toplevel').strip()
      
    winplatforms=["storm.grc.nasa.gov"]  #list of windows platforms to remote into
    if hostname in winplatforms:     #if we are remoting into a windows host
        devbindir='devenv\Scripts'
        unpacktar="7z x" 
        pyversion="python"   #for some reason, on storm the python2.6 alias doesn't work on storm
        removeit="""rmdir /s /q""" 
        env.shell="cmd /C"
        user=env.user
        # env.user="ndc\\"+env.user   #no longer need to preface username with ndc\\ to get into storm
    else:
        devbindir='devenv/bin'
        unpacktar="tar xvf"
        pyversion="python2.6"
        removeit="rm -rf"
        env.shell="/bin/bash -l -c"

    #Copy exported branch tarfile to desired test platform in user's root dir
    with settings(host_string=hostname):
        filetocopy = os.path.join(branchdir, tarfilename)
        if hostname not in winplatforms:     #if we are not remoting into a windows host
            #remove any previous testbranches on remote host
            res1=run('%s testbranch' % removeit)
            #copy exported branch tartile to test platform in user's root dir 
            put(filetocopy, tarfilename)
            #unpack the tarfile
            run('%s %s' % (unpacktar, tarfilename))  
            with cd('testbranch'):
                #Make a .git directory to fool go-openmdao-dev.py into thinking this is a real repository
                run('mkdir .git')   
                #build it
                run('%s go-openmdao-dev.py' % pyversion)
                #change to devenv/bin, activate the envronment, and run tests
                with cd(devbindir):
                    print("Please wait while the environment is activated and the tests are run")
                    run('source activate && echo $PATH && echo environment activated, please wait while tests run && openmdao_test -xv')
                    print('Tests completed on %s' % hostname)
            res2=run('%s testbranch' % removeit) 
            res3=run('%s *testbranch.tar *testbranch.tar.gz' % removeit)
         
        else:  #we're remoting into windows (storm)
            #remove any previous testbranches on remote host
            run("if exist testbranch/nul rmdir /s /q testbranch") 
            run("if exist stormtestbranch.tar del stormtestbranch.tar")
            run("if exist stormtestbranch.tar.gz del stormtestbranch.tar.gz")
            #copy exported branch tartile to test platform (storm) in user's root dir  
            filedestination = user + "@%s:" % hostname + tarfilename
            local('scp %s %s' % (filetocopy, filedestination)) 
            #unpack the tarfile
            run("7z.exe x %s" % tarfilename)
            tf2 = tarfilename.split(".")
            del tf2[-1]
            run("call 7z.exe x " + tarfilename.split(".")[0] + ".tar")
            run("call python testbranch\go-openmdao-dev.py")
            #Hack - Must build and test from a batch file in order for environment to be correct
            #Windows and fabric don't always play nicely together
            teststeps="""chdir testbranch\devenv\Scripts
                call activate.bat
                set PYTHON_EGG_CACHE=C:\Users\\%USERNAME%\\testbranch
                echo "environment activated, please wait while tests run"
                openmdao_test.exe -xv"""
            #need to export teststeps to batch file
            with open('winteststeps.bat', 'w') as f:
                f.write(teststeps)
            #Then copy the newly generated batch file to windows platform (storm)
            filetocopy = os.path.join(branchdir, 'winteststeps.bat')
            local('scp %s %s@%s:winteststeps.bat' % (filetocopy, user, hostname)) 
            #change to devenv\Scripts, activate the envronment, and run tests
            run('call winteststeps.bat')
            print('Tests completed on %s' % hostname)

def waitForLine(fname, linePattern, grepArgs=''):
    run("tail -F '%s' | grep -m 1 %s '%s'" % (fname, grepArgs, linePattern))

def main(argv=None):
    if argv is None:
        argv = sys.argv[1:]

    #Figure out what branch we're in
    startdir=os.getcwd()
    branchdir=subprocess.Popen(['git rev-parse --show-toplevel'], 
                               stdout=subprocess.PIPE, shell=True).communicate()[0]
    print("Testing repo %s" % branchdir)

    #parse through any command line options
    parser = OptionParser()
    parser.add_option("-l", "--runlocal", action="store_true", dest="runlocal", default=False,
                  help="force tests to run also on current platform")
    parser.add_option("-p", "--platform", action="append", dest="runplatforms",  
                   help="add a host url to run the tests on", 
                   default=["torpedo.grc.nasa.gov", "viper.grc.nasa.gov", 
                            "storm.grc.nasa.gov"])  
    (options, args) = parser.parse_args(argv)

    runlocal = options.runlocal
    currenthost = gethostname()

    print("Testing on hosts: %s" % options.runplatforms)

    # ensure that all network connections are closed
    # TODO: once we move to Fabric 0.9.4, just use disconnect_all() function
    tarfilename = "testbranch.tar.gz"
    try:
        _make_archive(tarfilename)
        for hostname in options.runplatforms:
            if not runlocal and (currenthost == hostname):
                print("skipping tests on %s" % currenthost)  #skip local platform unless runlocal is true
            else:
                _testbranch(hostname)
        os.remove(tarfilename)
    finally:
        for key in connections.keys():
            connections[key].close()
            del connections[key]

   
if __name__ == '__main__': #pragma: no cover
    main()
