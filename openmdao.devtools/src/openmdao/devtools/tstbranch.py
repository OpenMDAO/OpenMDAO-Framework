#testbranch script (formerly in fabfile.py)

import sys
import os
import shutil
import subprocess
from optparse import OptionParser
from fabric.api import run, env, local, put, cd, get, settings, prompt, hide, hosts
from fabric.state import connections
from socket import gethostname

from openmdao.devtools.utils import get_git_branch, repo_top, remote_tmpdir, \
                                    push_and_run

#import paramiko.util
#paramiko.util.log_to_file('paramiko.log')


#def _testbranch(hostname, tarfilename):
    #"""Builds and runs tests on a branch on a specified host platform.
    #"""
    #print('running tests on %s' % hostname)
    #remotehost = hostname.split(".")[0]
    #branchdir=local('git rev-parse --show-toplevel').strip()
      
    #winplatforms=["storm.grc.nasa.gov"]  #list of windows platforms to remote into
    #if hostname in winplatforms:     #if we are remoting into a windows host
        #devbindir='devenv\Scripts'
        #unpacktar="7z x" 
        #pyversion="python"   #for some reason, on storm the python2.6 alias doesn't work on storm
        #removeit="""rmdir /s /q""" 
        #env.shell="cmd /C"
        #user=env.user
        ## env.user="ndc\\"+env.user   #no longer need to preface username with ndc\\ to get into storm
    #else:
        #devbindir='devenv/bin'
        #unpacktar="tar xvf"
        #pyversion="python2.6"
        #removeit="rm -rf"
        #env.shell="/bin/bash -l -c"

    ##Copy exported branch tarfile to desired test platform in user's root dir
    #with settings(host_string=hostname):
        #filetocopy = os.path.join(branchdir, tarfilename)
        #if hostname not in winplatforms:     #if we are not remoting into a windows host
            ##remove any previous testbranches on remote host
            #res1=run('%s testbranch' % removeit)
            ##copy exported branch tartile to test platform in user's root dir 
            #put(filetocopy, tarfilename)
            ##unpack the tarfile
            #run('%s %s' % (unpacktar, tarfilename))  
            #with cd('testbranch'):
                ##Make a .git directory to fool go-openmdao-dev.py into thinking this is a real repository
                #run('mkdir .git')   
                ##build it
                #run('%s go-openmdao-dev.py' % pyversion)
                ##change to devenv/bin, activate the envronment, and run tests
                #with cd(devbindir):
                    #print("Please wait while the environment is activated and the tests are run")
                    #run('source activate && echo $PATH && echo environment activated, please wait while tests run && openmdao_test -xv')
                    #print('Tests completed on %s' % hostname)
            #res2=run('%s testbranch' % removeit) 
            #res3=run('%s *testbranch.tar *testbranch.tar.gz' % removeit)
         
        #else:  #we're remoting into windows (storm)
            ##remove any previous testbranches on remote host
            #run("if exist testbranch/nul rmdir /s /q testbranch") 
            #run("if exist stormtestbranch.tar del stormtestbranch.tar")
            #run("if exist stormtestbranch.tar.gz del stormtestbranch.tar.gz")
            ##copy exported branch tartile to test platform (storm) in user's root dir  
            #filedestination = user + "@%s:" % hostname + tarfilename
            #local('scp %s %s' % (filetocopy, filedestination)) 
            ##unpack the tarfile
            #run("7z.exe x %s" % tarfilename)
            #tf2 = tarfilename.split(".")
            #del tf2[-1]
            #run("call 7z.exe x " + tarfilename.split(".")[0] + ".tar")
            #run("call python testbranch\go-openmdao-dev.py")
            ##Hack - Must build and test from a batch file in order for environment to be correct
            ##Windows and fabric don't always play nicely together
            #teststeps="""chdir testbranch\devenv\Scripts
                #call activate.bat
                #set PYTHON_EGG_CACHE=C:\Users\\%USERNAME%\\testbranch
                #echo "environment activated, please wait while tests run"
                #openmdao_test.exe -xv"""
            ##need to export teststeps to batch file
            #with open('winteststeps.bat', 'w') as f:
                #f.write(teststeps)
            ##Then copy the newly generated batch file to windows platform (storm)
            #filetocopy = os.path.join(branchdir, 'winteststeps.bat')
            #local('scp %s %s@%s:winteststeps.bat' % (filetocopy, user, hostname)) 
            ##change to devenv\Scripts, activate the envronment, and run tests
            #run('call winteststeps.bat')
            #print('Tests completed on %s' % hostname)

#def main(argv=None):
    #if argv is None:
        #argv = sys.argv[1:]

    #startdir = os.getcwd()
    #branchdir = repo_top()
    #print("Testing branch %s" % get_git_branch())

    ##parse through any command line options
    #parser = OptionParser()
    #parser.add_option("-l", "--runlocal", action="store_true", dest="runlocal", default=False,
                  #help="force tests to run also on current platform")
    #parser.add_option("-p", "--platform", action="append", dest="runplatforms",  
                   #help="add a host url to run the tests on", 
                   #default=["torpedo.grc.nasa.gov", "viper.grc.nasa.gov", 
                            #"storm.grc.nasa.gov"])  
    #(options, args) = parser.parse_args(argv)

    #runlocal = options.runlocal
    #currenthost = gethostname()

    #print("Testing on hosts: %s" % options.runplatforms)

    ## ensure that all network connections are closed
    ## TODO: once we move to Fabric 0.9.4, just use disconnect_all() function
    #tarfilename = "testbranch.tar.gz"
    #try:
        #_make_archive(tarfilename)
        #for hostname in options.runplatforms:
            #if not runlocal and (currenthost == hostname):
                #print("skipping tests on %s" % currenthost)  #skip local platform unless runlocal is true
            #else:
                #_testbranch(hostname)
        #os.remove(tarfilename)
    #finally:
        #for key in connections.keys():
            #connections[key].close()
            #del connections[key]

def _make_archive(tarfilename):
    #export the current branch to a tarfile
    startdir = os.getcwd()
    os.chdir(repo_top())
    try:
        local("git archive -o %s --prefix=testbranch/ HEAD" % tarfilename)
    finally:
        os.chdir(startdir)
        
def _test_remote(fname, pyversion='python', keep=False, 
                 branch=None, args=()):
    loctstfile = os.path.join(os.path.dirname(__file__), 'loctst.py')
    
    remtmp = remote_tmpdir()
    remote_script_name = os.path.join(remtmp, os.path.basename(loctstfile))
    if os.path.isfile(fname):
        remotefname = os.path.join(remtmp, os.path.basename(fname))
        put(fname, remotefname) # copy file to remote host
        remoteargs = ['-f', os.path.basename(fname)]
    else:
        remoteargs = ['-f', fname]
        
    remoteargs.append('--pyversion=%s' % pyversion)
    if keep:
        remoteargs.append('--keep')
    if branch:
        remoteargs.append('--branch=%s' % branch)
    remoteargs.extend(args)
    try:
        push_and_run(loctstfile, 
                     remotepath=os.path.basename(loctstfile),
                     args=remoteargs)
    finally:
        if remtmp is not None and not keep:
            rm_remote_tree(remtmp)

def main(argv=None):
    if argv is None:
        argv = sys.argv[1:]
        
    parser = OptionParser()
    parser.add_option("--host", action='append', dest='hosts', default=[],
                      metavar='HOST',
                      help="add a host to test the current branch on")
    parser.add_option("--pyversion", action="store", type='string', 
                      dest='pyversion',
                      default="python", 
                      help="python version to use, e.g., 'python2.6'")
    parser.add_option("-k","--keep", action="store_true", dest='keep',
                      help="don't delete temporary build directory")
    parser.add_option("-b","--branch", action="store", type='string', 
                      dest='branch',
                      help="if file_url is a git repo, supply branch name here")
    parser.add_option("-f","--file", action="store", type='string', 
                      dest='fname',
                      help="pathname of a tarfile or URL of a git repo")

    (options, args) = parser.parse_args(sys.argv[1:])
    
    if options.hosts is None:
        options.hosts = [h.strip() for h in 
                           os.environ.get('OPENMDAO_TEST_HOSTS','').split(':')]
        
    if options.hosts is None:
        print "you must supply host(s) to test the branch on"
        sys.exit(-1)
            
    try:
        if options.fname is None: # assume testing current repo
            options.fname = os.path.join(os.getcwd(), 'testbranch.tar')
            _make_archive(options.fname)
            subprocess.check_call(['gzip', options.fname])
            options.fname = options.fname+'.gz'
            
        fname = options.fname
        
        if fname.endswith('.tar.gz') or fname.endswith('.tar'):
            if not os.path.isfile(fname):
                print "can't find tar file '%s'" % fname
                sys.exit(-1)
        elif fname.endswith('.git'):
            pass
        else:
            parser.print_help()
            print "\nfilename must end in '.tar.gz', '.tar', or '.git'"
            sys.exit(retcode)
            
        # TODO: run these concurrently
        for host in options.hosts:
            with settings(host_string=host):
                print "testing %s on host %s" % (fname, host)
                _test_remote(fname, pyversion=options.pyversion,
                             keep=options.keep, branch=options.branch,
                             args=args)
    finally:
        # ensure that all network connections are closed
        # TODO: once we move to Fabric 0.9.4, just use disconnect_all() function
        for key in connections.keys():
            connections[key].close()
            del connections[key]
            
if __name__ == '__main__': #pragma: no cover
    main()
