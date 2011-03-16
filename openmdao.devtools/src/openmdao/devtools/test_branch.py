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
#import urllib2

paramiko.util.log_to_file('paramiko.log')


def _testbranch(hostname):
    """Builds and runs tests on a branch on a specified host platform.
    You can run from anywhere in the branch, but recommend running from branchroot/scripts dir.
    """
    print('running tests on %s' % hostname)

    startdir=os.getcwd()
    branchdir=local('bzr root').strip()    
    #print("starting directory is %s" % startdir)
    remotehost = hostname.split(".")[0]
    tarfilename=remotehost+"testbranch.tar.gz"
    #print("tarfilename is %s" % tarfilename)    
    #export the current branch to a tarfile
    os.chdir(branchdir)  #change to top dir of branch
    local("bzr export %s --root=testbranch" % tarfilename)
    #paramiko_log="paramiko.log." + remotehost
    #paramiko.util.log_to_file(paramiko_log)    #this doesn't work
      
    winplatforms=["storm.grc.nasa.gov"]  #list of windows platforms to remote into
    if hostname in winplatforms:     #if we are remoting into a windows host
        devbindir='devenv\Scripts'
        unpacktar="7z x" 
        pyversion="python"   #for some reason, on storm the python2.6 alias doesn't work on storm
        removeit="""rmdir /s /q"""  #previously in triple quotes
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
            #run('%s testbranch' % removeit)  
            #copy exported branch tartile to test platform in user's root dir 
            put(filetocopy, tarfilename)
            #unpack the tarfile
            run('%s %s' % (unpacktar, tarfilename))  
            with cd('testbranch'):
                #Make a .bzr directory to fool go-openmdao-dev.py into thinking this is a real repository
                run('mkdir .bzr')   
                #build it
                run('%s go-openmdao-dev.py' % pyversion)
                #change to devenv/bin, activate the envronment, and run tests
                with cd(devbindir):
                    print("Please wait while the environment is activated and the tests are run")
                    run('source activate && echo $PATH && echo environment activated, please wait while tests run && openmdao_test -xv')
                    print('Tests completed on %s' % hostname)
         
        else:  #we're remoting into windows (storm)
            #check connection on windows to prevent hanging due to ndc password change
            #checkcon=run("dir")
            #print("check succeeded is %s" % checkcon.succeeded)
	    #sys.exit()
            #remove any previous testbranches on remote host
            run("""if exist testbranch/nul rmdir /s /q testbranch""") 
            run("""if exist stormtestbranch.tar del stormtestbranch.tar""")     
            run("""if exist stormtestbranch.tar.gz del stormtestbranch.tar.gz""")    
            #copy exported branch tartile to test platform (storm) in user's root dir  
            filedestination = user + """@storm.grc.nasa.gov:""" + tarfilename   
            local('scp %s %s' % (filetocopy, filedestination)) 
            #unpack the tarfile
            run("7z.exe x %s" % tarfilename)
            tf2 = tarfilename.split(".")
	    del tf2[-1]
            run("""call 7z.exe x """ + tarfilename.split(".")[0] + """.tar""")
            run("""call python testbranch\go-openmdao-dev.py""")  
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
            f.close()
            #Then copy the newly generated batch file to windows platform (storm)
            filetocopy = os.path.join(branchdir, 'winteststeps.bat')
            filedestination = user + """@storm.grc.nasa.gov:winteststeps.bat"""   #probably should be generic!!!!!
            local('scp %s %s' % (filetocopy, filedestination)) 
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
    branchdir=subprocess.Popen(["bzr root"], stdout=subprocess.PIPE, shell=True).communicate()[0]
    #print("starting directory is %s" % startdir)
    print("Testing on branch %s" % branchdir)

    #fabfilename="fabfile.py"      #no longer need this
    #fabfilepath=os.path.join(startdir, fabfilename)
    #specifyfabfile="--fabfile=%s" % fabfilepath
    
    #parse through any command line options
    parser = OptionParser()
    parser.add_option("-v", "--verbose", action="store_true", dest="verbose", help="show all output") #keep this???
    parser.add_option("-l", "--runlocal", action="store_true", dest="runlocal", default=False,
                  help="force tests to run also on current platform")	
    parser.add_option("-i", "--ignorebzr", action="store_true", dest="ignoreBzrStatus", default=False,
                   help="runs tests even if there are uncommitted files")			   	   		   
    parser.add_option("-p", "--platform", action="append", dest="runplatforms",  
                   help="add a host url to run the tests on")  
    (options, args) = parser.parse_args(argv)

    runlocal = options.runlocal
    ignoreBzrStatus = options.ignoreBzrStatus
    #currenthost = gethostname().split('.')[0]
    currenthost = gethostname()

    #Check for uncommitted changes first
    uncommittedChanges=subprocess.Popen(['bzr status -SV'], stdout=subprocess.PIPE, shell=True).communicate()[0]
    if uncommittedChanges is not None: 
        #There are uncommitted changes
        if not ignoreBzrStatus: #raise error if uncommitted changes on current branch and not set to ignore
            raise RuntimeError("There are uncommitted changes on this branch.  Please commit changes then restart this script.")
        else:   #if running special debugging version, you'll get a msg only and will be allowed to continue
            print('There are uncommitted changes on this branch.  Continue at your own risk')

    if options.runplatforms is not None:    #replace defaults with command line options, if they exist
        runplatforms = options.runplatforms
    else:
        runplatforms = ["torpedo.grc.nasa.gov", "viper.grc.nasa.gov", "storm.grc.nasa.gov"] #default platforms to run tests on
    #numberofplatforms=len(runplatforms)    #probably don't need anymore 
    print("Testing on hosts: %s" % runplatforms)


    #%env_vars = os.environ.copy()   #Do I really need this??
        
    # ensure that all network connections are closed
    # TODO: once we move to Fabric 0.9.4, just use disconnect_all() function
    try:
        for hostname in runplatforms:
            if not runlocal and (currenthost == hostname):
                print("skipping tests on %s" % currenthost)  #skip local platform unless runlocal is true
            else:
	        _testbranch(hostname)
    finally:
        for key in connections.keys():
            connections[key].close()
            del connections[key]
	    
	    
	    
	    
"""            #outfile="out"+remotehost
            #fileout=open(outfile, "w")
            print('running tests on %s' % remotehost)
            #call the _testbranch function with appropriate host arg  
            cmd = "fab testbranch:host=" + remotehost
	    #cmd2="fab "+ specifyfabfile + " testbranch:host=" + remotehost
            print("cmd is %s" % cmd)
            #print("Please wait while tests are running on %s" % remotehost)
            #os.system(cmd)    #old style system command
            cmdargs=["fab, testbranch:host=" + remotehost]
            print("cmdargs is %s" % cmdargs)
            #testout=subprocess.Popen([cmdargs], stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True).communicate()[0]
            p=subprocess.Popen([cmd], env=env_vars, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            (stdout, stderr) = p.communicate()
            #subprocess.Popen([cmd], stdout=subprocess.PIPE, shell=True).communicate()[0]
            #subprocess.Popen([cmd], stdout=fileout, shell=True).communicate()[0]
            #fileout.close()
            #if hostname=="storm":
            #    processes.append(subprocess.Popen([cmd], stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=False))	    
            #else:
            #    processes.append(subprocess.Popen([cmd], stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True))
            #print("process has been appended")
        # Handle error condition (deal with stdout being None, too)

        class _AttributeString(str): pass	
        out = _AttributeString(stdout.strip() if stdout else "")
        err = _AttributeString(stderr.strip() if stderr else "")
        out.failed = False
        out.stderr = err
        out.return_code = p.returncode
        if p.returncode != 0:
            out.failed = True
            msg = "local() encountered an error (return code %s) while executing '%s'" % (p.returncode, command)
            _handle_failure(message=msg)
        out.succeeded = not out.failed
        # If we were capturing, this will be a string; otherwise it will be None.
        #return out
	print(out)
    
    print("now, we're done, but where is the output?")"""
   
if __name__ == '__main__': #pragma: no cover
    main()
