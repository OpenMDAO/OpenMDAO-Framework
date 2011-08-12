import sys
import os
import shutil
import subprocess
import atexit
import fnmatch
from fabric.api import run, env, local, put, cd, get, settings, prompt, \
                       hide, show

from openmdao.devtools.utils import get_git_branch, repo_top, remote_tmpdir, \
                                    push_and_run, rm_remote_tree, make_git_archive,\
                                    fabric_cleanup, remote_listdir, remote_mkdir,\
                                    ssh_test
from openmdao.devtools.remote_cfg import CfgOptionParser, process_options, \
                                         run_host_processes

from openmdao.devtools.tst_ec2 import run_on_ec2_image

import paramiko.util

def test_on_remote_host(remotedir=None, fname=None, 
                        pyversion='python', keep=False, 
                        branch=None, testargs=(), hostname='', **kwargs):
    if remotedir is None:
        raise RuntimeError("test_on_remote_host: missing arg 'remotedir'")
    if fname is None:
        raise RuntimeError("test_on_remote_host: missing arg 'fname'")
    
    remote_mkdir(remotedir)
    
    locbldfile = os.path.join(os.path.dirname(__file__), 'locbuild.py')
    loctstfile = os.path.join(os.path.dirname(__file__), 'loctst.py')
    
    if fname.endswith('.py'):
        build_type = 'release'
    else:
        build_type = 'dev'
        
    if os.path.isfile(fname):
        remotefname = os.path.join(remotedir, os.path.basename(fname))
        print 'putting %s on remote host' % fname
        put(fname, remotefname) # copy file to remote host
        remoteargs = ['-f', os.path.basename(fname)]
    else:
        remoteargs = ['-f', fname]
        
    remoteargs.append('--pyversion=%s' % pyversion)
    if branch:
        remoteargs.append('--branch=%s' % branch)
        
    expectedfiles = set(['locbuild.py','build.out'])
    dirfiles = set(remote_listdir(remotedir))
    
    print 'building...'
    with settings(warn_only=True):
        result = push_and_run(locbldfile, 
                              remotepath=os.path.join(remotedir,
                                                      os.path.basename(locbldfile)),
                              args=remoteargs)
    print result
    
    # for some reason, even when the build works fine, there is a non-zero
    # return code, so we just print it here and keep going regardless of the
    # value.
    print "build return code =", result.return_code
    
    print '\ntesting...'
    newfiles = set(remote_listdir(remotedir)) - dirfiles - expectedfiles
    
    if build_type == 'dev':
        if len(newfiles) != 1:
            raise RuntimeError("expected a single new file in %s after building but got %s" %
                               (remotedir, list(newfiles)))
        
        builddir = newfiles.pop()
        envdir = os.path.join(builddir, 'devenv')
    else: # test a release
        print 'newfiles = ',newfiles
        matches = fnmatch.filter(newfiles, 'openmdao-?.*')
        if len(matches) > 1:
            raise RuntimeError("can't uniquely determine openmdao env directory from %s" % matches)
        elif len(matches) == 0:
            raise RuntimeError("can't find an openmdao environment directory to test in")
        envdir = matches[0]

    remoteargs = ['-d', envdir]
    remoteargs.append('--pyversion=%s' % pyversion)
    if keep:
        remoteargs.append('--keep')
    if len(testargs) > 0:
        remoteargs.append('--')
        remoteargs.extend(testargs)
        
    result = push_and_run(loctstfile, 
                          remotepath=os.path.join(remotedir,
                                                  os.path.basename(loctstfile)),
                          args=remoteargs)
    print result
        
    if remotedir is not None and (result.return_code==0 or not keep):
        rm_remote_tree(remotedir)
        
    return result.return_code
        

def test_branch(argv=None):
    atexit.register(fabric_cleanup, True)
    paramiko.util.log_to_file('paramiko.log')
    
    if argv is None:
        argv = sys.argv[1:]
        
    parser = CfgOptionParser()
    parser.add_option("-k","--keep", action="store_true", dest='keep',
                      help="if there are test/build failures, don't delete "
                           "the temporary build directory "
                           "or terminate the remote instance if testing on EC2.")
    parser.add_option("-f","--file", action="store", type='string', dest='fname',
                      help="pathname of a tarfile or URL of a git repo")
    parser.add_option("-b","--branch", action="store", type='string', 
                      dest='branch',
                      help="if file_url is a git repo, supply branch name here")

    (options, args) = parser.parse_args(argv)
    
    config, conn, image_hosts = process_options(options)
    
    startdir = os.getcwd()
    
    if options.fname is None: # assume we're testing the current repo
        print 'creating tar file of current branch: ',
        options.fname = os.path.join(os.getcwd(), 'testbranch.tar')
        ziptarname = options.fname+'.gz'
        if os.path.isfile(ziptarname): # clean up the old tar file
            os.remove(ziptarname)
        make_git_archive(options.fname)
        subprocess.check_call(['gzip', options.fname])
        options.fname = os.path.abspath(options.fname+'.gz')
        print options.fname
        
    fname = os.path.abspath(os.path.expanduser(options.fname))
    
    if fname.endswith('.tar.gz') or fname.endswith('.tar'):
        if not os.path.isfile(fname):
            print "can't find file '%s'" % fname
            sys.exit(-1)
    elif fname.endswith('.git'):
        pass
    else:
        parser.print_help()
        print "\nfilename must end in '.tar.gz', '.tar', or '.git'"
        sys.exit(-1)
        
    funct_kwargs = { 'keep': options.keep,
                     'testargs': args,
                     'fname': fname,
                     'remotedir': options.remotedir,
                     'branch': options.branch,
                     }
        
    run_host_processes(config, conn, image_hosts, options, 
                       funct=test_on_remote_host, funct_kwargs=funct_kwargs)
    

def test_release(argv=None):
    atexit.register(fabric_cleanup, True)
    paramiko.util.log_to_file('paramiko.log')
    
    if argv is None:
        argv = sys.argv[1:]
        
    parser = CfgOptionParser()
    parser.add_option("-k","--keep", action="store_true", dest='keep',
                      help="if there are test/build failures, don't delete "
                           "the temporary build directory "
                           "or terminate the remote instance if testing on EC2.")
    parser.add_option("-f","--file", action="store", type='string', dest='fname',
                      help="pathname or URL of a go-openmdao.py file")

    (options, args) = parser.parse_args(argv)
    
    config, conn, image_hosts = process_options(options)
    
    startdir = os.getcwd()
    
    if options.fname is None:
        print 'you must supply the pathname or URL of a go-openmdao.py file'
        sys.exit(-1)
        
    fname = options.fname
    if not fname.startswith('http'):
        fname = os.path.abspath(os.path.expanduser(fname))
    
    if fname.endswith('.py'):
        if not fname.startswith('http'):
            if not os.path.isfile(fname):
                print "can't find file '%s'" % fname
                sys.exit(-1)
    else:
        parser.print_help()
        print "\nfilename must be a pathname or URL of a go-openmdao.py file"
        sys.exit(-1)
        
    funct_kwargs = { 'keep': options.keep,
                     'testargs': args,
                     'fname': fname,
                     'remotedir': options.remotedir,
                     }
        
    run_host_processes(config, conn, image_hosts, options, 
                       funct=test_on_remote_host, funct_kwargs=funct_kwargs)

# make nose ignore these functions
test_release.__test__ = False
test_branch.__test__ = False

