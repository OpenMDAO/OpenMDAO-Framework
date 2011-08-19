import sys
import os
import subprocess
import atexit
import fnmatch
from fabric.api import run, env, local, put, cd, get, settings, prompt, \
                       hide, show

from openmdao.devtools.utils import get_git_branch, repo_top, remote_tmpdir, \
                                    push_and_run, rm_remote_tree, make_git_archive,\
                                    fabric_cleanup, remote_listdir, remote_mkdir,\
                                    ssh_test, put_dir, cleanup
from openmdao.devtools.remote_cfg import CfgOptionParser, process_options, \
                                         run_host_processes, get_tmp_user_dir

from openmdao.devtools.ec2 import run_on_ec2

import paramiko.util

def _remote_build_and_test(fname=None, pyversion='python', keep=False, 
                          branch=None, testargs=(), hostname='', **kwargs):
    if fname is None:
        raise RuntimeError("_remote_build_and_test: missing arg 'fname'")
    
    remotedir = get_tmp_user_dir()
    remote_mkdir(remotedir)
    
    locbldtstfile = os.path.join(os.path.dirname(__file__), 'loc_bld_tst.py')
    
    pushfiles = [locbldtstfile]
    
    build_type = 'release' if fname.endswith('.py') else 'dev'
        
    if os.path.isfile(fname):
        pushfiles.append(fname)
        remoteargs = ['-f', os.path.basename(fname)]
    elif os.path.isdir(fname):
        put_dir(fname, os.path.join(remotedir, os.path.basename(fname)))
        if sys.platform.startswith('win'):
            vername = 'latest' #readlink doesn't work on windows, so try 'latest'
        else:
            vername = os.readlink(os.path.join(fname,
                                               'downloads',
                                               'latest'))
        remoteargs = ['-f', os.path.join(os.path.basename(fname),
                                         'downloads', 
                                         vername,
                                         'go-openmdao.py')]
    else:
        remoteargs = ['-f', fname]
        
    remoteargs.append('--pyversion=%s' % pyversion)
    if branch:
        remoteargs.append('--branch=%s' % branch)
        
    if len(testargs) > 0:
        remoteargs.append('--')
        remoteargs.extend(testargs)
        
    try:
        result = push_and_run(pushfiles, remotedir=remotedir, args=remoteargs)
        print result
        return result.return_code
    finally:
        if not keep:
            print "removing remote directory: %s" % remotedir
            rm_remote_tree(remotedir)

def test_branch(argv=None):
    atexit.register(fabric_cleanup, True)
    paramiko.util.log_to_file('paramiko.log')
    
    if argv is None:
        argv = sys.argv[1:]
        
    parser = CfgOptionParser()
    parser.add_option("-k","--keep", action="store_true", dest='keep',
                      help="If there are test/build failures, don't delete "
                           "the temporary build directory "
                           "or terminate the remote instance if testing on EC2.")
    parser.add_option("-f","--file", action="store", type='string', dest='fname',
                      help="Pathname of a tarfile or URL of a git repo")
    parser.add_option("-b","--branch", action="store", type='string', 
                      dest='branch',
                      help="If file is a git repo, supply branch name here")

    (options, args) = parser.parse_args(argv)
    
    config, conn, ec2_hosts = process_options(options, parser)
    
    if not options.hosts:
        parser.print_help()
        print "nothing to do - no hosts specified"
        sys.exit(0)
    
    startdir = os.getcwd()
    
    if options.fname is None: # assume we're testing the current repo
        print 'creating tar file of current branch: ',
        options.fname = os.path.join(os.getcwd(), 'testbranch.tar')
        ziptarname = options.fname+'.gz'
        cleanup(ziptarname) # clean up the old tar file
        make_git_archive(options.fname)
        subprocess.check_call(['gzip', options.fname])
        options.fname = os.path.abspath(ziptarname)
        print options.fname
        cleanup_tar = True
    else:
        cleanup_tar = False
        
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
                     'remotedir': get_tmp_user_dir(),
                     'branch': options.branch,
                     }
        
    try:
        retcode = run_host_processes(config, conn, ec2_hosts, options, 
                                     funct=_remote_build_and_test, 
                                     funct_kwargs=funct_kwargs)
    finally:
        if cleanup_tar:
            cleanup(ziptarname)
    
    if retcode == 0:
        cleanup('paramiko.log')
        
    return retcode

def _is_release_dir(dname):
    if not isinstance(dname, basestring):
        return False
    if not os.path.isdir(dname):
        return False
    dirstuff = os.listdir(dname)
    if not 'dists' in dirstuff:
        return False
    return 'downloads' in dirstuff

def test_release(argv=None):
    atexit.register(fabric_cleanup, True)
    paramiko.util.log_to_file('paramiko.log')
    cleanup_files = ['paramiko.log']
    
    if argv is None:
        argv = sys.argv[1:]
        
    parser = CfgOptionParser()
    parser.add_option("-k","--keep", action="store_true", dest='keep',
                      help="If there are test/build failures, don't delete "
                           "the temporary build directory "
                           "or terminate the remote instance if testing on EC2.")
    parser.add_option("-f","--file", action="store", type='string', dest='fname',
                      help="URL or pathname of a go-openmdao.py file or pathname of a release dir")

    (options, args) = parser.parse_args(argv)
    
    if options.fname is None:
        parser.print_help()
        print '\nyou must supply a release directory or the pathname or URL of a go-openmdao.py file'
        sys.exit(-1)
        
    config, conn, ec2_hosts = process_options(options, parser)
    
    startdir = os.getcwd()
    
    fname = options.fname
    if fname.startswith('http'):
        # if they cut & paste from the openmdao website, the fname
        # will be followed by #md5=..., so get rid of that part
        fname = fname.split('#')[0]
    else:
        fname = os.path.abspath(os.path.expanduser(fname))
    
    if fname.endswith('.py'):
        if not fname.startswith('http'):
            if not os.path.isfile(fname):
                print "can't find file '%s'" % fname
                sys.exit(-1)
    elif _is_release_dir(fname):
        pass
    elif os.path.isdir(fname):
        #create a structured release directory
        release_dir = "%s__release" % fname
        subprocess.check_call(['push_release', fname, release_dir])
        fname = options.fname = release_dir
        cleanup_files.append(release_dir)
    else:
        parser.print_help()
        print "\nfilename must be a release directory or a pathname or URL of a go-openmdao.py file"
        sys.exit(-1)
        
    funct_kwargs = { 'keep': options.keep,
                     'testargs': args,
                     'fname': fname,
                   }
    retval = 0
    if len(options.hosts) > 0:
        retval = run_host_processes(config, conn, ec2_hosts, options, 
                                    funct=_remote_build_and_test, 
                                    funct_kwargs=funct_kwargs)
    if not options.keep:
        cleanup(*cleanup_files)

        
# make nose ignore these functions
test_release.__test__ = False
test_branch.__test__ = False
_remote_build_and_test.__test__ = False
