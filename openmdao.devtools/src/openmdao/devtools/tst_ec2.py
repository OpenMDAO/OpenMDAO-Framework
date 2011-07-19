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
                                    push_and_run, rm_remote_tree

#import paramiko.util
#paramiko.util.log_to_file('paramiko.log')

# machine name: (image id, platform)
vminfo = {
    'lovejoy': ('ami-2638c34f', 'c1.medium', 'linux2'),
    'sideshowbob': ('ami-1cf20975', 'c1.medium', 'win32'),
    'discostu': ('ami-3038c359', 'm1.large', 'linux2'),
    'smithers': ('ami-72e3181b', 'm1.large', 'win32'),
    }


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
        remoteargs = ['-f', remotefname]
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
    parser.add_option("-i","--identity", action="store", type='string', 
                      dest='identity', default='~/.ssh/lovejoy.pem',
                      help="pathname of identity file")

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
            if host in vminfo:
                env.key_filename = [options.identity]
                env.user = 'ubuntu'
            else:
                env.key_filename = None
                env.user = None
                
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
