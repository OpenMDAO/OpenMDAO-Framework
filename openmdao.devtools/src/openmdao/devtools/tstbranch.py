#testbranch script (formerly in fabfile.py)

import sys
import os
import shutil
import subprocess
import atexit
from optparse import OptionParser
from fabric.api import run, env, local, put, cd, get, settings, prompt, hide, hosts
from fabric.state import connections
from socket import gethostname

from openmdao.devtools.utils import get_git_branch, repo_top, remote_tmpdir, \
                                    push_and_run, rm_remote_tree, make_git_archive,\
                                    fabric_cleanup
from openmdao.devtools.tst_ec2 import run_on_ec2_host, imagedict

#import paramiko.util
#paramiko.util.log_to_file('paramiko.log')

# machine name: (image id, platform)
vminfo = {
    'lovejoy': ('ami-2638c34f', 'c1.medium', 'linux2'),
    'sideshowbob': ('ami-1cf20975', 'c1.medium', 'win32'),
    'discostu': ('ami-3038c359', 'm1.large', 'linux2'),
    'smithers': ('ami-72e3181b', 'm1.large', 'win32'),
    }


def test_on_remote_host(fname, pyversion='python', keep=False, 
                        branch=None, testargs=()):
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
    if len(testargs) > 0:
        remoteargs.append('--')
        remoteargs.extend(testargs)
    try:
        push_and_run(loctstfile, 
                     remotepath=os.path.basename(loctstfile),
                     args=remoteargs)
    finally:
        if remtmp is not None and not keep:
            rm_remote_tree(remtmp)

def run_on_host(host, funct, settings_args=None, *args, **kwargs):
    if settings_args is None:
        settings_args = {}
    
    settings_args['host_string'] = host
        
    with settings(**settings_args):
        funct(*args, **kwargs)
            
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
    parser.add_option("-f","--file", action="store", type='string', 
                      dest='fname',
                      help="pathname of a tarfile or URL of a git repo")
    parser.add_option("-b","--branch", action="store", type='string', 
                      dest='branch',
                      help="if file_url is a git repo, supply branch name here")
    parser.add_option("-i","--identity", action="store", type='string', 
                      dest='identity', default='~/.ssh/lovejoy.pem',
                      help="pathname of identity file needed for testing on EC2. "
                           "default is '~/.ssh/lovejoy.pem'")

    (options, args) = parser.parse_args(sys.argv[1:])
    
    if options.hosts is None:
        options.hosts = [h.strip() for h in 
                           os.environ.get('OPENMDAO_TEST_HOSTS','').split(':')]
        
    if options.hosts is None:
        print "you must supply host(s) to test the branch on"
        sys.exit(-1)
            
    # make sure fabric connections are all closed when we exit
    atexit.register(fabric_cleanup, True)
    
    if options.fname is None: # assume we're testing the current repo
        options.fname = os.path.join(os.getcwd(), 'testbranch.tar')
        make_git_archive(options.fname)
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
        
    # first, find out which hosts are ec2 hosts, if any
    ec2_hosts = []
    hosts = []
    for host in options.hosts:
        if host in imagedict or host.startswith('ec2-'):
            ec2_hosts.append(host)
        else:
            hosts.append(host)
    
    if len(ec2_hosts) > 0:
        from boto.ec2.connection import EC2Connection
        conn = EC2Connection()
        key_name = os.path.splitext(os.path.basename(options.identity))[0]

    for host in hosts:
        run_on_host(host, test_on_remote_host, None, fname, pyversion=options.pyversion,
                         keep=options.keep, branch=options.branch,
                         testargs=args)
        
    for host in ec2_hosts:
        print 'would test on %s' % host
        if False:
            run_on_ec2_host(host, conn, options.identity, key_name,
                        test_on_remote_host, fname, pyversion=options.pyversion,
                        keep=options.keep, branch=options.branch,
                        testargs=args)
            
if __name__ == '__main__': #pragma: no cover
    main()
