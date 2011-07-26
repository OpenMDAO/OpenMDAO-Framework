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
import ConfigParser

from openmdao.devtools.utils import get_git_branch, repo_top, remote_tmpdir, \
                                    push_and_run, rm_remote_tree, make_git_archive,\
                                    fabric_cleanup
from openmdao.devtools.tst_ec2 import run_on_ec2_host
from openmdao.util.debug import print_fuct_call

#import paramiko.util
#paramiko.util.log_to_file('paramiko.log')

def test_on_remote_host(fname, pyversion='python', keep=False, 
                        branch=None, testargs=(), hostname=''):
    loctstfile = os.path.join(os.path.dirname(__file__), 'loctst.py')
    
    try:
        remtmp = remote_tmpdir()
    except Exception as err:
        print str(err)
        sys.exit(-1)
    if remtmp.failed:
        print "creation of remote temp dir failed"
        sys.exit(-1)
    print "remote temp dir = ", remtmp
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
        retcode = push_and_run(loctstfile, 
                               remotepath=os.path.basename(loctstfile),
                               args=remoteargs)
        if retcode != 0:
            print 'retrieving build file from failed host %s' % hostname
            get('_build.out', '%s_build.out' % hostname)
    finally:
        if remtmp is not None and (retcode==0 or not keep):
            rm_remote_tree(remtmp)

def run_on_host(host, config, funct, settings_kwargs=None, *args, **kwargs):
    if settings_kwargs is None:
        settings_kwargs = {}
    settings_args = []
    
    debug = config.getboolean(host, 'debug')
    settings_kwargs['host_string'] = config.get(host, 'addr')
        
    if debug:
        print "settings_kwargs = ", str(settings_kwargs)
        print "calling %s" % print_fuct_call(funct, *args, **kwargs)
    else:
        settings_args.append(hide('running'))

    with settings(*settings_args, **settings_kwargs):
        return funct(*args, **kwargs)
            
        
def main(argv=None):
    
    if argv is None:
        argv = sys.argv[1:]
        
    parser = OptionParser()
    parser.add_option("-c", "--config", action='store', dest='cfg', metavar='CONFIG',
                      default='~/.openmdao/testing.cfg',
                      help="path of config file where info for test hosts is located")
    parser.add_option("--host", action='append', dest='hosts', metavar='HOST',
                      default=[],
                      help="select host from config file to run tests on. "
                           "If not supplied, tests will run on all hosts in "
                           "config file. To test on a subset of the hosts in "
                           "the config file, use multiple --host args")
    parser.add_option("-k","--keep", action="store_true", dest='keep',
                      help="if there are test/build failures, don't delete "
                           "the temporary build directory "
                           "or terminate the remote instance if testing on EC2.")
    parser.add_option("-f","--file", action="store", type='string', dest='fname',
                      help="pathname of a tarfile or URL of a git repo")
    parser.add_option("-b","--branch", action="store", type='string', 
                      dest='branch',
                      help="if file_url is a git repo, supply branch name here")

    (options, args) = parser.parse_args(sys.argv[1:])
    
    config = ConfigParser.ConfigParser()
    config.readfp(open(options.cfg))
    
    hostlist = config.sections()
    if options.hosts:
        hosts = []
        for host in options.hosts:
            if host in hostlist:
                hosts.append(host)
            else:
                raise RuntimeError("host '%s' is not in config file %s" % 
                                   (host, options.cfg))
    else:
        hosts = hostlist

    if not hosts:
        print "no hosts were found in config file %s" % options.cfg
        sys.exit(-1)
        
    if options.fname is None: # assume we're testing the current repo
        options.fname = os.path.join(os.getcwd(), 'testbranch.tar')
        ziptarname = options.fname+'.gz'
        if os.path.isfile(ziptarname): # clean up the old tar file
            os.remove(ziptarname)
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
    ec2_hosts = set()
    for host in hosts:
        if config.has_option(host, 'addr'):
            addr = config.get(host, 'addr')
            if addr.startswith('ec2-') or '@ec2-' in addr:
                ec2_hosts.add(host)
        if config.has_option(host, 'image_id'):
            ec2_hosts.add(host)
                
    hosts = set(hosts) - ec2_hosts
    
    if len(ec2_hosts) > 0:
        from boto.ec2.connection import EC2Connection
        conn = EC2Connection()

    for host in hosts:
        run_on_host(host, config, test_on_remote_host, None, fname,
                    keep=options.keep, branch=options.branch,
                    testargs=args, hostname=host)
        
    for host in ec2_hosts:
        run_on_ec2_host(host, config, conn, test_on_remote_host, 
                        fname, keep=options.keep, branch=options.branch,
                        testargs=args, hostname=host)
            
if __name__ == '__main__': #pragma: no cover
    atexit.register(fabric_cleanup, True)
    main()
