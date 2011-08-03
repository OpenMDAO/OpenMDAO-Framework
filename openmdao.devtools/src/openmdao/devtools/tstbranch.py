#testbranch script (formerly in fabfile.py)

import sys
import os
import shutil
import subprocess
import atexit
import time
import datetime
import getpass
import fnmatch
from optparse import OptionParser
from fabric.api import run, env, local, put, cd, get, settings, prompt, \
                       hide, show, hosts
from fabric.state import connections
from socket import gethostname
import ConfigParser
from multiprocessing import Process

from openmdao.devtools.utils import get_git_branch, repo_top, remote_tmpdir, \
                                    push_and_run, rm_remote_tree, make_git_archive,\
                                    fabric_cleanup, remote_listdir, remote_mkdir,\
                                    ssh_test
from openmdao.devtools.tst_ec2 import run_on_ec2_image
from openmdao.util.debug import print_fuct_call

import paramiko.util

atexit.register(fabric_cleanup, True)

def test_on_remote_host(fname, shell, remotedir, pyversion='python', keep=False, 
                        branch=None, testargs=(), hostname=''):
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
        
    #remoteargs.extend(['-d', remotedir])
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
    # retrieve build output file
    get(os.path.join(remotedir, 'build.out'), 'build.out')
    
    if result.return_code != 0:
        raise RuntimeError("problem with remote build (return code = %s)" % 
                           result.return_code)
    
    print 'build successful\ntesting...'
    newfiles = set(remote_listdir(remotedir)) - dirfiles - expectedfiles
    
    if build_type == 'dev':
        if len(newfiles) != 1:
            raise RuntimeError("expected a single new file in %s after building but got %s" %
                               (remotedir, list(newfiles)))
        
        builddir = newfiles.pop()
        envdir = os.path.join(builddir, 'devenv')
    else:
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


def run_on_host(host, config, funct, *args, **kwargs):
    settings_kwargs = {}
    settings_args = []
    
    debug = config.getboolean(host, 'debug')
    settings_kwargs['host_string'] = config.get(host, 'addr', None)
        
    ident = config.get(host, 'identity', None)
    if ident:
        settings_kwargs['key_filename'] = os.path.expanduser(
            os.path.expandvars(ident))
    usr = config.get(host, 'user', None)
    if usr:
        settings_kwargs['user'] = usr
        
    if debug:
        settings_args.append(show('debug'))
        print "calling %s" % print_fuct_call(funct, *args, **kwargs)
    else:
        settings_args.append(hide('running'))
        
    settings_kwargs['shell'] = config.get(host, 'shell')
    
    with settings(*settings_args, **settings_kwargs):
        return funct(*args, **kwargs)
            
        
def main(argv=None):
    t1 = time.time()
    
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
    parser.add_option("-l","--log", action="store_true", dest='log',
                      help="create a paramiko.log file")
    parser.add_option("-f","--file", action="store", type='string', dest='fname',
                      help="pathname of a tarfile or URL of a git repo")
    parser.add_option("-b","--branch", action="store", type='string', 
                      dest='branch',
                      help="if file_url is a git repo, supply branch name here")
    parser.add_option("-o","--outdir", action="store", type='string', 
                      dest='outdir', default='test_results',
                      help="output directory for test results "
                           "(has a subdirectory for each host tested)")
    parser.add_option("-r","--remotedir", action="store", type='string', 
                      dest='remotedir',
                      help="remote directory used for building/testing")

    (options, args) = parser.parse_args(sys.argv[1:])
    
    if options.log:
        paramiko.util.log_to_file('paramiko.log')
    
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
        
    # find out which hosts are ec2 images, if any
    image_hosts = set()
    ec2_needed = False
    for host in hosts:
        if config.has_option(host, 'image_id'):
            image_hosts.add(host)
            ec2_needed = True
        elif config.has_option(host, 'instance_id'):
            ec2_needed = True

    if ec2_needed:
        from boto.ec2.connection import EC2Connection
        print 'connecting to EC2'
        conn = EC2Connection()

    for host in hosts:
        if host not in image_hosts and not config.has_option(host, 'addr'):
            if not config.has_option(host, 'instance_id'):
                raise RuntimeError("can't determine address for host %s" % host)
            instance_id = config.get(host, 'instance_id')
            reslist = conn.get_all_instances([instance_id])
            if len(reslist) > 0:
                inst = reslist[0].instances[0]
            else:
                raise RuntimeError("can't find a running instance of host %s" % host)
            config.set(host, 'addr', inst.public_dns_name)
            
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
        
    orig_stdout = sys.stdout
    orig_stderr = sys.stderr
    
    options.outdir = os.path.abspath(os.path.expanduser(
                                     os.path.expandvars(options.outdir)))
    processes = []
    files = []
    
    if options.remotedir is None:
        uname = getpass.getuser()
        options.remotedir = 'test_%s_%s' % (uname, datetime.datetime.now())
        options.remotedir = options.remotedir.replace(' ','_')
        # if you try to set up a virtualenv in any directory with ':'
        # in the name, you'll get errors ('no module named os', etc.) 
        options.remotedir = options.remotedir.replace(':','.')
    try:
        for host in hosts:
            shell = config.get(host, 'shell')
            if host in image_hosts:
                proc_args = [host, config, conn, test_on_remote_host,
                             fname, shell, options.remotedir]
                target = run_on_ec2_image
            else:
                proc_args = [host, config, test_on_remote_host, fname,
                             shell, options.remotedir]
                target = run_on_host
            hostdir = os.path.join(options.outdir, host)
            if not os.path.isdir(hostdir):
                os.makedirs(hostdir)
            os.chdir(hostdir)
            sys.stdout = sys.stderr = f = open('test.out', 'wb', 40)
            files.append(f)
            p = Process(target=target,
                        name=host,
                        args=proc_args,
                        kwargs={ 'keep': options.keep,
                                 'branch': options.branch,
                                 'testargs': args,
                                 'hostname': host,
                                 })
            processes.append(p)
            orig_stdout.write("starting build/test process for %s\n" % p.name)
            p.start()
            
            sys.stdout = orig_stdout
            sys.stderr = orig_stderr
        
        while len(processes) > 0:
            time.sleep(1)
            for i,p in enumerate(processes):
                if p.exitcode is not None:
                    break
            else:
                continue
            p = processes[i]
            processes.remove(p)
            if len(processes) > 0:
                remaining = '(%d hosts remaining)' % len(processes)
            else:
                remaining = ''
            orig_stdout.write('%s finished. exit code=%d %s\n' % 
                              (p.name, p.exitcode, remaining))
            
    finally:
        sys.stdout = orig_stdout
        sys.stderr = orig_stderr
        for f in files:
            f.close()
        os.chdir(startdir)
        
        t2 = time.time()
        orig_stdout.write('\nElapsed time: %10.3f s\n' % (t2-t1))


if __name__ == '__main__': #pragma: no cover
    main()
