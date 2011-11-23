import sys
import os
import getpass
import datetime
import time
import socket
import pprint

from optparse import OptionParser
import ConfigParser
from multiprocessing import Process

from openmdao.devtools.ec2 import run_on_ec2
from openmdao.util.debug import print_fuct_call

def run_on_host(host, config, conn, funct, outdir, **kwargs):
    """Runs the given funct on the specified host."""
    from fabric.api import settings, show, hide

    hostdir = os.path.join(outdir, host)
    if not os.path.isdir(hostdir):
        os.makedirs(hostdir)
    os.chdir(hostdir)
    orig_stdout = sys.stdout
    orig_stderr = sys.stderr
    print '<%s>: writing stdout/stderr to %s' % (host,
                       os.path.join(os.getcwd(), 'run.out'))
    sys.stdout = sys.stderr = open('run.out', 'wb')
    
    settings_kwargs = {}
    settings_args = []
    
    debug = config.getboolean(host, 'debug')
    settings_kwargs['host_string'] = config.get(host, 'addr', None)
        
    if config.has_option(host, 'identity'):
        settings_kwargs['key_filename'] = os.path.expanduser(
            os.path.expandvars(config.get(host, 'identity')))
        
    if config.has_option(host, 'user'):
        settings_kwargs['user'] = config.get(host, 'user')
        
    platform = config.get(host, 'platform')
    if platform == 'windows':
        settings_kwargs['shell'] = 'cmd /C'
    else:
        settings_kwargs['shell'] = '/bin/bash -l -c'
    
    if debug:
        settings_args.append(show('debug'))
        orig_stdout.write("<%s>: calling %s" % 
                          (host, print_fuct_call(funct, **kwargs)))
    else:
        settings_args.append(hide('running'))
        
    with settings(*settings_args, **settings_kwargs):
        return funct(**kwargs)
            

class CfgOptionParser(OptionParser):
    def __init__(self, *args, **kwargs):
        OptionParser.__init__(self, *args, **kwargs)
        self.add_option("-c", "--config", action='store', dest='cfg', metavar='CONFIG',
                          default='~/.openmdao/testhosts.cfg',
                          help="Path of config file where info for hosts is located")
        self.add_option("--host", action='append', dest='hosts', metavar='HOST',
                          default=[],
                          help="Select host from config file to run on. "
                               "To run on multiple hosts, use multiple --host args")
        self.add_option("--all", action="store_true", dest='allhosts',
                        help="If True, run on all hosts in config file.")
        self.add_option("-o","--outdir", action="store", type='string', 
                          dest='outdir', default='host_results',
                          help="Output directory for results "
                               "(defaults to ./host_results)")

def read_config(options, parser):
    """Reads the config file specified in options.cfg and looks for sections
    in the config file that match the host names specified in options.hosts.
    
    Returns a tuple of the form (hosts, config), where hosts is the list of
    host names and config is the ConfigParser object for the config file.
    """
    options.cfg = os.path.expanduser(options.cfg)
    
    config = ConfigParser.ConfigParser()
    config.readfp(open(options.cfg))
    
    hostlist = config.sections()
    if options.allhosts:
        if options.buildtype:
            btype = 'test_'+options.buildtype
            hosts = [h for h in hostlist if config.has_option(h, btype) 
                                           and config.getboolean(h, btype)]
            if len(hosts) == 0:
                print "WARNING: no hosts found with %s = true" % btype
        else:
            hosts = hostlist
    elif options.hosts:
        hosts = []
        for host in options.hosts:
            if host in hostlist:
                hosts.append(host)
            else:
                raise RuntimeError("host '%s' is not in config file %s" % 
                                   (host, options.cfg))

        if not hosts:
            raise RuntimeError("no hosts were found in config file %s" % options.cfg)
    else:
        hosts = []

    return (hosts, config)

def get_tmp_user_dir():
    """Generate a directory name based on username and the current
    date and time.
    """
    udir = '%s_%s' % (getpass.getuser(), datetime.datetime.now())
    # if you try to set up a virtualenv in any directory with ':'
    # in the name, you'll get errors ('no module named os', etc.) 
    return udir.replace(' ','_').replace(':','.')
    
def process_options(options, parser):
    """Handles some options found in CfgOptionParser so that the code
    doesn't have to be duplicated when inheriting from CfgOptionParser.
    """
    hosts, config = read_config(options, parser)
        
    # find out which hosts are ec2 images, if any
    ec2_hosts = set()
    ec2_needed = False
    for host in hosts:
        if config.has_option(host, 'image_id'):
            ec2_hosts.add(host)
            ec2_needed = True
        elif config.has_option(host, 'instance_id'):
            ec2_needed = True

    if ec2_needed:
        from boto.ec2.connection import EC2Connection
        print 'connecting to EC2'
        conn = EC2Connection()
    else:
        conn = None

    for host in hosts:
        if host not in ec2_hosts and not config.has_option(host, 'addr'):
            if not config.has_option(host, 'instance_id'):
                raise RuntimeError("can't determine address for host %s" % host)
            
            # use instance_id to look up public dns name
            instance_id = config.get(host, 'instance_id')
            reslist = conn.get_all_instances([instance_id])
            if len(reslist) > 0:
                inst = reslist[0].instances[0]
            else:
                raise RuntimeError("can't find a running instance of host %s" % host)
            if inst.state == u'stopped':
                ec2_hosts.add(host)
            else:
                config.set(host, 'addr', inst.public_dns_name)
            
    options.hosts = hosts
    
    options.outdir = os.path.abspath(os.path.expanduser(
                                     os.path.expandvars(options.outdir)))

    return (config, conn, ec2_hosts)


def run_host_processes(config, conn, ec2_hosts, options, funct, funct_kwargs):
    """Start up a different process for each host in options.hosts. Hosts can
    be either EC2 images, EC2 instances, or any other kind of host as long
    as the caller has ssh access to it.  This routine returns after funct
    has been run on all hosts. Displays total elapsed time when finished.
    """
    t1 = time.time()
    socket.setdefaulttimeout(30)
    
    startdir = os.getcwd()
    
    processes = []
    
    retcode = 0
    
    summary = {}
    
    try:
        for host in options.hosts:
            if host in ec2_hosts:
                runner = run_on_ec2
            else:
                runner = run_on_host
            proc_args = [host, config, conn, funct, options.outdir]
            kw_args = funct_kwargs.copy()
            debug = config.getboolean(host, 'debug')
            platform = config.get(host, 'platform')
            kw_args['debug'] = debug
            kw_args['hostname'] = host
            py = config.get(host, 'py')
            if platform.startswith('win') and '.' in py:
                # convert pythonX.Y form over to C:/PythonXY/python.exe
                ver = py[6:]
                py = 'C:/Python%s/python.exe' % ver.replace('.','')
            kw_args['pyversion'] = py
            if debug:
                print "creating Process"
                print "   args = %s" % proc_args
                print "   kw_args = %s" % pprint.pformat(kw_args)
            p = Process(target=runner,
                        name=host,
                        args=proc_args,
                        kwargs=kw_args)
            processes.append(p)
            print "starting process for %s" % p.name
            p.start()
        
        while len(processes) > 0:
            time.sleep(10)
            for p in processes:
                if p.exitcode is not None:
                    summary[p.name] = p.exitcode
                    processes.remove(p)
                    if len(processes) > 0:
                        remaining = '\nremaining hosts: %s' % ([pr.name for pr in processes],)
                    else:
                        remaining = ''
                    print '%s finished. exit code=%d %s\n' % (p.name, 
                                                              p.exitcode, 
                                                              remaining)
                    if p.exitcode != 0:
                        retcode = p.exitcode
                    break
    finally:
        os.chdir(startdir)
        
        t2 = time.time()
        secs = t2-t1
        
        hours = int(secs)/3600
        mins = int(secs-hours*3600.0)/60
        secs = secs-(hours*3600.)-(mins*60.)
        
        print '\nResult Summary:  Host, Return Code'
        for k,v in summary.items():
            print '  %s, %s' % (k, v)
            
        print '\n\nElapsed time:',
        if hours > 0:
            print ' %d hours' % hours,
        if mins > 0:
            print ' %d minutes' % mins,
        print ' %5.2f seconds\n\n' % secs
        
    return retcode
