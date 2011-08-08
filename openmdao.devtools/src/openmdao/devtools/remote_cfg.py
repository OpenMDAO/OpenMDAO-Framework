import sys
import os
import getpass
import datetime

from optparse import OptionParser
import ConfigParser

class CfgOptionParser(OptionParser):
    def __init__(self):
        OptionParser.__init__(self)
        self.add_option("-c", "--config", action='store', dest='cfg', metavar='CONFIG',
                          default='~/.openmdao/testing.cfg',
                          help="path of config file where info for test hosts is located")
        self.add_option("--host", action='append', dest='hosts', metavar='HOST',
                          default=[],
                          help="select host from config file to run tests on. "
                               "If not supplied, tests will run on all hosts in "
                               "config file. To test on a subset of the hosts in "
                               "the config file, use multiple --host args")
        self.add_option("-o","--outdir", action="store", type='string', 
                          dest='outdir', default='host_results',
                          help="output directory for results "
                               "(has a subdirectory for each host)")
        self.add_option("-r","--remotedir", action="store", type='string', 
                          dest='remotedir',
                          help="remote directory where execution will take place")

def process_options(options):
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
        raise RuntimeError("no hosts were found in config file %s" % options.cfg)
        
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
    else:
        conn = None

    for host in hosts:
        if host not in image_hosts and not config.has_option(host, 'addr'):
            if not config.has_option(host, 'instance_id'):
                raise RuntimeError("can't determine address for host %s" % host)
            
            # use instance_id to look up public dns name
            instance_id = config.get(host, 'instance_id')
            reslist = conn.get_all_instances([instance_id])
            if len(reslist) > 0:
                inst = reslist[0].instances[0]
            else:
                raise RuntimeError("can't find a running instance of host %s" % host)
            config.set(host, 'addr', inst.public_dns_name)
            
    options.hosts = hosts
    
    if options.remotedir is None:
        uname = getpass.getuser()
        options.remotedir = '%s_%s' % (uname, datetime.datetime.now())
        options.remotedir = options.remotedir.replace(' ','_')
        # if you try to set up a virtualenv in any directory with ':'
        # in the name, you'll get errors ('no module named os', etc.) 
        options.remotedir = options.remotedir.replace(':','.')

    options.outdir = os.path.abspath(os.path.expanduser(
                                     os.path.expandvars(options.outdir)))

    return (config, conn, image_hosts)

