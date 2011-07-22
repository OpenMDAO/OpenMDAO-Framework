#testbranch script (formerly in fabfile.py)

import sys
import os
import shutil
import subprocess
import atexit
import time
from optparse import OptionParser
from fabric.api import run, env, local, put, cd, get, settings, prompt, hide, hosts
from fabric.state import connections
from socket import gethostname
from inspect import getargvalues, formatargvalues, currentframe

from openmdao.devtools.utils import get_git_branch, repo_top, remote_tmpdir, \
                                    push_and_run, rm_remote_tree, make_git_archive, \
                                    ssh_test

from openmdao.util.debug import print_fuct_call

#import paramiko.util
#paramiko.util.log_to_file('paramiko.log')

class ImageInfo(object):
    def __init__(self, ami_id, instance_type, platform, username):
        self.ami_id = ami_id
        self.instance_type = instance_type
        self.platform = platform
        self.username = username
        

# enter info for all of the EC2 images used for testing
# machine name: (image id, instance_type, platform)

#imagedict = {
    #'lovejoy': ImageInfo('ami-2638c34f', 'c1.medium', 'linux2', 'ubuntu'),
    #'sideshowbob': ImageInfo('ami-1cf20975', 'c1.medium', 'win32', 'administrator'),
    #'discostu': ImageInfo('ami-3038c359', 'm1.large', 'linux2', 'ubuntu'),
    #'smithers': ImageInfo('ami-72e3181b', 'm1.large', 'win32', 'administrator'),
    #}

#instancedict = { }

def check_image_state(imgname, inst, start_state, sleeptime=10,
                      debug=False):
    """Keeps querying the 'state' attribute of the instance until
    the state changes from start_state.
    """
    while True:
        time.sleep(sleeptime)
        inst.update()
        if debug:
            print '%s state = %s' % (imgname, inst.state)
        if inst.state != start_state:
            break
   
def start_instance(conn, config, name, sleep=6, max_tries=10):
    """Starts up an EC2 instance having the specified 'short' name and
    returns the image, the instance, and the public dns name.
    """
    debug = config.getboolean(name, 'debug')
    img = conn.get_image(config.get(name, 'image_id'))
    instance_type = config.get(name, 'instance_type')
    identity = config.get(name, 'identity')
    key_name = os.path.splitext(os.path.basename(identity))[0]
    security_groups = [s.strip() for s in config.get(name, 'security_groups').split()
                       if len(s.strip())>0]
    if debug:
        print "%s image = %s" % (name, img)
        print "%s location = %s" % (name, img.location)
        print 'running %s' % name
        
    reservation = img.run(key_name=key_name, 
                          security_groups=security_groups,
                          instance_type=instance_type)
    inst = reservation.instances[0]
    check_image_state(name, inst, u'pending', debug=debug)
    if inst.state != u'running':
        raise RuntimeError("instance of '%s' failed to run (went from state 'pending' to state '%s')" %
                           (name, inst.state))
    
    if debug:
        print "instance at address '%s' is running" % inst.public_dns_name
    for i in range(max_tries):
        time.sleep(sleep)
        if debug: print "testing connection (try #%d)" % (i+1)
        # even though the instance is running, it takes a while before
        # sshd is running, so we have to wait a bit longer
        if ssh_test(inst.public_dns_name):
            break
    else:
        raise RuntimeError("instance of '%s' ran but ssh connection attempts failed" % name)
    return inst.public_dns_name

def inst_from_dns(conn, dns_name):
    for reservation in conn.get_all_instances():
        for inst in reservation.instances:
            if inst.public_dns_name == dns_name:
                return inst
    return None
    

def run_on_ec2_host(host, config, conn, funct, *args, **kwargs):
    """Runs the given function on the specified EC2 instance.  If host is an
    image, then an instance will be started using that image.
    """
    settings_args = {}
    
    settings_args['key_filename'] = os.path.expanduser(
               os.path.expandvars(config.get(host, 'identity').strip()))
    settings_args['user'] = config.get(host, 'user')
    debug = config.get(host, 'debug')
    
    if config.has_option(host, 'addr'): # it's a running instance
        settings_args['host_string'] = config.get(host, 'addr')
    else:
        # stand up an instance of the specified image
        settings_args['host_string'] = start_instance(conn, config, host)

    with settings(**settings_args):
        if debug:
            print "calling %s" % print_fuct_call(funct, *args, **kwargs)
        funct(*args, **kwargs)

        