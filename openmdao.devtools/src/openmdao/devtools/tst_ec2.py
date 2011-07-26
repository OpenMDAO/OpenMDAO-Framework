#testbranch script (formerly in fabfile.py)

import sys
import os
import shutil
import subprocess
#import multiprocessing
import atexit
import time
from optparse import OptionParser
from fabric.api import run, env, local, put, cd, get, settings, prompt, hide, hosts
from fabric.state import connections
from socket import gethostname
from inspect import getargvalues, formatargvalues, currentframe

from openmdao.devtools.utils import get_git_branch, repo_top, remote_tmpdir, \
                                    push_and_run, rm_remote_tree, make_git_archive, \
                                    fabric_cleanup, ssh_test

from openmdao.util.debug import print_fuct_call

#import paramiko.util
#paramiko.util.log_to_file('paramiko.log')


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
    return inst.state
   
def start_instance(conn, config, name, sleep=6, max_tries=10):
    """Starts up an EC2 instance having the specified 'short' name and
    returns the image, the instance, and the public dns name.
    """
    platform = config.get(name, 'platform')
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
        print "%s platform = %s" % (name, platform)
        print 'running %s' % name
        
    #user_data = user_datas.get(platform, None)

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
    #subprocess.check_call(['python',
                           #os.path.join(os.path.dirname(__file__),
                                        #'ssh_tester.py'),
                           #'--host=%s'%inst.public_dns_name])
    for i in range(max_tries):
        time.sleep(sleep)
        if debug: print "testing connection (try #%d)" % (i+1)
        # even though the instance is running, it takes a while before
        # sshd is running, so we have to wait a bit longer
        if ssh_test(inst.public_dns_name):
            ## even though ssh works now, we're still not guaranteed
            ## that the instance has finished booting up, so wait a bit longer
            #time.sleep(120)
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
    settings_kwargs = {}
    
    settings_kwargs['key_filename'] = os.path.expanduser(
               os.path.expandvars(config.get(host, 'identity').strip()))
    settings_kwargs['user'] = config.get(host, 'user')
    debug = config.get(host, 'debug')
    
    if config.has_option(host, 'addr'): # it's a running instance
        settings_kwargs['host_string'] = config.get(host, 'addr')
    else:
        # stand up an instance of the specified image
        settings_kwargs['host_string'] = start_instance(conn, config, host)

    with settings(**settings_kwargs):
        if debug:
            print "calling %s" % print_fuct_call(funct, *args, **kwargs)
        funct(*args, **kwargs)

        