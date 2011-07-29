#testbranch script (formerly in fabfile.py)

import sys
import os
import shutil
import subprocess
#import multiprocessing
import atexit
import time
from optparse import OptionParser
from fabric.api import run, env, local, put, cd, get, settings, prompt, \
                       hide, show, hosts
from fabric.state import connections
from socket import gethostname
from inspect import getargvalues, formatargvalues, currentframe

from openmdao.devtools.utils import get_git_branch, repo_top, remote_tmpdir, \
                                    rm_remote_tree, make_git_archive, \
                                    fabric_cleanup, ssh_test

from openmdao.util.debug import print_fuct_call

#import paramiko.util
#paramiko.util.log_to_file('paramiko.log')


def check_image_state(inst, start_state, imgname='', sleeptime=10,
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
    returns the instance.
    """
    debug = config.getboolean(name, 'debug')
    img = conn.get_image(config.get(name, 'image_id'))
    instance_type = config.get(name, 'instance_type')
    shell = config.get(name, 'shell')
    identity = config.get(name, 'identity')
    key_name = os.path.splitext(os.path.basename(identity))[0]
    security_groups = [s.strip() for s in config.get(name, 'security_groups').split()
                       if len(s.strip())>0]
    
    print 'starting instance of image %s' % name
    print "   image: %s" % img
    print "   location: %s" % img.location
    print "   shell: %s" % shell
    print "   identity: %s" % identity
    print "   key name: %s" % key_name
    print "   security_groups: %s" % security_groups
        
    reservation = img.run(key_name=key_name, 
                          security_groups=security_groups,
                          instance_type=instance_type)
    
    inst = reservation.instances[0]
    check_image_state(inst, u'pending', imgname=name, debug=debug)
    if inst.state != u'running':
        raise RuntimeError("instance of '%s' failed to run (went from state 'pending' to state '%s')" %
                           (name, inst.state))
    
    if debug:
        print "instance at address '%s' is running" % inst.public_dns_name
        
    for i in range(max_tries):
        time.sleep(sleep)
        if debug: 
            print "testing ssh connection (try #%d)" % (i+1)
        # even though the instance is running, it takes a while before
        # sshd is running, so we have to wait a bit longer
        if ssh_test(inst.public_dns_name):
            break
    else:
        raise RuntimeError("instance of '%s' ran but ssh connection attempts failed (%d attempts)" % (name,max_tries))

    time.sleep(20)
    
    return inst


def run_on_ec2_image(host, config, conn, funct, *args, **kwargs):
    """Runs the given function on an instance of the specified EC2 image. The
    instance will be started, the function will run, and the instance will be
    terminated, unless there is an error or keep==True, which will result in
    the image being stopped but not terminated.
    """
    settings_kwargs = {}
    settings_args = []
    
    settings_kwargs['key_filename'] = os.path.expanduser(
               os.path.expandvars(config.get(host, 'identity').strip()))
    settings_kwargs['user'] = config.get(host, 'user')
    debug = config.getboolean(host, 'debug')
    
    # stand up an instance of the specified image
    inst = start_instance(conn, config, host)
    settings_kwargs['host_string'] = inst.public_dns_name

    if debug:
        settings_args.append(show('debug'))
    else:
        settings_args.append(hide('running'))

    settings_kwargs['shell'] = config.get(host, 'shell', None)

    with settings(**settings_kwargs):
        if debug:
            print "calling %s" % print_fuct_call(funct, *args, **kwargs)
        retval = funct(*args, **kwargs)
        
    keep = kwargs.get('keep', False)
    if retval == 0 or not keep:
        print "terminating %s" % host
        inst.terminate()
        check_image_state(inst, u'shutting-down', imgname=host, debug=debug)
        if inst.state == u'terminated':
            print 'instance of %s is terminated' % host
        else:
            print 'instance of %s failed to terminate!' % host
    else:
        print "stopping %s" % host
        inst.stop()
        check_image_state(inst, u'stopping', imgname=host, debug=debug)
        if inst.state == u'stopped':
            print 'instance of %s is stopped' % host
        else:
            print 'instance of %s failed to stop!' % host
            
    return retval
