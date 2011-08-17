import sys
import os
import shutil
import subprocess
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
                                    fabric_cleanup, ssh_test, fab_connect

from openmdao.util.debug import print_fuct_call


def check_inst_state(inst, state, imgname='', sleeptime=10, maxtries=50,
                      debug=False, stream=sys.stdout):
    """Keeps querying the 'state' attribute of the instance every 'sleeptime'
    seconds until the state equals the specified state.
    """
    tries = 1
    while True:
        time.sleep(sleeptime)
        inst.update()
        if debug:
            stream.write("%s state = %s\n" % (imgname, inst.state))
        if inst.state == state or tries >= maxtries:
            break
        tries += 1
    return inst.state
   
def start_instance_from_image(conn, config, name, sleep=10, max_tries=50):
    """Starts up an EC2 instance having the specified 'short' name and
    returns the instance.
    """
    debug = config.getboolean(name, 'debug')
    img = conn.get_image(config.get(name, 'image_id'))
    instance_type = config.get(name, 'instance_type')
    platform = config.get(name, 'platform')
    identity = config.get(name, 'identity')
    key_name = os.path.splitext(os.path.basename(identity))[0]
    security_groups = [s.strip() for s in config.get(name, 'security_groups').split()
                       if len(s.strip())>0]
    
    print 'starting instance of image %s' % name
    print "   image: %s" % img
    print "   location: %s" % img.location
    print "   platform: %s" % platform
    print "   identity: %s" % identity
    print "   key name: %s" % key_name
    print "   security_groups: %s" % security_groups
        
    reservation = img.run(key_name=key_name, 
                          security_groups=security_groups,
                          instance_type=instance_type)
    
    inst = reservation.instances[0]
    check_inst_state(inst, u'running', imgname=name, debug=debug)
    if inst.state != u'running':
        raise RuntimeError("instance of '%s' failed to run" % name)
    
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

def start_instance(conn, inst_id, debug=False, sleep=10, max_tries=50):
    """Starts up an existing EC2 instance given its id"""
    if debug:
        print 'starting a instance (id=%s)' % inst_id
        
    insts = conn.start_instances(instance_ids=[inst_id])
    inst = insts[0]
    
    check_inst_state(inst, u'running', debug=debug)
    if inst.state != u'running':
        raise RuntimeError("instance with id '%s' failed to start" % inst_id)
    
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

    return inst

def stop_instance(inst, host, stream):
    inst.stop()
    check_inst_state(inst, u'stopped', imgname=host, 
                     stream=stream)
    if inst.state == u'stopped':
        stream.write("instance of %s has stopped\n" % host)
        return True
    else:
        stream.write("instance of %s failed to stop! (state=%s)\n" % 
                     (host, inst.state))
        return False

def run_on_ec2(host, config, conn, funct, outdir, **kwargs):
    """Runs the given function on an EC2 instance. The instance may be either
    created from an image or may be an existing image that is stopped.
    In either case, the instance will be started and the function will run.
    If the instance was created from an image, it will be
    terminated, unless there is an error or keep==True, which will result in
    the instance being stopped but not terminated.  If the instance was
    pre-existing, then it will just be stopped instead of terminated.
    """
    hostdir = os.path.join(outdir, host)
    if not os.path.isdir(hostdir):
        os.makedirs(hostdir)
    os.chdir(hostdir)
    
    orig_stdout = sys.stdout
    orig_stderr = sys.stderr
    sys.stdout = sys.stderr = open('run.out', 'wb')
    
    settings_kwargs = {}
    settings_args = []
    
    settings_kwargs['key_filename'] = os.path.expanduser(
               os.path.expandvars(config.get(host, 'identity').strip()))
    settings_kwargs['user'] = config.get(host, 'user')
    debug = config.getboolean(host, 'debug')
    
    if config.has_option(host, 'instance_id'): # start a stopped instance
        inst_id = config.get(host, 'instance_id')
        orig_stdout.write("starting instance %s from stopped instance\n" % host)
        inst = start_instance(conn, inst_id, debug=debug)
        terminate = False
    else: # stand up an instance of the specified image
        orig_stdout.write("starting instance %s from image\n" % host)
        inst = start_instance_from_image(conn, config, host)
        terminate = True
    
    settings_kwargs['host_string'] = inst.public_dns_name
    settings_kwargs['disable_known_hosts'] = True
    orig_stdout.write("instance %s was started successfully. dns=%s\n" % 
                      (host, inst.public_dns_name))

    if debug:
        settings_args.append(show('debug'))
    else:
        settings_args.append(hide('running'))

    platform = config.get(host, 'platform')
    if platform == 'windows':
        settings_kwargs['shell'] = 'cmd /C'
    else:
        settings_kwargs['shell'] = '/bin/bash -l -c'

    try:
        with settings(**settings_kwargs):
            # first, make sure that the connection is really working...
            # on EC2 even if ssh connects there can be timeouts during authentication,
            # so try to connect multiple times if there's a timeout
            client = fab_connect(settings_kwargs['user'],
                                 settings_kwargs['host_string'], debug=debug)
            if debug:
                orig_stdout.write("<%s>: calling %s\n" % 
                                  (host, print_fuct_call(funct, **kwargs)))
            retval = funct(**kwargs)
    except (SystemExit, Exception):
        retval = -1
        
    keep = kwargs.get('keep', False)
    if retval == 0 or not keep:
        if terminate is True:
            orig_stdout.write("terminating %s\n" % host)
            inst.terminate()
            check_inst_state(inst, u'terminated', imgname=host, debug=debug,
                             stream=orig_stdout)
            if inst.state == u'terminated':
                orig_stdout.write("instance of %s is terminated.\n" % host)
            else:
                orig_stdout.write("instance of %s failed to terminate!\n" % host)
        else:
            stop_instance(inst, host, orig_stdout)
    else:
        orig_stdout.write("run failed, so stopping %s instead of terminating it.\n" % host)
        orig_stdout.write("%s will have to be terminated manually.\n" % host)
        stop_instance(inst, host, orig_stdout)
        
    return retval
