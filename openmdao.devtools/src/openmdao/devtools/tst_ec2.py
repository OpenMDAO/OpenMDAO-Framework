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


def check_image_state(inst, start_state, imgname='', sleeptime=10,
                      debug=False, stream=sys.stdout):
    """Keeps querying the 'state' attribute of the instance every 'sleeptime'
    seconds until the state changes from start_state.
    """
    while True:
        time.sleep(sleeptime)
        inst.update()
        if debug:
            stream.write("%s state = %s\n" % (imgname, inst.state))
        if inst.state != start_state:
            break
    return inst.state
   
def start_instance(conn, config, name, sleep=10, max_tries=50):
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


def run_on_ec2_image(host, config, conn, funct, outdir, **kwargs):
    """Runs the given function on an instance of the specified EC2 image. The
    instance will be started, the function will run, and the instance will be
    terminated, unless there is an error or keep==True, which will result in
    the image being stopped but not terminated.
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
    
    # stand up an instance of the specified image
    orig_stdout.write("starting instance %s\n" % host)
    
    inst = start_instance(conn, config, host)
    
    settings_kwargs['host_string'] = inst.public_dns_name
    settings_kwargs['disable_known_hosts'] = True
    orig_stdout.write("instance %s was started successfully. dns=%s\n" % 
                      (host, inst.public_dns_name))

    if debug:
        settings_args.append(show('debug'))
    else:
        settings_args.append(hide('running'))

    settings_kwargs['shell'] = config.get(host, 'shell', None)

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
        orig_stdout.write("terminating %s\n" % host)
        inst.terminate()
        check_image_state(inst, u'shutting-down', imgname=host, debug=debug,
                          stream=orig_stdout)
        if inst.state == u'terminated':
            orig_stdout.write("instance of %s is terminated.\n" % host)
        else:
            orig_stdout.write("instance of %s failed to terminate!\n" % host)
    else:
        orig_stdout.write("run failed, so stopping %s instead of terminating it.\n" % host)
        orig_stdout.write("%s will have to be terminated manually.\n" % host)
        inst.stop()
        check_image_state(inst, u'stopping', imgname=host, debug=debug,
                          stream=orig_stdout)
        if inst.state == u'stopped':
            orig_stdout.write("instance of %s has stopped\n" % host)
        else:
            orig_stdout.write("instance of %s failed to stop! (state=%s)\n" % 
                              (host, inst.state))
    return retval
