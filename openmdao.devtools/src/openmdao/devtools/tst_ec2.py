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
                                    push_and_run, rm_remote_tree, make_git_archive

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

imagedict = {
    'lovejoy': ImageInfo('ami-2638c34f', 'c1.medium', 'linux2', 'ubuntu'),
    'sideshowbob': ImageInfo('ami-1cf20975', 'c1.medium', 'win32', 'administrator'),
    'discostu': ImageInfo('ami-3038c359', 'm1.large', 'linux2', 'ubuntu'),
    'smithers': ImageInfo('ami-72e3181b', 'm1.large', 'win32', 'administrator'),
    }

instancedict = { }

def check_image_state(imgname, inst, start_state, sleeptime=10,
                      debug=False):
    """Keeps querying the 'state' attribute of the instance until
    the state changes from start_state.
    """
    while True:
        inst.update()
        if debug:
            print '%s state = %s' % (imgname, inst.state)
        if inst.state != start_state:
            break
        time.sleep(sleeptime)
   
def start_instance(conn, name, key_name='lovejoykey',
                   security_groups=('default'),
                   instance_type=None,
                   debug=False):
    """Starts up an EC2 instance having the specified 'short' name and
    returns the image, the instance, and the public dns name.
    """
    img = conn.get_image(imagedict[name].ami_id)
    if instance_type is None:
        instance_type = imagedict[name].instance_type
    if debug:
        print "%s image = %s" % (name, img)
        print "%s location = %s" % (name, img.location)
        print 'running %s' % name
    reservation = img.run(key_name=key_name, 
                          security_groups=list(security_groups),
                          instance_type=instance_type)
    inst = reservation.instances[0]
    check_image_state(name, inst, u'pending', debug=debug)
    if inst.state != u'running':
        raise RuntimeError("instance of '%s' failed to run (went from state 'pending' to state '%s')" %
                           (name, inst.state))
    instancedict[inst.public_dns_name] = (inst, name)
    return (img, inst, inst.public_dns_name)


def run_on_ec2_host(host, conn, identity, key_name,
                    funct, *args, **kwargs):
    settings_args = {}
    
    if host.startswith('ec2-'): # it's a dns name of an existing image
        hoststring = host
        shortname = instancedict[host][1]
        settings_args['user'] = imagedict[shortname].username
        instance_platform = imagedict[shortname].platform
    else:
        image_id = imagedict[host].ami_id
        instance_type = imagedict[host].instance_type
        instance_platform = imagedict[host].platform
        settings_args['user'] = imagedict[host].username
        
        # stand up an instance of the specified image
        img, inst, hoststring = start_instance(conn, 
                            host, key_name=key_name,
                            security_groups=('default'),
                            instance_type=None,
                            debug=False)
        
    settings_args['host_string'] = hoststring
    settings_args['key_filename'] = [identity] 
        
    with settings(**settings_args):
        funct(*args, **kwargs)
