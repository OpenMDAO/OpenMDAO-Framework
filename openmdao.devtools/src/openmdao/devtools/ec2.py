import sys
import os
import time

from boto.ec2.connection import EC2Connection

from openmdao.devtools.utils import ssh_test, fab_connect, run
from openmdao.util.debug import print_funct_call
from openmdao.util.user import get_username

def check_inst_state(inst, state, imgname='', sleeptime=10, maxtries=50,
                      debug=False, stream=sys.stdout):
    """Keeps querying the 'state' attribute of the instance every 'sleeptime'
    seconds until the state equals the specified state (or maxtries are 
    reached).
    """
    tries = 1
    while True:
        time.sleep(sleeptime)
        try:
            inst.update()
        except Exception as err:
            if debug:
                stream.write("ERROR while attempting to get instance state: %s"
                             % str(err))
        else:
            if debug:
                stream.write("%s state = %s\n" % (imgname, inst.state))
            if inst.state == state:
                return state
        if tries >= maxtries:
            break
        tries += 1
   

def start_instance_from_image(conn, config, name, sleep=10, max_tries=50,
                              stream=sys.stdout):
    """Starts up an EC2 instance having the specified 'short' name and
    returns the instance.
    """
    debug = config.getboolean(name, 'debug')
    img_id = config.get(name, 'image_id')
    # Sometimes requires retry due to:
    # SSLError: [Errno 8] _ssl.c:504: EOF occurred in violation of protocol
    for retry in range(3):
        try:
            img = conn.get_image(img_id)
        except Exception as exc:
            print 'get_image(%s) failed: %s' % (img_id, exc)
            time.sleep(1)
        else:
            break
    else:
        raise RuntimeError("Can't get image for %s" % img_id)
    
    instance_type = config.get(name, 'instance_type')
    platform = config.get(name, 'platform')
    identity = config.get(name, 'identity')
    py = config.get(name, 'py')
    key_name = os.path.splitext(os.path.basename(identity))[0]
    security_groups = [s.strip() for s in config.get(name, 'security_groups').split()
                       if len(s.strip())>0]
    
    print 'starting instance of image %s' % name
    print "   image id: %s" % img_id
    print "   location: %s" % img.location
    print "   platform: %s" % platform
    print "   identity: %s" % identity
    print "   key name: %s" % key_name
    print "   security_groups: %s" % security_groups
    print "   python: %s" % py
        
    reservation = img.run(key_name=key_name, 
                          security_groups=security_groups,
                          instance_type=instance_type)
        
    inst = reservation.instances[0]
    check_inst_state(inst, u'running', imgname=name, debug=debug)
    if inst.state != u'running':
        raise RuntimeError("instance of '%s' failed to run" % name)
    
    if debug:
        print "instance at address '%s' is running" % inst.public_dns_name
        
    if platform == 'windows':
        # Extremely slow startup, don't even try for a minute.
        print 'pausing for lethargic windows...'
        time.sleep(60)

    successes = 0
    for i in range(max_tries):
        time.sleep(sleep)
        if debug: 
            print "testing ssh connection (try #%d)" % (i+1)
        # even though the instance is running, it takes a while before
        # sshd is running, so we have to wait a bit longer
        # In addition, sometimes the initial success is a fluke and the
        # next connection fails, so we wait for three successes in a row.
        if ssh_test(inst.public_dns_name):
            successes += 1
            if successes > 2:
                break
        else:
            successes = 0
    else:
        stream.write("\nssh connection to %s failed after %d attempts."
                     "  terminating...\n" % (name, max_tries))
        terminate_instance(inst, name, stream, debug)
        raise RuntimeError("couldn't connect to %s via ssh" % name)

    time.sleep(20)
    
    try:
        conn.create_tags([inst.id], {'Name': "%s_%s" % (get_username(), name)} )
    except Exception as err:
        stream.write(str(err))
    return inst

def start_instance(conn, inst_id, debug=False, sleep=10, max_tries=50):
    """Starts up an existing EC2 instance given its id."""
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
        raise RuntimeError("instance '%s' ran but ssh connection attempts"
                           " failed (%d attempts)" % (inst_id, max_tries))

    return inst

def stop_instance(inst, host, stream, debug):
    inst.stop()
    check_inst_state(inst, u'stopped', imgname=host, 
                     stream=stream, debug=debug)
    if inst.state == u'stopped':
        stream.write("instance of %s has stopped\n" % host)
        return True
    else:
        stream.write("instance of %s failed to stop! (state=%s)\n" % 
                     (host, inst.state))
        return False

def terminate_instance(inst, host, stream, debug):
    stream.write("terminating %s\n" % host)
    inst.terminate()
    check_inst_state(inst, u'terminated', imgname=host, debug=debug,
                     stream=stream)
    if inst.state == u'terminated':
        stream.write("instance of %s is terminated.\n" % host)
        return True
    else:
        stream.write("instance of %s failed to terminate!\n" % host)
        return False

class MultiFile(object):
    def __init__(self, *files):
        self._files = files
        
    def write(self, s):
        for f in self._files:
            f.write(s)

def run_on_ec2(host, config, conn, funct, outdir, **kwargs):
    """Runs the given function on an EC2 instance. The instance may be either
    created from an image or may be an existing image that is stopped.
    In either case, the instance will be started and the function will run.
    If the instance was created from an image, it will be
    terminated unless there is an error or keep==True, which will result in
    the instance being stopped but not terminated.  If the instance was
    pre-existing, then it will just be stopped instead of terminated.
    """
    # put fabric import here to prevent sphinx doc generation failure
    # on windows when win32api isn't installed
    from fabric.api import settings, hide, show
    
    hostdir = os.path.join(outdir, host)
    if not os.path.isdir(hostdir):
        os.makedirs(hostdir)
    os.chdir(hostdir)
    
    orig_stdout = sys.stdout
    orig_stderr = sys.stderr
    sys.stdout = sys.stderr = open('run.out', 'wb')
    
    outf = MultiFile(sys.stdout, orig_stdout)
    
    settings_kwargs = {}
    settings_args = []
    
    settings_kwargs['key_filename'] = os.path.expanduser(
               os.path.expandvars(config.get(host, 'identity').strip()))
    settings_kwargs['user'] = config.get(host, 'user')
    debug = config.getboolean(host, 'debug')
    
    if config.has_option(host, 'instance_id'): # start a stopped instance
        inst_id = config.get(host, 'instance_id')
        outf.write("starting instance %s from stopped instance\n" % host)
        inst = start_instance(conn, inst_id, debug=debug)
        terminate = False
    else: # stand up an instance of the specified image
        outf.write("starting instance %s from image\n" % host)
        inst = start_instance_from_image(conn, config, host, stream=outf)
        terminate = True
    
    settings_kwargs['host_string'] = inst.public_dns_name
    settings_kwargs['disable_known_hosts'] = True
    outf.write("instance %s was started successfully. dns=%s\n" % 
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
                outf.write("<%s>: calling %s\n" % 
                                  (host, print_funct_call(funct, **kwargs)))
                
            py = config.get(host, 'py')
            if platform.startswith('win') and '.' in py:
                # convert pythonX.Y form over to C:/PythonXY/python.exe
                ver = py[6:]
                py = 'C:/Python%s/python.exe' % ver.replace('.','')
        
            max_tries = 25
            sleep = 10
            for i in range(max_tries):
                time.sleep(sleep)
                if debug: 
                    print "testing python call over fabric connection (try #%d)" % (i+1)
                # even though the instance is running, it takes a while (on Windows) before
                # python actually works, so try some simple python command until it works.
                try:
                    retval = run('%s -c "dir()"' % py)
                except Exception as exc:
                    print '    caught exception:', exc
                else:
                    if retval.failed:
                        print '    failed:', retval.failed
                    elif retval.return_code != 0:
                        print '    return code:', retval.return_code
                    else:
                        break
            else:
                outf.write("\nrunning python via fabric on %s failed after %d"
                           " attempts.  terminating...\n" % (host, max_tries))
                terminate_instance(inst, host, outf, debug)
                raise RuntimeError("couldn't run python on %s via fabric" % host)

            retval = funct(**kwargs)
    except (SystemExit, Exception) as err:
        outf.write(str(err))
        if isinstance(err, SystemExit) and err.code is not None:
            retval = err.code
        else:
            retval = -1
        
    # try to retrieve console output if we can
    try:
        out = inst.get_console_output().output
        with open('console.out', 'wb') as f:
            f.write(out)
    except:
        outf.write("\ncouldn't retrieve console output\n")

    keep = kwargs.get('keep', False)
    if keep or not terminate:
        outf.write("stopping %s instead of terminating it.\n" % host)
        outf.write("%s will have to be terminated manually.\n" % host)
        if not stop_instance(inst, host, orig_stdout, debug):
            retval = -1
    else:
        if not terminate_instance(inst, host, orig_stdout, debug):
            retval = -1
        
    if retval != 0:
        raise RuntimeError("return value = %s" % retval)


#def find_instance(inst_id):
    #reslist = EC2Connection().get_all_instances([inst_id])
    #if len(reslist) > 0:
        #return reslist[0].instances[0]
    #return None
    
#def ec2_start(ident=None):
    #if ident is None:
        #ident = sys.argv[1]
    #if ident.startswith('ami-'): # image
        #pass
    #elif ident.startswith('i-'):  # instance
        #insts = EC2Connection().start_instances(instance_ids=[ident])
        #inst = insts[0]
        #check_inst_state(inst, u'running')
        #return inst.state == u'running'
    #else:
        #raise RuntimeError("id '%s' is not an ec2 image or an ec2 instance" % ident)

#def ec2_stop(inst_id=None):
    #if inst_id is None:
        #inst_id = sys.argv[1]
    #inst = find_instance(inst_id)
    #if inst is None:
        #return False
    #inst.stop()
    #check_inst_state(inst, u'stopped')
    #return inst.state == u'stopped'
