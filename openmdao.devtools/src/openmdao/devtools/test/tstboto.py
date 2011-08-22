import sys
import time
from boto.ec2.connection import EC2Connection

# machine name: (image id, platform)
vminfo = {
    'lovejoy': ('ami-2638c34f', 'c1.medium', 'linux2'),
    'sideshowbob': ('ami-1cf20975', 'c1.medium', 'win32'),
    'discostu': ('ami-3038c359', 'm1.large', 'linux2'),
    'smithers': ('ami-72e3181b', 'm1.large', 'win32'),
    }

def check_inst_state(imgname, inst, start_state, sleeptime=10):
    while True:
        inst.update()
        print '%s state = %s' % (imgname, inst.state)
        if inst.state != start_state:
            break
        time.sleep(sleeptime)
   
def start_instance(name):
    img = conn.get_image(vminfo[name][0])
    print "%s image = %s" % (name, img)
    print "%s location = %s" % (name, img.location)
    print 'running %s' % name
    reservation = img.run(key_name='lovejoykey', security_groups=['default'],
                          instance_type=vminfo[name][1])
    inst = reservation.instances[0]
    check_inst_state(name, inst, u'pending')
    return (img, inst)


if __name__ == '__main__':
    # id and key come from .boto config file
    conn = EC2Connection()
    
    #print 'instanceType = ', conn.get_instance_attribute('i-ef65db81', 'instanceType')
    #print 'userData = ', conn.get_instance_attribute('i-ef65db81', 'userData')
    #print 'instanceInitiatedShutdownBehavior = ', conn.get_instance_attribute('i-ef65db81', 'instanceInitiatedShutdownBehavior')

    if len(sys.argv) < 2:
        for reservation in conn.get_all_instances():
            print 'reservation = ', reservation
            for inst in reservation.instances:
                print '  instance = ', inst
                print '  state = ', inst.state
                if inst.state == u'running':
                    print vars(inst)
    else:
        name = sys.argv[1]
        img, inst = start_instance(name)
        
        print 'dns name: ',inst.public_dns_name

#console = get_console_output(image_id)


