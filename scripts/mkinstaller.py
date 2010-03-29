
import sys
from optparse import OptionParser
import virtualenv



def main():
    """
    Takes a file containing a list of requirements, one per line, and
    generates an install script that makes a virtualenv with all of the
    required packages based on the contents of the requirements file.
    """
    
    script_str = """

def adjust_options(options, args):
    if sys.version_info[:2] < (2,6) or sys.version_info[:2] >= (3,0):
        print 'ERROR: python version must be >= 2.6 and <= 3.0. yours is %%s' %% sys.version.split(' ')[0]
        sys.exit(-1)
    #options.use_distribute = True  # force use of distribute instead of setuptools
    
def after_install(options, home_dir):
    global logger
    reqs = %(reqs)s
    cmds = %(cmds)s
    for req in reqs:
        #cmdline = [join(home_dir, 'bin', 'easy_install')] + cmds + [req]
        cmdline = [join(home_dir, 'bin', 'pip'), 'install'] + cmds + [req]
        logger.debug("running command: %%s" %% ' '.join(cmdline))
        subprocess.check_call(cmdline)

    """
    parser = OptionParser()
    parser.add_option("-r", "--requirement", action="store", type="string", dest='req', 
                      help="requirements file") 
    
    (options, args) = parser.parse_args()
    
    if not options.req:
        print "ERROR: a requirements file was not specified"
        sys.exit(-1)
    
    reqf = open(options.req, 'r')
    lines = [s.strip() for s in reqf.read().split('\n') if s.strip()]
    reqs = []
    cmds = []
    for line in lines:
        if line.startswith('-'):
            cmds.extend(line.split(' '))
        else:
            reqs.append(line)
    
    optdict = {'reqs':reqs, 'cmds':cmds}
    print virtualenv.create_bootstrap_script(script_str % optdict)


if __name__ == '__main__':
    main()
