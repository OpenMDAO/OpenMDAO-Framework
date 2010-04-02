
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
    options.use_distribute = True  # force use of distribute instead of setuptools
    
    
def _single_install(cmds, req, bin_dir):
    cmdline = [join(bin_dir, 'easy_install')] + cmds + [req]
    #cmdline = [join(bin_dir, 'pip'), 'install'] + cmds + [req]
    logger.debug("running command: %%s" %% ' '.join(cmdline))
    subprocess.check_call(cmdline)

def after_install(options, home_dir):
    global logger
    reqs = %(reqs)s
    cmds = %(cmds)s
    etc = join(home_dir, 'etc')
    ## TODO: this should all come from distutils
    ## like distutils.sysconfig.get_python_inc()
    if sys.platform == 'win32':
        lib_dir = join(home_dir, 'Lib')
        bin_dir = join(home_dir, 'Scripts')
    elif is_jython:
        lib_dir = join(home_dir, 'Lib')
        bin_dir = join(home_dir, 'bin')
    else:
        lib_dir = join(home_dir, 'lib', py_version)
        bin_dir = join(home_dir, 'bin')

    if not os.path.exists(etc):
        os.makedirs(etc)
    # check for specific numpy version in reqs
    reqnumpy = 'numpy'
    for req in reqs:
        if req.startswith('numpy'):
            reqs.remove(req)
            reqnumpy = req
            break
    _single_install(cmds, reqnumpy, bin_dir) # force numpy first so we can use f2py later
    for req in reqs:
        _single_install(cmds, req, bin_dir)

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
