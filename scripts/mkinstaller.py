"""
Generates a virtualenv bootstrapping script that will create a virtualenv with
openmdao and all of its dependencies installed in it. The script is written to
stdout.  The generated script will take an 'openmdaover' argument to allow the
script user to install a specific openmdao version.
"""

import sys
from optparse import OptionParser
import virtualenv



def main():
    
    script_str = """

openmdaoreq = 'openmdao'

def extend_parser(optparse_parser):
    optparse_parser.add_option("", "--openmdaover", action="store", type="string", dest='openmdaover', 
                      help="specify openmdao version (default is latest)")

def adjust_options(options, args):
    global openmdaoreq
    if sys.version_info[:2] < (2,6) or sys.version_info[:2] >= (3,0):
        print 'ERROR: python version must be >= 2.6 and <= 3.0. yours is %%s' %% sys.version.split(' ')[0]
        sys.exit(-1)
    ## setting use_distribute seems to force a local install even if package is already on sys.path
    #options.use_distribute = True  # force use of distribute instead of setuptools
    if options.openmdaover:
        openmdaoreq = 'openmdao==%%s' %% options.openmdaover
    
def _single_install(cmds, req, bin_dir):
    cmdline = [join(bin_dir, 'easy_install')] + cmds + [req]
    #cmdline = [join(bin_dir, 'pip'), 'install'] + cmds + [req]
    logger.debug("running command: %%s" %% ' '.join(cmdline))
    subprocess.check_call(cmdline)

def after_install(options, home_dir):
    global logger, openmdaoreq
    reqs = %(reqs)s
    reqs.append(openmdaoreq)
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
    reqnumpy = 'numpy'   # TODO: grab openmdao dist and query its deps for specific numpy version
    _single_install(cmds, reqnumpy, bin_dir) # force numpy first so we can use f2py later
    for req in reqs:
        _single_install(cmds, req, bin_dir)  

    """
    parser = OptionParser()
    parser.add_option("-f", "--find-links", action="append", type="string", dest='flinks', 
                      help="find-links options") 
    parser.add_option("-r", "--requirement", action="append", type="string", dest='reqs', 
                      help="add an additional required package (multiple are allowed)")
    
    
    (options, args) = parser.parse_args()
    
    reqs = options.reqs if options.reqs is not None else []
    if options.flinks is not None:
        cmds = [ '-f %s' % x for x in options.flinks]
    else:
        cmds = []
    
    optdict = { 'reqs': reqs, 'cmds':cmds }
    print virtualenv.create_bootstrap_script(script_str % optdict)


if __name__ == '__main__':
    main()
