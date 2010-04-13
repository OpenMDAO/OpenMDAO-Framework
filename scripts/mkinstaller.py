"""
Generates a virtualenv bootstrapping script that will create a virtualenv with
openmdao and all of its dependencies installed in it. The script is written to
a file called go-openmdao.py.
"""

import sys, os
from optparse import OptionParser
import virtualenv



def main():
    
    script_str = """

#openmdaoreq = 'openmdao'

#def extend_parser(optparse_parser):
    #optparse_parser.add_option("", "--openmdaover", action="store", type="string", dest='openmdaover', 
                      #help="specify openmdao version (default is latest)")

def adjust_options(options, args):
    if sys.version_info[:2] < (2,6) or sys.version_info[:2] >= (3,0):
        print 'ERROR: python version must be >= 2.6 and <= 3.0. yours is %%s' %% sys.version.split(' ')[0]
        sys.exit(-1)
    ## setting use_distribute seems to force a local install even if package is already on sys.path
    #options.use_distribute = True  # force use of distribute instead of setuptools
    if len(args) == 0:
        args.append('openmdao-%%s' %% '%(version)s')
    
def _single_install(cmds, req, bin_dir):
    cmdline = [join(bin_dir, 'easy_install')] + cmds + [req]
    #cmdline = [join(bin_dir, 'pip'), 'install'] + cmds + [req]
    logger.debug("running command: %%s" %% ' '.join(cmdline))
    subprocess.call(cmdline)

def after_install(options, home_dir):
    global logger
    reqs = %(reqs)s
    reqs.append('openmdao==%(version)s')
    cmds = %(cmds)s
    url = 'http://openmdao.org/dists'
    found = [c for c in cmds if url in c]
    if not found:
        cmds.extend(['-f',url])
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
    reqnumpy = 'numpy==1.3.0'   # FIXME: grab openmdao dist and query its deps for specific numpy version
    _single_install(cmds, reqnumpy, bin_dir) # force numpy first so we can use f2py later
    for req in reqs:
        _single_install(cmds, req, bin_dir)  

    """
    parser = OptionParser()
    # setuptools doesn't seem to support multiple find-links, but pip does
    parser.add_option("-f", "--find-links", action="append", type="string", dest='flinks', 
                      help="find-links URL") 
    parser.add_option("-r", "--requirement", action="append", type="string", dest='reqs', 
                      help="add an additional required package (multiple are allowed)")
    parser.add_option("-v", "--version", action="store", type="string", dest='version', 
                      help="specify openmdao version that the generated script will install")
    parser.add_option("-d", "--destination", action="store", type="string", dest='dest', 
                      help="specify destination directory", default='.')
    
    
    (options, args) = parser.parse_args()
    
    if not options.version:
        print 'You must supply a version id'
        parser.print_help()
        sys.exit(-1)
    
    reqs = options.reqs if options.reqs is not None else []
    if options.flinks is not None:
        cmds = [ '-f %s' % x for x in options.flinks]
    else:
        cmds = []
    
    optdict = { 'reqs': reqs, 'cmds':cmds, 'version': options.version }
    
    dest = os.path.abspath(options.dest)
    scriptname = os.path.join(dest,'go-openmdao-%s.py' % options.version)
    with open(scriptname, 'wb') as f:
        f.write(virtualenv.create_bootstrap_script(script_str % optdict))
    os.chmod(scriptname, 0755)

if __name__ == '__main__':
    main()
