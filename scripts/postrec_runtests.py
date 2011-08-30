
"""
A script to run an OpenMDAO branch test triggered by a post_recieve
hook on github.
"""

import os
import shutil
from optparse import OptionParser


def run_testbranch(argv=None):
    if argv is None:
        argv = sys.argv[1:]
        
    parser = OptionParser(usage="%prog [OPTIONS] -- [options to openmdao_test]")
    parser.add_option("-d", "--dir", action='store', dest='workingdir', 
                      help="working directory")
    parser.add_option("--host", action='append', dest='hosts', metavar='HOST',
                      default=[],
                      help="Select host from config file to run on. "
                           "To run on multiple hosts, use multiple --host args")
    parser.add_option("--all", action="store_true", dest='allhosts',
                      help="If True, run on all hosts in config file.")
    parser.add_option("-k","--keep", action="store_true", dest='keep',
                      help="Don't delete the temporary build directory. "
                           "If testing on EC2 stop the instance instead of terminating it.")

    (options, args) = parser.parse_args(argv)
    
    if not options.hosts and not options.allhosts:
        parser.print_help()
        print "nothing to do - no hosts specified"
        sys.exit(0)
    
    startdir = os.getcwd()
    
    fname = os.path.abspath(os.path.expanduser(options.fname))
    
    if not fname.endswith('.git'):
        parser.print_help()
        print "\nfilename must end in '.git'"
        sys.exit(-1)
        
    funct_kwargs = { 'keep': options.keep,
                     'testargs': args,
                     'fname': fname,
                     'remotedir': get_tmp_user_dir(),
                     'branch': options.branch,
                     }
        
            
    return retcode

def activate_and_run(envdir, cmd):
    """"
    Runs the given command from within an activated OpenMDAO virtual environment located
    in the specified directory.
    
    Returns the output and return code of the command as a tuple (output, returncode).
    """
    if sys.platform.startswith('win'):
        devbindir = 'Scripts'
        command = ['activate.bat',  '&&'] + cmd
    else:
        devbindir = 'bin'
        command = ['. ./activate', '&&'] + cmd
    
    # activate the environment and run command
    devbinpath = os.path.join(envdir, devbindir)
    os.chdir(devbinpath)
    print("running %s from %s" % (command, devbinpath))
    env = os.environ.copy()
    for name in ['VIRTUAL_ENV','_OLD_VIRTUAL_PATH','_OLD_VIRTUAL_PROMPT']:
        if name in env: 
            del env[name]

    return _run_sub(' '.join(command), env=env)

def _run_sub(cmd, env=None):
    p = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE,
                         stderr=subprocess.STDOUT, env=env)
    output = p.communicate()[0]
    return (output, p.returncode)



if __name__ == "__main__":
    run_testbranch()

