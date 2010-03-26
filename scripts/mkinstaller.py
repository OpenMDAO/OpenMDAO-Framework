
from optparse import OptionParser
import virtualenv



#def run(argvs=None):
    #"""Set up the OpenMDAO environment in the specified directory. The 
    #directory must NOT exist prior to calling this function.
    #"""
    #if argvs is None:
        #argvs = sys.argv[1:]
    #parser = OptionParser()
    #parser.add_option("-f", "--find-links", action="append", type="string", dest='findlinks', 
                      #help="URLs where distributions may be found") 
    #parser.add_option("-d", "", action="store", type="string", dest='destination',
                      #help="destination") 
    
    #(options, args) = parser.parse_args(argvs)
    
    #if options.destination:
        #install_dir = options.destination
    #else:
        #raise RuntimeError("the install directory was not specified")

    #startdir = os.getcwd()
    #os.makedirs(install_dir)
    #path = os.path.abspath(install_dir)
    #if options.findlinks:
        #flinks = ' '.join(options.findlinks)
    #else:
        #flinks = ''
    
    #opt_dict = {
        #'find-links': flinks,
        #}

    #try:
        #bstrapfname = os.path.join(os.path.dirname(__file__), 
                                   #'bootstrap.py')
        #bconfigfname = os.path.join(os.path.dirname(__file__), 
                                    #'buildout.cfg')
        #os.chdir(path)
        
        #infile = open(bconfigfname, 'r')
        #boconf_template = infile.read()
        #infile.close()
        
        #boconf = boconf_template % opt_dict
        
        #outf = open(os.path.join(path, 'buildout.cfg'), 'w')
        #outf.write(boconf)
        #outf.close()
        
        ##shutil.copy(bconfigfname, os.path.join(path, 'buildout.cfg'))
        #check_call([sys.executable, bstrapfname])
        #build_cmd = [os.path.join('bin','buildout')]
        #check_call(build_cmd)
    #finally:
        #os.chdir(startdir)


def main():
    """
    Takes a file containing a list of requirements, one per line, and
    generates an install script that makes a virtualenv with all of the
    required packages based on the contents of the requirements file.
    """
    
    script_str = """

def after_install(options, home_dir):
    global logger
    reqs = %(reqs)s
    cmds = %(cmds)s
    for req in reqs:
        cmdline = [join(home_dir, 'bin', 'easy_install')] + cmds+[req]
        logger.debug("running command: %%s" %% ' '.join(cmdline))
        subprocess.check_call(cmdline)

    """
    parser = OptionParser()
    parser.add_option("-r", "--requirement", action="store", type="string", dest='req', 
                      help="requirements file") 
    
    (options, args) = parser.parse_args()
    
    if not options.req:
        raise RuntimeError("a requirements file was not specified")
    
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
    print virtualenv.create_bootstrap_script(script_str % optdict, '2.6')


if __name__ == '__main__':
    main()