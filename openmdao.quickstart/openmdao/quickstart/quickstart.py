import os
import sys
import shutil
import os.path
from subprocess import Popen, STDOUT, check_call
from optparse import OptionParser
    

def run(argvs=None):
    """Set up the OpenMDAO environment in the specified directory. The 
    directory must NOT exist prior to calling this function.
    """
    if argvs is None:
        argvs = sys.argv[1:]
    parser = OptionParser()
    parser.add_option("-f", "--find-links", action="append", type="string", dest='findlinks', 
                      help="URLs where distributions may be found") 
    parser.add_option("-d", "", action="store", type="string", dest='destination',
                      help="destination") 
    
    (options, args) = parser.parse_args(argvs)
    
    if options.destination:
        install_dir = options.destination
    else:
        raise RuntimeError("the install directory was not specified")

    startdir = os.getcwd()
    os.makedirs(install_dir)
    path = os.path.abspath(install_dir)
    if options.findlinks:
        flinks = ' '.join(options.findlinks)
    else:
        flinks = ''
    
    opt_dict = {
        'find-links': flinks,
        }

    try:
        bstrapfname = os.path.join(os.path.dirname(__file__), 
                                   'bootstrap.py')
        bconfigfname = os.path.join(os.path.dirname(__file__), 
                                    'buildout.cfg')
        os.chdir(path)
        
        infile = open(bconfigfname, 'r')
        boconf_template = infile.read()
        infile.close()
        
        boconf = boconf_template % opt_dict
        
        outf = open(os.path.join(path, 'buildout.cfg'), 'w')
        outf.write(boconf)
        outf.close()
        
        #shutil.copy(bconfigfname, os.path.join(path, 'buildout.cfg'))
        check_call([sys.executable, bstrapfname])
        build_cmd = [os.path.join('bin','buildout')]
        check_call(build_cmd)
    finally:
        os.chdir(startdir)