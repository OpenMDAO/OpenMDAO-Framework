
import sys
import os
import shutil
import urllib2
import subprocess
import tempfile
from optparse import OptionParser


def get_release_script(site_url, version):
    """Grabs the specified openmdao release installer script from the specified 
    url, so it can be tested on our dev platforms. The script will be placed in
    the current directory.
    """
    script_url =  '%s/downloads/%s/go-openmdao.py' % (site_url, version)

    if script_url.startswith('http'):
        resp = urllib2.urlopen(script_url)
        gofile = open('go-openmdao.py', 'wb')
        shutil.copyfileobj(resp.fp, gofile)
        gofile.close()
    else: # release in local file system
        shutil.copy(script_url, 'go-openmdao.py')

def build_release(site_url, version, pyversion):
    """
    Buids an OpenMDAO release in the current directory.
    
    site_url: str
        The go-openmdao.py file will be copied from here. This
        can either be a URL or a local directory name.
        
    version: str
        The version of OpenMDAO to be tested.
        
    pyversion: str
        The version of python, e.g., '2.6' or '2.7', to be
        used to create the OpenMDAO virtual environment. Only
        major version numbers should be used, i.e., use '2.6'
        rather then '2.6.5'.
    
    Returns the relative name of the newly built release directory.
    """
    get_release_script(site_url, version)
    
    dirfiles = set(os.listdir('.'))
    
    try:
        print "building openmdao environment"
        subprocess.check_call([pyversion, 'go-openmdao.py'])
        newfiles = set(os.listdir('.')) - dirfiles
        if len(newfiles) != 1:
            raise RuntimeError("didn't expect %s in build directory" % 
                               list(newfiles))
        releasedir = newfiles.pop()
    finally:
        os.chdir(startdir)

    return releasedir
    

def validate_release(releasedir):
    """"
    Runs the test suite on an OpenMDAO release environment located
    in the specified directory.
    
    Returns a tuple of the form (retcode, output), the 
    return code and output of the process that runs the test suite.
    """
    if sys.platform.startswith('win'):
        devbindir = 'Scripts'
        command = 'activate.bat && openmdao_test -x'
    else:
        devbindir = 'bin'
        command = '. ./activate && openmdao_test -x'
        
    # activate the environment and run tests
    devbinpath = os.path.join(releasedir, devbindir)
    os.chdir(devbinpath)
    print("running tests from %s" % devbinpath)
    env = os.environ.copy()
    for name in ['VIRTUAL_ENV','_OLD_VIRTUAL_PATH','_OLD_VIRTUAL_PROMPT']:
        if name in env: 
            del env[name]
    proc = subprocess.Popen(command,
                            stdout = subprocess.PIPE,
                            shell = True,
                            cwd = os.getcwd(),
                            env=env)
    out = proc.communicate()[0]
    proc.wait()
    return (proc.returncode, out)
    

if __name__ == '__main__':
    parser = OptionParser()
    parser.add_option("-s", "--site", type="string", dest='site', 
                      default='http://openmdao.org',
                      help="URL where release files are located. "
                           "This can be just a directory in the file system as well.")
    parser.add_option("-v", "--version", action="store", type='string', dest='version', 
                      help="version to test", default='latest')
    parser.add_option("--pyversion", action="store", type='string', dest='pyversion',
                      default="", help="python version to use")
    parser.add_option("-k","--keep", action="store_true", dest='keep',
                      help="don't delete temporary build directory")

    (options, args) = parser.parse_args(sys.argv[1:])
    
    startdir = os.getcwd()
    tmpdir = tempfile.mkdtemp()
    os.chdir(tmpdir)
    
    try:
        reldir = build_release(options.site, options.version, 
                               pyversion="python%s"%options.pyversion)
        retcode, out = validate_release(os.path.join(tmpdir,reldir))
        print out
        print 'return code from test was %s' % retcode
        
    finally:
        os.chdir(startdir)
        if options.keep:
            print "keeping temp dir %s" % tmpdir
        else:
            print "removing temp dir %s" % tmpdir
            shutil.rmtree(tmpdir)

    