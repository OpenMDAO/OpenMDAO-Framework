#envirodump.py
#USAGE:  "python envirodump.py", text file generated in that spot
"""
TO DO:
*Fix Mac errors
*get package VERSIONS
*fix aliases problem
*do a WHICH on compilers?
*deal with sys.exit on bad Python version?
Xmake docstrings consistent
Xmake Exception handling consistent 
http://www.opensource.apple.com/source/python/python-3/python/Tools/freeze/modulefinder.py
"""

def callit(f, funct):
    try:
        funct(f)
    except Exception, err:
        f.write(str(err)+'\n')

try:
    import compiler
    import datetime
    import os
    import platform
    import site
    import subprocess
    import sys
    from cStringIO import StringIO
    from imp import find_module, load_module, PY_SOURCE, PKG_DIRECTORY
except ImportError as err:
    print "Import Error:  ", str(err)


def system_info_dump(f):
    """
       This function runs the rest of the utility.
       It creates an object in which to write and passes it to each separate function.
       Finally, it writes the value of that object into a dumpfile.
    """
    callit(f, get_dump_time)
    callit(f, get_python_info)
    callit(f, get_platform_info)
    callit(f, get_system_env)
    callit(f, get_aliases)
    callit(f, get_compiler_info)
    callit(f, get_pkg_info)


def get_dump_time(f):
    """
    Writes the time and date of the system dump
    """
    f.write('Date of environment dump: \n')
    f.write(str(datetime.datetime.now())+'\n')


def get_python_info(f):
    """
        This function will capture specific python information,
        such as version number, compiler, and build.
    """
    f.write('\n\n================PYTHON INFORMATION================\n')
    f.write('Python Version:  ')
    vers = platform.python_version()
    f.write(vers)
    split_vers = vers.split('.')
    f_zero = float(split_vers[0])
    f_one = float(split_vers[1])
    if ((f_zero != 2) or (f_one < 6)):
        f.write('\nERROR: OpenMDAO WILL NOT WORK with a python version before 2.6 or after 3.0')

    f.write('\n')
    f.write('Python Compiler:  ')
    f.write(platform.python_compiler())
    f.write('\n')
    f.write('Python Build:  ')
    f.write(str(platform.python_build()))
    f.write('\n')
    f.write('Python Path:  \n')
    for subp in sys.path:
        f.write("    "+str(subp)+"\n")
    f.write('\n')
        
    
def get_platform_info(f):
    """
        This function will capture specific platform information,
        such as OS name, architecture, and linux distro.
    """
    f.write('\n\n================PLATFORM INFORMATION================\n')
    f.write('Platform:  ')
    f.write(platform.system())
    f.write('\n')
    f.write('Operating System:  ')
    f.write(platform.platform())
    f.write('\n')
    f.write('Architecture:  ')
    f.write(str(platform.architecture()))
    f.write('\n')
    if  platform.system()=="Linux":
        try:  
            pld = platform.linux_distribution() 
            if(pld):
                f.write('Linux Distribution:  ')
                f.write(str(platform.linux_distribution()))
                f.write('\n')
        except Exception as exc:  
            f.write('This Linux distribution does not support linux_distribution var:\n')
            f.write(str(exc))


    f.write('\n\n')


def get_system_env(f):
    """
        This function captures the values of a system's environment 
        variables at the time of the run, and presents them
        in alphabetical order.
    """
    f.write('================ENVIRONMENT VARIABLES================\n')

    #this bit sorts the environment dictionary by key name.   
    vardump = os.environ
    env_vars = vardump.keys()
    env_vars.sort()

    #spit out env vars alphabetical order
    for env_var in env_vars:
        f.write(env_var)
        f.write(':  ')
        if env_var.endswith("PATH"):
            paths = vardump[env_var]
            if  platform.system() is "Windows":
                splitpaths = paths.split(';')
            else:
                splitpaths = paths.split(':')
            f.write("\n")                
            for path in splitpaths:
                f.write("    "+path+"\n")
        else:
            f.write(vardump[env_var])
        f.write('\n')


def get_aliases(f):
    """
    Gets aliases on Unix/Mac
    """
    if  platform.system() is not "Windows":
        f.write('\n================ALIASES================\n')
        cmd = 'alias'
        result = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        (ferr, fout) = (result.stderr, result.stdout)                
        f.write(fout.read())
        f.write(ferr.read())


def get_compiler_info(f):
    """
        This function will call out for specific information on each compiler.
    """
    f.write('\n\n================COMPILER INFORMATION================\n')
    f.write('Compilers:  \n')
    if  platform.system() is not "Windows":
        try:
            find_compiler(f, 'gcc')
            find_compiler(f, 'g77')
            find_compiler(f, 'gfortran')
        except Exception as exc:
            f.write('\nERROR searching for compiler:\n')
            f.write(str(exc)+'\n')
    else:
        try:
            find_compiler(f, 'cl', '.exe')
            find_compiler(f, 'gcc', '.exe')
            find_compiler(f, 'gfortran', '.exe')
            find_compiler(f, 'g77', '.exe')
        except Exception as exc:
            f.write('\nERROR searching for compiler:\n')
            f.write(str(exc)+'\n')


def find_compiler(f, name, ext=''):
    """
        This function will find compilers, print their paths,
        and reveal their versions.
    """
    compiler_found = 0
    path = os.environ.get('PATH')
    for dir in path.split(os.pathsep):
        binpath = os.path.join(dir, name) + ext
        if os.path.exists(binpath):
            codeOut=StringIO()
            f.write('  '+name+ext+' FOUND:  ')
            f.write(os.path.abspath(binpath))
            f.write('\n')
            compiler_found = 1


            #execute code here to get version as long as it's not "cl.exe"
            if name!="cl.exe":
                executable = os.path.abspath(binpath)
                result = subprocess.Popen([executable, '--version'], stdin=subprocess.PIPE, stdout=subprocess.PIPE)                
                (fin, fout) = (result.stdin, result.stdout)
                f.write('  '+name+' version info: \n')
                f.write(fout.read())

    if not compiler_found:
        f.write('  '+name+ext+' NOT FOUND in PATH. \n\n')
       
        
def _is_py_file(path):
    """Return True if path ends in .py, .pyc, .pyo, or .pyd"""
    return os.path.splitext(path)[1] in ['.py','.pyc','.pyo','.pyd']

def _is_py_pkg(path):
    return os.path.isdir(path) and (
        os.path.isfile(os.path.join(path,'__init__.py')) or 
        os.path.isfile(os.path.join(path,'__init__.pyc')) or
        os.path.isfile(os.path.join(path,'__init__.pyo')))
    
def _add_egg(entry, pkgs):
    if os.path.isfile(entry):
        pkgs.append(entry)
    else:
        contents = os.listdir(entry)

def _add_egg_link(entry, pkgs):
    f = open(entry, 'r')
    lines = [line.strip() for line in f]
    assert(len(lines)==2)
    path = os.path.join(lines[0],lines[1])
    abspath = os.path.abspath(path)
    pkgs.append(abspath)

def _add_dir(entry, pkgs):
    files = os.listdir(entry)
    modset = set()
    for f in files:
        if _is_py_file(f):
            modset.add(os.path.splitext(f)[0])
        elif f.endswith('.egg-link'):
            _add_egg_link(os.path.join(entry,f), pkgs)
        elif _is_py_pkg(os.path.join(entry,f)):
            pkgs.append(os.path.join(entry, f))
    for mod in modset:
        pkgs.append(os.path.join(entry, mod))

def _add_from_path_entry(entry, pkgs):
    """
        For the given path entry, if it's a module, add it to the pkg list.
        If it's a directory, find the modules/packages in it.
    """
    if os.path.isfile(entry) and _is_py_file(entry):
        pkgs.append(entry)
    elif entry.endswith('.egg'):
        _add_egg(entry, pkgs)
    elif entry.endswith('.egg-link'):
        _add_egg_link(entry, pkgs)
    elif os.path.isdir(entry):
        if _is_py_pkg(entry):
            pkgs.append(entry)
        else:
            _add_dir(entry, pkgs)
    
def get_pkg_info(f):
    """
        This function will list python packages and versions.
    """

    f.write('\n\n================PYTHON PACKAGES================\n')

    # loop over each entry in sys.path and return the names of all
    # packages in the order that they're found
    pkgs = []
    for entry in sys.path:
        _add_from_path_entry(entry, pkgs)
    for p in pkgs:
        f.write(p+'\n')
    
    ##swallow all the junk output that is spit out during this part.
    #deadout = sys.stdout
    #deaderr = sys.stderr
    #if  platform.system() is not "Windows":
        #fsock = open('/dev/null', 'w')
    #else:
        #fsock = open('nul', 'w')
    #sys.stdout = fsock
    #sys.stderr = fsock
    #u = PkgUtil()
    #u.list_packages(f)
    #sys.stdout = deadout
    #sys.stderr = deaderr


#class PkgUtil(object):
    #""" Utility class for querying information about
    #installed packages and modules """

    #def __init__(self, paths=None):
        #self.paths = sys.path
        #if paths:
            #self.paths = paths + self.paths

    #def find_standard_package(self, pkgname):
        #"""Search in standard paths for a package/module """
        #try:
            #result = find_module(pkgname)
            #return result
        #except ImportError, e:
            #return ()

    #def get_package_init_path(self, pkgname, pkgdir):
        #""" Return the init file path for the package.
        #This has to be called only for directory packages """

        #pkgdir = os.path.abspath(pkgdir)

        ## Try __init__.py
        #pkginitfile = os.path.join(pkgdir, '__init__.py')
        ## If it does not exist, try <pkgname>.py
        #if not os.path.isfile(pkginitfile):
            #pkginitfile = os.path.join(pkgdir,pkgname + '.py')

        #if os.path.isfile(pkginitfile):
            #return pkginitfile
        #else:
            ## Everything failed, return pkgdir itself!
            #return pkgdir


    #def find_package(self, pkgname):
        ## Query for package/module and return a dictionary
        ## with the following fields
        ## 'name': Package/module name,
        ## 'path' : Full path of the package/module,
        ## 'type' : What kind of a package/module is it
        ##          This has the following values
        ## 'doc'  : Package documentation

        #d = {}
        #packages = pkgname.split('.')
        #top_level = packages[0]

        #try:
            ## First look for built-in modules
            #result = self.find_standard_package(pkgname)
            #if not result and self.paths:
                #result = find_module(pkgname, self.paths)
            #if result:
                #of, pathname, desc = result
                ## Last or only component of package
                #if len(packages)==1:
                    ## Load module
                    #try:
                        #M = load_module(pkgname, of, pathname, desc)
                    #except Exception, e:
                        #return d

                    #d['name'] = pkgname
                    #d['type'] = desc[2]
                    #d['doc']=''

                    #if os.path.dirname(pathname):
                        #d['path'] = self.get_package_init_path(pkgname, pathname)
                    #else:
                        ## For built-in modules
                        #d['path']=pathname
                    #if M:
                        #if M.__doc__:
                            ## Set doc string
                            #d['doc'] = M.__doc__
                        #else:
                            #pkgfile = ''
                            ## Load comments from the package file
                            #if d['type'] == PY_SOURCE:
                                #pkgfile = d['path']
                            #elif d['type'] == PKG_DIRECTORY:
                                #if os.path.isfile(d['path']):
                                    #pkgfile = d['path']

                            ##if pkgfile:
                                ##d['doc'] = self.load_comments(pkgfile)

                    #return d


        #except ImportError, e:
            #if len(packages)>1:
                #try:
                    #result = find_module(top_level, self.paths)
                    #if result:
                        #of, pathname, desc = result
                        #try:
                            #M = load_module(top_level, of, pathname, desc)
                            ## Remove the top_level package from the name
                            #pkgname = reduce(lambda x,y: x+'.'+y, packages[1:])
                            ## Call this recursively
                            #if hasattr(M, '__path__'):
                                #return self.find_package(pkgname, M.__path__)
                        #except ImportError, e:
                            #pass
                        #except Exception, e:
                            #pass
                #except ImportError, e:
                    #pass
            #else:
                #pass
        #return d


    #def list_packages(self, f):
        #""" An ambitious function which attempts to list all Python packages
        #in your system, according to the configuration """

        ## First extract loaded module names from sys.modules
        #sys_modules = sys.modules.keys()

        #packages = {}

        ## Loop through all directories in sys.path and check for modules
        ## Dont iterate through <prefix>/lib directory
        #libdir = os.path.join(sys.prefix, 'lib')
        #walked = []
        #for top_level in self.paths:
            #if not os.path.isdir(top_level):
                #continue

            ## Dont iterate through libdir
            #if os.path.abspath(top_level) == os.path.abspath(libdir):
                #continue

            #walked.append(top_level)
            #for item in os.listdir(top_level):

                #fullpath = os.path.join(top_level, item)
                #if fullpath in walked: continue

                #walked.append(fullpath)
                ## Remove the extension
                #idx = item.find('.')
                #if idx != -1: item = item[:idx]
                #d = self.find_package(item)
                #if not d: continue
                #try:
                    #pkginfo = packages[d['type']]
                    #if d['type'] == 5:
                        #pkginfo[d['name']] = d['path']
                #except Exception, e:
                    #if d['type'] == 5:
                        #packages[d['type']] = { d['name'] : d['path'] }

        #for key,item in packages.items():
            #listofitems = item.keys()
            #listofitems.sort()
            #for key2 in listofitems:
                #f.write(key2+': '+item[key2]+'\n')

if __name__ == "__main__":
    if len(sys.argv) > 1:
        f = open(sys.argv[1], 'w')
    else:
        f = sys.stdout
    try:
        system_info_dump(f)
    finally:
        if f is not sys.stdout:
            f.close()
    
    