"""

::

  envirodump.py
  USAGE:  "python envirodump.py", text file generated in that spot

  TO DO:
    get package VERSIONS
        attempt to use pip freeze if available
    fix aliases problem
    do a WHICH on compilers?
    deal with sys.exit on bad Python version?
  X make docstrings consistent
  X make Exception handling consistent
    http://www.opensource.apple.com/source/python/python-3/python/Tools/freeze/modulefinder.py
"""

def callit(f, funct):
    try:
        funct(f)
    except (SystemExit, Exception), err:
        f.write(str(err)+'\n')

def envdump():
    if len(sys.argv) > 1:
        f = open(sys.argv[1], 'wb')
    else:
        f = sys.stdout
    try:
        system_info_dump(f)
    finally:
        if f is not sys.stdout:
            f.close()

try:
    import datetime
    import os
    import platform
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
        This function will capture specific Python information,
        such as version number, compiler, and build.
    """
    f.write('\n\n================PYTHON INFORMATION================\n')
    f.write('Python Version:  ')
    vers = platform.python_version()
    f.write(vers)
    split_vers = vers.split('.')
    f_zero = float(split_vers[0])
    f_one = float(split_vers[1])
    if ((f_zero != 2) or (f_one < 7)):
        f.write('\nERROR: OpenMDAO WILL NOT WORK with a python version before 2.7 or after 3.0')

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
    Gets aliases on Unix/Mac.
    """
    if  platform.system() is not "Windows":
        f.write('\n================ALIASES================\n')
        p = subprocess.Popen('alias', shell=True,
                             stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        out = p.communicate()[0]
        f.write(out)


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

def _get_real_pkg_name(path):
    initpath = os.path.join(path, '__init__.py')
    if os.path.isfile(initpath):
        f = open(initpath, 'r')
        contents = f.read()
        if 'declare_namespace(__name__)' in contents or 'pkgutil.extend_path' in contents:
            pdirs = [f for f in os.listdir(path)
                         if os.path.isdir(os.path.join(path,f)) and
                         os.path.isfile(os.path.join(path,f,'__init__.py'))]
            if len(pdirs) == 1:
                return _get_real_pkg_name(os.path.join(path, pdirs[0]))
    return path

def _is_py_pkg(path):
    return os.path.isdir(path) and (
        os.path.isfile(os.path.join(path,'__init__.py')) or
        os.path.isfile(os.path.join(path,'__init__.pyc')) or
        os.path.isfile(os.path.join(path,'__init__.pyo')))

def _add_egg(entry, pkgs):
    if os.path.isfile(entry):
        pkgs.append(entry)
    else:
        toplevel = os.path.join(entry,'EGG-INFO','top_level.txt')
        if os.path.isfile(toplevel):
            f = open(toplevel, 'r')
            for line in f:
                tl = line.strip()
                pkgs.append(_get_real_pkg_name(os.path.join(entry,tl)))
        else:
            pkgs.append(entry)

def _add_egg_link(entry, pkgs):
    #f = open(entry, 'r')
    #lines = [line.strip() for line in f]
    #assert(len(lines)==2)
    #path = os.path.join(lines[0],lines[1])
    #abspath = os.path.abspath(path)
    #pkgs.append(abspath)
    pass

def _add_dir(entry, pkgs):
    if _is_py_pkg(entry):
        pkgs.append(_get_real_pkg_name(entry))
    else:
        files = os.listdir(entry)
        for f in files:
            path = os.path.join(entry,f)
            if _is_py_file(f):
                pass
            elif f.endswith('.egg-link'):
                _add_egg_link(path, pkgs)
            elif _is_py_pkg(path):
                pkgs.append(_get_real_pkg_name(path))

def _add_from_path_entry(entry, pkgs):
    """
        For the given path entry, if it's a module, add it to the pkg list.
        If it's a directory, find the modules/packages in it.
    """
    if os.path.isfile(entry) and _is_py_file(entry):
        pass
    elif entry.endswith('.egg'):
        _add_egg(entry, pkgs)
    elif entry.endswith('.egg-link'):
        _add_egg_link(entry, pkgs)
    elif os.path.isdir(entry):
        _add_dir(entry, pkgs)

def get_pkg_info(f):
    """
        This function will list Python packages found on sys.path
    """

    f.write('\n\n================PYTHON PACKAGES================\n')

    # loop over each entry in sys.path and return the names of all
    # packages in the order that they're found
    paths = {}
    for entry in sys.path:
        pkgs = []
        _add_from_path_entry(entry, pkgs)
        if pkgs:
            if entry.rstrip('/').rstrip('\\') == pkgs[0].rstrip('/').rstrip('\\'):
                f.write("\nfrom %s:\n" % os.path.dirname(entry))
                f.write("    %s\n" % os.path.basename(entry))
            else:
                f.write("\nfrom %s:\n" % entry)
                for p in pkgs:
                    f.write("    %s\n" %
                            p[len(entry)+1:].replace('/','.').replace('\\','.'))

if __name__ == "__main__":
    envdump()
