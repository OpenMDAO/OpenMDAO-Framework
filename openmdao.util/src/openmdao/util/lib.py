import os
import sys

from openmdao.util.fileutil import find_files

_lpdict = {
    'linux2': 'LD_LIBRARY_PATH',
    'linux': 'LD_LIBRARY_PATH',
    'darwin': 'DYLD_LIBRARY_PATH',
    'win32': 'PATH',
}


def get_env_libpath():
    """Find all of the shared libraries in the current virtual environment and
    return the string that must be added to LD_LIBRARY_PATH (or equivalent)
    in order to locate them.
    """
    libpathvname = _lpdict.get(sys.platform)

    if libpathvname:
        topdir = os.path.dirname(os.path.dirname(os.path.abspath(sys.executable)))

        libfiles = []

        if sys.platform.startswith('win'):
            pkgdir = os.path.join(topdir, 'Lib',
                                  'site-packages')
            checker = '*.dll'
        else:
            pkgdir = os.path.join(topdir, 'lib',
                                  'python%s.%s' % sys.version_info[:2],
                                  'site-packages')
            if sys.platform == 'darwin':
                checker = lambda n: n.endswith('.so') or n.endswith('.dylib')
            else:
                checker = "*.so"

        for d in os.listdir(pkgdir):
            d = os.path.join(pkgdir, d)
            if os.path.isdir(d):
                # find any shared libs that don't have a matching .py bootstrapper
                newlibs = []
                for f in find_files(d, checker):
                    if not os.path.isfile(os.path.splitext(f)[0]+'.py'):
                        newlibs.append(f)
                libfiles.extend(newlibs)

        # if the same library appears multiple times under the same subdir parent, remove
        # it from the libpath.
        # Better to fail due to missing lib than to use one with the wrong bitsize...
        # TODO: add some smarts to figure out desired bitsize and keep the correct lib
        #       in the libpath
        bases = {}
        for fname in libfiles:
            bases.setdefault(os.path.basename(fname), []).append(fname)

        if len(bases) < len(libfiles):
            for base, paths in bases.items():
                if len(paths) > 1:
                    for p in paths:
                        libfiles.remove(p)

        added = set([os.path.dirname(n) for n in libfiles])
        return os.pathsep.join(added)
    return ''

def get_full_libpath():
    """Find all of the shared libraries in the current virtual environment and
    return the required LD_LIBRARY_PATH string (or equivalent) necessary
    to find them.
    """
    libpathvname = _lpdict.get(sys.platform)

    if libpathvname:
        return add_to_pathvar(libpathvname, get_env_libpath())
    return ''

def add_to_pathvar(pathvarname, newpaths):
    """Reads the value of the pathvarname variable in the environment,
    add the specified paths to the end of it, removes duplicates, and returns
    the new value.  The environment variable specified by pathvarname
    is assumed to be a 'path' type of variable, e.g. PATH, LD_LIBRARY_PATH, etc.
    """
    envvar = os.environ.get(pathvarname, '')
    parts = [p for p in envvar.split(os.pathsep) if p.strip()]
    pset = set(parts)

    if isinstance(newpaths, basestring):
        entries = [p for p in newpaths.split(os.pathsep) if p.strip()]
    else:
        entries = newpaths

    for path in entries:
        if path not in pset:
            pset.add(path)
            parts.append(path)

    return os.pathsep.join(parts)

def combine_paths():
    """Calls add_to_varpath with the first two command line args and
    prints the result.
    """
    print add_to_pathvar(sys.argv[1], sys.argv[2])

def update_libpath():
    """Updates LD_LIBRARY_PATH (or equivalent) in the activate script
    with new entries based on shared libraries found while searching the 
    current virtual environment.  Because the changes are applied to the
    activate (or activate.bat) script, the virtual environment must
    be re-activated in order for the environment to be updated.
    """
    bindir = os.path.dirname(os.path.abspath(sys.executable))
    libpathvname = _lpdict.get(sys.platform)
    if libpathvname:
        if sys.platform.startswith('win'):
            activate_base = 'activate.bat'
        else:
            activate_base = 'activate'

        absbin = os.path.abspath(bindir)
        activate_fname = os.path.join(absbin, activate_base)
        with open(activate_fname, 'r') as inp:
            content = inp.read()

        newpath = get_env_libpath()

        lines = content.split('\n')

        if sys.platform.startswith('win'):
            newline = '''for /f "delims=" %%A in ('combine_paths %s "%s"') do @set PATH=%%A''' % (libpathvname, newpath)
        else:
            newline = '%s=$(combine_paths %s "%s"); export %s' % (libpathvname, 
                                                                  libpathvname, 
                                                                  newpath, libpathvname)
        for i,line in enumerate(lines):
            if 'combine_paths' in line:
                lines[i] = newline
                break
        else: 
            lines.append(newline)

        with open(activate_fname, 'w') as out:
            for line in lines:
                out.write(line+'\n')
