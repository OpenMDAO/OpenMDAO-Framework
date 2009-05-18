
import os.path
import distutils
from subprocess import Popen,PIPE,STDOUT


def _run_command(cmd, sh=True):
    """Run a command using Popen and return its output (stdout and stderr)
    and its return code as a tuple.
    """

    p = Popen(cmd, stdout=PIPE, stderr=STDOUT, env=os.environ, shell=sh)
    output = p.communicate()[0]
    return (output, p.returncode)


def findfiles(dirname):
    """Return a list of files (no directories) only if they are versioned in
    a bazaar repository, using relative paths based on the directory name 
    passed in.  The bazaar executable (bzr) must be in the path because this
    routine just makes a system call to 'bzr ls --versioned <dirname>'.
    """
    try:
        out, ret = _run_command('bzr ls --versioned %s' % dirname)
    except Exception, err:
        distutils.log.warn(str(err))

    names = []
    for name in out.splitlines():
        sname = name.strip()
        if sname != '':
            path = os.path.join(dirname, sname)
            if os.path.isfile(path):
                names.append(path)
                
    return names
