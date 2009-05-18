import os,sys
import linecache
import fnmatch
from subprocess import Popen,PIPE,STDOUT

def traceit(frame, event, arg):
    """a function useful for tracing python execution. wherever you want the 
    tracing to start, insert a call to sys.settrace(traceit)"""
    if event == "line":
        lineno = frame.f_lineno
        filename = frame.f_globals["__file__"]
        if (filename.endswith(".pyc") or
            filename.endswith(".pyo")):
            filename = filename[:-1]
        name = frame.f_globals["__name__"]
        line = linecache.getline(filename, lineno)
        print "%s:%s: %s" % (name, lineno, line.rstrip())
    return traceit


def run_command(cmd, sh=True):
    """run a command using Popen and return its output (stdout and stderr)
    and its return code as a tuple.
    """

    p = Popen(cmd, stdout=PIPE, stderr=STDOUT, env=os.environ, shell=sh)
    output = p.communicate()[0]
    return (output, p.returncode)

