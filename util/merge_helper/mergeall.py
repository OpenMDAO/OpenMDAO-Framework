import os
import sys
from subprocess import Popen,PIPE,STDOUT

from gmerge import gmerge

def run_command(cmd, sh=True):
   """Run a command using Popen and return its output (stdout and stderr)
   and its return code as a tuple. If the command is a python file, prepend
   python to the command string
   """
   p = Popen(cmd, stdout=PIPE, stderr=STDOUT, env=os.environ, shell=sh)
   output = p.communicate()[0]
   return (output, p.returncode)



def main():
    out, ret = run_command('bzr conflicts')
    for line in out.splitlines():
        if line.startswith('Text conflict'):
            name = line.split()[3]
            gmerge([name])
    
    
    
if __name__ == '__main__':
    main()
