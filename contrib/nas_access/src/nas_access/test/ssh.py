"""
Fake version of 'ssh' for testing.
"""

import os.path
import shutil
import subprocess
import sys


def main():
    """ python ssh.py root host cmd [args...] """
    root = sys.argv[1]
    if not os.path.exists(root):
        os.mkdir(root)
    os.chdir(root)
    if sys.platform == 'win32':
        # Python implementation for a  limited set of commands.
        if sys.argv[3] == 'ls':
            directory = sys.argv[5] if len(sys.argv) > 5 else '.'
            for name in os.listdir(directory):
                print name
            retcode = 0
        elif sys.argv[3] == 'mkdir':
            os.mkdir(sys.argv[4])
            retcode = 0
        elif sys.argv[3] == 'rm':
            if sys.argv[4].startswith('-r'):  # Directory;
                if os.path.exists(sys.argv[5]):
                    shutil.rmtree(sys.argv[5])
                retcode = 0
            else:  # File.
                os.remove(sys.argv[4])
                retcode = 0
        else:
            raise RuntimeError('Unsupported command %r' % sys.argv[3:])
    else:
        retcode = subprocess.call(sys.argv[3:])
    sys.exit(retcode)


if __name__ == '__main__':
    main()

