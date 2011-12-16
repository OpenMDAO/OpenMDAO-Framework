"""
Fake version of 'ssh' for testing.
"""

import os.path
import subprocess
import sys


def main():
    """ python ssh.py root host cmd [args...] """
    root = sys.argv[1]
    if not os.path.exists(root):
        os.mkdir(root)
    os.chdir(root)
    retcode = subprocess.call(sys.argv[3:])
    sys.exit(retcode)


if __name__ == '__main__':
    main()

