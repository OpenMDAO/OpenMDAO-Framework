"""
Fake version of 'ssh' for testing.
"""

import os.path
import pkg_resources
import subprocess
import sys

_ROOT = 'Fake-DMZ'


def main():
    """ python ssh.py host cmd [args...] """
    directory = \
        os.path.realpath(pkg_resources.resource_filename('nas_access', 'test'))
    root = os.path.join(directory, 'Fake-DMZ')
    if not os.path.exists(root):
        os.mkdir(root)
    os.chdir(root)
    retcode = subprocess.call(sys.argv[2:])
    sys.exit(retcode)


if __name__ == '__main__':
    main()

