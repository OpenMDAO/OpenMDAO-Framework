"""
Fake version of 'ssh' for testing.
"""

import glob
import os.path
import shutil
import subprocess
import sys


def main():
    """ python ssh.py root -l user host cmd [args...] """
    root = sys.argv[1]
    if not os.path.exists(root):
        os.mkdir(root)
    os.chdir(root)

    # Normally the sending shell would remove the escape ('\\*' => '*').
    for i, arg in enumerate(sys.argv[6:]):
        sys.argv[6+i] = arg.replace('\\', '')

    if sys.platform == 'win32':
        # Python implementation for a  limited set of commands.
        if sys.argv[5] == 'ls':
            for name in os.listdir('.'):
                print name
            retcode = 0
        elif sys.argv[5] == 'rm':
            if sys.argv[6] == '-f':
                for name in glob.glob(sys.argv[7]):
                    os.remove(name)
            else:
                for name in sys.argv[6:]:
                    os.remove(name)
            retcode = 0
        elif sys.argv[5] == 'date':
            with open(sys.argv[7], 'w') as out:
                out.write('empty\n')
            retcode = 0
        else:
            raise RuntimeError('Unsupported command %r' % sys.argv[5:])
    else:
        retcode = subprocess.call(' '.join(sys.argv[5:]), shell=True)

    sys.exit(retcode)


if __name__ == '__main__':
    main()

