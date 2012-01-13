"""
Fake 'qsub' for testing.
"""

import subprocess
import sys


def main():
    cmd = 'no-cmd-set'
    args = []
    print ' '.join(sys.argv[1:])

    i = 1
    while i < len(sys.argv):
        opt = sys.argv[i]
        i += 1
        if opt in ('-V',):
            print opt
        elif opt in ('-j', '-C', '-S', '-W'):
            arg = sys.argv[i]
            i += 1
            print opt, 'arg', arg
        else:
            cmd = opt
            args = sys.argv[i:]
            break

    cmdlist = [cmd]
    cmdlist.extend(args)
    print ' '.join(cmdlist)

    retcode = subprocess.call(cmdlist, shell=True)
    sys.exit(retcode)


if __name__ == '__main__':
    main()

