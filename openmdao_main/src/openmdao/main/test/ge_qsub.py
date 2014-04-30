"""
Fake 'qsub' for testing.
"""

import subprocess
import sys


def main():
    cmd = 'no-cmd-set'
    args = []
    stdin = 'nul:' if sys.platform == 'win32' else '/dev/null'
    stdout = 'qsub.stdout'
    stderr = 'qsub.stderr'
    join_eo = False

    print ' '.join(sys.argv[1:])

    i = 1
    while i < len(sys.argv):
        opt = sys.argv[i]
        i += 1
        if opt in ('-V', '-cwd', '-h'):
            print opt
        elif opt in ('-sync', '-b', '-N', '-wd', '-M', '-m', '-a', '-dl',
                     '-r', '-ar', '-q', '-p', '-A', '-ac'):
            arg = sys.argv[i]
            i += 1
            print opt, 'arg', arg
        elif opt == '-pe':
            arg = sys.argv[i]
            i += 1
            min_max = sys.argv[i]
            i += 1
            print opt, arg, min_max
        elif opt == '-i':
            stdin = sys.argv[i]
            i += 1
            print opt, 'stdin', stdin
        elif opt == '-o':
            stdout = sys.argv[i]
            i += 1
            print opt, 'stdout', stdout
        elif opt == '-e':
            stderr = sys.argv[i]
            i += 1
            print opt, 'stderr',  stderr
        elif opt == '-j':
            join = sys.argv[i]
            i += 1
            print opt, 'join', join
            if join.startswith('y'):
                join_eo = True
        elif opt == '-l':
            resource_value = sys.argv[i]
            i += 1
            print opt, 'resource',  resource_value
        else:
            cmd = opt
            args = sys.argv[i:]
            break

    cmdlist = [cmd]
    cmdlist.extend(args)
    print ' '.join(cmdlist)

    inp = open(stdin, 'r')
    out = open(stdout, 'w')
    if join_eo:
        err = subprocess.STDOUT
    else:
        err = open(stderr, 'w')

    retcode = subprocess.call(cmdlist, stdin=inp, stdout=out, stderr=err,
                              shell=sys.platform=='win32')
    sys.exit(retcode)


if __name__ == '__main__':
    main()

