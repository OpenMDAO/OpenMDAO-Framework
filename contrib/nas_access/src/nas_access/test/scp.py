"""
Fake version of 'scp' for testing.
"""

import os.path
import sys


def main():
    """ python scp.py root [--rje] src dst """
    root = sys.argv[1]
    if not os.path.exists(root):
        os.mkdir(root)
    os.chdir(root)

    extra = 2 if sys.platform == 'win32' else 0
    if sys.argv[2] == '--rje':
        src = fixup(sys.argv[3 + extra], rje=True)
        dst = fixup(sys.argv[4 + extra], rje=True)
    else:
        src = fixup(sys.argv[2 + extra], rje=False)
        dst = fixup(sys.argv[3 + extra], rje=False)

    with open(src, 'rb') as inp:
        with open(dst, 'wb') as out:
            out.write(inp.read())
    sys.exit(0)


def fixup(path, rje):
    """ Fix `path` for remote/local handling. """
    if ':' in path:  # Remote.
        host, colon, path = path.partition(':')
    elif rje:
        path = os.path.join('..', 'RJE', path)
    else:
        path = os.path.join('..', path)
    return path


if __name__ == '__main__':
    main()

