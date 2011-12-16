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
    if sys.argv[2] == '--rje':
        src = fixup(sys.argv[3], rje=True)
        dst = fixup(sys.argv[4], rje=True)
    else:
        src = fixup(sys.argv[2], rje=False)
        dst = fixup(sys.argv[3], rje=False)
    with open(src, 'r') as inp:
        with open(dst, 'w') as out:
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

