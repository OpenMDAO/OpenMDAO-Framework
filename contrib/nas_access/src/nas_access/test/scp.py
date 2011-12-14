"""
Fake version of 'scp' for testing.
"""

import os.path
import pkg_resources
import sys


def main():
    """ python scp.py src dst """
    directory = \
        os.path.realpath(pkg_resources.resource_filename('nas_access', 'test'))
    root = os.path.join(directory, 'Fake-DMZ')
    if not os.path.exists(root):
        os.mkdir(root)
    os.chdir(root)
    if sys.argv[1] == '--rje':
        src = fixup(sys.argv[2], rje=True)
        dst = fixup(sys.argv[3], rje=True)
    else:
        src = fixup(sys.argv[1], rje=False)
        dst = fixup(sys.argv[2], rje=False)
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

