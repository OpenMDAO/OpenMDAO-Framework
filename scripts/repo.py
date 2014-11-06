#!/usr/bin/env python
"""
Repository maintenance.
Can record lock/unlock state of a repository.
Useful to keep multiple developers from stepping on each other,
but requires some discipline :-(
"""

import glob
import optparse
import os.path
import platform
import shutil
import stat
import subprocess
import sys
import time

if sys.platform != 'win32':
    import pwd

LOCKFILE = 'repo_lock'


def main():
    """ Repository maintenance. """

    usage = """\
%prog OP [options] repository, where OP may be:
   check  -- check for lock
   lock   -- lock repository
   unlock -- unlock repository
   set    -- set this as current repository
             (sets OPENMDAO_REPO environment variable and starts a new shell)
   fix    -- fix permissions and remove generated directories
   rmpyc  -- remove 'orphan' .pyc files"""

    parser = optparse.OptionParser(usage)
    parser.add_option('-f', '--force', action='store_true',
                      default=False, help='forced unlock')
    parser.add_option('-v', '--verbose', action='store_true',
                      default=False, help='print info messages')
    try:
        operation = sys.argv.pop(1)
    except IndexError:
        parser.print_help()
        sys.exit(1)
    options, arguments = parser.parse_args()
    repository = ''
    if len(arguments) > 0:
        if len(arguments) == 1:
            repository = arguments[0]
        else:
            parser.print_help()
            sys.exit(1)
    if not repository:
        try:
            repository = os.environ['OPENMDAO_REPO']
        except KeyError:
            pass

    this_user = get_username()
    path = find_repository(repository, this_user)
    if not path:
        print 'Cannot find repository!'
        sys.exit(2)
    if options.verbose:
        print 'Repository root:', path

    if operation == 'check':
        do_check(path)
    elif operation == 'lock':
        do_lock(path)
    elif operation == 'unlock':
        do_unlock(path, options)
    elif operation == 'set':
        do_set(path, this_user)
    elif operation == 'fix':
        do_fix(path, options)
        do_rmpyc(path)
    elif operation == 'rmpyc':
        do_rmpyc(path)
    else:
        parser.print_help()
        sys.exit(1)


def do_check(path):
    """ Perform 'check' operation. """
    (user, mtime) = check_lockfile(path)
    if user is not None:
        print 'Repository locked by', user, 'at', mtime
    else:
        print 'Repository unlocked'
    sys.exit(0)

def do_lock(path):
    """ Perform 'lock' operation. """
    (user, mtime) = check_lockfile(path)
    if user is None:
        create_lockfile(path)
        sys.exit(0)
    else:
        print 'Repository already locked by', user, 'at', mtime
        sys.exit(1)

def do_unlock(path, options):
    """ Perform 'unlock' operation. """
    (user, mtime) = check_lockfile(path)
    if user is None:
        print 'Repository is not locked'
        sys.exit(1)
    elif user == get_username() or options.force:
        remove_lockfile(path)
        sys.exit(0)
    else:
        print 'Repository locked by', user, 'at', mtime
        sys.exit(1)

def do_set(path, user):
    """ Perform 'set' operation. """
    if find_repository(os.getcwd(), user) != path:
        print 'Moving to', path
        os.chdir(path)
    os.environ['OPENMDAO_REPO'] = path
    os.environ['PATH'] = os.path.join(path, 'buildout', 'bin') \
                       + os.pathsep + os.path.join(path, 'scripts') \
                       + os.pathsep + os.environ['PATH']
    if sys.platform == 'win32':
        sys.exit(subprocess.call(os.environ['ComSpec']))
    else:
        sys.exit(subprocess.call(os.environ['SHELL']))

def do_fix(repo_path, options):
    """ Check/fix permissions and remove generated directories. """
    directories = (
        'buildout/bin',
        'buildout/develop-eggs',
        'buildout/eggs',
        'buildout/html',
        'buildout/parts',
        'docs/_build',
    )
    files = (
        'examples/openmdao.examples.bar3simulation/openmdao/examples/bar3simulation/bar3-f2pywrappers.f',
        'examples/openmdao.examples.bar3simulation/openmdao/examples/bar3simulation/bar3module.c'
    )
    for relpath in directories:
        if sys.platform == 'win32':
            relpath.replace('/', '\\')
        directory = os.path.join(repo_path, relpath)
        if os.path.exists(directory):
            shutil.rmtree(directory)
    for relpath in files:
        if sys.platform == 'win32':
            relpath.replace('/', '\\')
        filename = os.path.join(repo_path, relpath)
        if os.path.exists(filename):
            os.remove(filename)

    for dirpath, dirnames, filenames in os.walk(repo_path):
        if options.verbose:
            print dirpath[len(repo_path):]

        names = dirnames
        names.extend(filenames)
        for name in names:
            path = os.path.join(dirpath, name)
            info = os.stat(path)
            mode = info.st_mode

            fixup = mode
            if (mode & stat.S_IRUSR) and not (mode & stat.S_IRGRP):
                fixup |= stat.S_IRGRP
            if (mode & stat.S_IWUSR) and not (mode & stat.S_IWGRP):
                fixup |= stat.S_IWGRP
            if (mode & stat.S_IXUSR) and not (mode & stat.S_IXGRP):
                fixup |= stat.S_IXGRP

            if options.verbose:
                if fixup != mode:
                    print '   fixing %s %s' % (permission_bits(mode), name)
                else:
                    print '   %s %s' % (permission_bits(mode), name)
            elif fixup != mode:
                print 'fixing %s %s' % (permission_bits(mode), path)

            if fixup != mode:
                try:
                    os.chmod(path, fixup)
                except OSError, exc:
                    print '    %s' % exc
                    print '    (owner %s)' % get_username(info.st_uid)

def do_rmpyc(repo_path):
    """ Remove 'orphan' .pyc files. """
    for dirpath, dirnames, filenames in os.walk(repo_path):
        for name in filenames:
            if not name.endswith('.pyc'):
                continue
            path = os.path.join(dirpath, name)
            if not os.path.exists(path[:-1]):
                print 'removing', path
                os.remove(path)


def permission_bits(mode):
    """ Format permission bits in UNIX 'ls' style. """
    bits = ''

    if mode & stat.S_IRUSR:
        bits += 'r'
    else:
        bits += '-'
    if mode & stat.S_IWUSR:
        bits += 'w'
    else:
        bits += '-'
    if mode & stat.S_IXUSR:
        bits += 'x'
    else:
        bits += '-'

    if mode & stat.S_IRGRP:
        bits += 'r'
    else:
        bits += '-'
    if mode & stat.S_IWGRP:
        bits += 'w'
    else:
        bits += '-'
    if mode & stat.S_IXGRP:
        bits += 'x'
    else:
        bits += '-'

    if mode & stat.S_IROTH:
        bits += 'r'
    else:
        bits += '-'
    if mode & stat.S_IWOTH:
        bits += 'w'
    else:
        bits += '-'
    if mode & stat.S_IXOTH:
        bits += 'x'
    else:
        bits += '-'
    
    return bits

def find_repository(repository, user):
    """ Return repository's root directory path, or None. """
    user_base = os.path.join(os.sep, 'OpenMDAO', 'dev', user)
    shared_base = os.path.join(os.sep, 'OpenMDAO', 'dev', 'shared')

    if not repository:
        path = find_bzr()
        if path and platform.node() == 'torpedo.grc.nasa.gov' and \
           not path.startswith('/OpenMDAO'):
            # On OpenMDAO home use default search if not an OpenMDAO repository.
            path = ''
        if not path:
            # Use default if this user only has one.
            paths = glob.glob(os.path.join(user_base, '*'))
            if len(paths) == 1:
                repository = paths[0]
            elif len(paths) == 0:
                repository = 'working_main'  # Default shared if no user repo.
            else:
                print 'Default repository is ambiguous:'
                for path in paths:
                    print '   ', path
                sys.exit(1)
    path = find_bzr(repository)
    if not path:
        path = os.path.join(user_base, repository)
        path = find_bzr(path)
    if not path:
        path = os.path.join(shared_base, repository)
        path = find_bzr(path)
    return path

def find_bzr(path=None):
    """ Return bzr root directory path, or None. """
    if not path:
        path = os.getcwd()
    if not os.path.exists(path):
        return None
    while path:
        if os.path.exists(os.path.join(path, '.bzr')) or \
           os.path.exists(os.path.join(path, '.bzrignore')):
            return os.path.abspath(path)
        else:
            pth = path
            path = os.path.dirname(path)
            if path == pth:
                return None
    return None

def check_lockfile(path):
    """ Return (user, modification time) of lockfile, or (None, None). """
    path = os.path.join(path, LOCKFILE)
    if os.path.exists(path):
        try:
            info = os.stat(path)
        except OSError, exc:
            print 'Cannot access lockfile:', exc
            sys.exit(1)
        else:
            user = get_username(info.st_uid)
            mtime = time.asctime(time.localtime(info.st_mtime))
            return (user, mtime)
    else:
        return (None, None)

def create_lockfile(path):
    """ Create lockfile. """
    path = os.path.join(path, LOCKFILE)
    try:
        os.open(path, os.O_CREAT|os.O_EXCL|os.O_WRONLY, 0660)
    except OSError, exc:
        print 'Cannot create lockfile:', exc
        sys.exit(1)

def remove_lockfile(path):
    """ Remove lockfile. """
    path = os.path.join(path, LOCKFILE)
    try:
        os.unlink(path)
    except OSError, exc:
        print 'Cannot remove lockfile:', exc
        sys.exit(1)


def get_username(uid=None):
    """ Return username for `uid`, or current username if `uid` is None. """
    if uid:
        if sys.platform == 'win32':
            return 'unknown-%s' % uid
        else:
            return pwd.getpwuid(uid).pw_name
    else:
        if sys.platform == 'win32':
            return os.environ['USERNAME']
        else:
            return pwd.getpwuid(os.getuid()).pw_name


if __name__ == '__main__':
    main()

