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
import pwd
import stat
import subprocess
import sys
import time

LOCKFILE = 'repo_lock'


def main():
    """ Repository maintenance. """

    usage = """\
%prog OP [options] repository, where OP may be:
   check  -- check for lock
   lock   -- lock repository
   unlock -- unlock repository
   set    -- set this as current repository
   fix    -- fix permissions"""

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

    this_user = pwd.getpwuid(os.getuid()).pw_name
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
    elif user == pwd.getpwuid(os.getuid()).pw_name or options.force:
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
    sys.exit(subprocess.call(os.environ['SHELL']))

def do_fix(path, options):
    """ Check/fix permissions. """
    for dirpath, dirnames, filenames in os.walk(path):
        if options.verbose:
            print dirpath[len(path):]

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
                    print '    (owner %s)' % pwd.getpwuid(info.st_uid).pw_name


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
        if not path or not path.startswith('/OpenMDAO'):
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
        if os.path.exists(os.path.join(path, '.bzr')):
            return path
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
            user = pwd.getpwuid(info.st_uid).pw_name
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


if __name__ == '__main__':
    main()

