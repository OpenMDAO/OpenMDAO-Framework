#!/usr/bin/env python
"""
Record lock/unlock state of a repository.
Useful to keep multiple developers from stepping on each other,
but requires some discipline :-(
"""

import optparse
import os
import os.path
import pwd
import subprocess
import sys
import time

LOCKFILE = 'repo_lock'

def main():
    """ Repository lockfile maintenance. """

    usage = '%prog OP [options] [repository], where OP may be:\n' \
            '   check  -- check for lock\n' \
            '   lock   -- lock repository\n' \
            '   unlock -- unlock repository\n' \
            '   set    -- set this as current repository'

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
    if options.verbose:
        print 'Repository root:', path

    if operation == 'check':
        do_check(path)
    elif operation == 'lock':
        do_lock(path)
    elif operation == 'unlock':
        do_unlock(path, options)
    elif operation == 'set':
        do_set(path, this_user, options)
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

def do_set(path, user, options):
    """ Perform 'set' operation. """
    if find_repository(os.getcwd(), user) != path:
        if options.verbose:
            print 'Moving to repository root.'
        os.chdir(path)
    bin = os.path.join(path, 'buildout', 'bin')
    os.environ['PATH'] = bin+os.pathsep+os.environ['PATH']
    process = subprocess.Popen(os.environ['SHELL'])
    os.waitpid(process.pid, 0)
    sys.exit(process.returncode)


def find_repository(repository, user):
    """ Return repository's root directory path, or None. """
    path = find_bzr(repository)
    if not path:
        path = os.path.join(os.sep, 'OpenMDAO', 'dev', user, repository)
        path = find_bzr(path)
    if not path:
        path = os.path.join(os.sep, 'OpenMDAO', 'dev', 'shared', repository)
        path = find_bzr(path)
    if not path:
        print 'Cannot find repository!'
        sys.exit(2)
    return path

def find_bzr(path):
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
            stat = os.stat(path)
        except OSError, exc:
            print 'Cannot access lockfile:', exc
            sys.exit(1)
        else:
            user = pwd.getpwuid(stat.st_uid).pw_name
            mtime = time.asctime(time.localtime(stat.st_mtime))
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

