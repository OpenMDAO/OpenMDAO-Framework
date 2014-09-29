#!/usr/bin/env python
"""
Run pylint on tree of code.
"""

import difflib
import glob
import optparse
import os
import os.path
import sys

def main():
    """ Parse options, then process files. """
    parser = optparse.OptionParser()
    parser.add_option('--local', '-l', action='store_true',
                      help='start in local directory only')
    parser.add_option('--recurse', '-r', action='store_true',
                      help='recurse down directory tree')
    parser.add_option('--specific', '-s', action='store_true',
                      help='process specific files')
    parser.add_option('--update', '-u', action='store_true',
                      help='update saved pylint results')
    options, arguments = parser.parse_args()

    # Find location of OpenMDAO root.
    root = os.path.abspath(os.path.dirname(os.path.dirname(__file__)))
    python = os.path.join(root, 'devenv', 'bin', 'python')
    pylint = '/usr/local/lib/python2.7/site-packages/scripts/pylint'

    # If specific files requested, just process those and exit.
    if options.specific:
        for filename in arguments:
            process(python, pylint, '.', filename, options.update)
        sys.exit(0)

    if arguments:
        parser.print_help()
        sys.exit(1)

    if options.local:
        roots = (os.getcwd(),)
    else:
        roots = (
            os.path.join(root, 'openmdao.lib'),
            os.path.join(root, 'openmdao.main'),
            os.path.join(root, 'openmdao.devtools'),
            os.path.join(root, 'openmdao.test'),
            os.path.join(root, 'openmdao.util'),
            os.path.join(root, 'contrib'),
            os.path.join(root, 'examples'),
            os.path.join(root, 'scripts'),
            os.path.join(root, 'testplugins'),
        )

    # Traverse trees looking for '.py' files.
    for root in roots:
        os.chdir(root)
        if options.recurse:
            for dirpath, dirnames, filenames in os.walk('.'):
                dirs = dirpath.split(os.sep)
                if 'build' in dirs:
                    continue  # Skip setup.py build directory.
                filenames.sort()
                for filename in filenames:
                    if filename.endswith('.py'):
                        process(python, pylint, dirpath, filename, options.update)
        else:
            filenames = glob.glob('*.py')
            filenames.sort()
            for filename in filenames:
                process(python, pylint, '.', filename, options.update)
    print >> sys.stderr, '\nProcessing complete'


def process(python, pylint, dirpath, filename, update):
    """ Process one file. """
    # Using stderr since pylint sends some messages there.
    saved_dir = os.getcwd()
    path = os.path.join(os.path.basename(saved_dir), dirpath[2:], filename)
    print >> sys.stderr, '\nProcessing %s...' % path
    os.chdir(dirpath)

    args = '--files-output=y'
    # Check for directory-specific configuration file.
    if os.path.exists('pylint.rc'):
        args += ' --rcfile=pylint.rc'
    args += ' %s' % filename

    # Run pylint.
    status = os.system('%s %s %s' % (python, pylint, args))
# status unreliable since using buildout/bin/pylint.
#    if status:
#        print >> sys.stderr, '    pylint returned', status
#        os.chdir(saved_dir)
#        return

    # Print code rating.
    glbl = 'pylint_global.txt'
    if os.path.exists(glbl):
        inp = open(glbl, 'r')
        lines = inp.readlines()
        inp.close()
        print >> sys.stderr, '   ', lines[-2],
        os.remove(glbl)

    # Compare against previous output.
    directory = 'pylint'
    basename = filename[:-3]
    old_lint = os.path.join(directory, '%s.txt' % basename)
    new_lint = 'pylint_%s.txt' % basename
    diff_lines = None
    if os.path.exists(new_lint):
        if os.path.exists(old_lint):
            inp = open(old_lint, 'r')
            old_lines = inp.readlines()
            inp.close()
            inp = open(new_lint, 'r')
            new_lines = inp.readlines()
            inp.close()
            generator = difflib.context_diff(old_lines, new_lines,
                                             'old', 'new', n=0)
            diff_lines = [line for line in generator]
            if diff_lines:
                print >> sys.stderr, ''
                sys.stderr.writelines(diff_lines)
        else:
            print >> sys.stderr, '    No previous output.'
    else:
        print >> sys.stderr, '    No pylint output!'

    # If updating, update!
    if os.path.exists(new_lint):
        if update and ((diff_lines is None) or (len(diff_lines) > 0)):
            if not os.path.exists(directory):
                os.mkdir(directory)
            if os.path.exists(old_lint):
                os.remove(old_lint)
            os.rename(new_lint, old_lint)
            print >> sys.stderr, '    Updated', old_lint
        else:
            os.remove(new_lint)

    # Restore directory.
    os.chdir(saved_dir)


if __name__ == '__main__':
    main()
