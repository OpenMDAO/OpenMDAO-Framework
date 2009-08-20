import sys
import os

import nose


oldexit = sys.exit

def dumexit(ret=0):
    print '  --all                 Run all OpenMDAO related unit tests'
    sys.exit = oldexit
    sys.exit(ret)
    
def run_openmdao_suite():
    """This function is exported as a script that is runnable from the buildout
    as bin/test.  Add any directories to search for tests to tlist.
    """
    
    tlist = ['openmdao', 'npsscomponent']
    
    # In case --with-coverage is used, default these options in.
    if '--with-coverage' in sys.argv:
        sys.argv.append('--cover-erase')
        if '--all' in sys.argv:
            for pkg in tlist:
                opt = '--cover-package=%s' % pkg
                if opt not in sys.argv:
                    sys.argv.append(opt)

        # at the moment, html annotation doesn't work through nose when
        # using version 3.0.1 of coverage...
        if '--cover-html' in sys.argv:
            for arg in sys.argv:
                if arg.startswith('--cover-html-dir='):
                    break
            else:
                sys.argv.append('--cover-html-dir=html_coverage')

    # this tells it to put enable_console calls in generated python
    # scripts so test runner can see all output for debugging purposes.
    if '--enable_console' in sys.argv:
        sys.argv.remove('--enable_console')
        os.environ['OPENMDAO_ENABLE_CONSOLE'] = 'TRUE'
        
    if '--all' in sys.argv:
        sys.argv.remove('--all')
        nose.run_exit(argv=sys.argv+tlist)
    elif '--help' in sys.argv or '-h' in sys.argv:
        # TODO: find a better way to do this, maybe by messing with optparse?
        # since nose.run() immediately exits after printing its help info,
        # we need to temporarily hijack the sys.exit function in order to 
        # get our help info for --all to show up in the right spot.
        sys.exit=dumexit
        nose.run()
    else:
        nose.run_exit()


if __name__ == '__main__':
    run_openmdao_suite()
