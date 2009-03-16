
import nose
import sys


oldexit = sys.exit

def dumexit(ret=0):
    print '  --all                 Run all OpenMDAO related unit tests'
    sys.exit = oldexit
    sys.exit(ret)
    
def run_openmdao_suite():
    """This function is exported as a script that is runnable from the buildout
    as bin/test.  Add any directories to search for tests to tlist.
    """
    
    tlist = ['openmdao', '../eggsrc/npsscomponent']
    
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
