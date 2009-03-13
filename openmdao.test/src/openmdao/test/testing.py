
import nose
import sys

def run_openmdao_suite():
    """This function is exported as a script that is runnable from the buildout
    as bin/test.  Add any directories to search for tests to tlist.
    """
    
    tlist = ['openmdao', '../eggsrc/npsscomponent']
    
    if 'all' in sys.argv:
        sys.argv.remove('all')
        nose.run_exit(argv=sys.argv+tlist)
    else:
        nose.run_exit()


if __name__ == '__main__':
    run_openmdao_suite()
