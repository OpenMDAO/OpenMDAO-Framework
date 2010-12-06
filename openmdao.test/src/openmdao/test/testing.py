import sys
import os

import nose

def run_openmdao_suite():
    """This function is exported as a script that is runnable as part of
    an OpenMDAO virtual environment as openmdao_test.
    
    This function wraps nosetests, so any valid nose args should also
    work here.
    """
    
    #Add any default packages/directories to search for tests to tlist.
    tlist = ['openmdao']
    
    break_check = ['--help', '-h', '--all']
    
    covpkg = False # if True, --cover-package was specified by the user
    
    # check for args not starting with '-'
    args = sys.argv
    for i, arg in enumerate(args):
        if arg.startswith('--cover-package'):
            covpkg = True
        if (i>0 and not arg.startswith('-')) or arg in break_check:
            break
    else:  # no non '-' args, so assume they want to run the whole test suite
        args.append('--all')
        
    args.append('--exe') # by default, nose will skip any .py files that are
                         # executable. --exe prevents this behavior
    
    if '--with-coverage' in args:
        args.append('--cover-erase')
        if '--all' in args and not covpkg:
            for pkg in tlist:
                opt = '--cover-package=%s' % pkg
                if opt not in args:
                    args.append(opt)

            # Better coverage if we clobber cached data.
            base = os.path.expanduser(os.path.join('~', '.openmdao'))
            for name in ('eggsaver.dat', 'keys'):
                path = os.path.join(base, name)
                if os.path.exists(path):
                    os.remove(path)

    # this tells it to enable the console in the environment so that
    # the logger will print output to stdout. This helps greatly when 
    # debugging openmdao scripts running in separate processes.
    if '--enable_console' in args:
        args.remove('--enable_console')
        os.environ['OPENMDAO_ENABLE_CONSOLE'] = 'TRUE'
        
    if '--all' in args:
        args.remove('--all')
        args.extend(tlist)
        
    nose.run_exit(argv=args)


if __name__ == '__main__':
    run_openmdao_suite()
