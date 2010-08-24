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
    
    # check for args not starting with '-'
    args = sys.argv
    for i, arg in enumerate(args):
        if (i>0 and not arg.startswith('-')) or arg in break_check:
            break
    else:  # no non '-' args, so assume they want to run the whole test suite
        args.append('--all')
        
    args.append('--exe') # by default, nose will skip any .py files that are
                         # executable. --exe prevents this behavior
    
    # In case --with-coverage2 is used, default these options in.
    if '--with-coverage2' in args:
        args.append('--cover2-erase')
        if '--all' in args:
            for pkg in tlist:
                opt = '--cover2-package=%s' % pkg
                if opt not in args:
                    args.append(opt)

        if '--cover2-html' in args:
            for arg in args:
                if arg.startswith('--cover2-html-dir='):
                    break
            else:
                args.append('--cover2-html-dir=html_coverage')

    if '--with-coverage' in args:
        args.append('--cover-erase')
        if '--all' in args:
            for pkg in tlist:
                opt = '--cover-package=%s' % pkg
                if opt not in args:
                    args.append(opt)

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
