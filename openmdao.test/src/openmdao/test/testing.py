import sys
import os

import nose


oldexit = sys.exit

def dumexit(ret=0):
    print '  --all                 Run all OpenMDAO related unit tests'
    sys.exit = oldexit
    sys.exit(ret)
    
def run_openmdao_suite(*pargs, **kwargs):
    """This function is exported as a script that is runnable from the buildout
    as bin/test.  Add any directories to search for tests to tlist.
    """
    
    args = sys.argv[:]
        
    tlist = ['openmdao']
    
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

    # this tells it to enable the console in the environment so that
    # the logger will print output to stdout. This helps greatly when 
    # debugging openmdao scripts running in separate processes.
    if '--enable_console' in args:
        args.remove('--enable_console')
        os.environ['OPENMDAO_ENABLE_CONSOLE'] = 'TRUE'
        
    if '--all' in args:
        args.remove('--all')
        args.extend(tlist)
        args.extend(pargs)
        nose.run_exit(argv=args)
    elif '--help' in args or '-h' in args:
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
