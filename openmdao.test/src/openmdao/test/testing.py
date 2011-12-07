import sys
import os

import ConfigParser

import nose
from pkg_resources import working_set, to_filename

from openmdao.main.resource import ResourceAllocationManager

def _get_openmdao_packages():
    # pkg_resources uses a 'safe' name for dists, which replaces all 'illegal' chars with '-'
    # '_' is an illegal char used in one of our packages
    return [to_filename(d.project_name) for d in working_set 
            if d.project_name.startswith('openmdao.')]

def read_config(options):
    """Reads the config file specified in options.cfg.
    
    Returns a tuple of the form (hosts, config), where hosts is the list of
    host names and config is the ConfigParser object for the config file.
    """
    options.cfg = os.path.expanduser(options.cfg)
    
    config = ConfigParser.ConfigParser()
    config.readfp(open(options.cfg))
    
    hostlist = config.sections()
    
    return (hostlist, config)
    

def filter_config(hostlist, config, options):
    """Looks for sections in the config file that match the host names 
    specified in options.hosts.
    
    Returns a list of host names that match the given options.
    """
    hosts = []
    if options.hosts:
        for host in options.hosts:
            if host in hostlist:
                hosts.append(host)
            else:
                raise RuntimeError("host '%s' is not in config file %s" % 
                                   (host, options.cfg))

        if not hosts:
            raise RuntimeError("no hosts were found in config file %s" % options.cfg)
    elif options.allhosts:
        hosts = hostlist

    if options.filters:
        final_hosts = []
        for h in hosts:
            for f in options.filters:
                parts = [p.strip() for p in f.split('==') if p.strip()]
                if len(parts) == 2:
                    pass
                else:
                    raise RuntimeError("filter '%s' is invalid" % f)
                if config.has_option(h, parts[0]) and config.get(h, parts[0]) == parts[1]:
                    continue
                else:
                    break
            else:
                final_hosts.append(h)
    else:
        final_hosts = hosts
    
    return final_hosts


def run_openmdao_suite():
    """This function is exported as a script that is runnable as part of
    an OpenMDAO virtual environment as openmdao_test.
    
    This function wraps nosetests, so any valid nose args should also
    work here.
    """
    
    #Add any default packages/directories to search for tests to tlist.
    tlist = _get_openmdao_packages()
    
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
    
    # Clobber cached eggsaver data in case Python environment has changed.
    base = os.path.expanduser(os.path.join('~', '.openmdao'))
    path = os.path.join(base, 'eggsaver.dat')
    if os.path.exists(path):
        os.remove(path)

    # Avoid having any user-defined resources causing problems during testing.
    ResourceAllocationManager.configure('')

    if '--with-coverage' in args:
        args.append('--cover-erase')
        if '--all' in args and not covpkg:
            for pkg in tlist:
                opt = '--cover-package=%s' % pkg
                if opt not in args:
                    args.append(opt)

            # Better coverage if we clobber credential data.
            path = os.path.join(base, 'keys')
            if os.path.exists(path):
                os.remove(path)

    # this tells it to enable the console in the environment so that
    # the logger will print output to stdout. This helps greatly when 
    # debugging openmdao scripts running in separate processes.
    if '--enable_console' in args:
        args.remove('--enable_console')
        os.environ['OPENMDAO_ENABLE_CONSOLE'] = '1'

    if '--all' in args:
        args.remove('--all')
        args.extend(tlist)
        
    # some libs we use call multiprocessing.cpu_count() on import, which can
    # raise NotImplementedError, so try to monkeypatch it here to return 1 if
    # that's the case
    try:
        import multiprocessing
        multiprocessing.cpu_count()
    except ImportError:
        pass
    except NotImplementedError:
        multiprocessing.cpu_count = lambda: 1
    
    nose.run_exit(argv=args)

if __name__ == '__main__':
    run_openmdao_suite()
