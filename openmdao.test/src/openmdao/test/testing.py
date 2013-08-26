import sys
import os

import ConfigParser

import nose
from nose.plugins.base import Plugin
from pkg_resources import working_set, to_filename

import atexit


class TestFailureSummary(Plugin):
    """This plugin lists the names of the failed tests. Run nose
    with the option ``--with-fail-summary`` to activate it.
    """

    name = 'fail-summary'
    score = 1

    def options(self, parser, env):
        """Sets additional command line options."""
        super(TestFailureSummary, self).options(parser, env)

    def configure(self, options, config):
        """Configures the plugin."""
        super(TestFailureSummary, self).configure(options, config)
        self.config = config
        self._failed_tests = []
        self._error_tests = []

    def startTest(self, test):
        pass

    def report(self, stream):
        """Report the test failures."""
        if not self.enabled:
            return
        if self._failed_tests:
            stream.writeln("The following tests had failures:")
            for test in self._failed_tests:
                stream.writeln(test)
        if self._error_tests:
            stream.writeln("\nThe following tests had errors:")
            for test in self._error_tests:
                stream.writeln(test)

    def addError(self, test, err, capt=None):
        self._error_tests.append(test.id())

    def addFailure(self, test, err, capt=None, tb_info=None):
        self._failed_tests.append(test.id())

    def addSuccess(self, test, capt=None):
        pass


# Code based on Python 2.7 atexit.py
def _run_exitfuncs():
    """
    Run any registered exit functions.
    _exithandlers is traversed in reverse order so functions are executed
    last in, first out.
    """
    pid = os.getpid()
    print >> sys.__stderr__, '\n[%s] run_exitfuncs %s' \
          % (pid, len(atexit._exithandlers))
    sys.__stderr__.flush()
    exc_info = None
    while atexit._exithandlers:
        func, targs, kargs = atexit._exithandlers.pop()
        print >> sys.__stderr__, '[%s]    %s %s %s' \
              % (pid, func, targs, kargs)
        print >> sys.__stderr__, '[%s]    %s %s' \
              % (pid, func.func_code.co_filename, func.func_code.co_firstlineno)
        sys.__stderr__.flush()
        try:
            func(*targs, **kargs)
        except SystemExit:
            print >> sys.__stderr__, '[%s]    SystemExit' % pid
            sys.__stderr__.flush()
            exc_info = sys.exc_info()
        except:
            print >> sys.__stderr__, '[%s]    exception' % pid
            sys.__stderr__.flush()
            import traceback
            print >> sys.__stderr__, '[%s] Error in atexit._run_exitfuncs: %s' \
                  % (pid, traceback.format_exc())
            exc_info = sys.exc_info()
        print >> sys.__stderr__, '[%s]    handler done, nleft %s' \
              % (pid, len(atexit._exithandlers))
        sys.__stderr__.flush()

    print >> sys.__stderr__, '[%s] run_exitfuncs done, exc_info %s' \
          % (pid, exc_info)
    sys.__stderr__.flush()
    if exc_info is not None:
        raise exc_info[0], exc_info[1], exc_info[2]

def _trace_atexit():
    """
    This code can be used to display atexit handlers as they are executed during
    Python shutdown.
    """
    print >> sys.__stderr__, '\n[%s] _trace_atexit' % os.getpid()
    sys.__stderr__.flush()
    atexit._run_exitfuncs = _run_exitfuncs
    sys.exitfunc = _run_exitfuncs


def _get_openmdao_packages():
    # pkg_resources uses a 'safe' name for dists, which replaces all 'illegal' chars with '-'
    # '_' is an illegal char used in one of our packages
    return [to_filename(d.project_name) for d in working_set 
            if d.project_name.startswith('openmdao.')]

def read_config(options):
    """Reads the config file specified in options.cfg.
    
    Returns a tuple of the form (hosts, config), where `hosts` is the list of
    host names and `config` is the ConfigParser object for the config file.
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


def run_openmdao_suite_deprecated():
    try:
        run_openmdao_suite()
    finally:
        print '\n***'
        print "'openmdao_test' is deprecated and will be removed in a later release."
        print "Please use 'openmdao test' instead"
        print '***'
        
def is_dev_install():
    return (os.path.basename(os.path.dirname(os.path.dirname(sys.executable))) == "devenv")

def run_openmdao_suite(argv=None):
    """This function is exported as a script that is runnable as part of
    an OpenMDAO virtual environment as openmdao test.
    
    This function wraps nosetests, so any valid nose args should also
    work here.
    """
    if argv is None:
        argv = sys.argv

    #Add any default packages/directories to search for tests to tlist.
    tlist = _get_openmdao_packages()
    
    break_check = ['--help', '-h', '--all']
    
    covpkg = False # if True, --cover-package was specified by the user
    
    # check for args not starting with '-'
    args = argv[:]
    for i, arg in enumerate(args):
        if arg.startswith('--cover-package'):
            covpkg = True
        if (i>0 and not arg.startswith('-')) or arg in break_check:
            break
    else:  # no non '-' args, so assume they want to run the default test suite
        # in a release install, default is the set of tests specified in release_tests.cfg
        if not is_dev_install() or '--small' in args:
            if '--small' in args:
                args.remove('--small')
            args.extend(['-c', os.path.join(os.path.dirname(__file__), 'release_tests.cfg')])
        else: # in a dev install, default is all tests
            args.append('--all') 
        
    args.append('--exe') # by default, nose will skip any .py files that are
                         # executable. --exe prevents this behavior
    
    # Clobber cached data in case Python environment has changed.
    base = os.path.expanduser(os.path.join('~', '.openmdao'))
    for name in ('eggsaver.dat', 'fileanalyzer.dat'):
        path = os.path.join(base, name)
        if os.path.exists(path):
            os.remove(path)

    # Avoid having any user-defined resources causing problems during testing.
    os.environ['OPENMDAO_RAMFILE'] = ''

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
    
    if '--plugins' in args:
        args.remove('--plugins')
        from openmdao.main.plugin import plugin_install, _get_plugin_parser
        argv = ['install', '--all']
        parser = _get_plugin_parser()
        options, argz = parser.parse_known_args(argv) 
        plugin_install(parser, options, argz)

    # The default action should be to run the GUI functional tests.
    # The 'win32' test here is to allow easily changing the default for Windows.
    if sys.platform == 'win32':
        do_gui_tests = True
    else:
        do_gui_tests = True

    # run GUI functional tests, overriding default action
    if '--gui' in args:
        args.remove('--gui')
        do_gui_tests = True

    # skip GUI functional tests, overriding default action
    if '--skip-gui' in args:
        args.remove('--skip-gui')
        do_gui_tests = False

    if not do_gui_tests:
        os.environ['OPENMDAO_SKIP_GUI'] = '1'

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
    
#    _trace_atexit()
    nose.run_exit(argv=args)

if __name__ == '__main__':
    run_openmdao_suite()
