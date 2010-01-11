"""If you have Ned Batchelder's coverage_ module installed, you may activate a
coverage report with the ``--with-coverage2`` switch or NOSE_WITH_COVERAGE2
environment variable. The coverage report will cover any python source module
imported after the start of the test run, excluding modules that match
testMatch. If you want to include those modules too, use the ``--cover2-tests``
switch, or set the NOSE_COVER2_TESTS environment variable to a true value. To
restrict the coverage report to modules from a particular package or packages,
use the ``--cover2-packages`` switch or the NOSE_COVER2_PACKAGES environment
variable.

.. _coverage: http://www.nedbatchelder.com/code/modules/coverage.html

This is a modified version of the coverage plugin that comes with nose.
This version uses the builtin html report generation feature from the
coverage package and also allows access to the cobertura output feature
which is also built in to the coverage package.
"""
import logging
import os
import sys
from nose.plugins.base import Plugin
from nose.util import src, tolist

log = logging.getLogger('nose.plugins.'+__name__)

class Coverage2(Plugin):
    """
    Activate a coverage report using Ned Batchelder's coverage module.
    """
    coverTests = False
    coverPackages = None
    score = 190
    status = {}

    def options(self, parser, env):
        """
        Add options to command line.
        """
        Plugin.options(self, parser, env)
        parser.add_option("--cover2-package", action="append",
                          default=env.get('NOSE_COVER2_PACKAGE'),
                          metavar="PACKAGE",
                          dest="cover2_packages",
                          help="Restrict coverage output to selected packages "
                          "[NOSE_COVER2_PACKAGE]")
        parser.add_option("--cover2-erase", action="store_true",
                          default=env.get('NOSE_COVER2_ERASE'),
                          dest="cover2_erase",
                          help="Erase previously collected coverage "
                          "statistics before run")
        parser.add_option("--cover2-tests", action="store_true",
                          dest="cover2_tests",
                          default=env.get('NOSE_COVER2_TESTS'),
                          help="Include test modules in coverage report "
                          "[NOSE_COVER2_TESTS]")
        parser.add_option("--cover2-inclusive", action="store_true",
                          dest="cover2_inclusive",
                          default=env.get('NOSE_COVER2_INCLUSIVE'),
                          help="Include all python files under working "
                          "directory in coverage report.  Useful for "
                          "discovering holes in test coverage if not all "
                          "files are imported by the test suite. "
                          "[NOSE_COVER2_INCLUSIVE]")
        parser.add_option("--cover2-html", action="store_true",
                          default=env.get('NOSE_COVER2_HTML'),
                          dest='cover2_html',
                          help="Produce HTML coverage information")
        parser.add_option('--cover2-html-dir', action='store',
                          default=env.get('NOSE_COVER2_HTML_DIR', 'cover_html'),
                          dest='cover2_html_dir',
                          metavar='DIR',
                          help='Produce HTML coverage information in dir')
        parser.add_option('--cover2-cobertura', action='store_true',
                          default=env.get('NOSE_COVER2_COBERTURA'),
                          dest='cover2_cobertura',
                          help='Produce cobertura compatible output')

    def configure(self, options, config):
        """
        Configure plugin.
        """
        try:
            self.status.pop('active')
        except KeyError:
            pass
        Plugin.configure(self, options, config)
        try:
            if config.worker:
                return
        except AttributeError:
            pass
        if self.enabled:
            try:
                import coverage
            except ImportError:
                log.error("Coverage not available: "
                          "unable to import coverage module")
                self.enabled = False
                return
        self.conf = config
        self.coverErase = options.cover2_erase
        self.coverTests = options.cover2_tests
        self.coverPackages = []
        if options.cover2_packages:
            for pkgs in [tolist(x) for x in options.cover2_packages]:
                self.coverPackages.extend(pkgs)
        self.coverInclusive = options.cover2_inclusive
        if self.coverPackages:
            log.debug("Coverage report will include only packages: %s",
                     self.coverPackages)
        self.coverHtmlDir = None
        if options.cover2_html:
            self.coverHtmlDir = options.cover2_html_dir
            log.debug('Will put HTML coverage report in %s', self.coverHtmlDir)
        if self.enabled:
            self.status['active'] = True
        if options.cover2_cobertura:
            self.coverCobertura = True
        else:
            self.coverCobertura = False

    def begin(self):
        """
        Begin recording coverage information.
        """
        log.debug("Coverage2 begin")
        import coverage
        self.skipModules = sys.modules.keys()[:]
        self.coverage = coverage.coverage()
        if self.coverErase:
            log.debug("Clearing previously collected coverage statistics")
            self.coverage.erase()
        self.coverage.exclude('#pragma[: ]+[nN][oO] [cC][oO][vV][eE][rR]')
        self.coverage.start()

    def report(self, stream):
        """
        Output code coverage report.
        """
        log.debug("Coverage2 report")
        import coverage
        self.coverage.stop()
        modules = [ module
                    for name, module in sys.modules.items()
                    if self.wantModuleCoverage(name, module) ]
        log.debug("Coverage report will cover modules: %s", modules)
        if self.coverHtmlDir or self.coverCobertura:
            files = {}
            for m in modules:
                if hasattr(m, '__name__') and hasattr(m, '__file__'):
                    if m.__file__.endswith('pyc'):
                        files[m.__name__] = m.__file__[:-1]
                    else:
                        files[m.__name__] = m.__file__
            if self.coverHtmlDir:
                if not os.path.exists(self.coverHtmlDir):
                    os.makedirs(self.coverHtmlDir)
                log.debug("Generating HTML coverage report")
                self.coverage.html_report(files.values(), 
                                         directory=self.coverHtmlDir)
            if self.coverCobertura:
                log.debug("Generating Cobertura coverage report")
                self.coverage.xml_report(files.values(), 
                                         outfile='cobertura.xml')
        else:
            self.coverage.report(modules, file=stream)
        
        

    def wantModuleCoverage(self, name, module):
        if not hasattr(module, '__file__'):
            log.debug("no coverage of %s: no __file__", name)
            return False
        module_file = src(module.__file__)
        if not module_file or not module_file.endswith('.py'):
            log.debug("no coverage of %s: not a python file", name)
            return False
        if self.coverPackages:
            for package in self.coverPackages:
                if (name.startswith(package)
                    and (self.coverTests
                         or not self.conf.testMatch.search(name))):
                    log.debug("coverage for %s", name)
                    return True
        if name in self.skipModules:
            log.debug("no coverage for %s: loaded before coverage start",
                      name)
            return False
        if self.conf.testMatch.search(name) and not self.coverTests:
            log.debug("no coverage for %s: is a test", name)
            return False
        # accept any package that passed the previous tests, unless
        # coverPackages is on -- in that case, if we wanted this
        # module, we would have already returned True
        return not self.coverPackages

    def wantFile(self, file, package=None):
        """If inclusive coverage enabled, return true for all source files
        in wanted packages.
        """
        if self.coverInclusive:
            if file.endswith(".py"):
                if package and self.coverPackages:
                    for want in self.coverPackages:
                        if package.startswith(want):
                            return True
                else:
                    return True
        return None
