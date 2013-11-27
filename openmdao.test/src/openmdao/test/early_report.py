import os
import time

import nose
from nose.plugins.base import Plugin

class TestInfo(object):
    def __init__(self, name):
        self.start = time.time()
        self.end = None
        self.name = name
        self.status = None

class EarlyTestInfo(Plugin):
    """This plugin writes test info (failures, run time) to
    a file as the tests are running instead of waiting until
    the end to report anything. Run nose
    with the option ``--early-report`` to activate it.
    """

    name = 'early-report'
    score = 1

    def options(self, parser, env):
        """Sets additional command line options."""
        parser.add_option(
            '--append',
            action='store_true',
            dest='append',
            help="set to append to report file instead of overwite it")
        parser.add_option("--report", action="store", type="string",
                          dest="report",
                          default="test_report.out",
                          help="name of report file. (defaults to 'test_report.out')")
        super(EarlyTestInfo, self).options(parser, env)

    def configure(self, options, config):
        """Configures the plugin."""
        super(EarlyTestInfo, self).configure(options, config)
        self.config = config
        self._all_tests = []
        self._failed_tests = []
        self._error_tests = []
        if options.append:
            self._append = True 
        else:
            self._append = False
        self._report_path = os.path.abspath(options.report)

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

