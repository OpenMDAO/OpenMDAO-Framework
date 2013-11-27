import os
import time
import traceback

from nose.plugins import Plugin

class TestInfo(object):
    def __init__(self, name):
        self.start = time.time()
        self.end = None
        self.name = name
        self.status = None

    def end(self):
        self.elapsed = time-time() - self.start

class BroadcastStream:
    """Sends stream output to multiple streams."""
    def __init__(self, *streams):
        self._streams = streams

    def write(self, *arg):
        for s in self._streams:
            s.write(*arg)

    def writeln(self, *arg):
        for s in self._streams:
            s.writeln(*arg)


class EarlyTestInfo(Plugin):
    """This plugin writes test info (failures, run time) to
    a file as the tests are running instead of waiting until
    the end to report anything. Run nose
    with the option ``--early-report`` to activate it.
    """

    name = 'early-report'
    score = 100

    def options(self, parser, env):
        """Sets additional command line options."""
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
        self._report_path = os.path.abspath(options.report)

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

    def formatErr(self, err):
        exctype, value, tb = err
        return ''.join(traceback.format_exception(exctype, value, tb))
    
    def addError(self, test, err, capt=None):
        self._error_tests.append(test.id())

    def addFailure(self, test, err, capt=None, tb_info=None):
        self._failed_tests.append(test.id())

    def addSuccess(self, test, capt=None):
        pass

    def finalize(self, result):
        self.html.append('<div>')
        self.html.append("Ran %d test%s" %
                         (result.testsRun, result.testsRun != 1 and "s" or ""))
        self.html.append('</div>')
        self.html.append('<div>')
        if not result.wasSuccessful():
            self.html.extend(['<span>FAILED ( ',
                              'failures=%d ' % len(result.failures),
                              'errors=%d' % len(result.errors),
                              ')</span>'])                             
        else:
            self.html.append('OK')
        self.html.append('</div></body></html>')
        # print >> sys.stderr, self.html
        for l in self.html:
            self.stream.writeln(l)

    def setOutputStream(self, stream):
        # grab for own use
        self.stream = stream        
        # return dummy stream
        class dummy:
            def write(self, *arg):
                pass
            def writeln(self, *arg):
                pass
        d = dummy()
        return d

    def startContext(self, ctx):
        try:
            n = ctx.__name__
        except AttributeError:
            return
        # TODO: possibly add indenting here per module and TestCase

    def stopContext(self, ctx):
        pass
    
    def startTest(self, test):
        self.html.extend([ '<div><span>',
                           test.shortDescription() or str(test),
                           '</span>' ])
        
    def stopTest(self, test):
        self.html.append('</div>')
