import os
import time
import traceback

from nose.plugins import Plugin
from nose import SkipTest

class TestInfo(object):
    def __init__(self, test):
        self.start = time.time()
        try:
            ctxt = test.context.__name__
        except:
            ctxt = None
        self.name = test.shortDescription()
        if self.name is None:
            self.name = test.id()
        self.status = None
        self.elapsed = 0.

    def end(self):
        self.elapsed = time.time() - self.start


class BroadcastStream(object):
    """Sends stream output to multiple streams."""
    def __init__(self, *streams):
        self._streams = streams[:]

    def write(self, *arg):
        for s in self._streams:
            s.write(*arg)

    def writeln(self, *arg):
        for s in self._streams:
            for a in arg:
                s.write(a)
            s.write('\n')

    def close(self):
        for s in self._streams:
            try:
                s.close()
            except:
                pass
            
    def flush(self):
        for s in self._streams:
            try:
                s.flush()
            except:
                pass
            

class EarlyTestInfo(Plugin):
    """This plugin writes test info (failures, run time) to
    a file as the tests are running instead of waiting until
    the end to report anything. Run nose
    with the option ``--early-report`` to activate it.
    """

    name = 'early-report'
    score = 1001 # need high score to get called before ErroClassPlugin,
                 # otherwise we lose Skips

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
        self._tests = {}
        self._failed_tests = []
        self._error_tests = []
        self._report_path = os.path.abspath(options.report)

    def report(self, stream):
        """Report the test failures."""
        if not self.enabled:
            return

        failed = [k for k,v in self._tests.items() if v.status=='F']
        errors = [k for k,v in self._tests.items() if v.status=='E']
        skips  = [k for k,v in self._tests.items() if v.status=='S']

        self.stream.writeln("\n\n")
        
        if skips:
            stream.writeln("The following tests were skipped")
            for test in skips:
                stream.writeln(test)
        if failed:
            stream.writeln("\nThe following tests had failures:")
            for test in failed:
                stream.writeln(test)
        if errors:
            stream.writeln("\nThe following tests had errors:")
            for test in errors:
                stream.writeln(test)
                
        self.stream.writeln("\n")

    def formatErr(self, err):
        exctype, value, tb = err
        return ''.join(traceback.format_exception(exctype, value, tb))
    
    def _show_test(self, tinfo):
        tinfo.end()
        self.stream.write(tinfo.name)
        self.stream.write(' (%.3g sec) ... ' % tinfo.elapsed)
        if tinfo.status is None:
            self.stream.writeln('OK')
        elif tinfo.status == 'F':
            self.stream.writeln('FAIL')
        elif tinfo.status == 'E':
            self.stream.writeln('ERROR')
        elif tinfo.status == 'S':
            self.stream.writeln('SKIP')

    def addError(self, test, err, capt=None):
        if err[0] == SkipTest:
            self._tests[test.id()].status = 'S'
        else:
            self._tests[test.id()].status = 'E'
        self._show_test(self._tests[test.id()])
        self.stream.writeln(self.formatErr(err))

    def addFailure(self, test, err, capt=None, tb_info=None):
        if err[0] == SkipTest:
            self._tests[test.id()].status = 'S'
        else:
            self._tests[test.id()].status = 'F'
        self._show_test(self._tests[test.id()])
        self.stream.writeln(self.formatErr(err))

    def addSuccess(self, test, capt=None):
        self._show_test(self._tests[test.id()])
        
    def addSkip(self, test, *args, **kwargs):
        self._show_test(self._tests[test.id()])

    def finalize(self, result):
        self.stream.writeln("\n\nRan %d test%s\n" %
                         (result.testsRun, result.testsRun != 1 and "s" or ""))

        skips  = [k for k,v in self._tests.items() if v.status=='S']

        if result.wasSuccessful():
            self.stream.write("OK")
        else:
            self.stream.write('FAILED ( failures=%d ' % len(result.failures))
            self.stream.write('errors=%d )' % len(result.errors))

        if skips:
            self.stream.write('  skipped=%d ' % len(skips))
                        
        self.report(self.stream)

    def setOutputStream(self, stream):
        outfile = open(self._report_path, 'w')

        # grab for own use
        self.stream = BroadcastStream(stream, outfile)
        
        # return dummy stream so normal output isn't displayed
        class dummy:
            def write(self, *arg):
                pass
            def writeln(self, *arg):
                pass
            def flush(self):
                pass
        d = dummy()
        return d

    def startContext(self, ctx):
        try:
            self._context = ctx.__name__
        except AttributeError:
            return
        # TODO: possibly add indenting here per module and TestCase

    def stopContext(self, ctx):
        self._context = None
    
    def startTest(self, test):
        self._tests[test.id()] = TestInfo(test)
        
    def stopTest(self, test):
        pass
