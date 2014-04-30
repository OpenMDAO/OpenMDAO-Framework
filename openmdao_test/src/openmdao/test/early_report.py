import os
import time
import traceback

from nose.plugins import Plugin
from nose import SkipTest

class TestInfo(object):
    def __init__(self, test):
        self.start = time.time()
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
    with the option ``--with-early`` to activate it.
    """

    name = 'early'
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

        failed = []
        errors = []
        skips = []
        total_time = 0.

        for v in self._tests.values():
            total_time += v.elapsed

            if v.status == 'F':
                failed.append(v.name)
            elif v.status == 'E':
                errors.append(v.name)
            elif v.status == 'S':
                skips.append(v.name)

        self.stream.writeln("\n\n")
        
        if skips:
            stream.writeln("The following tests were skipped")
            for test in skips:
                stream.writeln(test)
        if failed or errors:
            stream.writeln("\nThe following tests failed:")
            for test in failed:
                stream.writeln(test)
            for test in errors:
                stream.writeln(test)

        hrs = int(total_time/3600)
        total_time -= (hrs * 3600)
        mins = int(total_time/60)
        total_time -= (mins * 60)
        stream.writeln("\n\nTotal elapsed time: %02d:%02d:%.2f" % 
                         (hrs, mins, total_time))
                
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
        elif tinfo.status in ['F', 'E']:
            self.stream.writeln('FAIL')
        elif tinfo.status == 'S':
            self.stream.writeln('SKIP')

    def addError(self, test, err, capt=None):
        if err[0] == SkipTest:
            if id(test) not in self._tests:
                self.startTest(test)
            self._tests[id(test)].status = 'S'
        else:
            self._tests[id(test)].status = 'E'
        self._show_test(self._tests[id(test)])
        self.stream.writeln(self.formatErr(err))

    def addFailure(self, test, err, capt=None, tb_info=None):
        if err[0] == SkipTest:
            self._tests[id(test)].status = 'S'
        else:
            self._tests[id(test)].status = 'F'
        self._show_test(self._tests[id(test)])
        self.stream.writeln(self.formatErr(err))

    def addSuccess(self, test, capt=None):
        self._show_test(self._tests[id(test)])
        
    def addSkip(self, test, *args, **kwargs):
        self._show_test(self._tests[id(test)])

    def finalize(self, result):
        self.report(self.stream)

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

        self.stream.write("\n\n")
                        

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
        self._tests[id(test)] = TestInfo(test)
        
    def stopTest(self, test):
        pass
