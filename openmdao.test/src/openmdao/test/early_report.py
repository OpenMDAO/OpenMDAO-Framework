import os
import time
import traceback
from itertools import chain

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
        parser.add_option("--quicktime", action="store", type="float",
                          dest="quicktime",
                          default=1.0,
                          help="cutoff time for tests saved in quick.cfg")
        parser.add_option("--save-configs", action="store_true",
                          dest="save_configs",
                          help="set this to save passing/failing tests in passing.cfg and failing.cfg files")
        super(EarlyTestInfo, self).options(parser, env)

    def configure(self, options, config):
        """Configures the plugin."""
        super(EarlyTestInfo, self).configure(options, config)
        self.config = config
        self._tests = {}
        self._failed_tests = []
        self._error_tests = []
        self._report_path = os.path.abspath(options.report)
        self._save_configs = options.save_configs
        self._quick_time = options.quicktime

    def report(self, stream):
        """Report the test failures."""
        if not self.enabled:
            return

        failed = []
        errors = []
        skips = []
        passed = []
        quick = []

        total_time = 0.

        for v in self._tests.values():
            total_time += v.elapsed

            if v.elapsed <= self._quick_time:
                quick.append(v.name)

            if v.status == 'F':
                failed.append(v.name)
            elif v.status == 'E':
                errors.append(v.name)
            elif v.status == 'S':
                skips.append(v.name)
            else:
                passed.append(v.name)

        self.stream.writeln("\n\n")
        
        total_failed = sorted(chain(failed, errors))
        skips = sorted(skips)

        if skips:
            stream.writeln("The following tests were skipped")
            for test in skips:
                stream.writeln(test)
        if failed or errors:
            stream.writeln("\nThe following tests failed:")
            for test in total_failed:
                stream.writeln(test)

        hrs = int(total_time/3600)
        total_time -= (hrs * 3600)
        mins = int(total_time/60)
        total_time -= (mins * 60)
        stream.writeln("\n\nTotal elapsed time: %02d:%02d:%.2f" % 
                         (hrs, mins, total_time))
                
        self.stream.writeln("\n")

        if self._save_configs:
            self._save_passing(sorted(passed))
            self._save_failing(total_failed)
            self._save_quick(quick)

    def _cvt_test_path(self, tname):
        """Converts from an all dotted name to a name
        with a ':' before the TestCase name.
        """
        parts = tname.split('.')
        part1 = '.'.join(parts[:-2])
        part2 = '.'.join(parts[-2:])

        return ':'.join((part1, part2))

    def _sort_tests(self, tests):
        tlist = []
        for test in tests:
            tail = test.rsplit('.',1)[1]
            if not tail.startswith('test'):
                continue
            tlist.append(self._cvt_test_path(test))
        return sorted(tlist)
            
    def _save_passing(self, passed):
        with open("passing.cfg", "w") as f:
            f.write("\n[nosetests]\ntests=")
            for i,test in enumerate(self._sort_tests(passed)):
                if i>0:
                    f.write(",\n   ")
                f.write(test)
            f.write('\n')

    def _save_failing(self, failed):
        with open("failing.cfg", "w") as f:
            f.write("\n[nosetests]\ntests=")
            for i,test in enumerate(self._sort_tests(failed)):
                if i>0:
                    f.write(",\n   ")
                f.write(test)
            f.write('\n')

    def _save_quick(self, quick):
        with open("quick.cfg", "w") as f:
            f.write("# all tests here ran in <= %s seconds\n" % self._quick_time)
            f.write("# (if they failed at the time of recording, \n")
            f.write("#   they may take significantly longer when they pass)\n")
            f.write("\n[nosetests]\ntests=")
            for i,test in enumerate(self._sort_tests(quick)):
                if i>0:
                    f.write(",\n   ")
                f.write(test)
            f.write('\n')

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
        if id(test) not in self._tests:
            self.startTest(test)
        if err[0] == SkipTest:
            self._tests[id(test)].status = 'S'
        else:
            self._tests[id(test)].status = 'E'
        self._show_test(self._tests[id(test)])
        self.stream.writeln(self.formatErr(err))

    def addFailure(self, test, err, capt=None, tb_info=None):
        if id(test) not in self._tests:
            self.startTest(test)
        if err[0] == SkipTest:
            self._tests[id(test)].status = 'S'
        else:
            self._tests[id(test)].status = 'F'
        self._show_test(self._tests[id(test)])
        self.stream.writeln(self.formatErr(err))

    def addSuccess(self, test, capt=None):
        if id(test) not in self._tests:
            self.startTest(test)
        self._show_test(self._tests[id(test)])
        
    def addSkip(self, test, *args, **kwargs):
        if id(test) not in self._tests:
            self.startTest(test)
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
