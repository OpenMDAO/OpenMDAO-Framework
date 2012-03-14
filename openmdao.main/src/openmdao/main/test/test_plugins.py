import cStringIO
import logging
import nose
import os
import shutil
import sys
import tempfile
import unittest
from subprocess import check_call

from openmdao.main.plugin import _get_plugin_parser, plugin_quickstart, \
                                 plugin_build_docs, plugin_makedist, \
                                 plugin_list, _plugin_docs, plugin_install
from openmdao.util.fileutil import find_files
from openmdao.util.testutil import assert_raises


class PluginsTestCase(unittest.TestCase):
    def setUp(self):
        self.tdir = tempfile.mkdtemp()

    def tearDown(self):
        shutil.rmtree(self.tdir)

    def test_quickstart(self):
        orig_dir = os.getcwd()
        os.chdir(self.tdir)
        try:
            # Defaults.
            argv = ['quickstart', 'foobar']
            parser = _get_plugin_parser()
            options, args = parser.parse_known_args(argv)
            retval = plugin_quickstart(parser, options, args)
            self.assertEqual(retval, 0)
            fandd = find_files(self.tdir, nodirs=False)
            self.assertEqual(set([os.path.basename(f) for f in fandd]), 
                             set(['foobar', 'src', 'docs', 'setup.cfg', 'setup.py',
                                  'MANIFEST.in', '__init__.py', 'conf.py',
                                  'usage.rst', 'index.rst',
                                  'srcdocs.rst', 'pkgdocs.rst', 'foobar.py', 
                                  'README.txt',
                                  'test','test_foobar.py']))
        finally:
            os.chdir(orig_dir)

        shutil.rmtree(self.tdir)
        os.mkdir(self.tdir)

        # All options.
        argv = ['quickstart', 'foobar', '-c', 'FooBar', '-g', 'component',
                '-v', '1.1', '-d', self.tdir]
        parser = _get_plugin_parser()
        options, args = parser.parse_known_args(argv)
        retval = plugin_quickstart(parser, options, args)
        self.assertEqual(retval, 0)
        fandd = find_files(self.tdir, nodirs=False)
        self.assertEqual(set([os.path.basename(f) for f in fandd]), 
                         set(['foobar', 'src', 'docs', 'setup.cfg', 'setup.py',
                              'MANIFEST.in', '__init__.py', 'conf.py',
                              'usage.rst', 'index.rst',
                              'srcdocs.rst', 'pkgdocs.rst', 'foobar.py', 
                              'README.txt',
                              'test','test_foobar.py']))

        # Errors.
        code = 'plugin_quickstart(parser, options, args)'
        assert_raises(self, code, globals(), locals(), OSError,
                      "Can't create directory %r because it already exists."
                      % os.path.join(self.tdir, 'foobar'))

        argv = ['quickstart', 'foobar', 'stuff']
        parser = _get_plugin_parser()
        options, args = parser.parse_known_args(argv)
        retval = plugin_quickstart(parser, options, args)
        self.assertEqual(retval, -1)

    def test_makedist(self):
        logging.debug('')
        logging.debug('test_makedist')
        argv = ['quickstart', 'foobar', '-v', '1.1', '-d', self.tdir]
        parser = _get_plugin_parser()
        options, args = parser.parse_known_args(argv)
        plugin_quickstart(parser, options, args)
        
        startdir = os.getcwd()
        orig_stdout = sys.stdout
        orig_stderr = sys.stderr
        sys.stdout = cStringIO.StringIO()
        sys.stderr = cStringIO.StringIO()
        logdata = ''
        os.chdir(os.path.join(self.tdir, 'foobar'))
        try:
            argv = ['makedist', 'foobar']
            parser = _get_plugin_parser()
            options, args = parser.parse_known_args(argv)
            retval = plugin_makedist(parser, options, args, capture='makedist.out')
            with open('makedist.out', 'r') as inp:
                logdata = inp.read()
            self.assertEqual(retval, 0)
            self.assertTrue(os.path.exists('foobar-1.1.tar.gz'))
        finally:
            captured_stdout = sys.stdout.getvalue()
            captured_stderr = sys.stderr.getvalue()
            sys.stdout = orig_stdout
            sys.stderr = orig_stderr
            os.chdir(startdir)
            logging.debug('captured stdout:')
            logging.debug(captured_stdout)
            logging.debug('captured stderr:')
            logging.debug(captured_stderr)
            logging.debug('captured subprocess data:')
            logging.debug(logdata)

        # Errors.
        argv = ['makedist', 'foobar', 'distdir', 'stuff']
        parser = _get_plugin_parser()
        options, args = parser.parse_known_args(argv)
        retval = plugin_makedist(parser, options, args)
        self.assertEqual(retval, -1)

        argv = ['makedist', 'foobar', 'no-such-directory']
        parser = _get_plugin_parser()
        options, args = parser.parse_known_args(argv)
        code = 'plugin_makedist(parser, options, args)'
        distdir = os.path.join(os.getcwd(), 'no-such-directory')
        assert_raises(self, code, globals(), locals(), IOError,
                      'directory %r does not exist' % distdir)

        # Existing distribution.
        sys.stdout = cStringIO.StringIO()
        sys.stderr = cStringIO.StringIO()
        os.chdir(os.path.join(self.tdir, 'foobar'))
        try:
            argv = ['makedist', 'foobar']
            parser = _get_plugin_parser()
            options, args = parser.parse_known_args(argv)
            retval = plugin_makedist(parser, options, args, capture='makedist.out')
        finally:
            captured_stdout = sys.stdout.getvalue()
            captured_stderr = sys.stderr.getvalue()
            sys.stdout = orig_stdout
            sys.stderr = orig_stderr
            os.chdir(startdir)
        self.assertEqual(retval, -1)

    def test_list(self):
        orig_stdout = sys.stdout
        orig_stderr = sys.stderr
        sys.stdout = cStringIO.StringIO()
        sys.stderr = cStringIO.StringIO()
        try:
            argv = ['list']
            parser = _get_plugin_parser()
            options, args = parser.parse_known_args(argv)
            retcode = plugin_list(parser, options, args)
        finally:
            captured_stdout = sys.stdout.getvalue()
            captured_stderr = sys.stderr.getvalue()
            sys.stdout = orig_stdout
            sys.stderr = orig_stderr
        self.assertEqual(retcode, 0)
        # Just a selection from each category.
        expected = ['openmdao.lib.architectures.bliss.BLISS',
                    'openmdao.lib.casehandlers.caseset.CaseArray',
                    'openmdao.lib.components.broadcaster.Broadcaster',
                    'openmdao.lib.datatypes.array.Array',
                    'openmdao.lib.differentiators.finite_difference.FiniteDifference',
                    'openmdao.lib.doegenerators.central_composite.CentralComposite',
                    'openmdao.lib.drivers.broydensolver.BroydenSolver',
                    'openmdao.lib.surrogatemodels.kriging_surrogate.KrigingSurrogate',
                    'openmdao.main.assembly.Assembly']
        for plugin in expected:
            self.assertTrue(plugin in captured_stdout)

        sys.stdout = cStringIO.StringIO()
        sys.stderr = cStringIO.StringIO()
        try:
            argv = ['list', '-g', 'driver', '--builtin']
            parser = _get_plugin_parser()
            options, args = parser.parse_known_args(argv)
            retcode = plugin_list(parser, options, args)
        finally:
            captured_stdout = sys.stdout.getvalue()
            captured_stderr = sys.stderr.getvalue()
            sys.stdout = orig_stdout
            sys.stderr = orig_stderr
        self.assertEqual(retcode, 0)
        if 'openmdao.lib.drivers.doedriver.DOEdriver' not in captured_stdout:
            self.fail('-g driver filter failed to include')
        if 'openmdao.main.assembly.Assembly' in captured_stdout:
            self.fail('-g driver filter failed to exclude')

        sys.stdout = cStringIO.StringIO()
        sys.stderr = cStringIO.StringIO()
        try:
            argv = ['list', '-g', 'driver', '-g', 'container', '--external']
            parser = _get_plugin_parser()
            options, args = parser.parse_known_args(argv)
            retcode = plugin_list(parser, options, args)
        finally:
            captured_stdout = sys.stdout.getvalue()
            captured_stderr = sys.stderr.getvalue()
            sys.stdout = orig_stdout
            sys.stderr = orig_stderr
        self.assertEqual(retcode, 0)

        # Errors.
        argv = ['list', 'stuff']
        parser = _get_plugin_parser()
        options, args = parser.parse_known_args(argv)
        retcode = plugin_list(parser, options, args)
        self.assertEqual(retcode, -1)

    def test_build_docs(self):
        argv = ['quickstart', 'foobar', '-v', '1.1', '-d', self.tdir]
        parser = _get_plugin_parser()
        options, args = parser.parse_known_args(argv)
        plugin_quickstart(parser, options, args)

        orig_dir = os.getcwd()
        os.chdir(os.path.join(self.tdir, 'foobar'))
        orig_stdout = sys.stdout
        orig_stderr = sys.stderr
        sys.stdout = cStringIO.StringIO()
        sys.stderr = cStringIO.StringIO()
        try:
            argv = ['build_docs', 'foobar']
            parser = _get_plugin_parser()
            options, args = parser.parse_known_args(argv)
            plugin_build_docs(parser, options, args)
        finally:
            captured_stdout = sys.stdout.getvalue()
            captured_stderr = sys.stderr.getvalue()
            sys.stdout = orig_stdout
            sys.stderr = orig_stderr
            os.chdir(orig_dir)

        # Errors.
        argv = ['build_docs', 'foobar', 'no-such-directory', 'stuff']
        parser = _get_plugin_parser()
        options, args = parser.parse_known_args(argv)
        retcode = plugin_build_docs(parser, options, args)
        self.assertEqual(retcode, -1)

        argv = ['build_docs', 'foobar', 'no-such-directory']
        parser = _get_plugin_parser()
        options, args = parser.parse_known_args(argv)
        code = 'plugin_build_docs(parser, options, args)'
        distdir = os.path.join(os.getcwd(), 'no-such-directory')
        assert_raises(self, code, globals(), locals(), IOError,
                      'directory %r does not exist' % distdir)

        distdir = os.path.join(self.tdir, 'foobar')
        os.remove(os.path.join(distdir, 'setup.py'))
        argv = ['build_docs', 'foobar', distdir]
        parser = _get_plugin_parser()
        options, args = parser.parse_known_args(argv)
        code = 'plugin_build_docs(parser, options, args)'
        assert_raises(self, code, globals(), locals(), IOError,
                      "directory %r does not contain 'setup.py'" % distdir)

    def test_docs(self):
        argv = ['docs', 'no-such-plugin']
        parser = _get_plugin_parser()
        options, args = parser.parse_known_args(argv)
        code = '_plugin_docs(options.plugin_dist_name)'
        assert_raises(self, code, globals(), locals(), RuntimeError,
                      "Can't locate package/module 'no-such-plugin'")

        argv = ['docs', 'subprocess']
        parser = _get_plugin_parser()
        options, args = parser.parse_known_args(argv)
        url = _plugin_docs(options.plugin_dist_name)
        expected = os.path.join(os.path.dirname(sys.modules['subprocess'].__file__),
                                'sphinx_build', 'html', 'index.html')
        self.assertEqual(url, expected)

    def test_install(self):
        argv = ['install', 'no-such-plugin', 'stuff']
        parser = _get_plugin_parser()
        options, args = parser.parse_known_args(argv)
        retcode = plugin_install(parser, options, args)
        self.assertEqual(retcode, -1)


if __name__ == '__main__':
    sys.argv.append('--cover-package=openmdao.main')
    sys.argv.append('--cover-erase')
    nose.runmodule()

