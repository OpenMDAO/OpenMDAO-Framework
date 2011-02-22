

import unittest
import os
import shutil
import tempfile

from openmdao.devtools.plugins import plugin_quickstart
from openmdao.util.fileutil import find_files

class PluginsTestCase(unittest.TestCase):
    def setUp(self):
        self.tdir = tempfile.mkdtemp()

    def tearDown(self):
        shutil.rmtree(self.tdir)

    def test_quickstart(self):
        argv = ['foobar', self.tdir, 'version=1.1']
        plugin_quickstart(argv)
        fandd = find_files(self.tdir, nodirs=False)
        self.assertEqual(set([os.path.basename(f) for f in fandd]), 
                         set(['foobar', 'src', 'docs', 'setup.cfg', 'setup.py',
                              'MANIFEST.in', 'conf.py', 'foobar.rst', 'index.rst',
                              'srcdocs.rst', 'foobar.py']))

if __name__ == '__main__':
    unittest.main()