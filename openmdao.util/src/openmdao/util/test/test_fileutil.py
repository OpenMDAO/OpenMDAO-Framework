"""
Test File Utility Functions
"""

import os
import shutil
import logging
import os.path
import sys
import unittest
import tempfile

from openmdao.util.fileutil import find_in_path, build_directory, find_files

structure = {
    'top': {
        'foo/bar.exe': 'some stuff...',
        'blah': {
            'somefile': '# a comment',
            },
        'somedir/dir2': {
                    }
        }
    }

class FileUtilTestCase(unittest.TestCase):

    def setUp(self):
        self.startdir = os.getcwd()
        self.tempdir = tempfile.mkdtemp()
        os.chdir(self.tempdir)
        build_directory(structure)

    def tearDown(self):
        os.chdir(self.startdir)
        shutil.rmtree(self.tempdir)

    def test_find_in_path(self):
        if sys.platform == 'win32':
            path=r'C:\a\b\c;top;top\blah;top\foo'
        else:
            path = '/a/b/c:top:top/blah:top/foo'
        fname = find_in_path('bar', path)
        self.assertEqual(fname, None)
        # search for a file with an extension
        fname = find_in_path('bar', path, exts=('.exe',))
        self.assertTrue(fname is not None)
        self.assertEqual(os.path.basename(fname), 'bar.exe')
        # search for a file without an extension
        fname = find_in_path('somefile', path)
        self.assertTrue(fname is not None)
        self.assertEqual(os.path.basename(fname), 'somefile')
        # make sure we don't find directories
        fname = find_in_path('blah', path)
        self.assertEqual(fname, None)
        
    def test_find_files(self):
        flist = find_files(self.tempdir)
        self.assertEqual(set([os.path.basename(f) for f in flist]), 
                         set(['bar.exe', 'somefile']))
        flist = find_files(self.tempdir, '*.exe')
        self.assertEqual(set([os.path.basename(f) for f in flist]), 
                         set(['bar.exe']))
        flist = find_files(self.tempdir, ['*.exe','*some*'])
        self.assertEqual(set([os.path.basename(f) for f in flist]), 
                         set(['bar.exe', 'somefile']))
        flist = find_files(self.tempdir, exclude='*.exe')
        self.assertEqual(set([os.path.basename(f) for f in flist]), 
                         set(['somefile']))
        flist = find_files(self.tempdir, exclude=['*.exe', '*some*'])
        self.assertEqual(set([os.path.basename(f) for f in flist]), 
                         set([]))
        flist = find_files(self.tempdir, match='*.exe', exclude=['*.exe', '*some*'])
        self.assertEqual(set([os.path.basename(f) for f in flist]), 
                         set([]))
        
if __name__ == '__main__':
    unittest.main()

