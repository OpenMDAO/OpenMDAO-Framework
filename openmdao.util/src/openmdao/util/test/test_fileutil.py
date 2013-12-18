"""
Test File Utility Functions
"""

import os
import shutil
import os.path
import sys
import unittest
import tempfile
from fnmatch import fnmatch

from openmdao.util.fileutil import find_in_path, build_directory, find_files, onerror

structure = {
    'd1': {
        'd1d1/d1d1f1.exe': 'some stuff...',
        'd1d2': {
            'd1d2f2': '# a comment',
        },
        'd1d3/d1d3d1': {
        }
    },
    '_d2': {
        '_d2d1/_d2d1f1.foo': 'some stuff...',
        '_d2d2': {
            '_d2d2f1.txt': '# a comment',
        },
        '_d2d3/_d2d3d1': {
        },
        '_d2d4/.d2d4d1': {
            'd2d4d1f1.blah': '# a comment',
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
        shutil.rmtree(self.tempdir, onerror=onerror)

    def test_find_in_path(self):
        if sys.platform == 'win32':
            path = r'C:\a\b\c;d1;d1\d1d1;d1\d1d2'
        else:
            path = '/a/b/c:d1:d1/d1d1:d1/d1d2'
        fname = find_in_path('bar', path)
        self.assertEqual(fname, None)
        # search for a file with an extension
        fname = find_in_path('d1d1f1', path, exts=('.exe',))
        self.assertTrue(fname is not None)
        self.assertEqual(os.path.basename(fname), 'd1d1f1.exe')
        # search for a file without an extension
        fname = find_in_path('d1d2f2', path)
        self.assertTrue(fname is not None)
        self.assertEqual(os.path.basename(fname), 'd1d2f2')
        # make sure we don't find directories
        fname = find_in_path('d1d2', path)
        self.assertEqual(fname, None)

    def test_find_files(self):
        # find all files
        flist = find_files(self.tempdir)
        self.assertEqual(set([os.path.basename(f) for f in flist]),
                         set(['d1d1f1.exe', 'd1d2f2', '_d2d1f1.foo', '_d2d2f1.txt', 'd2d4d1f1.blah']))
        
        # find all .exe files
        flist = find_files(self.tempdir, '*.exe')
        self.assertEqual(set([os.path.basename(f) for f in flist]),
                         set(['d1d1f1.exe']))
        
        # find exe files or files starting with an underscore
        matcher = lambda name: fnmatch(name, '*.exe') or name.startswith('_')
        flist = find_files(self.tempdir, matcher)
        self.assertEqual(set([os.path.basename(f) for f in flist]),
                         set(['d1d1f1.exe', '_d2d1f1.foo', '_d2d2f1.txt', ]))
        
        # find all files except .exe files
        flist = find_files(self.tempdir, exclude='*.exe')
        self.assertEqual(set([os.path.basename(f) for f in flist]),
                         set(['d1d2f2', '_d2d1f1.foo', '_d2d2f1.txt', 'd2d4d1f1.blah']))
        
        # find all files except .exe files and files starting with '_'
        flist = find_files(self.tempdir, exclude=matcher)
        self.assertEqual(set([os.path.basename(f) for f in flist]),
                         set(['d1d2f2', 'd2d4d1f1.blah']))
        
        # only match .exe but exclude .exe and starting with '_', which results in no matches
        flist = find_files(self.tempdir, match='*.exe', exclude=matcher)
        self.assertEqual(set([os.path.basename(f) for f in flist]),
                         set([]))
        
        # find all files except those under directories staring with '_'
        flist = find_files(self.tempdir, direxclude='_*')
        self.assertEqual(set([os.path.basename(f) for f in flist]),
                         set(['d1d1f1.exe', 'd1d2f2']))
        

if __name__ == '__main__':
    unittest.main()
