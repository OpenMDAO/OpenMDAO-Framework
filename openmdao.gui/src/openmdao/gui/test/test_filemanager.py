import unittest
import os.path
import shutil
import tempfile
import zipfile

from openmdao.gui.filemanager import FileManager
from openmdao.util.fileutil import onerror


class FileManagerTestCase(unittest.TestCase):

    def setUp(self):
        tempdir = tempfile.mkdtemp()
        self.tempdir = os.path.realpath(tempdir)  # osx

    def tearDown(self):
        shutil.rmtree(self.tempdir, onerror=onerror)

    def test_filemanager(self):
        # exercise filemanager functions

        tempdir = self.tempdir

        # constructor
        filemanager = FileManager('test', tempdir)

        # ensure_dir
        dname = 'subdirectory'
        filemanager.ensure_dir(dname)
        self.assertTrue(os.path.exists(os.path.join(tempdir, dname)))

        # write_file
        fname = 'hello.py'
        hello = 'print "hello world!"'
        filemanager.write_file(fname, hello)
        self.assertTrue(os.path.exists(os.path.join(tempdir, fname)))

        sname = os.path.join(dname, fname)
        filemanager.write_file(sname, hello)
        self.assertTrue(os.path.exists(os.path.join(tempdir, sname)))

        # get_file
        (contents, mimetype, encoding) = filemanager.get_file(sname)
        self.assertEqual(contents, hello)
        self.assertEqual(mimetype, 'text/x-python')
        self.assertEqual(encoding, None)

        # get_files
        # note that os.sep will be prepended to all file names
        files = filemanager.get_files()
        self.assertEqual(len(files), 2)
        s_fname = os.sep + fname
        s_dname = os.sep + dname
        s_sname = os.sep + sname
        self.assertTrue(s_fname in files)
        self.assertTrue(s_dname in files)
        self.assertEqual(files[s_fname], len(hello))
        self.assertTrue(len(files[s_dname]), 1)
        self.assertTrue(s_sname in files[s_dname])
        self.assertEqual(files[s_dname][s_sname], len(hello))

        # delete_file
        filemanager.delete_file(dname)

        filemanager.delete_file(sname)
        self.assertTrue(not os.path.exists(os.path.join(tempdir, sname)))

        filemanager.delete_file(dname)
        self.assertTrue(not os.path.exists(os.path.join(tempdir, dname)))

        # cleanup
        filemanager.cleanup()
        self.assertEqual(os.getcwd(), filemanager.orig_dir)

    def test_add_file(self):
        # exercise filemanager add_file function

        # create a zip file
        tempdir = tempfile.mkdtemp()
        tempdir = os.path.realpath(tempdir)  # osx
        temptxt = os.path.join(tempdir, 'temp.txt')
        try:
            with open(temptxt, 'w') as f:
                f.write('this is just a test')
            tempzip = os.path.join(tempdir, 'temp.zip')
            zf = zipfile.ZipFile(tempzip, mode='w')
            try:
                zf.write(temptxt, arcname='testfile.txt')
            finally:
                zf.close()
            with open(tempzip, 'rb') as f:
                contents = f.read()
        finally:
            shutil.rmtree(tempdir, onerror=onerror)

        # add_file
        filemanager = FileManager('test', self.tempdir)
        filemanager.add_file('unzip me', contents)
        files = filemanager.get_files()
        self.assertEqual(len(files), 1)
        filename = os.sep + 'testfile.txt'
        self.assertTrue(filename in files)
        self.assertEqual(files[filename], len('this is just a test'))

        # cleanup
        filemanager.cleanup()


if __name__ == "__main__":
    unittest.main()
