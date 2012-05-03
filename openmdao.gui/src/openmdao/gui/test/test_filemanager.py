import unittest
import os.path
import tempfile
import zipfile

from openmdao.gui.filemanager import FileManager


class FileManagerTestCase(unittest.TestCase):

    def setUp(self):
        pass

    def test_filemanager(self):
        ''' exercise filemanager functions
        '''
        # constructor
        tempdir = tempfile.mkdtemp()
        filemanager = FileManager('test', tempdir)

        # getcwd
        self.assertEquals(tempdir, filemanager.getcwd())

        # ensure_dir
        filemanager.ensure_dir('subdirectory')
        self.assertTrue(os.path.exists(os.path.join(tempdir, 'subdirectory')))

        # write_file
        hello_filename = 'hello.py'
        hello_contents = 'print "hello world!"'
        filemanager.write_file(hello_filename, hello_contents)
        self.assertTrue(os.path.exists(os.path.join(tempdir, hello_filename)))

        hello_filename = os.path.join('subdirectory', 'hello.py')
        filemanager.write_file(hello_filename, hello_contents)
        self.assertTrue(os.path.exists(os.path.join(tempdir, hello_filename)))

        # get_file
        contents = filemanager.get_file(hello_filename)
        self.assertEqual(contents, hello_contents)

        # get_files
        files = filemanager.get_files()
        self.assertEqual(len(files), 2)
        self.assertTrue('/hello.py' in files)
        self.assertTrue('/subdirectory' in files)
        self.assertEqual(files['/hello.py'], len(hello_contents))
        self.assertTrue(len(files['/subdirectory']), 1)
        self.assertTrue('/subdirectory/hello.py' in files['/subdirectory'])
        self.assertEqual(files['/subdirectory']['/subdirectory/hello.py'], len(hello_contents))

        # delete_file
        try:
            filemanager.delete_file('subdirectory')
        except OSError as (errno, errmsg):
            # OSError: [Errno 39] Directory not empty
            self.assertEqual(errno, 39)
        self.assertTrue(os.path.exists(os.path.join(tempdir, 'subdirectory')))

        filemanager.delete_file(hello_filename)
        self.assertTrue(not os.path.exists(os.path.join(tempdir, hello_filename)))

        filemanager.delete_file('subdirectory')
        self.assertTrue(not os.path.exists(os.path.join(tempdir, 'subdirectory')))

        # cleanup
        filemanager.cleanup()
        self.assertTrue(not os.path.exists(tempdir))


    def test_add_file(self):
        ''' exercise filemanager add_file function
        '''
        # create a zip file
        tempdir = tempfile.mkdtemp()
        temptxt = os.path.join(tempdir,'temp.txt')
        with open(temptxt,'w') as f:
            f.write('this is just a test')
        tempzip = os.path.join(tempdir,'temp.zip')
        zf = zipfile.ZipFile(tempzip,mode='w')
        try:
            zf.write(temptxt, arcname='filename.txt')
        finally:
            zf.close()
        with open(tempzip,'rb') as f:
            contents = f.read()

        # add_file
        filemanager = FileManager('test')
        filemanager.add_file(os.path.basename(tempzip),contents)
        files = filemanager.get_files()
        self.assertEqual(len(files), 1)
        self.assertTrue('/filename.txt' in files)
        self.assertEqual(files['/filename.txt'], len('this is just a test'))

    def tearDown(self):
        pass


if __name__ == "__main__":
    unittest.main()
