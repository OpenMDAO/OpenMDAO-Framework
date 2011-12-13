

import unittest
import os
import sys
import tempfile
import shutil
from subprocess import Popen, PIPE, STDOUT
from argparse import Namespace

class SphinxDocsTestCase(unittest.TestCase):
    def test_docs(self):
        tmpdir = tempfile.mkdtemp()
        try:
            fpath = os.path.join(tmpdir, 'outfile')
            openmdao_cmd = os.path.join(os.path.dirname(sys.executable), 'openmdao')
            with open(fpath, 'wb') as f:
                p = Popen('%s test_docs' % openmdao_cmd, stdout=f, stderr=STDOUT,
                          env=os.environ, shell=True)
                p.wait()
            with open(fpath, 'rb') as f:
                output = f.read()
            retval = p.returncode
            if not output.strip().endswith('build succeeded.'):
                self.fail('problem in building sphinx documentation:\\n'+output)
        finally:
            shutil.rmtree(tmpdir, ignore_errors=True)

