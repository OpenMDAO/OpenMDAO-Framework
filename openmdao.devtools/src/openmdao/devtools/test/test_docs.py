

import unittest
import os
from os.path import join
from subprocess import Popen, PIPE, STDOUT

from openmdao.devtools.build_docs import test_docs

class SphinxDocsTestCase(unittest.TestCase):
    def test_docs(self):
        p = Popen('openmdao test_docs', stdout=PIPE, stderr=STDOUT, 
                  env=os.environ, shell=True)
        output = p.communicate()[0]
        retval = p.returncode
        if not output.strip().endswith('build succeeded.'):
            self.fail('problem in building sphinx documentation:\\n'+output)

