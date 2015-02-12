import unittest
import os
import sys
import tempfile
import shutil
from subprocess import Popen, PIPE, STDOUT
from argparse import Namespace

class SphinxDocsTestCase(unittest.TestCase):
    def test_docs(self):
        openmdao_cmd = os.path.join(os.path.dirname(sys.executable), 'openmdao')

        if sys.platform == "win32":
            openmdao_cmd = "{}.exe".format(openmdao_cmd)

        #If path doesn't exist,
        #sys.executable must be part of a
        #a standard Python or anaconda distribution
        #on Windows, so look for `openmdao` in a scripts directory
        if not os.path.isfile(openmdao_cmd):
            openmdao_cmd = os.path.join(
                os.path.dirname(sys.executable),
                'scripts',
                'openmdao'
            )

        p = Popen([openmdao_cmd, 'test_docs'], stdout=PIPE, stderr=STDOUT,
                  env=os.environ, cwd=os.getcwd())
        output = p.communicate()[0]
        retval = p.returncode
        if not output.strip().endswith('build succeeded.'):
            self.fail('problem in building sphinx documentation:\\n'+output)

if __name__ == '__main__':
    unittest.main()
