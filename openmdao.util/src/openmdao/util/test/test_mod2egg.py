import unittest
import os
import sys
import shutil
import tempfile
from subprocess import check_call

from openmdao.main.api import PkgResourcesFactory
import openmdao.util.mod2egg
from openmdao.util.testutil import find_python

class mod2eggTestCase(unittest.TestCase):
    
    def setUp(self):
        self.pudir = tempfile.mkdtemp()
        self.version = '1.999'
        self.mod2egg = openmdao.util.mod2egg.__file__.replace('.pyc','.py')
        self.python = find_python('openmdao.util')
        self.srcfile = os.path.join(os.path.dirname(__file__),'src','doubler.py')
    
    def tearDown(self):
        if os.path.exists(self.pudir):
            shutil.rmtree(self.pudir)
    
    def use_egg(self):
        factory = PkgResourcesFactory(['openmdao.component'],
                                           [self.pudir])
        foo = factory.create('Doubler')
        foo.x = 4.
        foo.run()
        self.assertEqual(foo.y, 8.)
        
        del sys.modules[foo.__class__.__module__]
        for name in sys.path:
            if (foo.__class__.__module__+self.version) in name:
                sys.path.remove(name)
                if os.path.isfile(name):
                    os.remove(name)
                elif os.path.isdir(name):
                    shutil.rmtree(name)
                break
        
    def test_mod2egg_zipped(self):
        check_call([self.python, 
                    self.mod2egg, 
                    self.srcfile,
                    '-v',self.version,'-z','-i',self.pudir])        
        self.use_egg()
        
    def test_mod2egg(self):
        check_call([self.python, 
                    self.mod2egg, 
                    self.srcfile,
                    '-v',self.version,'-d',self.pudir,
                    '-i',self.pudir])        
        self.use_egg()
      
        
if __name__ == '__main__':
    unittest.main()
