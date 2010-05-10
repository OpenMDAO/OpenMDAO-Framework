import unittest
import os
import sys
import shutil
import tempfile
from subprocess import check_call
import pkg_resources
import copy

from openmdao.main.pkg_res_factory import PkgResourcesFactory
import openmdao.util.mod2dist
from openmdao.util.testutil import find_python

orig_path = copy.copy(pkg_resources.working_set.entries)

class mod2distTestCase(unittest.TestCase):
    
    def setUp(self):
        # make a new pkg_resources.working_set and sys.path each time so that
        # later tests don't benefit from something an earlier test added
        sys.path = copy.copy(orig_path)
        pkg_resources.working_set = pkg_resources.WorkingSet()
        self.pudir = tempfile.mkdtemp()
        self.version = '1.999'
        self.mod2dist = openmdao.util.mod2dist.__file__.replace('.pyc','.py')
        self.mod2dist = self.mod2dist.replace('.pyo','.py')
        self.python = find_python()
        self.srcfile = os.path.join(os.path.dirname(__file__),'src','doubler.py')
    
    def tearDown(self):
        if os.path.exists(self.pudir):
            shutil.rmtree(self.pudir)
    
    def use_distribution(self):
        factory = PkgResourcesFactory(['openmdao.component'],
                                           [self.pudir])
        foo = factory.create('Doubler')
        foo.x = 4.
        foo.run()
        self.assertEqual(foo.y, 8.)
        
        # do some cleaning up here so that later tests won't accidentally pass
        dist = pkg_resources.working_set.find(pkg_resources.Requirement.parse('doubler==%s' % self.version))
        del sys.modules['doubler']
        for name in sys.path:
            if dist.location in name:
                sys.path.remove(name)
                if os.path.isfile(name):
                    os.remove(name)
                elif os.path.isdir(name):
                    shutil.rmtree(name)
                break
        
    def test_mod2dist(self):
        check_call([self.python, 
                    self.mod2dist, 
                    self.srcfile,
                    '-v',self.version,'-d',self.pudir,
                    '-i',self.pudir])        
        self.use_distribution()
      
        
if __name__ == '__main__':
    unittest.main()
