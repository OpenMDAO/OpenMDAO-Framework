
import unittest
import tempfile
import shutil
import time
import os

from openmdao.util.fileutil import build_directory
from openmdao.main.projdirfactory import ProjDirFactory

_dstruct = {
    "mycomp.py": 
"""
from openmdao.main.api import Component
class MyComp(Component):
    __openmdao_meta__ = {
        'version': '1.0',
    }
""",
    "mydrv.py":
"""
from openmdao.main.api import Driver
class MyDrv(Driver):
    __openmdao_meta__ = {
        'version': '1.0',
    }
"""
    }

class ProjDirFactoryTestCase(unittest.TestCase):

    def setUp(self):
        self.tdir = tempfile.mkdtemp()
        build_directory(_dstruct, topdir=self.tdir)
        
    def tearDown(self):
        shutil.rmtree(self.tdir)

    def test_modify_files(self):
        pdf = ProjDirFactory(self.tdir)
        try:
            expected = ['mydrv.MyDrv', 'mycomp.MyComp']
            types = pdf.get_available_types()
            typenames = [n for n,mdata in types]
            self.assertEqual(set(typenames), set(expected))
            with open(os.path.join(self.tdir, 'mycomp2.py'), 'w') as f:
                f.write("""
from openmdao.main.api import Component
class MyComp2(Component):
    pass
                """)
            time.sleep(1.0)
            types = pdf.get_available_types()
            typenames = [n for n,mdata in types]
            self.assertEqual(set(typenames), set(expected+['mycomp2.MyComp2']))
            
            # now test removal
            os.remove(os.path.join(self.tdir, 'mycomp2.py'))
            time.sleep(1.0)
            types = pdf.get_available_types()
            typenames = [n for n,mdata in types]
            self.assertEqual(set(typenames), set(expected))
            
        finally:
            pdf.cleanup()
        
    
