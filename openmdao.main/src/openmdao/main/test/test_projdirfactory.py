
import unittest
import tempfile
import shutil
import time
import os

from openmdao.util.fileutil import build_directory
from openmdao.gui.projdirfactory import ProjDirFactory, _startmods
from openmdao.main.driver import Driver
from openmdao.main.component import Component

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
            for typ,meta in types:
                if typ=='mydrv.MyDrv':
                    self.assertEqual(set(meta['ifaces']), set(['IContainer','IComponent','IDriver']))
                elif typ=='mycomp.MyComp':
                    self.assertEqual(set(meta['ifaces']), set(['IContainer','IComponent']))
                else:
                    self.fail("type %s was not expected" % typ)
            self.assertEqual(len(pdf.analyzer.fileinfo), 2+len(_startmods))
            self.assertEqual(len(pdf.analyzer.modinfo), 2+len(_startmods))
            self.assertTrue('mydrv.MyDrv' in pdf.analyzer.class_file_map)
            self.assertTrue('mycomp.MyComp' in pdf.analyzer.class_file_map)
            
            # now try creating a MyDrv
            mydrv = pdf.create('mydrv.MyDrv')
            self.assertTrue(isinstance(mydrv, Driver))
            
            # now create a new file
            with open(os.path.join(self.tdir, 'mycomp2.py'), 'w') as f:
                f.write("""
from openmdao.main.api import Component
class MyComp2(Component):
    pass
                """)
            time.sleep(2.0)
            types = pdf.get_available_types()
            typenames = [n for n,mdata in types]
            self.assertEqual(set(typenames), set(expected+['mycomp2.MyComp2']))
            for typ,meta in types:
                if typ=='mycomp2.MyComp2':
                    self.assertEqual(set(meta['ifaces']), set(['IContainer','IComponent']))
                    
            self.assertEqual(len(pdf.analyzer.fileinfo), 3+len(_startmods))
            self.assertEqual(len(pdf.analyzer.modinfo), 3+len(_startmods))
            self.assertTrue('mycomp2.MyComp2' in pdf.analyzer.class_file_map)
            
            # now test removal
            os.remove(os.path.join(self.tdir, 'mycomp2.py'))
            time.sleep(2.0)
            types = pdf.get_available_types()
            typenames = [n for n,mdata in types]
            self.assertEqual(set(typenames), set(expected))
            self.assertEqual(len(pdf.analyzer.fileinfo), 2+len(_startmods))
            self.assertEqual(len(pdf.analyzer.modinfo), 2+len(_startmods))
            self.assertTrue('mycomp2.MyComp2' not in pdf.analyzer.class_file_map)
            
            # now try modifying an existing file
            with open(os.path.join(self.tdir, 'mydrv.py'), 'w') as f:
                f.write("""
from openmdao.main.api import Component
class MyDrv(Component):  #old MyDrv was a Driver, new one is just a Component
    pass
    
class Foo(Component):
    pass
                """)
            time.sleep(2.0)
            expected = ['mydrv.MyDrv', 'mydrv.Foo', 'mycomp.MyComp']
            types = pdf.get_available_types()
            typenames = [n for n,mdata in types]
            self.assertEqual(set(typenames), set(expected))
            for typ,meta in types:
                if typ=='mydrv.MyDrv':
                    self.assertEqual(set(meta['ifaces']), set(['IContainer','IComponent']))
                elif typ=='mydrv.Foo':
                    self.assertEqual(set(meta['ifaces']), set(['IContainer','IComponent']))
                elif typ=='mycomp.MyComp':
                    self.assertEqual(set(meta['ifaces']), set(['IContainer','IComponent']))
                else:
                    self.fail("type %s was not expected" % typ)
            
            # now try creating a MyDrv Component
            mydrv = pdf.create('mydrv.MyDrv')
            self.assertFalse(isinstance(mydrv, Driver))
            self.assertTrue(isinstance(mydrv, Component))
            
        finally:
            pdf.cleanup()
        
    
