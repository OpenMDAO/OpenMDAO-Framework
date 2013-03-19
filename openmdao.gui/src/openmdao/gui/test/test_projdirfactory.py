
import unittest
import tempfile
import shutil
import time
import os
import sys

from openmdao.util.fileutil import build_directory, get_module_path, find_files, onerror
from openmdao.gui.projdirfactory import ProjDirFactory
from openmdao.main.driver import Driver
from openmdao.main.component import Component
from openmdao.main.publisher import Publisher

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

class MyDrv2(Driver):
    __openmdao_meta__ = {
         'version': '1.0',
    }
"""
}


class ProjDirFactoryTestCase(unittest.TestCase):

    def setUp(self):
        Publisher.silent = True
        self.tdir = tempfile.mkdtemp()
        build_directory(_dstruct, topdir=self.tdir)

    def tearDown(self):
        for pyfile in find_files(self.tdir, "*.py"):
            modpath = get_module_path(pyfile)
            if modpath in sys.modules:
                del sys.modules[modpath]
        shutil.rmtree(self.tdir, onerror=onerror)

    def test_with_observer(self):
        sys.path = [self.tdir] + sys.path
        pdf = ProjDirFactory(self.tdir)
        try:
            expected = ['mydrv.MyDrv', 'mydrv.MyDrv2', 'mycomp.MyComp']
            types = dict(pdf.get_available_types())
            typenames = types.keys()
            self.assertEqual(set(typenames), set(expected))
            self.assertEqual(set(types['mydrv.MyDrv']['ifaces']),
                             set(['IContainer', 'IComponent', 'IDriver', 'IHasEvents']))
            self.assertEqual(set(types['mycomp.MyComp']['ifaces']),
                             set(['IContainer', 'IComponent']))
            self.assertTrue('mydrv.MyDrv' in pdf._classes)
            self.assertTrue('mydrv.MyDrv2' in pdf._classes)
            self.assertTrue('mycomp.MyComp' in pdf._classes)

            # now try creating a MyDrv
            mydrv = pdf.create('mydrv.MyDrv')
            self.assertEqual(type(mydrv).__name__, 'MyDrv')
            self.assertEqual(type(mydrv).mro()[1].__name__, 'Driver')

            # now create a new file
            with open(os.path.join(self.tdir, 'mycomp2.py'), 'w') as f:
                f.write("""
from openmdao.main.api import Component
class MyComp2(Component):
    pass
                """)

            time.sleep(2.0)
            types = dict(pdf.get_available_types())
            typenames = types.keys()
            self.assertEqual(set(typenames), set(expected + ['mycomp2.MyComp2']))
            self.assertEqual(set(types['mycomp2.MyComp2']['ifaces']),
                             set(['IContainer', 'IComponent']))
            self.assertTrue('mycomp2.MyComp2' in pdf._classes)

            # now test removal
            os.remove(os.path.join(self.tdir, 'mycomp2.py'))
            time.sleep(2.0)
            types = dict(pdf.get_available_types())
            typenames = types.keys()
            self.assertEqual(set(typenames), set(expected))
            self.assertTrue('mycomp2.MyComp2' not in pdf._classes)

            # now try modifying an existing file
            with open(os.path.join(self.tdir, 'mydrv.py'), 'w') as f:
                time.sleep(6)
                f.write("""
from openmdao.main.api import Component
class MyDrv(Component):  #old MyDrv was a Driver, new one is just a Component
    pass

class Foo(Component):
    pass
                """)
                time.sleep(3)
            time.sleep(2.0)
            expected = ['mydrv.MyDrv', 'mydrv.Foo', 'mycomp.MyComp']
            types = dict(pdf.get_available_types())
            typenames = types.keys()
            self.assertEqual(set(typenames), set(expected))
            self.assertEqual(set(types['mycomp.MyComp']['ifaces']), set(['IContainer', 'IComponent']))
            self.assertEqual(set(types['mydrv.MyDrv']['ifaces']), set(['IContainer', 'IComponent']))
            self.assertEqual(set(types['mydrv.Foo']['ifaces']), set(['IContainer', 'IComponent']))

            # now try creating a MyDrv Component
            mydrv = pdf.create('mydrv.MyDrv')
            self.assertNotEqual(type(mydrv).mro()[1].__name__, 'Driver')
            self.assertTrue(isinstance(mydrv, Component))

        finally:
            pdf.cleanup()

    def test_manual(self):
        try:
            sys.path = [self.tdir]+sys.path

            pdf = ProjDirFactory(self.tdir, use_observer=False)
            expected = ['mydrv.MyDrv', 'mydrv.MyDrv2', 'mycomp.MyComp']
            types = pdf.get_available_types()
            typenames = [n for n, mdata in types]
            self.assertEqual(set(typenames), set(expected))

            # now try creating a MyDrv
            mydrv = pdf.create('mydrv.MyDrv')
            self.assertTrue(isinstance(mydrv, Driver))

            # now create a new file
            fpath = os.path.join(self.tdir, 'mycomp2.py')
            with open(fpath, 'w') as f:
                f.write("""
from openmdao.main.api import Component
class MyComp2(Component):
    pass
                """)
            added_set = set()
            changed_set = set()
            deleted_set = set()
            pdf.on_modified(fpath, added_set, changed_set, deleted_set)  # manual notification
            types = pdf.get_available_types()
            typenames = [n for n, mdata in types]
            self.assertEqual(set(typenames), set(expected + ['mycomp2.MyComp2']))
            self.assertEqual(set(['mycomp2.MyComp2']), added_set)
            self.assertEqual(set(), changed_set)
            self.assertEqual(set(), deleted_set)

            # now test removal
            os.remove(fpath)
            deleted_set = set()
            pdf.on_deleted(fpath, deleted_set)  # manual notification
            types = pdf.get_available_types()
            typenames = [n for n, mdata in types]
            self.assertEqual(set(typenames), set(expected))
            self.assertEqual(set(['mycomp2.MyComp2']), deleted_set)

            # now try modifying an existing file
            time.sleep(2)  # Windows timestamp granularity is one second.
            fpath = os.path.join(self.tdir, 'mydrv.py')
            with open(fpath, 'w') as f:
                f.write("""
from openmdao.main.api import Component
class MyDrv(Component):  #old MyDrv was a Driver, new one is just a Component
    pass

class Foo(Component):
    pass
                """)
            added_set = set()
            changed_set = set()
            deleted_set = set()
            pdf.on_modified(fpath, added_set, changed_set, deleted_set)  # manual notification
            expected = ['mydrv.MyDrv', 'mydrv.Foo', 'mycomp.MyComp']
            types = pdf.get_available_types()
            typenames = [n for n, mdata in types]
            self.assertEqual(set(typenames), set(expected))
            self.assertEqual(set(['mydrv.Foo']), added_set)
            self.assertEqual(set(['mydrv.MyDrv']), changed_set)
            self.assertEqual(set(['mydrv.MyDrv2']), deleted_set)
        finally:
            sys.path = sys.path[1:]


if __name__ == '__main__':
    unittest.main()
