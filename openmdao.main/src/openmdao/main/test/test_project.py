import os
import sys
import unittest
import tempfile
import shutil

from openmdao.util.fileutil import build_directory, onerror
from openmdao.main.component import Component
from openmdao.main.project import Project, project_from_archive, PROJ_FILE_EXT, \
                                  filter_macro, ProjFinder, _match_insts
from openmdao.main.factorymanager import get_signature
from openmdao.lib.datatypes.api import Float


class Multiplier(Component):
    rval_in = Float(iotype='in')
    rval_out = Float(iotype='out')
    mult = Float(iotype='in')

    def __init__(self):
        super(Multiplier, self).__init__()
        self.rval_in = 4.
        self.rval_out = 6.
        self.mult = 1.5

    def execute(self):
        self.rval_out = self.rval_in * self.mult


class ProjectTestCase(unittest.TestCase):
    def setUp(self):
        self.startdir = os.getcwd()
        self.tdir = tempfile.mkdtemp()
        os.chdir(self.tdir)

    def tearDown(self):
        try:
            shutil.rmtree(self.tdir, onerror=onerror)
        except:
            pass
        finally:
            os.chdir(self.startdir)

    def _fill_project(self, proj):
        proj.command("top = set_as_top(create('openmdao.main.assembly.Assembly'))")
        proj.command('top.add("comp1", create("openmdao.main.test.test_project.Multiplier"))')
        proj.command('top.add("comp2", create("openmdao.main.test.test_project.Multiplier"))')

        proj.command("top.driver.workflow.add(['comp1', 'comp2'])")

        proj.command('top.comp1.mult = 2.0')
        proj.command('top.comp2.mult = 4.0')
        proj.command("top.connect('comp1.rval_out', 'comp2.rval_in')")
        proj.command("top.comp1.rval_in = 5.0")

    def test_project_export_import(self):
        proj = Project(os.path.join(self.tdir, 'proj1'))
        self.assertEqual(proj.config.items('info'),
                         [('version', '0'), ('description', '')])
        new_info = [('version', 'stinky'), ('description', 'Frobozz rulz!')]
        proj.set_info(dict(new_info))
        proj.activate()
        self._fill_project(proj)

        proj.export(destdir=self.tdir)
        proj.deactivate()

        newproj = project_from_archive(os.path.join(self.tdir,
                                                    'proj1%s' % PROJ_FILE_EXT),
                                       proj_name='proj2',
                                       dest_dir=self.tdir)

        self.assertEqual(newproj.path, os.path.join(self.tdir, 'proj2'))
        self.assertEqual(newproj.config.items('info'), new_info)

        try:
            newproj = project_from_archive(os.path.join(self.tdir,
                                                        'proj1%s' % PROJ_FILE_EXT),
                                           dest_dir=self.tdir)
        except Exception, err:
            self.assertTrue(str(err).endswith(' already exists'))
        else:
            self.fail("Exception expected")

    def test_using(self):
        proj = Project('a_proj')
        proj.activate()
        self._fill_project(proj)
        top = proj.get('top')
        top.run()
        self.assertEqual(top.comp1.rval_out, 10.)
        self.assertEqual(top.comp2.rval_out, 40.)
        proj.command("top.comp1.rval_in = 0.5")
        os.chdir(self.tdir)
        proj.export(projname='fooproj')

        fooproj = project_from_archive('fooproj.proj')
        fooproj.activate()
        footop = fooproj.get('top')
        self.assertEqual(footop.comp1.rval_in, top.comp1.rval_in)
        footop.run()
        self.assertEqual(footop.comp1.rval_out, 1.)
        self.assertEqual(footop.comp2.rval_out, 4.)

    def test_localfile_factory(self):
        proj = Project(os.path.join(self.tdir, 'proj2'))
        proj.activate()
        self._fill_project(proj)

    def test_filter_macro(self):
        lines = [
            "abc.xyz = 123.45",
            "execfile('foo.py')",
            "top.add('foo', create('MyClass'))",
            "top.foo.x = 8.9",
            "top.foo.execute()",
            "abc.xyz = 99.9",
            "some_unknown_funct(a,b,c)",
            "execfile('foo.py')",
            "top.add('foo', create('SomeClass'))",
            "top.foo.gg = 53",
            "top.blah.xx = 44",
            "top.remove('blah')",
            "abc.run()",
        ]
        expected = [
            "abc.xyz = 123.45",
            "top.add('foo', create('MyClass'))",
            "top.foo.x = 8.9",
            "abc.xyz = 99.9",
            "some_unknown_funct(a,b,c)",
            "execfile('foo.py')",
            "top.add('foo', create('SomeClass'))",
            "top.foo.gg = 53",
            "top.blah.xx = 44",
            "top.remove('blah')",
        ]
        #expected = [
            #"abc.xyz = 99.9",
            #"some_unknown_funct(a,b,c)",
            #"execfile('foo.py')",
            #"top.add('foo', create('SomeClass'))",
            #"top.foo.gg = 53",
            #]
        filtered = filter_macro(lines)
        self.assertEqual(filtered, expected)


class ProjFinderTestCase(unittest.TestCase):
    def setUp(self):
        self.startdir = os.getcwd()
        self.tdir = tempfile.mkdtemp()
        os.chdir(self.tdir)

    def tearDown(self):
        try:
            shutil.rmtree(self.tdir, onerror=onerror)
        except:
            pass
        finally:
            os.chdir(self.startdir)

    def test_importing(self):
        projdirname = 'myproj.projdir'
        dirstruct = {
            os.path.splitext(projdirname)[0]: {
                'top.py': """
from openmdao.main.api import Component
class MyClass(Component):
    pass
""",
                'pkgdir': {
                    '__init__.py': '',
                    'pkgfile.py': 'from openmdao.main.api import Component, Assembly',
                    'pkgdir2': {
                          '__init__.py': '',
                          'pkgfile2.py': """
from openmdao.main.api import Component
class PkgClass2(Component):
    def __init__(self, somearg=8, anotherarg=False):
        super(PkgClass2, self).__init__()
""",
                    }
                },
                'plaindir': {
                    'plainfile.py': """
from pkgdir.pkgdir2.pkgfile2 import PkgClass2
p = PkgClass2()
""",
                },
            },
        }

        build_directory(dirstruct, topdir=os.getcwd())
        try:
            sys.path_hooks = [ProjFinder]+sys.path_hooks
            sys.path = [os.path.join(os.getcwd(), projdirname)]+sys.path
            __import__('top')
            __import__('pkgdir.pkgfile')
            __import__('pkgdir.pkgdir2.pkgfile2')

            mod = sys.modules['pkgdir.pkgdir2.pkgfile2']
            expected_classname = 'pkgdir.pkgdir2.pkgfile2.PkgClass2'
            matches = _match_insts([expected_classname])
            self.assertEqual(matches, set())

            sig = get_signature('pkgdir.pkgdir2.pkgfile2.PkgClass2')
            self.assertEqual(sig['args'], [['somearg', '8'], ['anotherarg', 'False']])
            inst = getattr(mod, 'PkgClass2')()  # create an inst of PkgClass2
            matches = _match_insts([expected_classname])
            self.assertEqual(matches, set([expected_classname]))

        finally:
            sys.path = sys.path[1:]

if __name__ == "__main__":
    unittest.main()
