"""
Test of Component.
"""

import logging
import os.path
import sys
import stat
import unittest

from nose import SkipTest

from openmdao.main.api import Assembly, Component, Container, Driver
from openmdao.main.interfaces import ICaseRecorder, implements
from openmdao.lib.datatypes.api import Float
from openmdao.main.container import _get_entry_group
from openmdao.util.testutil import assert_raises


class MyComponent(Component):
    x = Float(1., iotype='in')
    xout = Float(2., iotype='out')

    def __init__(self):
        super(MyComponent, self).__init__()
        self.add('cont', Container())
        self.cont.add('dyntrait', Float(3.))

    def execute(self):
        self.xout = self.x * 2.


# Don't want to depend on lib.
class FakeRecorder(object):
    implements(ICaseRecorder)

    def record(self, case):
        pass

    def close(self):
        pass

    def get_iterator(self):
        pass


class TestCase(unittest.TestCase):
    """ Test of Component. """

    def setUp(self):
        self.comp = MyComponent()

    def test_get_valid(self):
        comp = MyComponent()
        valids = comp.get_valid(['x', 'xout'])
        self.assertEqual(valids, [True, False])
        try:
            comp.get_valid(['x', 'foobar'])
        except KeyError as err:
            self.assertEqual(str(err), "'foobar'")
        else:
            self.fail("Expected KeyError")

    def test_set_valid(self):
        comp = self.comp
        valids = comp.get_valid(['x', 'xout'])
        self.assertEqual(valids, [True, False])
        comp.set_valid(['x', 'xout'], True)
        newvalids = comp.get_valid(['x', 'xout'])
        self.assertEqual(newvalids, [True, True])

    def test_connect(self):
        comp = self.comp

        self.assertEqual(comp._depgraph.get_source('x'), None)
        vset = set(comp._valid_dict.keys())

        comp.connect('parent.foo', 'x')
        self.assertEqual(comp._depgraph.get_source('x'), 'parent.foo')

        comp.connect('xout', 'parent.bar')
        self.assertEqual(comp._depgraph.get_source('xout'), None)
        self.assertEqual(vset, set(comp._valid_dict.keys()))

        comp.connect('parent.blah', 'cont.dyntrait')
        # _valid_dict should have a new entry
        self.assertEqual(set(comp._valid_dict.keys())-vset, set(['cont.dyntrait']))

        # _valid_dict entry should go away
        comp.disconnect('parent.blah', 'cont.dyntrait')
        self.assertEqual(vset, set(comp._valid_dict.keys()))

    def test_illegal_directory(self):
        logging.debug('')
        logging.debug('test_bad_directory')

        try:
            # Set an illegal execution directory, verify error.
            comp = Component()
            comp.directory = '/illegal'
            comp.cpath_updated()
        except ValueError, exc:
            msg = ": Illegal path '/illegal', not a descendant of"
            self.assertEqual(str(exc)[:len(msg)], msg)
        else:
            self.fail('Expected ValueError')

    def test_protected_directory(self):
        if sys.platform == 'win32':
            raise SkipTest("Windows box has permission problems with this test")

        logging.debug('')
        logging.debug('test_protected_directory')

        # Create a protected directory.
        directory = 'protected'
        if os.path.exists(directory):
            os.rmdir(directory)
        os.mkdir(directory)
        os.chmod(directory, 0)
        exe_dir = os.path.join(directory, 'xyzzy')
        try:
            # Attempt auto-creation of execution directory in protected area.
            comp = Component()
            comp.directory = exe_dir
            comp.cpath_updated()
        except OSError, exc:
            msg = ": Can't create execution directory"
            self.assertEqual(str(exc)[:len(msg)], msg)
        else:
            self.fail('Expected OSError')
        finally:
            os.chmod(directory, stat.S_IWUSR | stat.S_IWRITE | stat.S_IREAD)
            os.rmdir(directory)

    def test_file_in_place_of_directory(self):
        logging.debug('')
        logging.debug('test_file_in_place_of_directory')

        # Create a plain file.
        directory = 'plain_file'
        if os.path.exists(directory):
            os.remove(directory)
        out = open(directory, 'w')
        out.write('Hello world!\n')
        out.close()
        try:
            # Set execution directory to plain file.
            comp = Component()
            comp.directory = directory
            comp.cpath_updated()
        except ValueError, exc:
            path = os.path.join(os.getcwd(), directory)
            if sys.platform == 'win32':
                path = path.lower()
            self.assertEqual(str(exc),
                ": Execution directory path '%s' is not a directory."
                % path)
        else:
            self.fail('Expected ValueError')
        finally:
            os.remove(directory)

    def test_bad_new_directory(self):
        logging.debug('')
        logging.debug('test_bad_new_directory')

        comp = Component()
        comp.directory = '/illegal'
        try:
            comp.run()
        except ValueError, exc:
            msg = ": Illegal path '/illegal', not a descendant of"
            self.assertEqual(str(exc)[:len(msg)], msg)
        else:
            self.fail('Expected ValueError')

    def test_execute(self):
        comp = Component()
        try:
            comp.execute()
        except NotImplementedError as err:
            self.assertEqual(str(err), ".execute")
        else:
            self.fail('expected NotImplementedError')

    def test_run(self):
        comp = Component()
        try:
            comp.run()
        except NotImplementedError as err:
            self.assertEqual(str(err), ".execute")
        else:
            self.fail('expected NotImplementedError')

    def test_get_entry_group(self):
        self.assertEqual(_get_entry_group(Component()), 'openmdao.component')

    def test_setattr_dependency_invalidation(self):
        # i.e., comp should not need to re-run if you set an input to the same value.

        self.comp.set('x', 45.5)
        self.assertEqual(self.comp._valid_dict['xout'], False)
        self.comp.run()
        self.assertEqual(self.comp._valid_dict['xout'], True)
        self.comp.set('x', 45.5)
        self.assertEqual(self.comp._valid_dict['xout'], True)
        self.comp.set('x', 99.999)
        self.assertEqual(self.comp._valid_dict['xout'], False)

    def test_override(self):
        code = """\
class BadComponent(Component):
    run = Float(iotype='in')
"""
        assert_raises(self, code, globals(), locals(), NameError,
                      "BadComponent overrides attribute 'run' of Component",
                      use_exec=True)

        code = "Component.add_class_trait('run', Float(iotype='in'))"
        assert_raises(self, code, globals(), locals(), NameError,
                      "Would override attribute 'run' of Component")

        comp = Component()
        comp.add_trait('x', Float(iotype='in'))

        code = "comp.add_trait('run', Float(iotype='in'))"
        assert_raises(self, code, globals(), locals(), NameError,
                      "Would override attribute 'run' of Component")

    def test_mimic(self):
        # Ensure we can mimic a driver.
        top = Assembly()
        top.add('c1', Component())
        top.add('c2', Component())
        top.driver.workflow.add(('c1', 'c2'))
        top.driver.printvars = ['c1.force_execute', 'c2.force_execute']

        recorder1 = FakeRecorder()
        recorder2 = FakeRecorder()
        top.driver.recorders = [recorder1, recorder2]

        workflow_id = id(top.driver.workflow)
        new_driver = Driver()
        new_id = id(new_driver)
        self.assertNotEqual(new_id, id(top.driver))

        top.replace('driver', new_driver)
        self.assertEqual(new_id, id(top.driver))
        self.assertEqual(workflow_id, id(top.driver.workflow))
        self.assertEqual(top.driver.printvars,
                         ['c1.force_execute', 'c2.force_execute'])
        self.assertEqual(top.driver.recorders, [recorder1, recorder2])

    def test_replace(self):
        # Ensure we can replace a child component.
        c0 = Component()
        c1 = Component()
        c2 = Component()

        c0.add('c', c1)
        self.assertEqual(id(c0.c), id(c1))

        c0.replace('c', c2)
        self.assertEqual(id(c0.c), id(c2))

    def test_driver(self):
        # Ensure we can't add a Driver to a component that is not an Assembly.
        comp = Component()

        try:
            comp.add('driver', Driver())
        except Exception as err:
            self.assertEqual(str(err),
                "A Driver may only be added to an Assembly")
            pass


if __name__ == '__main__':
    unittest.main()
