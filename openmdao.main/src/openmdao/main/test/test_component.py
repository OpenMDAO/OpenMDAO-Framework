"""
Test of Component.
"""

import logging
import os.path
import sys
import stat
import unittest

from nose import SkipTest

from openmdao.main.api import Component, Container, Driver
from openmdao.main.interfaces import ICaseRecorder, implements
from openmdao.main.datatypes.api import Float, Array
from openmdao.main.container import _get_entry_group
from openmdao.util.testutil import assert_raises


class MyComponent(Component):
    x = Float(1., iotype='in')
    xreq = Float(iotype='in')
    areq = Array(iotype='in')
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

    def test_illegal_directory(self):
        logging.debug('')
        logging.debug('test_illegal_directory')

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
        else:
            self.fail("Exception expected")


if __name__ == '__main__':
    unittest.main()
