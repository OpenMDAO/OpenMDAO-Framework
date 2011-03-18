"""
Test of Component.
"""

import logging
import os.path
import sys
import stat
import unittest

from nose import SkipTest

from enthought.traits.api import TraitError

from openmdao.main.api import Component, Container
from openmdao.lib.datatypes.api import Float
from openmdao.main.container import _get_entry_group


class MyComponent(Component):
    x = Float(1., iotype='in')
    xout = Float(2., iotype='out')
    
    def __init__(self, *args, **kwargs):
        super(MyComponent, self).__init__(*args, **kwargs)
        self.add('cont', Container())
        self.cont.add_trait('dyntrait', Float(3.))
    
    def execute(self):
        self.xout = self.x * 2.
        
        
class TestCase(unittest.TestCase):
    """ Test of Component. """

    def setUp(self):
        self.comp = MyComponent()

    def test_get_valid(self):
        comp = MyComponent()
        valids = comp.get_valid(['x','xout'])
        self.assertEqual(valids, [True, False])
        try:
            comp.get_valid(['x', 'foobar'])
        except KeyError as err:
            self.assertEqual(str(err), "'foobar'")
        else:
            self.fail("Expected KeyError")

    def test_set_valid(self):
        comp = self.comp
        valids = comp.get_valid(['x','xout'])
        self.assertEqual(valids, [True, False])
        comp.set_valid(['x','xout'], True)
        newvalids = comp.get_valid(['x','xout'])
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
            comp = Component(directory='/illegal')
            comp.tree_rooted()
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
            comp = Component(directory=exe_dir)
            comp.tree_rooted()
        except OSError, exc:
            msg = ": Can't create execution directory"
            self.assertEqual(str(exc)[:len(msg)], msg)
        else:
            self.fail('Expected OSError')
        finally:
            os.chmod(directory, stat.S_IWUSR|stat.S_IWRITE|stat.S_IREAD)
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
            comp = Component(directory=directory)
            comp.tree_rooted()
        except ValueError, exc:
            path = os.path.join(os.getcwd(), directory)
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
        


if __name__ == '__main__':
    unittest.main()

