import unittest

from openmdao.main.container import Container
from openmdao.main.interfaces import IContainer
import openmdao.main.factorymanager as factorymanager
from openmdao.main.importfactory import ImportFactory

import openmdao.main.containervar

class ContainerTestCase(unittest.TestCase):

    def setUp(self):
        """this setup function will be called before each test in this class"""
        
        # build a simple hierarchy of Containers
        self.root = Container('root', None)
        c1 = Container('c1', None)
        c2 = Container('c2', None)
        self.root.add_child(c1)
        self.root.add_child(c2)        
        c21 = Container('c21', None)
        c22 = Container('c22', None)
        c2.add_child(c21)
        c2.add_child(c22)
        c221 = Container('c221', None)
        c22.add_child(c221)

    def tearDown(self):
        """this teardown function will be called after each test in this class"""
        self.root = None

    def test_add_child(self):
        foo = Container('foo', None)
        non_container = 'some string'
        try:
            foo.add_child(non_container)
        except TypeError, err:
            self.assertEqual(str(err),"foo: '<type 'str'>' object has does not provide the IContainer interface")
            return
        self.fail('exception expected')
        
    def test_pathname(self):
        foo = Container('foo', None)
        self.root.add_child(foo)
        self.assertEqual(foo.get_pathname(), 'root.foo')


    def test_get(self):
        obj = self.root.get('c2.c21')
        self.assertEqual(obj.get_pathname(), 'root.c2.c21')


    def test_bad_get(self):
        try:
            obj = self.root.get('bogus')
        except AttributeError, err:
            self.assertEqual(str(err),"object 'root' has no attribute 'bogus'")
            return
        self.fail('bogus object get did not raise exception')


    def test_get_objs(self):
        objs = self.root.get_objs(IContainer, recurse=True)
        names = [x.get_pathname() for x in objs]
        self.assertEqual(names, ['root.c1', 'root.c2', 'root.c2.c21', 'root.c2.c22', 
                                 'root.c2.c22.c221'])
        
        objs = self.root.get_objs(IContainer)
        names = [x.get_pathname() for x in objs]
        self.assertEqual(names, ['root.c1', 'root.c2'])
        
        objs = self.root.get_objs(IContainer, recurse=True, _parent=self.root)
        names = [x.get_pathname() for x in objs]
        self.assertEqual(names, ['root.c1', 'root.c2'])        

        objs = self.root.get_objs(IContainer, recurse=True, _parent=self.root.get('c2'))
        names = [x.get_pathname() for x in objs]
        self.assertEqual(names, ['root.c2.c21', 'root.c2.c22'])        


    def test_get_names(self):
        names = self.root.get_names(IContainer, recurse=True)
        self.assertEqual(names, ['root.c1','root.c2', 'root.c2.c21',
                                 'root.c2.c22', 'root.c2.c22.c221'])
        
        names = self.root.get_names(IContainer)
        self.assertEqual(names, ['root.c1', 'root.c2'])
        
        names = self.root.get_names(IContainer, recurse=True, _parent=self.root)
        self.assertEqual(names, ['root.c1', 'root.c2'])        

        names = self.root.get_names(IContainer, recurse=True, _parent=self.root.get('c2'))
        self.assertEqual(names, ['root.c2.c21', 'root.c2.c22'])        


    def test_create(self):
        factorymanager.register_factory(ImportFactory())
        new_obj = self.root.create('openmdao.main.component.Component','mycomp')
        self.assertEqual(new_obj.__class__.__name__, 'Component')
        new_obj.run()

if __name__ == "__main__":
    unittest.main()

