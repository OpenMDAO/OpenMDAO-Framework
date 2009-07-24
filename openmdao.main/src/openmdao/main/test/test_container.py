# pylint: disable-msg=C0111,C0103

import unittest
import StringIO

from enthought.traits.api import Float, TraitError

import openmdao.util.save_load as constants
from openmdao.main.container import Container
from openmdao.main.interfaces import IContainer


class ContainerTestCase(unittest.TestCase):

    def setUp(self):
        """This sets up the following hierarchy of Containers:
        
                       root
                       /  \
                     c1    c2
                          /  \
                        c21  c22
                             /
                          c221
                          /
                        number
        """
        
        self.root = Container('root', None)
        c1 = Container('c1', self.root)
        c2 = Container('c2', self.root)
        c21 = Container('c21', c2)
        c22 = Container('c22', c2)
        c221 = Container('c221', c22)
        c221.add_trait('number', Float(3.14, iostatus='in'))

    def tearDown(self):
        """this teardown function will be called after each test"""
        self.root = None

    def test_add_bad_child(self):
        foo = Container('foo', None)
        non_container = 'some string'
        try:
            foo.add_child(non_container)
        except TypeError, err:
            self.assertEqual(str(err), "foo: '<type 'str'>' "+
                "object is not an instance of Container.")
        else:
            self.fail('TypeError expected')
        
    def test_pathname(self):
        foo = Container('foo', None)
        self.root.add_child(foo)
        self.assertEqual(foo.get_pathname(), 'root.foo')

    def test_get(self):
        obj = self.root.get('c2.c21')
        self.assertEqual(obj.get_pathname(), 'root.c2.c21')
        num = self.root.get('c2.c22.c221.number')
        self.assertEqual(num, 3.14)

    def test_get_attribute(self):
        self.assertEqual(self.root.get('c2.c22.c221').trait('number').iostatus, 
                         'in')

    def test_keys(self):
        lst = [x for x in self.root.keys(recurse=True)]
        self.assertEqual(lst, 
            ['c2', 'c2.c22', 'c2.c22.c221', 'c2.c22.c221.number', 'c2.c21', 'c1'])
        lst = [x for x in self.root.keys(recurse=True, iostatus='in')]
        self.assertEqual(lst, ['c2.c22.c221.number'])
        
    def test_full_items(self):
        lst = map(lambda x: x[0], self.root.items(recurse=True))
        self.assertEqual(lst,
            ['c2', 'c2.c22', 'c2.c22.c221', 'c2.c22.c221.number', 'c2.c21', 'c1'])
        
        items = [(x[0],isinstance(x[1],Container) or str(x[1])) 
                    for x in self.root.items(recurse=True)]
        
        # values of True in the list below just indicate that the value
        # is a Container
        self.assertEqual(items, [('c2', True), 
                                 ('c2.c22', True), 
                                 ('c2.c22.c221', True), 
                                 ('c2.c22.c221.number', '3.14'), 
                                 ('c2.c21', True), 
                                 ('c1', True)])
        
    def test_bad_get(self):
        try:
            x = self.root.bogus
        except AttributeError, err:
            self.assertEqual(str(err),"'Container' object has no attribute 'bogus'")
        else:
            self.fail('AttributeError expected')

    def test_iteration(self):
        names = [x.get_pathname() for x in self.root.values(recurse=True)
                                         if isinstance(x, Container)]
        self.assertEqual(sorted(names),
                         ['root.c1', 'root.c2', 'root.c2.c21', 
                          'root.c2.c22', 'root.c2.c22.c221'])
        
        names = [x.get_pathname() for x in self.root.values()
                                         if isinstance(x, Container)]
        self.assertEqual(sorted(names), ['root.c1', 'root.c2'])
        
        names = [x.get_pathname() for x in self.root.values(recurse=True)
                                 if isinstance(x, Container) and x.parent==self.root]
        self.assertEqual(sorted(names), ['root.c1', 'root.c2'])        

        names = [x.get_pathname() for x in self.root.values(recurse=True)
                                 if isinstance(x, Container) and x.parent==self.root.c2]
        self.assertEqual(sorted(names), ['root.c2.c21', 'root.c2.c22'])        

    def test_create(self):
        new_obj = self.root.create('openmdao.main.component.Component','mycomp')
        self.assertEqual(new_obj.__class__.__name__, 'Component')
 
    # TODO: all of these save/load test functions need to do more checking
    #       to verify that the loaded thing is equivalent to the saved thing
    
    def test_save_load_yaml(self):
        output = StringIO.StringIO()
        c1 = Container('c1', None)
        c2 = Container('c2', None)
        c1.add_child(c2)
        c1.save(output, constants.SAVE_YAML)
        
        inp = StringIO.StringIO(output.getvalue())
        newc1 = Container.load(inp, constants.SAVE_YAML)
                
    def test_save_load_libyaml(self):
        output = StringIO.StringIO()
        c1 = Container('c1', None)
        c2 = Container('c2', None)
        c1.add_child(c2)
        c1.save(output, constants.SAVE_LIBYAML)
        
        inp = StringIO.StringIO(output.getvalue())
        newc1 = Container.load(inp, constants.SAVE_LIBYAML)
                
    def test_save_load_cpickle(self):
        output = StringIO.StringIO()
        c1 = Container('c1', None)
        c2 = Container('c2', None)
        c1.add_child(c2)
        c1.save(output)
        
        inp = StringIO.StringIO(output.getvalue())
        newc1 = Container.load(inp)
        
    def test_save_load_pickle(self):
        output = StringIO.StringIO()
        c1 = Container('c1', None)
        c2 = Container('c2', None)
        c1.add_child(c2)
        c1.save(output, constants.SAVE_PICKLE)
        
        inp = StringIO.StringIO(output.getvalue())
        newc1 = Container.load(inp, constants.SAVE_PICKLE)
                

if __name__ == "__main__":
    unittest.main()

