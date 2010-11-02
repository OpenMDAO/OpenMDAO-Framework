import unittest
from nose import SkipTest

from openmdao.main.depgraph import DependencyGraph

_fakes = ['@exin', '@bin', '@bout', '@exout']
nodes = ['A', 'B', 'C', 'D']

def skip():
    pass
    #raise SkipTest()


class DepGraphTestCase(unittest.TestCase):

    def setUp(self):
        self.dep = dep = DependencyGraph()
        for name in nodes:
            dep.add(name)

        # add some internal connections
        dep.connect('A.c', 'B.a')
        dep.connect('B.c', 'C.a')
        dep.connect('C.c', 'D.a')
        
        # boundary connections
        dep.connect('parent.X.c', 'bound_a')
        dep.connect('D.c', 'bound_c')
        dep.connect('bound_c', 'parent.Y.a')
        
        # auto-passthroughs
        dep.connect('parent.X.d', 'B.b')
        dep.connect('C.d', 'parent.Y.b')

    def test_get_source(self):
        self.assertEqual(self.dep.get_source('B.a'), 'A.c')
        self.assertEqual(self.dep.get_source('C.a'), 'B.c')
        self.assertEqual(self.dep.get_source('D.a'), 'C.c')
        self.assertEqual(self.dep.get_source('D.b'), None)
        self.assertEqual(self.dep.get_source('bound_a'), 'parent.X.c')
        self.assertEqual(self.dep.get_source('bound_c'), 'D.c')

    def test_add(self):
        for name in nodes:
            self.assertTrue(name in self.dep)
        for name in _fakes:
            self.assertTrue(name in self.dep)
        
    def test_remove(self):
        self.dep.remove('B')
        self.assertTrue('B' not in self.dep)
        
    def test_invalidate_deps(self): 
        skip()

    def test_list_connections(self):
        self.assertEqual(set(self.dep.list_connections()), 
                         set([('A.c','B.a'),('C.c','D.a'),('B.c','C.a'),
                          ('D.c','bound_c')]))
        self.assertEqual(set(self.dep.list_connections(show_passthrough=False)), 
                         set([('A.c','B.a'),('C.c','D.a'),('B.c','C.a')]))
    
    def test_in_map(self):
        skip()
            
    def test_get_link(self):
        link = self.dep.get_link('A', 'D')
        self.assertEqual(link, None)
        link = self.dep.get_link('A', 'B')
        self.assertEqual(link._srcs.keys(), ['c'])

    def var_edges(self):
        skip()
    
    def test_var_in_edges(self):
        skip()
    
    def test_get_connected_inputs(self):
        self.assertEqual(set(self.dep.get_connected_inputs()), set(['bound_a','B.b']))
    
    def test_get_connected_outputs(self):
        self.assertEqual(set(self.dep.get_connected_outputs()), set(['bound_c', 'C.d']))
    
    def test_already_connected(self):
        # internal connection
        try:
            self.dep.connect('A.c', 'D.a')
        except Exception as err:
            self.assertEqual(str(err), 'D.a is already connected to source C.c')
        else:
            self.fail('Exception expected')
            
        # input boundary connection
        try:
            self.dep.connect('parent.foo.bar', 'bound_a')
        except Exception as err:
            self.assertEqual(str(err), 'bound_a is already connected to source parent.X.c')
        else:
            self.fail('Exception expected')

        # internal to boundary output connection
        try:
            self.dep.connect('B.d', 'bound_c')
        except Exception as err:
            self.assertEqual(str(err), 'bound_c is already connected to source D.c')
        else:
            self.fail('Exception expected')

    def test_connections_to(self):
        self.assertEqual(set(self.dep.connections_to('bound_c')),
                         set([('bound_c','parent.Y.a'),('D.c', 'bound_c')]))
        self.dep.connect('A.c', 'C.b')
        self.assertEqual(set(self.dep.connections_to('A.c')),
                         set([('A.c','C.b'),('A.c','B.a')]))
        self.assertEqual(self.dep.connections_to('A.a'),[])

    def test_disconnect(self):
        skip()
        
        
if __name__ == "__main__":
    unittest.main()


