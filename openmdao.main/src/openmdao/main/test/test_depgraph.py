import unittest
from nose import SkipTest

from openmdao.main.depgraph import DependencyGraph

_fakes = ['@exin', '@bin', '@bout', '@exout']
nodes = ['A', 'B', 'C', 'D']

def skip():
    #raise SkipTest()
    pass


class DepGraphTestCase(unittest.TestCase):

    def setUp(self):
        self.dep = dep = DependencyGraph()
        for name in nodes:
            dep.add(name)

        # add some internal connections
        dep.connect('A.c', 'B.a')
        dep.connect('B.c', 'C.a')
        dep.connect('C.c', 'D.a')
        
        dep.connect('parent.X.c', 'bound_a')
        dep.connect('D.c', 'bound_c')
        dep.connect('bound_c', 'parent.Y.a')

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
        skip()
    
    def test_in_map(self):
        skip()
            
    def test_get_mapping(self):
        skip()

    def test_in_links(self):
        skip()
    
    def test_out_links(self):
        skip()

    def var_edges(self):
        skip()
    
    def test_var_in_edges(self):
        skip()
    
    def test_get_connected_inputs(self):
        skip()
    
    def test_get_connected_outputs(self):
        skip()
    
    def test_connect(self):
        skip()

    def test_comp_connections(self):
        skip()
    
    def test_var_connections(self):
        skip()
    
    def test_connections_to(self):
        skip()

    def test_disconnect(self):
        skip()
        
        
if __name__ == "__main__":
    unittest.main()


