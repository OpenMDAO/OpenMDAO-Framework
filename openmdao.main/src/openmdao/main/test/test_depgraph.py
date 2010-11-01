import unittest
from nose import SkipTest

from openmdao.main.depgraph import DependencyGraph

class DepGraphTestCase(unittest.TestCase):

    def setUp(self):
        self.depgraph = DependencyGraph()

    def test_contains(self):
        raise SkipTest()
    
    def test_len(self):
        raise SkipTest()
        
    def test_subgraph(self):
        raise SkipTest()
    
    def test_copy_graph(self):
        raise SkipTest()
    
    def test_get_source(self):
        raise SkipTest()

    def test_add(self):
        raise SkipTest()

    def test_remove(self):
        raise SkipTest()
        
    def test_invalidate_deps(self): 
        raise SkipTest()

    def test_list_connections(self):
        raise SkipTest()
    
    def test_in_map(self):
        raise SkipTest()
            
    def test_get_mapping(self):
        raise SkipTest()

    def test_in_links(self):
        raise SkipTest()
    
    def test_out_links(self):
        raise SkipTest()

    def var_edges(self):
        raise SkipTest()
    
    def test_var_in_edges(self):
        raise SkipTest()
    
    def test_get_connected_inputs(self):
        raise SkipTest()
    
    def test_get_connected_outputs(self):
        raise SkipTest()
    
    def test_connect(self):
        raise SkipTest()

    def test_comp_connections(self):
        raise SkipTest()
    
    def test_var_connections(self):
        raise SkipTest()
    
    def test_connections_to(self):
        raise SkipTest()

    def test_disconnect(self):
        raise SkipTest()
        
        
if __name__ == "__main__":
    unittest.main()


