import unittest
import StringIO

from openmdao.main.depgraph import DependencyGraph

_fakes = ['@xin', '@bin', '@bout', '@xout']
nodes = ['A', 'B', 'C', 'D']


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
        
    def test_list_connections(self):
        self.assertEqual(set(self.dep.list_connections()), 
                         set([('A.c','B.a'),('C.c','D.a'),('B.c','C.a'),
                          ('D.c','bound_c')]))
        self.assertEqual(set(self.dep.list_connections(show_passthrough=False)), 
                         set([('A.c','B.a'),('C.c','D.a'),('B.c','C.a')]))
    
    def test_get_link(self):
        link = self.dep.get_link('A', 'D')
        self.assertEqual(link, None)
        link = self.dep.get_link('A', 'B')
        self.assertEqual(link._srcs.keys(), ['c'])

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
        self.dep.connect('bound_a', 'A.a')

        self.assertEqual(set(self.dep.connections_to('bound_c')),
                         set([('bound_c','parent.Y.a'),('D.c', 'bound_c')]))
        self.assertEqual(set(self.dep.connections_to('bound_a')),
                         set([('parent.X.c','bound_a'),('bound_a','A.a')]))
        
        self.dep.connect('A.c', 'C.b')
        self.assertEqual(set(self.dep.connections_to('A.c')),
                         set([('A.c','C.b'),('A.c','B.a')]))
        
        # unconnected var should return an empty list
        self.assertEqual(self.dep.connections_to('D.b'),[])

        # now test component connections
        self.assertEqual(set(self.dep.connections_to('B')),
                         set([]))

    def test_disconnect(self):
        self.dep.connect('bound_a', 'A.a')
        self.dep.disconnect('bound_a') # this should disconnect extern to bound_a and 
                                       # bound_a to A.a, completely removing the
                                       # linke between @bin and A.
        link = self.dep.get_link('@xin', '@bin')
        self.assertTrue('bound_a' not in link._dests)
        link = self.dep.get_link('@bin', 'A')
        self.assertEqual(link, None)
        
        # now if we delete the auto passthrough from parent.X.d to B.b,
        # there should be no link at all between @xin and @bin, or between
        # @bin and B.
        self.dep.disconnect('parent.X.d', 'B.b')
        link = self.dep.get_link('@xin', '@bin')
        self.assertEqual(link, None)
        link = self.dep.get_link('@bin', 'B')
        self.assertEqual(link, None)
        
        # not test a similar situation on the output side
        self.dep.disconnect('bound_c')
        link = self.dep.get_link('@bout', '@xout')
        self.assertTrue('bound_c' not in link._srcs)
        
        self.dep.disconnect('C.d', 'parent.Y.b')
        link = self.dep.get_link('@bout', '@xout')
        self.assertEqual(link, None)
        link = self.dep.get_link('C', '@bout')
        self.assertEqual(link, None)
        
    def test_link(self):
        self.dep.connect('B.d', 'C.b')
        link = self.dep.get_link('B', 'C')
        self.assertEqual(set(link.get_srcs()), set(['c','d']))
        self.assertEqual(set(link.get_srcs('b')), set(['d']))
        self.assertEqual(link.get_srcs('foo'), [])

        self.assertEqual(set(link.get_dests()), set(['a','b']))
        self.assertEqual(set(link.get_dests('c')), set(['a']))
        self.assertEqual(link.get_dests('foo'), [])
        
    def test_dump(self):
        s = StringIO.StringIO()
        self.dep.dump(s)
        lines = s.getvalue().split('\n')
        expected = ["A -> B",
                    "   c : ['a']",
                    "C -> @bout",
                    "   d : ['C.d']",
                    "C -> D",
                    "   c : ['a']",
                    "B -> C",
                    "   c : ['a']",
                    "D -> @bout",
                    "   c : ['bound_c']",
                    "@bin -> B",
                    "   B.b : ['b']",
                    "@xin -> @bin",
                    "   parent.X.c : ['bound_a']",
                    "   parent.X.d : ['B.b']",
                    "@bout -> @xout",
                    "   bound_c : ['parent.Y.a']",
                    "   C.d : ['parent.Y.b']"]
        for line, expect in zip(lines, expected):
            self.assertEqual(line, expect)

if __name__ == "__main__":
    unittest.main()


