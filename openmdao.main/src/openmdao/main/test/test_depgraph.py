import unittest
import StringIO

from openmdao.main.depgraph import DependencyGraph
from openmdao.main.expreval import ConnectedExprEvaluator

class DumbClass(object):
    def contains(self, name):
        return hasattr(self, name)

_fakes = ['@xin', '@bin', '@bout', '@xout']
nodes = ['A', 'B', 'C', 'D']


class DepGraphTestCase(unittest.TestCase):

    def make_graph(self, nodes=(), connections=()):
        scope = DumbClass()
        dep = DependencyGraph()
        for name in nodes:
            dep.add(name)
        for src,dest in connections:
            dep.connect(src, dest)
        return dep, scope

    def setUp(self):
        self.internal_conns = [
            ('A.c', 'B.b'),
            ('B.c', 'D.a'),
            ('C.c', 'D.b'),
            ]
        self.boundary_conns = [
            ('parent.X.c', 'a'),
            ('a', 'B.a'),
            ('D.c', 'c'),
            ('c', 'parent.Y.a'),
            ]
        self.cross_conns = [
            ('parent.X.d', 'A.b'),
            ('C.d', 'parent.Y.b'),
            ]
        self.dep, self.scope = self.make_graph(nodes, 
                                               self.internal_conns+
                                               self.boundary_conns+
                                               self.cross_conns)


    def test_get_source(self):
        self.assertEqual(self.dep.get_source('B.a'), 'a')
        self.assertEqual(self.dep.get_source('A.a'), None)
        self.assertEqual(self.dep.get_source('a'), 'parent.X.c')
        self.assertEqual(self.dep.get_source('c'), 'D.c')

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
                         set([('A.c','B.b'), ('B.c','D.a'), ('C.c','D.b'), ('a','B.a'), ('D.c','c')]))
        self.assertEqual(set(self.dep.list_connections(show_passthrough=False)), 
                         set([('A.c','B.b'), ('B.c','D.a'), ('C.c','D.b')]))
    
    def test_get_link(self):
        link = self.dep.get_link('C', 'B')
        self.assertEqual(link, None)
        link = self.dep.get_link('A', 'B')
        self.assertEqual(link._srcs.keys(), ['c'])

    def test_get_connected_inputs(self):
        self.assertEqual(set(self.dep.get_connected_inputs()), set(['a','A.b']))
    
    def test_get_connected_outputs(self):
        self.assertEqual(set(self.dep.get_connected_outputs()), set(['c', 'C.d']))
    
    def test_already_connected(self):
        # internal connection
        try:
            self.dep.check_connect('A.d', 'B.a')
        except Exception as err:
            self.assertEqual(str(err), "'B.a' is already connected to source 'a'")
        else:
            self.fail('Exception expected')
            
        # input boundary connection
        try:
            self.dep.check_connect('parent.foo.bar', 'a')
        except Exception as err:
            self.assertEqual(str(err), "'a' is already connected to source 'parent.X.c'")
        else:
            self.fail('Exception expected')

        # internal to boundary output connection
        try:
            self.dep.check_connect('B.d', 'c')
        except Exception as err:
            self.assertEqual(str(err), "'c' is already connected to source 'D.c'")
        else:
            self.fail('Exception expected')

    def test_connections_to(self):
        self.assertEqual(set(self.dep.connections_to('c')),
                         set([('@bout.c','@xout.parent.Y.a'),
                              ('D.c', '@bout.c')]))
        self.assertEqual(set(self.dep.connections_to('a')),
                         set([('@xin.parent.X.c','@bin.a'),
                              ('@bin.a','B.a')]))
        
        self.dep.connect('A.c', 'C.b')
        self.dep.connect('A.c', 'C.a')
        self.assertEqual(set(self.dep.connections_to('A.c')),
                         set([('A.c','C.b'),('A.c','C.a'),('A.c','B.b')]))
        
        # unconnected var should return an empty list
        self.assertEqual(self.dep.connections_to('A.a'),[])

        # now test component connections
        self.assertEqual(set(self.dep.connections_to('A')),
                         set([('@bin.A.b','A.b'),
                              ('A.c','B.b'),
                              ('A.c','C.a'),
                              ('A.c','C.b')]))

        self.assertEqual(set(self.dep.connections_to('D')),
                         set([('B.c','D.a'),
                              ('C.c','D.b'),
                              ('D.c','@bout.c')]))

    def test_var_edges(self):
        self.assertEqual(set(self.dep.var_edges()),
                         set([('@bout.C.d','@xout.parent.Y.b'),
                              ('@bout.c','@xout.parent.Y.a'),
                              ('A.c','B.b'), ('B.c','D.a'), ('C.c','D.b'),
                              ('D.c','@bout.c'),
                              ('C.d','@bout.C.d'),
                              ('@xin.parent.X.d','@bin.A.b'),
                              ('@xin.parent.X.c','@bin.a'),
                              ('@bin.a', 'B.a'), ('@bin.A.b', 'A.b')]))
        self.assertEqual(set(self.dep.var_edges('C')),
                         set([('C.c','D.b'), ('C.d','@bout.C.d')]))
        self.assertEqual(set(self.dep.var_edges('@xin')),
                         set([('@xin.parent.X.c','@bin.a'),
                              ('@xin.parent.X.d','@bin.A.b')]))
        self.assertEqual(self.dep.var_edges('@xout'),[])
        self.assertEqual(self.dep.var_edges('blah'),[])

    def test_var_in_edges(self):
        self.assertEqual(set(self.dep.var_in_edges()),
                         set([('@bout.C.d','@xout.parent.Y.b'),
                              ('@bout.c','@xout.parent.Y.a'),
                              ('A.c','B.b'), ('B.c','D.a'), ('C.c','D.b'),
                              ('D.c','@bout.c'),
                              ('C.d','@bout.C.d'),
                              ('@xin.parent.X.d','@bin.A.b'),
                              ('@xin.parent.X.c','@bin.a'),
                              ('@bin.a', 'B.a'), ('@bin.A.b', 'A.b')]))
        self.assertEqual(set(self.dep.var_in_edges('B')),
                         set([('A.c','B.b'),('@bin.a','B.a')]))
        self.assertEqual(set(self.dep.var_in_edges('@xout')),
                         set([('@bout.C.d','@xout.parent.Y.b'),
                              ('@bout.c','@xout.parent.Y.a')]))
        self.assertEqual(self.dep.var_in_edges('@xin'),[])
        self.assertEqual(self.dep.var_in_edges('blah'),[])

    def test_get_interior_edges(self):
        self.assertEqual(self.dep.get_interior_edges(set(['A', 'B', 'C', 'D'])),
                         set(self.internal_conns))
    
    def test_disconnect(self):
        self.dep.disconnect('a') # this should disconnect extern to a and 
                                 # a to B.a, completely removing the
                                 # link between @bin and B.
        link = self.dep.get_link('@xin', '@bin')
        self.assertTrue('a' not in link._dests)
        link = self.dep.get_link('@bin', 'B')
        self.assertEqual(link, None)
        
        # now if we delete the auto passthrough from parent.X.d to A.b,
        # there should be no link at all between @xin and @bin, or between
        # @bin and A.
        self.dep.disconnect('parent.X.d', 'A.b')
        link = self.dep.get_link('@xin', '@bin')
        self.assertEqual(link, None)
        link = self.dep.get_link('@bin', 'A')
        self.assertEqual(link, None)
        
        # now test a similar situation on the output side
        self.dep.disconnect('c')
        link = self.dep.get_link('@bout', '@xout')
        self.assertTrue('c' not in link._srcs)
        
        self.dep.disconnect('C.d', 'parent.Y.b')
        link = self.dep.get_link('@bout', '@xout')
        self.assertEqual(link, None)
        link = self.dep.get_link('C', '@bout')
        self.assertEqual(link, None)
        
    def test_link(self):
        self.dep.connect('B.d', 'C.b')
        self.dep.connect('B.c', 'C.a')
        link = self.dep.get_link('B', 'C')
        self.assertEqual(set(link.get_srcs()), set(['c','d']))
        self.assertEqual(set(link.get_srcs('b')), set(['d']))
        self.assertEqual(link.get_srcs('foo'), [])

        self.assertEqual(set(link.get_dests()), set(['a','b']))
        self.assertEqual(set(link.get_dests('c')), set(['a']))
        self.assertEqual(link.get_dests('foo'), [])
        
    def test_find_all_connecting(self):
        dep = DependencyGraph()
        for node in ['A','B','C','D','E','F']:
            dep.add(node)
        self.assertEqual(dep.find_all_connecting('A','F'), set())
        dep.connect('A.c', 'B.a')
        dep.connect('B.c', 'C.a')
        dep.connect('C.d', 'D.a')
        dep.connect('A.d', 'D.b')
        dep.connect('A.d', 'F.b')
        self.assertEqual(dep.find_all_connecting('A','F'), set(['A','F']))
        self.assertEqual(dep.find_all_connecting('A','D'), set(['A','B','C','D']))
        dep.connect('C.d', 'F.a')
        self.assertEqual(dep.find_all_connecting('A','F'), set(['A','B','C','F']))
        
    def test_expr(self):
        dep, scope = self.make_graph(nodes=['B','C'], connections=[('3.4*B.d+2.3', 'C.b')])
        self.assertEqual(dep.list_connections(), [('3.4*B.d+2.3','C.b')])
        dep.disconnect('3.4*B.d+2.3', 'C.b')
        self.assertEqual(dep.list_connections(), [])
        dep, scope = self.make_graph(nodes=['B','C'], connections=[('3.4*B.d+2.3', 'C.b'),
                                                                   ('3.4*B.d+2.3', 'C.a')])
        self.assertEqual(set(dep.list_connections()), set([('3.4*B.d+2.3','C.b'),('3.4*B.d+2.3','C.a')]))
        dep.disconnect('3.4*B.d+2.3', 'C.b')
        self.assertEqual(dep.list_connections(), [('3.4*B.d+2.3','C.a')])
        
        dep, scope = self.make_graph(nodes=['B','C'], connections=[('3.4*B.d+2.3', 'C.b'),
                                                                   ('3.4*B.d+2.3', 'C.a')])
        dep.disconnect('3.4*B.d+2.3')
        self.assertEqual(dep.list_connections(), [])
        
    def test_dump(self):
        s = StringIO.StringIO()
        self.dep.dump(s)
        lines = s.getvalue().split('\n')
        expected = ["@bin -> A",
                    "   A.b : ['b']",
                    "@bin -> B",
                    "   a : ['a']",
                    "@bout -> @xout",
                    "   c : ['parent.Y.a']",
                    "   C.d : ['parent.Y.b']",
                    "@xin -> @bin",
                    "   parent.X.c : ['a']",
                    "   parent.X.d : ['A.b']",
                    "A -> B",
                    "   c : ['b']",
                    "B -> D",
                    "   c : ['a']",
                    "C -> @bout",
                    "   d : ['C.d']",
                    "C -> D",
                    "   c : ['b']",
                    "D -> @bout",
                    "   c : ['c']"]

        for line, expect in zip(lines, expected):
            self.assertEqual(line, expect)
            

if __name__ == "__main__":
    unittest.main()


