import unittest
import StringIO

from openmdao.main.assembly import ExprMapper, Assembly, set_as_top
from openmdao.main.component import Component
from openmdao.main.datatypes.api import Int

_fakes = ['@xin', '@bin', '@bout', '@xout']
nodes = ['A', 'B', 'C', 'D', 'parent.X', 'parent.Y']

class Simple(Component):
    
    a = Int(iotype='in')
    b = Int(iotype='in')
    c = Int(iotype='out')
    d = Int(iotype='out')
    
    def __init__(self):
        super(Simple, self).__init__()
        self.a = 1
        self.b = 2
        self.c = 3
        self.d = -1

    def execute(self):
        self.c = self.a + self.b
        self.d = self.a - self.b

class ExprMapperTestCase(unittest.TestCase):

    def make_graph(self, nodes=(), connections=()):
        scope = set_as_top(Assembly())
        sub = scope.add('sub',Assembly())
        dep = ExprMapper(sub)
        for name in nodes:
            if name.startswith('parent.'):
                scope.add(name.split('.',1)[1], Simple())
            else:
                sub.add(name, Simple())
                dep.add(name)
        for src,dest in connections:
            if '.' not in src and not sub.contains(src):
                if dest.startswith('parent.'):
                    iotype='out'
                else:
                    iotype='in'
                sub.add(src, Int(1, iotype=iotype))
            if '.' not in dest and not sub.contains(dest):
                if src.startswith('parent.'):
                    iotype='in'
                else:
                    iotype='out'
                sub.add(dest, Int(1, iotype=iotype))
            dep.connect(src, dest, sub)
        return dep, sub

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
        dep, scope = self.make_graph(nodes, 
                                     self.internal_conns+
                                     self.boundary_conns+
                                     self.cross_conns)
        
        self.assertEqual(dep.get_source('B.a'), 'a')
        self.assertEqual(dep.get_source('A.a'), None)
        self.assertEqual(dep.get_source('a'), 'parent.X.c')
        self.assertEqual(dep.get_source('c'), 'D.c')

    def test_add(self):
        for name in nodes:
            if not name.startswith('parent.'):
                self.assertTrue(name in self.dep)
        #for name in _fakes:
            #self.assertTrue(name in self.dep)
        
    def test_remove(self):
        self.dep.remove('B')
        self.assertTrue('B' not in self.dep)
        
    def test_list_connections(self):
        self.assertEqual(set(self.dep.list_connections()), 
                         set([('A.c','B.b'), ('B.c','D.a'), ('C.c','D.b'), ('a','B.a'), ('D.c','c')]))
        self.assertEqual(set(self.dep.list_connections(show_passthrough=False)), 
                         set([('A.c','B.b'), ('B.c','D.a'), ('C.c','D.b')]))
    
    def test_get_connected_inputs(self):
        self.assertEqual(set(self.dep.get_connected_inputs()), set(['a','A.b']))
    
    def test_get_connected_outputs(self):
        self.assertEqual(set(self.dep.get_connected_outputs()), set(['c', 'C.d']))
    
    def test_already_connected(self):
        # internal connection
        try:
            self.dep.check_connect('A.d', 'B.a', self.scope)
        except Exception as err:
            self.assertEqual(str(err), "sub: 'B.a' is already connected to source 'a'")
        else:
            self.fail('Exception expected')
            
        # input boundary connection
        try:
            self.dep.check_connect('parent.foo.bar', 'a', self.scope)
        except Exception as err:
            self.assertEqual(str(err), "sub: 'a' is already connected to source 'parent.X.c'")
        else:
            self.fail('Exception expected')

        # internal to boundary output connection
        try:
            self.dep.check_connect('B.d', 'c', self.scope)
        except Exception as err:
            self.assertEqual(str(err), "sub: 'c' is already connected to source 'D.c'")
        else:
            self.fail('Exception expected')

    def test_connections_to(self):
        self.assertEqual(set(self.dep.connections_to('c')),
                         set([('c','parent.Y.a'),
                              ('D.c', 'c')]))
        self.assertEqual(set(self.dep.connections_to('a')),
                         set([('parent.X.c','a'),
                              ('a','B.a')]))
        
        self.dep.connect('A.c', 'C.b', self.scope)
        self.dep.connect('A.c', 'C.a', self.scope)
        self.assertEqual(set(self.dep.connections_to('A.c')),
                         set([('A.c','C.b'),('A.c','C.a'),('A.c','B.b')]))
        
        # unconnected var should return an empty list
        self.assertEqual(self.dep.connections_to('A.a'),[])

        # now test component connections
        self.assertEqual(set(self.dep.connections_to('A')),
                         set([('parent.X.d','A.b'),
                              ('A.c','B.b'),
                              ('A.c','C.a'),
                              ('A.c','C.b')]))

        self.assertEqual(set(self.dep.connections_to('D')),
                         set([('B.c','D.a'),
                              ('C.c','D.b'),
                              ('D.c','c')]))

    def test_disconnect(self):
        self.dep.disconnect('a') # this should disconnect extern to a and 
                                 # a to B.a, completely removing the
                                 # link between @bin and B.
        
        # now if we delete the auto passthrough from parent.X.d to A.b,
        # there should be no link at all between @xin and @bin, or between
        # @bin and A.
        self.dep.disconnect('parent.X.d', 'A.b')
        
        # now test a similar situation on the output side
        self.dep.disconnect('c')
        
        self.dep.disconnect('C.d', 'parent.Y.b')
        
    def test_find_all_connecting(self):
        dep = ExprMapper(self.scope)
        for node in ['A','B','C','D','E','F']:
            dep.add(node)
            if not self.scope.contains(node):
                self.scope.add('F', Simple())
        self.assertEqual(dep.find_all_connecting('A','F'), set())
        dep.check_connect('A.c', 'B.a', self.scope)
        dep.check_connect('B.c', 'C.a', self.scope)
        dep.check_connect('C.d', 'D.a', self.scope)
        dep.check_connect('A.d', 'D.b', self.scope)
        dep.check_connect('A.d', 'F.b', self.scope)
        
        dep.connect('A.c', 'B.a', self.scope)
        dep.connect('B.c', 'C.a', self.scope)
        dep.connect('C.d', 'D.a', self.scope)
        dep.connect('A.d', 'D.b', self.scope)
        dep.connect('A.d', 'F.b', self.scope)
        self.assertEqual(dep.find_all_connecting('A','F'), set(['A','F']))
        self.assertEqual(dep.find_all_connecting('A','D'), set(['A','B','C','D']))
        dep.check_connect('C.d', 'F.a', self.scope)
        dep.connect('C.d', 'F.a', self.scope)
        self.assertEqual(dep.find_all_connecting('A','F'), set(['A','B','C','F']))
        
    def test_find_referring_exprs(self):
        self.assertEqual(set(self.dep.find_referring_exprs('A')),
                         set(['A.c','A.b']))
        self.assertEqual(set(self.dep.find_referring_exprs('parent')),
                         set(['parent.X.c','parent.X.d','parent.Y.a','parent.Y.b']))
    
    def test_get_invalidated_destexprs(self):
        self.assertEqual(set([e.text for e in self.dep._get_invalidated_destexprs(self.scope, None, ['a','c'])]),
                         set(['B.a','parent.Y.a']))
        self.assertEqual(set([e.text for e in self.dep._get_invalidated_destexprs(self.scope, 'A', None)]),
                         set(['B.b']))
        self.assertEqual(set([e.text for e in self.dep._get_invalidated_destexprs(self.scope, 'C', None)]),
                         set(['D.b','parent.Y.b']))
        self.assertEqual(set([e.text for e in self.dep._get_invalidated_destexprs(self.scope, 'C', ['c'])]),
                         set(['D.b']))
        self.assertEqual(set([e.text for e in self.dep._get_invalidated_destexprs(self.scope, 'C', ['d'])]),
                         set(['parent.Y.b']))
        self.assertEqual(set([e.text for e in self.dep._get_invalidated_destexprs(self.scope, 'D', ['d'])]),
                         set())
        self.assertEqual(set([e.text for e in self.dep._get_invalidated_destexprs(self.scope, 'parent', ['X.c'])]),
                         set(['a']))
        self.assertEqual(set([e.text for e in self.dep._get_invalidated_destexprs(self.scope, 'parent', ['X.d'])]),
                         set(['A.b']))
    
    def test_expressions(self):
        dep, scope = self.make_graph(['E', 'A', 'B'], [])
        dep.add('E')
        dep.connect('parent.X.d+a', 'E.a[3]', scope)
        dep.connect('A.c', 'E.a[4]', scope)
        dep.connect('B.c', 'E.b', scope)
        self.assertEqual(set(dep._depgraph.var_in_edges('E')),
                         set([('@bin.E.a[3]','E.a[3]'),
                              ('@bin.a','E.a[3]'),
                              ('A.c', 'E.a[4]'),
                              ('B.c', 'E.b')]))


if __name__ == "__main__":
    unittest.main()


