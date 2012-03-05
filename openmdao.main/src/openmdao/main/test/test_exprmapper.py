import unittest
import StringIO

from openmdao.main.assembly import ExprMapper
from openmdao.main.component import Component

_fakes = ['@xin', '@bin', '@bout', '@xout']
nodes = ['A', 'B', 'C', 'D']


class ExprMapperTestCase(unittest.TestCase):

    def make_graph(self, nodes=(), connections=()):
        scope = Component()
        dep = ExprMapper(scope)
        for name in nodes:
            dep.add(name)
            setattr(scope, name, Component())
        for src,dest in connections:
            srcparts = src.split('.')
            destparts = dest.split('.')
            if srcparts[0] != 'parent':
                if len(srcparts) > 1:
                    if not getattr(scope, srcparts[0]).contains(srcparts[1]):
                        setattr(getattr(scope, srcparts[0]), srcparts[1], 1)
                else:
                    if not scope.contains(src):
                        setattr(scope, src, 1)
            if destparts[0] != 'parent':
                if len(destparts) > 1:
                    if not getattr(scope, destparts[0]).contains(destparts[1]):
                        setattr(getattr(scope, destparts[0]), destparts[1], 1)
                else:
                    if not scope.contains(dest):
                        setattr(scope, dest, 1)
                
            dep.check_connect(src, dest, scope)
            dep.connect(src, dest, scope)
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
    
    def test_get_connected_inputs(self):
        self.assertEqual(set(self.dep.get_connected_inputs()), set(['a','A.b']))
    
    def test_get_connected_outputs(self):
        self.assertEqual(set(self.dep.get_connected_outputs()), set(['c', 'C.d']))
    
    def test_already_connected(self):
        # internal connection
        try:
            self.dep.check_connect('A.d', 'B.a', self.scope)
        except Exception as err:
            self.assertEqual(str(err), ": 'B.a' is already connected to source 'a'")
        else:
            self.fail('Exception expected')
            
        # input boundary connection
        try:
            self.dep.check_connect('parent.foo.bar', 'a', self.scope)
        except Exception as err:
            self.assertEqual(str(err), ": 'a' is already connected to source 'parent.X.c'")
        else:
            self.fail('Exception expected')

        # internal to boundary output connection
        try:
            self.dep.check_connect('B.d', 'c', self.scope)
        except Exception as err:
            self.assertEqual(str(err), ": 'c' is already connected to source 'D.c'")
        else:
            self.fail('Exception expected')

    def test_connections_to(self):
        self.assertEqual(set(self.dep.connections_to('c')),
                         set([('@bout.c','@xout.parent.Y.a'),
                              ('D.c', '@bout.c')]))
        self.assertEqual(set(self.dep.connections_to('a')),
                         set([('@xin.parent.X.c','@bin.a'),
                              ('@bin.a','B.a')]))
        
        self.dep.connect('A.c', 'C.b', self.scope)
        self.dep.connect('A.c', 'C.a', self.scope)
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
        self.assertEqual(dep.find_all_connecting('A','F'), set())
        dep.connect('A.c', 'B.a', self.scope)
        dep.connect('B.c', 'C.a', self.scope)
        dep.connect('C.d', 'D.a', self.scope)
        dep.connect('A.d', 'D.b', self.scope)
        dep.connect('A.d', 'F.b', self.scope)
        self.assertEqual(dep.find_all_connecting('A','F'), set(['A','F']))
        self.assertEqual(dep.find_all_connecting('A','D'), set(['A','B','C','D']))
        dep.connect('C.d', 'F.a', self.scope)
        self.assertEqual(dep.find_all_connecting('A','F'), set(['A','B','C','F']))
        
            
    def test_expressions(self):
        dep, scope = self.make_graph(['E', 'A', 'B'], [])
        dep.add('E')
        dep.connect('parent.X.d+a', 'E.a[3]', scope)
        dep.connect('A.c', 'E.a[4]', scope)
        dep.connect('B.c', 'E.b', scope)

if __name__ == "__main__":
    unittest.main()


