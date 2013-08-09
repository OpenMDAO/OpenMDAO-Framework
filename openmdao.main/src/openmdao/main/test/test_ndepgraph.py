import unittest

from openmdao.main.ndepgraph import DependencyGraph, is_comp_node, is_driver_node,\
                                    is_var_node, is_subvar_node, is_pseudo_node, \
                                    is_param_pseudo_node, is_dangling

def fullpaths(cname, names):
    return ['.'.join([cname,n]) for n in names]


class DumbClass(object):
    def __init__(self, name, inputs=('a','b'), outputs=('c','d')):
        self.name = name
        self._inputs = inputs[:]
        self._outputs = outputs[:]

    def list_inputs(self):
        return self._inputs

    def list_outputs(self):
        return self._outputs

    def contains(self, name):
        return hasattr(self, name)


#
# tests TODO:
#
# boundary conections to a sub-assembly
#    - normal variable
#    - array entry
#    - vartree
#    - input and output


class DepGraphTestCase(unittest.TestCase):

    def make_graph(self, comps=(), variables=(), connections=()):
        scope = DumbClass('')
        dep = DependencyGraph()
        for comp in comps:
            if isinstance(comp, basestring):
                comp = DumbClass(comp)
            dep.add_component(comp.name, comp.list_inputs(), comp.list_outputs())
            setattr(scope, comp.name, comp)

        for v, iotype in variables:
            dep.add_boundary_var(v, iotype=iotype)

        for src, dest in connections:
            dep.connect(src, dest)

        return dep, scope

    def get_comp(self, name):
        return getattr(self.scope, name)

    def setUp(self):
        self.conns = [
            ('A.c[2]', 'B.a.x.y'),
            ('A.d.z', 'B.b[4]'),
            ('B.c', 'C.a'),
            ('B.d', 'C.b'),
        ]
        self.boundary_conns = [
            ('a', 'A.a'),
            ('C.c', 'c'),
            ('b[3]', 'A.b'),
            ('D.d', 'd.x'),
        ]
        self.comps = ['A','B','C','D']
        self.bvariables = [('a','in'), ('b','in'),
                          ('c','out'), ('d','out')]
        self.dep, self.scope = self.make_graph(self.comps,
                                               self.bvariables,
                                               self.conns+
                                               self.boundary_conns)

    def test_find_nodes(self):
        for finder in [self.dep.find_nodes, self.dep.find_nodes_iter]:
            self.assertEqual(set(finder(is_comp_node)),
                             set(self.comps))
            varnodes = set()
            for node in ['A','B','C','D']:
                for v in ['a','b','c','d']:
                    varnodes.add(node+'.'+v)
            varnodes.update(['a','b','c','d'])
    
            self.assertEqual(set(finder(is_var_node)),
                             varnodes)

    def test_add(self):
        for name in self.comps:
            self.assertTrue(name in self.dep)
            comp = self.get_comp(name)
            for inp in fullpaths(comp.name, comp.list_inputs()):
                self.assertTrue(inp in self.dep)
                # make sure edge exists
                self.dep[inp][name]
            for out in fullpaths(comp.name, comp.list_outputs()):
                self.assertTrue(out in self.dep)
                # make sure edge exists
                self.dep[name][out]
        
    def test_remove(self):
        comp = self.get_comp('B')
        self.dep.remove('B')
        self.assertTrue('B' not in self.dep)
        found = [inp for inp in fullpaths('B', comp.list_inputs())
                    if inp in self.dep]
        found.extend([out for out in 
                        fullpaths('B', comp.list_outputs())
                          if out in self.dep])
        self.assertEqual(found, [])
        
    def test_get_sources(self):
        self.assertEqual(self.dep.get_sources('B.a'), ['B.a.x.y'])
        self.assertEqual(self.dep.get_sources('A.a'), ['a'])
        self.assertEqual(self.dep.get_sources('a'), [])
        self.assertEqual(self.dep.get_sources('c'), ['C.c'])
        self.assertEqual(self.dep.get_sources('A.c'), ['A'])
        self.assertEqual(self.dep.get_sources('A.c[2]'), ['A.c'])
        self.assertEqual(self.dep.get_sources('B.b[4]'), ['A.d.z'])
        
    def test_get_base_var(self):
        self.assertEqual(self.dep.base_var('B.a'), 'B.a')
        self.assertEqual(self.dep.base_var('a'), 'a')
        self.assertEqual(self.dep.base_var('a.x'), 'a')
        self.assertEqual(self.dep.base_var('a.x.y'), 'a')
        self.assertEqual(self.dep.base_var('a.x[3].y'), 'a')
        self.assertEqual(self.dep.base_var('A.c[2]'), 'A.c')
        
    def test_is_attr_ref(self):
        self.assertEqual(self.dep._is_attr_ref('B.a'), False)
        self.assertEqual(self.dep._is_attr_ref('a'), False)
        self.assertEqual(self.dep._is_attr_ref('a.x'), True)
        self.assertEqual(self.dep._is_attr_ref('a.x.y'), True)
        self.assertEqual(self.dep._is_attr_ref('a.x.y[2]'), True)
        
    def test_list_connections(self):
        self.assertEqual(set(self.dep.list_connections()), 
                         set([('a','A.a'),('b[3]','A.b'),('A.c[2]','B.a.x.y'),
                              ('A.d.z','B.b[4]'),('B.c','C.a'),('B.d','C.b'),
                              ('C.c','c'),('D.d','d.x')]))

    def test_full_subgraph(self):
        sub = self.dep.full_subgraph(['A', 'B'])
        self.assertEqual(set(sub.nodes()), 
                         set(['A','A.a','A.b','A.c','A.d','A.c[2]','A.d.z',
                              'B','B.a','B.b','B.c','B.d', 'B.a.x.y', 'B.b[4]']))
        self.assertEqual(set(sub.edges()),
                         set([('A.a','A'),('A.b','A'),('A','A.c'),('A','A.d'),
                              ('B.a','B'),('B.b','B'),('B','B.c'),('B','B.d'),
                              ('A.c','A.c[2]'),('A.c[2]','B.a.x.y'),
                              ('B.a.x.y','B.a'),('A.d','A.d.z'),('A.d.z','B.b[4]'),
                              ('B.b[4]','B.b')]))
    
    # def test_get_connected_inputs(self):
    #     self.assertEqual(set(self.dep.get_connected_inputs()), set(['a','A.b']))
    
    # def test_get_connected_outputs(self):
    #     self.assertEqual(set(self.dep.get_connected_outputs()), set(['c', 'C.d']))
    
    def test_already_connected(self):
        # internal connection
        try:
            self.dep._check_connect('A.d', 'B.a')
        except Exception as err:
            self.assertEqual(str(err), "Can't connect 'A.d' to 'B.a'. 'B.a' is already connected to 'B.a.x.y'")
        else:
            self.fail('Exception expected')
           
        # internal to boundary output connection
        try:
            self.dep._check_connect('A.d', 'c')
        except Exception as err:
            self.assertEqual(str(err), "Can't connect 'A.d' to 'c'. 'c' is already connected to 'C.c'")
        else:
            self.fail('Exception expected')
    
        # TODO: input boundary connection

    def test_get_interior_connections(self):
        self.assertEqual(set(self.dep.get_interior_connections(['A', 'B', 'C', 'D'])),
                         set(self.conns))
    
    # def test_connections_to(self):
    #     self.assertEqual(set(self.dep.connections_to('c')),
    #                      set([('@bout.c','@xout.parent.Y.a'),
    #                           ('D.c', '@bout.c')]))
    #     self.assertEqual(set(self.dep.connections_to('a')),
    #                      set([('@xin.parent.X.c','@bin.a'),
    #                           ('@bin.a','B.a')]))
        
    #     self.dep.connect('A.c', 'C.b')
    #     self.dep.connect('A.c', 'C.a')
    #     self.assertEqual(set(self.dep.connections_to('A.c')),
    #                      set([('A.c','C.b'),('A.c','C.a'),('A.c','B.b')]))
        
    #     # unconnected var should return an empty list
    #     self.assertEqual(self.dep.connections_to('A.a'),[])

    #     # now test component connections
    #     self.assertEqual(set(self.dep.connections_to('A')),
    #                      set([('@bin.A.b','A.b'),
    #                           ('A.c','B.b'),
    #                           ('A.c','C.a'),
    #                           ('A.c','C.b')]))

    #     self.assertEqual(set(self.dep.connections_to('D')),
    #                      set([('B.c','D.a'),
    #                           ('C.c','D.b'),
    #                           ('D.c','@bout.c')]))

    # def test_var_edges(self):
    #     self.assertEqual(set(self.dep.var_edges()),
    #                      set([('@bout.C.d','@xout.parent.Y.b'),
    #                           ('@bout.c','@xout.parent.Y.a'),
    #                           ('A.c','B.b'), ('B.c','D.a'), ('C.c','D.b'),
    #                           ('D.c','@bout.c'),
    #                           ('C.d','@bout.C.d'),
    #                           ('@xin.parent.X.d','@bin.A.b'),
    #                           ('@xin.parent.X.c','@bin.a'),
    #                           ('@bin.a', 'B.a'), ('@bin.A.b', 'A.b')]))
    #     self.assertEqual(set(self.dep.var_edges('C')),
    #                      set([('C.c','D.b'), ('C.d','@bout.C.d')]))
    #     self.assertEqual(set(self.dep.var_edges('@xin')),
    #                      set([('@xin.parent.X.c','@bin.a'),
    #                           ('@xin.parent.X.d','@bin.A.b')]))
    #     self.assertEqual(self.dep.var_edges('@xout'),[])
    #     self.assertEqual(self.dep.var_edges('blah'),[])

    # def test_var_in_edges(self):
    #     self.assertEqual(set(self.dep.var_in_edges()),
    #                      set([('@bout.C.d','@xout.parent.Y.b'),
    #                           ('@bout.c','@xout.parent.Y.a'),
    #                           ('A.c','B.b'), ('B.c','D.a'), ('C.c','D.b'),
    #                           ('D.c','@bout.c'),
    #                           ('C.d','@bout.C.d'),
    #                           ('@xin.parent.X.d','@bin.A.b'),
    #                           ('@xin.parent.X.c','@bin.a'),
    #                           ('@bin.a', 'B.a'), ('@bin.A.b', 'A.b')]))
    #     self.assertEqual(set(self.dep.var_in_edges('B')),
    #                      set([('A.c','B.b'),('@bin.a','B.a')]))
    #     self.assertEqual(set(self.dep.var_in_edges('@xout')),
    #                      set([('@bout.C.d','@xout.parent.Y.b'),
    #                           ('@bout.c','@xout.parent.Y.a')]))
    #     self.assertEqual(self.dep.var_in_edges('@xin'),[])
    #     self.assertEqual(self.dep.var_in_edges('blah'),[])

    # def test_disconnect(self):
    #     self.dep.disconnect('a') # this should disconnect extern to a and 
    #                              # a to B.a, completely removing the
    #                              # link between @bin and B.
    #     link = self.dep.get_link('@xin', '@bin')
    #     self.assertTrue('a' not in link._dests)
    #     link = self.dep.get_link('@bin', 'B')
    #     self.assertEqual(link, None)
        
    #     # now if we delete the auto passthrough from parent.X.d to A.b,
    #     # there should be no link at all between @xin and @bin, or between
    #     # @bin and A.
    #     self.dep.disconnect('parent.X.d', 'A.b')
    #     link = self.dep.get_link('@xin', '@bin')
    #     self.assertEqual(link, None)
    #     link = self.dep.get_link('@bin', 'A')
    #     self.assertEqual(link, None)
        
    #     # now test a similar situation on the output side
    #     self.dep.disconnect('c')
    #     link = self.dep.get_link('@bout', '@xout')
    #     self.assertTrue('c' not in link._srcs)
        
    #     self.dep.disconnect('C.d', 'parent.Y.b')
    #     link = self.dep.get_link('@bout', '@xout')
    #     self.assertEqual(link, None)
    #     link = self.dep.get_link('C', '@bout')
    #     self.assertEqual(link, None)
        
    # def test_link(self):
    #     self.dep.connect('B.d', 'C.b')
    #     self.dep.connect('B.c', 'C.a')
    #     link = self.dep.get_link('B', 'C')
    #     self.assertEqual(set(link.get_srcs()), set(['c','d']))
    #     self.assertEqual(set(link.get_srcs('b')), set(['d']))
    #     self.assertEqual(link.get_srcs('foo'), [])

    #     self.assertEqual(set(link.get_dests()), set(['a','b']))
    #     self.assertEqual(set(link.get_dests('c')), set(['a']))
    #     self.assertEqual(link.get_dests('foo'), [])
        
    # def test_find_all_connecting(self):
    #     dep = DependencyGraph()
    #     for node in ['A','B','C','D','E','F']:
    #         dep.add(node)
    #     self.assertEqual(dep.find_all_connecting('A','F'), set())
    #     dep.connect('A.c', 'B.a')
    #     dep.connect('B.c', 'C.a')
    #     dep.connect('C.d', 'D.a')
    #     dep.connect('A.d', 'D.b')
    #     dep.connect('A.d', 'F.b')
    #     self.assertEqual(dep.find_all_connecting('A','F'), set(['A','F']))
    #     self.assertEqual(dep.find_all_connecting('A','D'), set(['A','B','C','D']))
    #     dep.connect('C.d', 'F.a')
    #     self.assertEqual(dep.find_all_connecting('A','F'), set(['A','B','C','F']))
        
    # def test_expr(self):
    #     dep, scope = self.make_graph(nodes=['B','C'], connections=[('3.4*B.d+2.3', 'C.b')])
    #     self.assertEqual(dep.list_connections(), [('3.4*B.d+2.3','C.b')])
    #     dep.disconnect('3.4*B.d+2.3', 'C.b')
    #     self.assertEqual(dep.list_connections(), [])
    #     dep, scope = self.make_graph(nodes=['B','C'], connections=[('3.4*B.d+2.3', 'C.b'),
    #                                                                ('3.4*B.d+2.3', 'C.a')])
    #     self.assertEqual(set(dep.list_connections()), set([('3.4*B.d+2.3','C.b'),('3.4*B.d+2.3','C.a')]))
    #     dep.disconnect('3.4*B.d+2.3', 'C.b')
    #     self.assertEqual(dep.list_connections(), [('3.4*B.d+2.3','C.a')])
        
    #     dep, scope = self.make_graph(nodes=['B','C'], connections=[('3.4*B.d+2.3', 'C.b'),
    #                                                                ('3.4*B.d+2.3', 'C.a')])
    #     dep.disconnect('3.4*B.d+2.3')
    #     self.assertEqual(dep.list_connections(), [])
        
    # def test_dump(self):
    #     s = StringIO.StringIO()
    #     self.dep.dump(s)
    #     lines = s.getvalue().split('\n')
    #     expected = ["@bin -> A",
    #                 "   A.b : ['b']",
    #                 "@bin -> B",
    #                 "   a : ['a']",
    #                 "@bout -> @xout",
    #                 "   c : ['parent.Y.a']",
    #                 "   C.d : ['parent.Y.b']",
    #                 "@xin -> @bin",
    #                 "   parent.X.c : ['a']",
    #                 "   parent.X.d : ['A.b']",
    #                 "A -> B",
    #                 "   c : ['b']",
    #                 "B -> D",
    #                 "   c : ['a']",
    #                 "C -> @bout",
    #                 "   d : ['C.d']",
    #                 "C -> D",
    #                 "   c : ['b']",
    #                 "D -> @bout",
    #                 "   c : ['c']"]

    #     for line, expect in zip(lines, expected):
    #         self.assertEqual(line, expect)
            

if __name__ == "__main__":
    unittest.main()


