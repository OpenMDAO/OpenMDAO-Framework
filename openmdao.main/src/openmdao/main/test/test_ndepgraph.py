import unittest

from openmdao.main.ndepgraph import DependencyGraph, is_comp_node, is_driver_node,\
                                    is_var_node, is_subvar_node, is_pseudo_node, \
                                    is_param_pseudo_node, is_attr_node, base_var, TransClosure

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
        self.ext_conns = [
            ('parent.C1.d', 'a'),
            ('parent.C0.c', 'D.b'),
            ('c', 'parent.C2.a'),
            ('D.d', 'parent.C3.a'),
        ]
        self.comps = ['A','B','C','D']
        self.bvariables = [('a','in'), ('b','in'),
                          ('c','out'), ('d','out')]
        self.dep, self.scope = self.make_graph(self.comps,
                                               self.bvariables,
                                               self.conns +
                                               self.boundary_conns +
                                               self.ext_conns)

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
        self.assertEqual(self.dep.get_sources('a'), ['parent.C1.d'])
        self.assertEqual(self.dep.get_sources('c'), ['C.c'])
        self.assertEqual(self.dep.get_sources('A.c'), ['A'])
        self.assertEqual(self.dep.get_sources('A.c[2]'), ['A.c'])
        self.assertEqual(self.dep.get_sources('B.b[4]'), ['A.d.z'])
        
    def test_base_var(self):
        self.assertEqual(base_var(self.dep, 'B.a'), 'B.a')
        self.assertEqual(base_var(self.dep, 'a'), 'a')
        self.assertEqual(base_var(self.dep, 'a.x'), 'a')
        self.assertEqual(base_var(self.dep, 'a.x.y'), 'a')
        self.assertEqual(base_var(self.dep, 'a.x[3].y'), 'a')
        self.assertEqual(base_var(self.dep, 'A.c[2]'), 'A.c')
        
    def test_is_attr_node(self):
        self.assertEqual(is_attr_node(self.dep, 'B.a'), False)
        self.assertEqual(is_attr_node(self.dep, 'a'), False)
        self.assertEqual(is_attr_node(self.dep, 'A.d.z'), True)
        self.assertEqual(is_attr_node(self.dep, 'A.c[2]'), False)
        self.dep.add_node('a.x.y[2]', subvar=True)
        self.assertEqual(is_attr_node(self.dep, 'a.x.y[2]'), True)
        
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
    
    def test_get_connected_inputs(self):
        self.assertEqual(set(self.dep.get_connected_inputs()), set(['a','D.b']))
    
    def test_get_connected_outputs(self):
        self.assertEqual(set(self.dep.get_connected_outputs()), set(['c', 'D.d']))
    
    def test_already_connected(self):
        # internal connection
        try:
            self.dep.check_connect('A.d', 'B.a')
        except Exception as err:
            self.assertEqual(str(err), "Can't connect 'A.d' to 'B.a'. 'B.a' is already connected to 'B.a.x.y'")
        else:
            self.fail('Exception expected')
           
        # internal to boundary output connection
        try:
            self.dep.check_connect('A.d', 'c')
        except Exception as err:
            self.assertEqual(str(err), "Can't connect 'A.d' to 'c'. 'c' is already connected to 'C.c'")
        else:
            self.fail('Exception expected')
    
        # TODO: input boundary connection

    def test_get_interior_connections(self):
        self.assertEqual(set(self.dep.get_interior_connections(['A', 'B', 'C', 'D'])),
                         set(self.conns))
    
    def test_disconnect_comp(self):
        allcons = set(self.dep.list_connections())
        self.dep.disconnect('B')
        self.assertEqual(set(self.dep.list_connections()), 
                         allcons-set([('A.c[2]','B.a.x.y'),('A.d.z','B.b[4]'),('B.c','C.a'),
                                    ('B.d','C.b')]))
        
    def test_disconnect_basevar(self):
        allcons = set(self.dep.list_connections())
        self.dep.disconnect('B.a')
        self.assertEqual(set(self.dep.list_connections()), 
                         allcons-set([('A.c[2]','B.a.x.y')]))
        
    def test_disconnect_boundary_in_var(self):
        allcons = set(self.dep.list_connections())
        self.dep.disconnect('b')
        self.assertEqual(set(self.dep.list_connections()), 
                         allcons-set([('b[3]','A.b')]))
        
    def test_disconnect_boundary_out_var(self):
        allcons = set(self.dep.list_connections())
        self.dep.disconnect('d')
        self.assertEqual(set(self.dep.list_connections()), 
                         allcons-set([('D.d','d.x')]))
        
    def test_disconnect_basevar2(self):
        allcons = set(self.dep.list_connections())
        self.dep.disconnect('D.d')
        self.assertEqual(set(self.dep.list_connections()), 
                         allcons-set([('D.d','d.x')]))
        
    def test_disconnect_basevar3(self):
        allcons = set(self.dep.list_connections())
        self.dep.disconnect('B.d')
        self.assertEqual(set(self.dep.list_connections()), 
                         allcons-set([('B.d','C.b')]))
        
    def test_disconnect_basevar_with_subvar(self):
        allcons = set(self.dep.list_connections())
        self.dep.disconnect('A.c')
        self.assertEqual(set(self.dep.list_connections()), 
                         allcons-set([('A.c[2]','B.a.x.y')]))
        
    def test_disconnect_basevar_to_basevar(self):
        allcons = set(self.dep.list_connections())
        self.dep.disconnect('B.d', 'C.b')
        self.assertEqual(set(self.dep.list_connections()), 
                         allcons-set([('B.d','C.b')]))
        
    def test_component_graph(self):
        g = self.dep.component_graph()
        self.assertEqual(set(g.nodes()), set(self.comps))
        self.assertEqual(set(g.edges()), set([('A','B'),('B','C')]))
        self.dep.connect('C.d', 'D.a')
        g = self.dep.component_graph()
        self.assertEqual(set(g.nodes()), set(self.comps))
        self.assertEqual(set(g.edges()), set([('A','B'),('B','C'),('C','D')]))
        self.dep.disconnect('A')
        g = self.dep.component_graph()
        self.assertEqual(set(g.nodes()), set(self.comps))
        self.assertEqual(set(g.edges()), set([('B','C'),('C','D')]))
        
    def test_trans_closure(self):
        tc = TransClosure(self.dep)
        self.assertEqual(set(tc['C']), set(['C.d','C.c','c','parent.C2.a']))
        self.assertEqual(set(tc['D.a']), set(['D','D.c','D.d','d.x','d','parent.C3.a']))
        
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
          

if __name__ == "__main__":
    unittest.main()


