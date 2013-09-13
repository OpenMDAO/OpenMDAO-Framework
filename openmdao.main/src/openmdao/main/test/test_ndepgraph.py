import unittest

from openmdao.main.ndepgraph import DependencyGraph, is_nested_node, base_var, find_all_connecting

def fullpaths(cname, names):
    return ['.'.join([cname,n]) for n in names]


class DumbClass(object):
    def __init__(self, name, inputs=None, outputs=None):
        self.name = name
        if inputs is None:
            inputs = ('a','b')
        if outputs is None:
            outputs = ('c','d')

        self._inputs = inputs[:]
        self._outputs = outputs[:]
        self._valid_dict = {}
        for inp in inputs:
            self._valid_dict[inp] = True
        for out in outputs:
            self._valid_dict[out] = False

    def run(self, *args, **kwargs):
        for name in self._valid_dict:
            self._valid_dict[name] = True

    def list_inputs(self):
        return self._inputs

    def list_outputs(self):
        return self._outputs

    def contains(self, name):
        return hasattr(self, name)

    def invalidate_deps(self, varnames, force=False):
        for name in varnames:
            self._valid_dict[name] = False
        for name in self._outputs:
            self._valid_dict[name] = False
        return None

#
# tests TODO:
#
# boundary conections to a sub-assembly
#    - normal variable
#    - array entry
#    - vartree
#    - input and output

def _make_xgraph():
    """Make an X shaped graph
    A   D
     \ /
      C
     / \ 
    B   E
    """
    conns = [
        ('A.c[2]', 'D.a[4]'),
        ('A.d.z', 'C.a.x.y'),
        ('B.c', 'C.b[1]'),
        ('B.d.x', 'E.b'),
        ('C.c', 'D.b'),
        ('C.d[2]', 'E.a')
    ]
    comps = ['A','B','C','D','E']
    bvariables = []
    return _make_graph(comps, bvariables, conns)

def _make_base_sub_permutations():
    """create a simple graph with all permutations of connections, i.e.,
    base-base, sub-sub, base-sub, sub-base
    """
    comps = ['C1', 'C2']
    conns = [
        ('C1.out1', 'C2.in1'),       # base to base
        ('C1.out2[1]', 'C2.in2'),    # sub to base
        ('C1.out3[1]', 'C2.in3[1]'), # sub to sub
        ('C1.out4', 'C2.in4[1]'),    # base to sub
    ]
    bvariables = []
    inputs = ('in1', 'in2', 'in3', 'in4', 'in5', 'in6', 'in7')
    outputs = ('out1', 'out2', 'out3', 'out4')
    return _make_graph(comps, bvariables, conns, inputs, outputs)

def _make_graph(comps=(), variables=(), connections=(), inputs=None, outputs=None):
    scope = DumbClass('')
    dep = DependencyGraph()
    for comp in comps:
        if isinstance(comp, basestring):
            comp = DumbClass(comp, inputs=inputs, outputs=outputs)
        dep.add_component(comp.name, comp)
        setattr(scope, comp.name, comp)

    for v, iotype in variables:
        dep.add_boundary_var(v, iotype=iotype)

    for src, dest in connections:
        dep.connect(src, dest)

    return dep, scope


class DepGraphTestCase(unittest.TestCase):

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
        self.dep, self.scope = _make_graph(self.comps,
                                           self.bvariables,
                                           self.conns +
                                           self.boundary_conns +
                                           self.ext_conns)

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
        
    def test_get_source(self):
        self.assertEqual(self.dep.get_sources('B.a'), ['A.c[2]'])
        self.assertEqual(self.dep.get_sources('A.a'), ['a'])
        self.assertEqual(self.dep.get_sources('a'), ['parent.C1.d'])
        self.assertEqual(self.dep.get_sources('c'), ['C.c'])
        self.assertEqual(self.dep.get_sources('A.c'), [])
        self.assertEqual(self.dep.get_sources('B.b[4]'), ['A.d.z'])
        
    def test_base_var(self):
        self.assertEqual(base_var(self.dep, 'B.a'), 'B.a')
        self.assertEqual(base_var(self.dep, 'a'), 'a')
        self.assertEqual(base_var(self.dep, 'a.x'), 'a')
        self.assertEqual(base_var(self.dep, 'a.x.y'), 'a')
        self.assertEqual(base_var(self.dep, 'a.x[3].y'), 'a')
        self.assertEqual(base_var(self.dep, 'A.c[2]'), 'A.c')
        
    def test_is_nested_node(self):
        self.assertEqual(is_nested_node(self.dep, 'B.a'), False)
        self.assertEqual(is_nested_node(self.dep, 'a'), False)
        self.assertEqual(is_nested_node(self.dep, 'A.d.z'), True)
        self.assertEqual(is_nested_node(self.dep, 'A.c[2]'), False)
        self.dep.add_node('a.x.y[2]', subvar=True)
        self.assertEqual(is_nested_node(self.dep, 'a.x.y[2]'), True)
        
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
        
    def test_multi_subvar_inputs(self):
        self.dep.connect('D.c', 'B.b[5]')
    
    def test_get_connected_inputs(self):
        self.assertEqual(set(self.dep.get_connected_inputs()), 
                         set(['a','D.b']))
    
    def test_get_connected_outputs(self):
        self.assertEqual(set(self.dep.get_connected_outputs()), 
                         set(['c', 'D.d']))
        self.dep.connect('D.a', 'parent.foo.bar')
        self.assertEqual(set(self.dep.get_connected_outputs()), 
                         set(['c', 'D.d']))
    
    def test_already_connected(self):
        # internal connection
        try:
            self.dep.check_connect('A.d', 'B.a')
        except Exception as err:
            self.assertEqual(str(err), "'B.a.x.y' is already connected to 'A.c[2]'")
        else:
            self.fail('Exception expected')
           
        # internal to boundary output connection
        try:
            self.dep.check_connect('A.d', 'c')
        except Exception as err:
            self.assertEqual(str(err), "'c' is already connected to 'C.c'")
        else:
            self.fail('Exception expected')
    
        # TODO: input boundary connection

    def test_get_interior_connections(self):
        self.assertEqual(set(self.dep.get_interior_connections(
                                            ['A', 'B', 'C', 'D'])),
                         set(self.conns))
        self.assertEqual(set(self.dep.get_interior_connections()),
                         set(self.conns))

        self.assertEqual(set(self.dep.get_interior_connections(
                                            ['A', 'B', 'D'])),
                         set([('A.c[2]','B.a.x.y'),('A.d.z','B.b[4]')]))


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
        
    def test_connections_to(self):
        self.assertEqual(set(self.dep.connections_to('c')),
                         set([('c', 'parent.C2.a'),
                              ('C.c', 'c')]))
        self.assertEqual(set(self.dep.connections_to('a')),
                         set([('parent.C1.d', 'a'),
                              ('a', 'A.a')]))
        
        # unconnected var should return an empty list
        self.assertEqual(self.dep.connections_to('D.a'),[])

        # now test component connections
        self.assertEqual(set(self.dep.connections_to('A')),
                         set([('a', 'A.a'),
                              ('b[3]', 'A.b'),
                              ('A.c[2]','B.a.x.y'),
                              ('A.d.z','B.b[4]')]))

        self.assertEqual(set(self.dep.connections_to('D')),
                         set([('parent.C0.c', 'D.b'),
                              ('D.d', 'parent.C3.a'),
                              ('D.d', 'd.x')]))
        
    def test_find_all_connecting(self):
        self.assertEqual(find_all_connecting(self.dep.component_graph(), 'A','D'), set())
        self.assertEqual(find_all_connecting(self.dep.component_graph(), 'A','C'), set(['A','B','C']))
        
    # Expression connections are now handled at the parent level (Assembly/Component)
    # so the DependencyGraph will never see anything other than connections
    # between base variables or subvars (or some permutation)
    #def test_expr(self):
        #dep, scope = _make_graph(comps=['B','C'], connections=[('3.4*B.d+2.3', 'C.b')])
        #self.assertEqual(dep.list_connections(), [('3.4*B.d+2.3','C.b')])
        #dep.disconnect('3.4*B.d+2.3', 'C.b')
        #self.assertEqual(dep.list_connections(), [])
        #dep, scope = _make_graph(nodes=['B','C'], connections=[('3.4*B.d+2.3', 'C.b'),
                                                               #('3.4*B.d+2.3', 'C.a')])
        #self.assertEqual(set(dep.list_connections()), set([('3.4*B.d+2.3','C.b'),('3.4*B.d+2.3','C.a')]))
        #dep.disconnect('3.4*B.d+2.3', 'C.b')
        #self.assertEqual(dep.list_connections(), [('3.4*B.d+2.3','C.a')])
       
        #dep, scope = _make_graph(nodes=['B','C'], connections=[('3.4*B.d+2.3', 'C.b'),
                                                               #('3.4*B.d+2.3', 'C.a')])
        #dep.disconnect('3.4*B.d+2.3')
        #self.assertEqual(dep.list_connections(), [])
        
    def test_invalidate(self):
        dep, scope = _make_graph(comps=['A','B'],
                                 connections=[('A.out1','B.in1')],
                                 inputs=['in1','in2'],
                                 outputs=['out1','out2'])
        scope.A.run()
        scope.B.run()
        self.assertEqual(scope.A._valid_dict.values(), [True]*4)
        self.assertEqual(scope.B._valid_dict.values(), [True]*4)

        dep.invalidate_deps(scope, 'A', ['out2'])
        self.assertEqual(scope.A._valid_dict.values(), [True]*4)
        self.assertEqual(scope.B._valid_dict.values(), [True]*4)

        dep.invalidate_deps(scope, 'A', ['out1'])
        self.assertEqual(scope.A._valid_dict.values(), [True]*4)
        self.assertEqual(scope.B._valid_dict['in1'], False)
        self.assertEqual(scope.B._valid_dict['in2'], True)
        self.assertEqual(scope.B._valid_dict['out1'], False)
        self.assertEqual(scope.B._valid_dict['out2'], False)

        dep.connect('B.out1', 'A.in1') # make a cycle

        scope.A.run()
        scope.B.run()

        dep.invalidate_deps(scope, 'A', ['out1'])
        self.assertEqual(scope.A._valid_dict['in1'], False)
        self.assertEqual(scope.A._valid_dict['in2'], True)
        self.assertEqual(scope.A._valid_dict['out1'], False)
        self.assertEqual(scope.A._valid_dict['out2'], False)
        self.assertEqual(scope.B._valid_dict['in1'], False)
        self.assertEqual(scope.B._valid_dict['in2'], True)
        self.assertEqual(scope.B._valid_dict['out1'], False)
        self.assertEqual(scope.B._valid_dict['out2'], False)

        scope.A.run()
        scope.B.run()

        dep.sever_edges([('B.out1','A.in1')]) # remove cycle
        dep.invalidate_deps(scope, 'A', ['out1'])
        self.assertEqual(scope.A._valid_dict.values(), [True]*4)
        self.assertEqual(scope.B._valid_dict['in1'], False)
        self.assertEqual(scope.B._valid_dict['in2'], True)
        self.assertEqual(scope.B._valid_dict['out1'], False)
        self.assertEqual(scope.B._valid_dict['out2'], False)

        dep.unsever_edges() # put cycle back
        scope.A.run()
        scope.B.run()

        dep.invalidate_deps(scope, 'A', ['out1'])
        self.assertEqual(scope.A._valid_dict['in1'], False)
        self.assertEqual(scope.A._valid_dict['in2'], True)
        self.assertEqual(scope.A._valid_dict['out1'], False)
        self.assertEqual(scope.A._valid_dict['out2'], False)
        self.assertEqual(scope.B._valid_dict['in1'], False)
        self.assertEqual(scope.B._valid_dict['in2'], True)
        self.assertEqual(scope.B._valid_dict['out1'], False)
        self.assertEqual(scope.B._valid_dict['out2'], False)

        dep.sever_edges([('B.out1','A.in1')]) # remove cycle
        try:
            dep.sever_edges([('A.out1','B.in1')])
        except Exception as err:
            self.assertEqual("only one set of severed edges is permitted", str(err))
        else:
            self.fail("Exception expected")


    def test_var_edge_iter(self):
        # basevar to basevar connection
        dep, scope = _make_graph(comps=['A','B'],
                                 connections=[('A.out1','B.in1')],
                                 inputs=['in1'],
                                 outputs=['out1'])
        self.assertEqual([('A.out1','B.in1')], list(dep.var_edge_iter('A.out1')))
        self.assertEqual([('A.out1','B.in1')], list(dep.var_edge_iter('B.in1', reverse=True)))
        
        # subvar to basevar connection
        dep, scope = _make_graph(comps=['A','B'],
                                 connections=[('A.out1[1]','B.in1')],
                                 inputs=['in1'],
                                 outputs=['out1'])
        self.assertEqual([('A.out1','A.out1[1]'),('A.out1[1]','B.in1')], list(dep.var_edge_iter('A.out1')))
        self.assertEqual([('A.out1[1]','B.in1'),('A.out1','A.out1[1]')], list(dep.var_edge_iter('B.in1', reverse=True)))
        
        # subvar to subvar connection (multiple connections between two basevars)
        dep, scope = _make_graph(comps=['A','B'],
                                 connections=[('A.out1[1]','B.in1[1]'),('A.out1[2]','B.in1[2]')],
                                 inputs=['in1'],
                                 outputs=['out1'])
        expected = [
            ('A.out1','A.out1[1]'),
            ('A.out1','A.out1[2]'),
            ('A.out1[1]','B.in1[1]'),
            ('A.out1[2]','B.in1[2]'),
            ('B.in1[1]','B.in1'),
            ('B.in1[2]','B.in1')
        ]
        self.assertEqual(set(expected), set(dep.var_edge_iter('A.out1')))
        self.assertEqual(set(expected), set(dep.var_edge_iter('B.in1', reverse=True)))
        
        # basevar to subvar connection
        dep, scope = _make_graph(comps=['A','B'],
                                 connections=[('A.out1','B.in1[1]')],
                                 inputs=['in1'],
                                 outputs=['out1'])
        self.assertEqual([('A.out1','B.in1[1]'),('B.in1[1]','B.in1')], list(dep.var_edge_iter('A.out1')))
        self.assertEqual([('B.in1[1]','B.in1'),('A.out1','B.in1[1]')], list(dep.var_edge_iter('B.in1', reverse=True)))
        
    def test_basevar_iter(self):
        dep = self.dep
        self.assertEqual(set(dep.basevar_iter('a')), set(['A.a']))
        self.assertEqual(set(dep.basevar_iter(['a'])), set(['A.a']))
        self.assertEqual(set(dep.basevar_iter(['parent.C1.d'])), set(['a']))
        self.assertEqual(set(dep.basevar_iter(['A.c','A.d'])), set(['B.a','B.b']))
        self.assertEqual(set(dep.basevar_iter(['B.d'])), set(['C.b']))
        self.assertEqual(set(dep.basevar_iter(['C.c'])), set(['c']))
        self.assertEqual(list(dep.basevar_iter(['parent.C2.a'])), [])
        self.assertEqual(list(dep.basevar_iter(['D.b'])), [])
        self.assertEqual(set(dep.basevar_iter(['D.a','D.b'])), set())
        self.assertEqual(len(list(dep.basevar_iter(['D.a','D.b']))), 0)

        dep, scope = _make_xgraph()
        self.assertEqual(set(dep.basevar_iter(['A.c','A.d'])), set(['C.a','D.a']))
        self.assertEqual(set(dep.basevar_iter(['C.a','C.b'], reverse=True)), 
                         set(['A.d','B.c']))

    def test_comp_iter(self):
        dep = self.dep
        self.assertEqual(list(dep.comp_iter('B')), ['C'])
        self.assertEqual(list(dep.comp_iter('B', include_pseudo=True)), ['C'])
        self.assertEqual(list(dep.comp_iter('B', reverse=True)), ['A'])
        dep.add_node('_pseudo_0', pseudo=True)
        dep.add_nodes_from(['_pseudo_0.in0','_pseudo_0.out0'], var=True)
        dep.add_edges_from([('_pseudo_0.in0','_pseudo_0'),
                            ('_pseudo_0','_pseudo_0.out0')])
        dep.connect('C.d', '_pseudo_0.in0')
        dep.connect('_pseudo_0.out0', 'D.a')
        self.assertEqual(list(dep.comp_iter('C', include_pseudo=False)), ['D'])
        self.assertEqual(list(dep.comp_iter('D', include_pseudo=False, reverse=True)),
                                            ['C'])
        self.assertEqual(list(dep.comp_iter('C')), ['_pseudo_0'])
        self.assertEqual(list(dep.comp_iter('D', reverse=True)), ['_pseudo_0'])

        dep, scope = _make_xgraph()

        self.assertEqual(set(dep.comp_iter('C')), set(['D','E']))
        self.assertEqual(set(dep.comp_iter('C', reverse=True)), set(['A','B']))
        self.assertEqual(set(dep.comp_iter('A')), set(['D','C']))

    def test_input_as_output(self):
        dep, scope = _make_graph(['A','B','C'], [],
                                 [('A.c','B.a'),('B.d','C.b')])
        self.assertEqual(dep.list_input_outputs('A'), [])
        dep.connect('A.a','C.a')
        self.assertEqual(dep.list_input_outputs('A'), ['A.a'])


          
if __name__ == "__main__":
    unittest.main()


