import unittest

import networkx as nx
from openmdao.main.depgraph import DependencyGraph, \
                                    find_all_connecting, \
                                    _get_inner_connections,\
                                    gsort
from openmdao.util.graph import nodes_matching_all, \
                                nodes_matching_some, edges_matching_all, \
                                edges_matching_some, base_var
from openmdao.main.interfaces import implements, IImplicitComponent

def fullpaths(cname, names):
    return ['.'.join([cname,n]) for n in names]


class DumbClass(object):
    implements(IImplicitComponent)

    def __init__(self, depgraph, name, inputs=('a','b'), outputs=('c','d'), states=(), resids=()):
        self.name = name
        self._depgraph = depgraph

        self._inputs = inputs[:]
        self._outputs = outputs[:]
        self._states = states[:]
        self._resids = resids[:]

    def get_pathname(self):
        return self.name

    def get(self, name):
        return getattr(self, name, None)

    def list_inputs(self):
        return self._inputs

    def list_outputs(self):
        return self._outputs

    def list_states(self):
        return self._states

    def list_residuals(self):
        return self._resids

    def contains(self, name):
        return name in self._inputs or name in self._outputs or hasattr(self, name)

    def _get_required_compnames(self):
        return []

    def list_deriv_vars(self):
        return self._inputs, self._outputs


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
        ('C1.out2[1][:]', 'C2.in2'),    # sub to base
        ('C1.out3[1][:]', 'C2.in3[1][:]'), # sub to sub
        ('C1.out4', 'C2.in4[1][:]'),    # base to sub
    ]
    bvariables = []
    inputs = ('in1', 'in2', 'in3', 'in4', 'in5', 'in6', 'in7')
    outputs = ('out1', 'out2', 'out3', 'out4')
    return _make_graph(comps, bvariables, conns, inputs, outputs)

def _make_graph(comps=(), variables=(), connections=(), inputs=('a','b'), outputs=('c','d'),
                states=(), resids=()):
    dep = DependencyGraph()
    scope = DumbClass(dep, '')
    for comp in comps:
        if isinstance(comp, basestring):
            comp = DumbClass(dep, comp, inputs=inputs, outputs=outputs,
                             states=states, resids=resids)
        dep.add_component(comp.name, comp)
        setattr(scope, comp.name, comp)

    for v, iotype in variables:
        dep.add_boundary_var(scope, v, iotype=iotype)

    for src, dest in connections:
        dep.connect(scope, src, dest)

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
        self.comps = ['A','B','C','D']
        self.bvariables = [('a','in'), ('b','in'),
                          ('c','out'), ('d','out')]
        self.dep, self.scope = _make_graph(self.comps,
                                           self.bvariables,
                                           self.conns +
                                           self.boundary_conns +
                                           [])

    def test_sorting(self):
        cgraph = self.dep.component_graph()
        order = ['C','D','B','A']
        neworder = gsort(cgraph, order)
        self.assertEqual(neworder, ['A','B','C','D'])


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

    def test_base_var(self):
        self.assertEqual(base_var(self.dep, 'B.a'), 'B.a')
        self.assertEqual(base_var(self.dep, 'a'), 'a')
        self.assertEqual(base_var(self.dep, 'a.x'), 'a')
        self.assertEqual(base_var(self.dep, 'a.x.y'), 'a')
        self.assertEqual(base_var(self.dep, 'a.x[3].y'), 'a')
        self.assertEqual(base_var(self.dep, 'A.c[2]'), 'A.c')

    def test_list_connections(self):
        self.assertEqual(set(self.dep.list_connections()),
                         set([('a','A.a'),('b[3]','A.b'),('A.c[2]','B.a.x.y'),
                              ('A.d.z','B.b[4]'),('B.c','C.a'),('B.d','C.b'),
                              ('C.c','c'),('D.d','d.x')]))

    def test_multi_subvar_inputs(self):
        self.dep.connect(self.scope, 'D.c', 'B.b[5]')

    def test_get_boundary_inputs(self):
        self.assertEqual(set(self.dep.get_boundary_inputs()),
                         set(['a','b']))

    def test_get_boundary_outputs(self):
        self.assertEqual(set(self.dep.get_boundary_outputs()),
                         set(['c','d']))

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
        self.dep.connect(self.scope, 'C.d', 'D.a')
        g = self.dep.component_graph()
        self.assertEqual(set(g.nodes()), set(self.comps))
        self.assertEqual(set(g.edges()), set([('A','B'),('B','C'),('C','D')]))
        self.dep.disconnect('A')
        g = self.dep.component_graph()
        self.assertEqual(set(g.nodes()), set(self.comps))
        self.assertEqual(set(g.edges()), set([('B','C'),('C','D')]))

    def test_comp_graph_input_as_output(self):
        dep, scope = _make_graph(comps=['A','B'],
                                 connections=[('A.in1','B.in1')],
                                 inputs=['in1','in2'],
                                 outputs=['out1','out2'])
        cgraph = dep.component_graph()
        self.assertEqual(set(cgraph.edges()), set([('A','B')]))

    def test_find_all_connecting(self):
        self.assertEqual(find_all_connecting(self.dep.component_graph(), 'A','D'), set())
        self.assertEqual(find_all_connecting(self.dep.component_graph(), 'A','C'),
                         set(['A','B','C']))

    def test_nodes_matching(self):
        g = nx.DiGraph()
        g.add_nodes_from(range(10), foo=True)
        g.node[3]['bar'] = True
        g.node[4]['bar'] = False
        g.node[5]['bar'] = True

        self.assertEqual(set(nodes_matching_all(g, foo=True, bar=True)),
                         set([3,5]))
        self.assertEqual(set(nodes_matching_all(g, foo=True, bar=True)),
                         set([3,5]))
        self.assertEqual(set(nodes_matching_all(g, foo=True)),
                         set(range(10)))
        self.assertEqual(set(nodes_matching_all(g, foo=False, bar=True)),
                         set())
        self.assertEqual(list(nodes_matching_all(g, bar=False)), [4])

        self.assertEqual(set(nodes_matching_some(g, foo=True, bar=False)),
                         set(range(10)))
        self.assertEqual(list(nodes_matching_some(g, foo=False, bar=False)), [4])

    def test_edges_matching(self):
        g = nx.DiGraph()
        g.add_path(range(10), foo=True, bar=False)
        g.edge[2][3]['baz'] = True
        g.edge[5][6]['baz'] = True
        g.edge[5][6]['bar'] = True

        self.assertEqual(set(edges_matching_all(g, foo=True, bar=False)),
                         set(g.edges())-set([(5,6)]))

        self.assertEqual(set(edges_matching_all(g, foo=True)),
                         set(g.edges()))

        self.assertEqual(set(edges_matching_all(g, foo=False, baz=True)),
                         set())

        self.assertEqual(set(edges_matching_all(g, foo=True, baz=True)),
                         set([(2,3),(5,6)]))

        self.assertEqual(set(edges_matching_some(g, baz=True, bar=True)),
                         set([(2,3),(5,6)]))

        self.assertEqual(set(edges_matching_some(g, foo=False, baz=False)),
                         set())

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

    def test_input_as_output(self):
        dep, scope = _make_graph(['A','B','C'], [],
                                 [('A.c','B.a'),('B.d','C.b')])
        self.assertEqual(dep.list_input_outputs('A'), [])
        dep.connect(scope, 'A.a','C.a')
        self.assertEqual(dep.list_input_outputs('A'), ['A.a'])

    def test_add_subvar(self):

        dep, scope = _make_graph(comps=['A','B'],
                                 variables=[('b','in'),('c','out')],
                                 connections=[],
                                 inputs=['a','b'],
                                 outputs=['c','d'])

        # component input
        subvar = 'B.b[1]'
        self.assertTrue(subvar not in dep.node)
        dep.add_subvar(subvar)
        self.assertTrue(subvar in dep.node)
        self.assertTrue('B.b' in dep.successors(subvar))

        # boundary input
        subvar = 'b[1]'
        self.assertTrue(subvar not in dep.node)
        dep.add_subvar(subvar)
        self.assertTrue(subvar in dep.node)
        self.assertTrue('b' in dep.predecessors(subvar))

        # component output
        subvar = 'B.c[1]'
        self.assertTrue(subvar not in dep.node)
        dep.add_subvar(subvar)
        self.assertTrue(subvar in dep.node)
        self.assertTrue('B.c' in dep.predecessors(subvar))

        # boundary output
        subvar = 'c[1]'
        self.assertTrue(subvar not in dep.node)
        dep.add_subvar(subvar)
        self.assertTrue(subvar in dep.node)
        self.assertTrue('c' in dep.successors(subvar))

class DepGraphStateTestCase1(unittest.TestCase):

    def get_comp(self, name):
        return getattr(self.scope, name)

    def setUp(self):
        self.comps = ['C1', 'C2']
        self.states = ['s1', 's2']
        self.resids = ['r1', 'r2']
        self.bvariables = [('a','in'),  ('b','in'),
                           ('c','out'), ('d','out')]

        self.conns = [
            ('C1.s1', 'C2.a'),
        ]
        self.boundary_conns = [
            ('a', 'C1.s1'),
            ('C2.s2', 'c'),
        ]

        self.dep, self.scope = _make_graph(self.comps,
                                           self.bvariables,
                                           self.conns +
                                           self.boundary_conns,
                                           states=self.states,
                                           resids=self.resids)

    def test_inner_connections(self):
        edges = _get_inner_connections(self.dep, ['a'], ['c'])
        self.assertEqual(set(edges), set([('C2.s2', 'c'), ('C1.s1', 'C2.a'), ('a', 'C1.s1')]))

class ReductionTestCase(unittest.TestCase):

    def _make_graph(self, comps, invars, outvars):
        g = DependencyGraph()
        g.add_nodes_from(comps, comp=True)
        for n in comps:
            g.add_nodes_from(["%s.%s" % (n, v) for v in invars], iotype='in')
            g.add_edges_from([("%s.%s"%(n,v),n) for v in invars])
        for n in comps:
            g.add_nodes_from(["%s.%s" % (n, v) for v in outvars], iotype='out')
            g.add_edges_from([(n,"%s.%s"%(n,v)) for v in outvars])
        return g

    def test_simple(self):
        g = self._make_graph(['C1','C2'], ['in'], ['out'])
        g.add_edge('C1.out','C2.in', conn=True)

        reduced = g.collapse_connections()

        self.assertEqual(set(reduced.nodes()),
                         set(['C1.in','C1','C2','C2.out',('C1.out',('C2.in',))]))
        self.assertEqual(set(reduced.edges()),
                         set([(('C1.out', ('C2.in',)), 'C2'), ('C2', 'C2.out'),
                              ('C1', ('C1.out', ('C2.in',))), ('C1.in', 'C1')]))

    def test_input_redirect(self):
        g = self._make_graph(['C1','C2','C3'], ['in'], ['out'])
        g.add_edge('C1.out','C2.in', conn=True)
        g.add_edge('C2.in','C3.in', conn=True)

        reduced = g.collapse_connections()

        self.assertEqual(set(reduced.nodes()),
                         set(['C1.in', ('C1.out', ('C3.in', 'C2.in')), 'C3', 'C2',
                              'C3.out', 'C2.out', 'C1']))
        self.assertEqual(set(reduced.edges()),
                         set([('C1.in', 'C1'), (('C1.out', ('C3.in', 'C2.in')), 'C3'),
                              (('C1.out', ('C3.in', 'C2.in')), 'C2'), ('C3', 'C3.out'),
                              ('C2', 'C2.out'), ('C1', ('C1.out', ('C3.in', 'C2.in')))]))

    def test_multiple_input_redirect(self):
        g = self._make_graph(['C1','C2','C3','C4'], ['in'], ['out'])
        g.add_edge('C1.out','C2.in', conn=True)
        g.add_edge('C2.in','C3.in', conn=True)
        g.add_edge('C3.in','C4.in', conn=True)

        reduced = g.collapse_connections()

        self.assertEqual(set(reduced.nodes()),
                         set([('C1.out', ('C3.in', 'C4.in', 'C2.in')),
                              'C1.in', 'C2.out', 'C4.out', 'C3.out',
                              'C1', 'C2', 'C3', 'C4']))
        self.assertEqual(set(reduced.edges()),
                         set([(('C1.out', ('C3.in', 'C4.in', 'C2.in')), 'C3'),
                              (('C1.out', ('C3.in', 'C4.in', 'C2.in')), 'C2'),
                              (('C1.out', ('C3.in', 'C4.in', 'C2.in')), 'C4'),
                              ('C1', ('C1.out', ('C3.in', 'C4.in', 'C2.in'))),
                              ('C1.in', 'C1'), ('C3', 'C3.out'),
                              ('C2', 'C2.out'), ('C4', 'C4.out')]))

    def test_subvar_conns(self):
        g, scope = _make_base_sub_permutations()
        reduced = g.collapse_connections()
        reduced.vars2tuples(g)
        reduced.prune([])
        self.assertEqual(set(reduced.nodes()),
                         set([('C1.out4', ('C2.in4[1][:]',)),
                              ('C1.out2[1][:]', ('C2.in2',)),
                              ('C1.out1', ('C2.in1',)),
                              ('C1.out3[1][:]', ('C2.in3[1][:]',)),
                              'C2', 'C1',
                              ]))

class CollapsedGraphTestCase(unittest.TestCase):
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
        self.dep, self.scope = _make_graph(self.comps,
                                           self.bvariables,
                                           self.conns +
                                           self.boundary_conns +
                                           [])

        self.reduced = self.dep.collapse_connections()

    def test_full_subgraph(self):
        sub = self.reduced.full_subgraph(['A', 'B'])
        self.assertEqual(set(sub.nodes()),
                         set([('b[3]', ('A.b', 'b[3]')), ('a', ('A.a', 'a')), ('B.c', ('C.a',)),
                              'A.c', 'A.d', 'B', 'A', ('A.c[2]', ('B.a.x.y',)), ('A.d.z', ('B.b[4]',)),
                              'B.b', 'B.a', ('B.d', ('C.b',))]))
        self.assertEqual(set(sub.edges()),
                         set([(('b[3]', ('A.b', 'b[3]')), 'A'), (('a', ('A.a', 'a')), 'A'),
                              ('B', ('B.c', ('C.a',))), ('B', ('B.d', ('C.b',))), ('A', 'A.d'),
                              ('A', ('A.c[2]', ('B.a.x.y',))), ('A', ('A.d.z', ('B.b[4]',))),
                              ('A', 'A.c'), (('A.c[2]', ('B.a.x.y',)), 'B'), (('A.d.z', ('B.b[4]',)), 'B'),
                              ('B.b', 'B'), ('B.a', 'B')]))


if __name__ == "__main__":
    unittest.main()
