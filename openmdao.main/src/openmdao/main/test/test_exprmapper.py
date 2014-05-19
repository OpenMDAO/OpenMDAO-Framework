import unittest

from openmdao.main.assembly import ExprMapper, Assembly, set_as_top
from openmdao.main.component import Component
from openmdao.main.datatypes.int import Int
from openmdao.main.expreval import ExprEvaluator

nodes = ['A', 'B', 'C', 'D']


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


def make_graph(nodes=(), connections=()):
    scope = set_as_top(Assembly())
    sub = scope.add('sub', Assembly())
    dep = ExprMapper(sub)
    for name in nodes:
        if name.startswith('parent.'):
            scope.add(name.split('.', 1)[1], Simple())
        else:
            sub.add(name, Simple())
            #dep.add(name)
    for src, dest in connections:
        if '.' not in src and not sub.contains(src):
            if dest.startswith('parent.'):
                iotype = 'out'
            else:
                iotype = 'in'
            sub.add(src, Int(1, iotype=iotype))
        if '.' not in dest and not sub.contains(dest):
            if src.startswith('parent.'):
                iotype = 'in'
            else:
                iotype = 'out'
            sub.add(dest, Int(1, iotype=iotype))
        dep.connect(ExprEvaluator(src, sub), ExprEvaluator(dest, sub), sub)
    return dep, sub


class ExprMapperTestCase(unittest.TestCase):

    def setUp(self):
        self.internal_conns = [
            ('A.c', 'B.b'),
            ('B.c', 'D.a'),
            ('C.c', 'D.b'),
        ]
        self.boundary_conns = [
            ('a', 'B.a'),
            ('D.c', 'c'),
        ]
        self.dep, self.scope = make_graph(nodes,
                                          self.internal_conns +
                                          self.boundary_conns)

    def test_get_source(self):
        dep, scope = make_graph(nodes,
                                self.internal_conns +
                                self.boundary_conns)

        self.assertEqual(dep.get_source('B.a'), 'a')
        self.assertEqual(dep.get_source('A.a'), None)
        self.assertEqual(dep.get_source('c'), 'D.c')

    def test_list_connections(self):
        self.assertEqual(set(self.dep.list_connections(show_passthrough=False)),
                         set([('A.c', 'B.b'), ('B.c', 'D.a'), ('C.c', 'D.b')]))
        self.assertEqual(set(self.dep.list_connections()),
                         set([('A.c', 'B.b'), ('B.c', 'D.a'), ('C.c', 'D.b'), ('a', 'B.a'), ('D.c', 'c')]))

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
            self.dep.check_connect('a', 'B.b', self.scope)
        except Exception as err:
            self.assertEqual(str(err), "sub: 'B.b' is already connected to source 'A.c'")
        else:
            self.fail('Exception expected')

        # internal to boundary output connection
        try:
            self.dep.check_connect('B.d', 'c', self.scope)
        except Exception as err:
            self.assertEqual(str(err), "sub: 'c' is already connected to source 'D.c'")
        else:
            self.fail('Exception expected')

    def test_disconnect(self):
        self.dep.disconnect('a') 
        self.dep.disconnect('c')

    def test_find_referring_exprs(self):
        self.assertEqual(set(self.dep.find_referring_exprs('A')),
                         set(['A.c']))

if __name__ == "__main__":
    unittest.main()
