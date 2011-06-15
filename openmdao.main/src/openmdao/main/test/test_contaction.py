import unittest

from openmdao.main.api import Component, Assembly, Case, set_as_top
from openmdao.lib.datatypes.api import Int
from openmdao.main.contaction import ActionManager, AddAction, \
                                     RenameAction, SetAction, ConnectAction, DisconnectAction

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

class ContainerActionTestCase(unittest.TestCase):

    def setUp(self):
        self.top = set_as_top(Assembly())
        self.top.add('comp1', Simple())
        self.top.add('comp2', Simple())
        self.top.connect('comp1.c', 'comp2.a')
        self.top.driver.workflow.add(['comp1','comp2'])
    
    def test_connect(self):
        act = ConnectAction('comp1.d', 'comp2.b')
        act.do(self.top)
        self.assertEqual(set(self.top.list_connections()), 
                         set([('comp1.d','comp2.b'),('comp1.c', 'comp2.a')]))
        act.undo(self.top)
        self.assertEqual(self.top.list_connections(), [('comp1.c','comp2.a')])

    def test_disconnect(self):
        self.top.connect('comp1.d', 'comp2.b')
        act = DisconnectAction('comp1.c', 'comp2.a')
        act.do(self.top)
        self.assertEqual(self.top.list_connections(), [('comp1.d','comp2.b')])
        act.undo(self.top)
        self.assertEqual(set(self.top.list_connections()), 
                         set([('comp1.d','comp2.b'),('comp1.c', 'comp2.a')]))

    def test_add(self):
        self.assertEqual(set(self.top.list_containers()), 
                         set(['comp1','comp2','driver']))
        am = ActionManager()
        am.do(AddAction('foo', Simple()), self.top)
        am.do(AddAction('foo2', Simple()), self.top)
        self.assertEqual(set(self.top.list_containers()), 
                         set(['comp1','comp2','driver','foo','foo2']))
        am.undo(self.top)
        self.assertEqual(set(self.top.list_containers()), 
                         set(['comp1','comp2','driver']))


if __name__ == "__main__":
    unittest.main()


