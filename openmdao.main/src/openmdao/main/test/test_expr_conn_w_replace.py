import unittest

from openmdao.main.api import Component, Assembly, set_as_top
from openmdao.main.datatypes.api import Float


class C1Base(Component):

    fin = Float(iotype='in')

    fpass = Float(iotype='out')


class C1(C1Base):

    def execute(self):
        self.fpass = 2*self.fin


class C2(Component):

    fpass_2 = Float(iotype='in')

    fout = Float(iotype='out')

    def execute(self):
        self.fout = 2*self.fpass_2


class A(Assembly):

    def configure(self):

        self.add('c1', C1Base())
        self.add('c2', C2())

        self.connect('c1.fpass * 2.0', 'c2.fpass_2')

        self.driver.workflow.add(['c1', 'c2'])


class MyTestCase(unittest.TestCase):
    def test_expr_conn_with_replace(self):
        a = set_as_top(A())
        a.replace('c1', C1())
        a.c1.fin = 5.0
        a.run()
        self.assertEqual(a.c2.fout, 40)

if __name__ == '__main__':
    unittest.main()
