import unittest

from openmdao.main.api import Component, Assembly, set_as_top
from openmdao.lib.datatypes.api import Float


class SimpleBase(Component):

    a = Float(iotype="in")
    b = Float(iotype="in")
    c = Float(iotype="in")

    y = Float(iotype="out")

    def execute(self):
        pass


class Simple1(SimpleBase):

    def execute(self):
        self.y = self.a + self.b + self.c

class BaseSubAsm(Assembly):

    def configure(self):
        self.add('comp', SimpleBase())

        self.create_passthrough('comp.a')
        self.create_passthrough('comp.b')
        self.create_passthrough('comp.c')
        self.create_passthrough('comp.y')

class SubAsm(BaseSubAsm):

    def configure(self):
        super(SubAsm, self).configure()

        self.replace('comp', Simple1())


class BaseAsm(Assembly):

    def configure(self):

        self.add('s1', SimpleBase())
        self.add('s2', SimpleBase())
        self.add('s3', SimpleBase())
        self.add('s4', SimpleBase())
        self.add('s5', SimpleBase())

        self.connect('s1.y', 's2.a')
        self.connect('s2.y', 's3.a')
        self.connect('s3.y', 's4.a')
        self.connect('s4.y', 's5.a')
        self.connect('s1.y', 's5.b')
        self.connect('s2.y', 's5.c')

        self.driver.workflow.add(['s1', 's2', 's3', 's4', 's5' ])

class RealAsm(BaseAsm):

    def configure(self):
        super(RealAsm, self).configure()

        #breaks things
        self.replace('s1', SubAsm())
        self.replace('s2', SubAsm())
        self.replace('s3', SubAsm())
        self.replace('s4', SubAsm())
        self.replace('s5', SubAsm())

        self.connect('s3.y','s4.b')


class TestCase(unittest.TestCase):
    def test_connections_after_add_trait(self):
        asm = set_as_top(RealAsm())
        asm._setup()
        conns = asm._depgraph.list_connections()
        self.assertEqual(set(conns),
                         set([('s4.y', 's5.a'),
                              ('s3.y', 's4.b'),
                              ('s3.y', 's4.a'),
                              ('s2.y', 's5.c'),
                              ('s2.y', 's3.a'),
                              ('s1.y', 's2.a'),
                              ('s1.y', 's5.b')]))


if __name__ == '__main__':
    unittest.main()
