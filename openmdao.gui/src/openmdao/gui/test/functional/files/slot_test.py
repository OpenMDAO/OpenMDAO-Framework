from openmdao.main.api import Component, Assembly, VariableTree

from openmdao.lib.datatypes.api import Float, Slot


class Something(VariableTree):

    data = Float(10.0)


class Dummy(Component):

    input = Float(iotype="in")

    output = Float(iotype='out')

    def execute(self):
        self.output = self.input * 2


class Dummy2(Dummy):

    def execute(self):
        self.output = self.input * 4


class AutoAssemb(Assembly):

    d2 = Slot(Dummy)

    def configure(self):

        self.add('d1', Dummy())
        self.add('d2', Dummy())
        self.add('d3', Dummy())

        self.driver.workflow.add(['d1', 'd2', 'd3'])
        self.connect('d1.output', 'd2.input')
        self.connect('d2.output', 'd3.input')

        self.create_passthrough('d1.input')
        self.create_passthrough('d3.output')


if __name__ == "__main__":

    aa = AutoAssemb()
    d = Dummy2()
    #aa.replace('d2',d)

    aa.input = 10
    aa.run()
    print aa.output

    aa.replace('d2', d)
    aa.run()
    print aa.output
