"""
Test of Component.
"""

import unittest

from openmdao.main.api import Component, Container, Assembly, set_as_top, VariableTree
from openmdao.main.datatypes.api import Float, Array, VarTree


class DumbVT(VariableTree):

    v1 = Float(desc='vv1')
    v2 = Float(2., desc='vv2')

class DumbVTSubVarReq(VariableTree):

    v1 = Float(desc='vv1', required=True)
    v2 = Float(2., desc='vv2')


class Source(Component):

    invar = Float(2.0, iotype="in")
    outvar = Float(1.5, iotype="out")
    outvar_array = Array(iotype="out")

    def execute(self):
        pass

class VarTreeSource(Component):

    invar = Float(2.0, iotype="in")
    outvar = VarTree(DumbVT(), iotype="out")

    def execute(self):
        pass


class VarTreeSink(Component):

    invar = VarTree(DumbVT(), iotype="in", required=True)
    outvar = Float(1.5, iotype="out")

    def execute(self):
        pass

class VarTreeSink2(Component):

    invar = VarTree(DumbVTSubVarReq(), iotype="in")
    outvar = Float(1.5, iotype="out")

    def execute(self):
        pass


class MyComponent(Component):
    x = Float(1., iotype='in')
    xreq = Float(iotype='in', required=True)
    areq = Array(iotype='in', required=True)
    xout = Float(2., iotype='out')

    def __init__(self):
        super(MyComponent, self).__init__()
        self.add('cont', Container())
        self.cont.add('dyntrait', Float(3.))

    def execute(self):
        self.xout = self.x * 2.

class DumbComp(Component):
    myvar = Float(1.1, iotype='in', required=True)
    def execute(self):
        print 'running'

class DumbCompA(Component):
    myvar = Array([1.1], iotype='in', required=True)
    def execute(self):
        print 'running'


class RequiredVarTestCase(unittest.TestCase):

    def test_required_input(self):
        comp = MyComponent()
        comp.areq = [1]
        try:
            comp.run()
        except Exception as err:
            self.assertEqual(str(err), ": required variables ['xreq'] were not set")
        else:
            self.fail("Exception expected")

        comp = MyComponent()
        comp.xreq = 1
        try:
            comp.run()
        except Exception as err:
            self.assertEqual(str(err), ": required variables ['areq'] were not set")
        else:
            self.fail("Exception expected")

    def test_required_input_connected(self):
        top = set_as_top(Assembly())
        source = top.add('source', Source())
        comp = top.add('comp', MyComponent())

        top.connect('source.outvar', 'comp.xreq')
        top.connect('source.outvar_array', 'comp.areq')

        try:
            comp.run()
        except RuntimeError as err:
            self.fail('required variables areq and xreq have incoming connections and should not raise an error')

    def test_required_input_vartree(self):
        comp = VarTreeSink()
        try:
            comp.run()
        except Exception as err:
            self.assertEqual(str(err), ": required variables ['invar.v1', 'invar.v2'] were not set")
        else:
            self.fail("Exception expected")


        comp = VarTreeSink2()
        try:
            comp.run()
        except Exception as err:
            self.assertEqual(str(err), ": required variables ['invar.v1'] were not set")
        else:
            self.fail("Exception expected")

    def test_required_input_vartree_connected(self):
        top = set_as_top(Assembly())
        source = top.add('source', VarTreeSource())
        comp = top.add('comp', VarTreeSink())

        top.connect('source.outvar', 'comp.invar')
        try:
            comp.run()
        except RuntimeError as err:
            self.fail('required variable invar has incoming connections and should not raise an error')


        top = set_as_top(Assembly())
        source = top.add('source', VarTreeSource())
        comp = top.add('comp', VarTreeSink())

        top.connect('source.outvar.v1', 'comp.invar.v1')
        try:
            comp.run()
        except RuntimeError as err:
            self.fail('required variable invar has incoming connections and should not raise an error')

    def test_required_input2(self):
        try:
            DumbComp()
        except Exception as err:
            self.assertEqual(str(err), ": variable 'myvar' is required and"
                                       " cannot have a default value")
        else:
            self.fail("Exception expected")

        try:
            DumbCompA()
        except Exception as err:
            self.assertEqual(str(err), ": variable 'myvar' is required and"
                                       " cannot have a default value")
        else:
            self.fail("Exception expected")


if __name__ == "__main__":
    unittest.main()

