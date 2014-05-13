import unittest

from openmdao.main.api import Component, VariableTree
from openmdao.main.datatypes.vtree import VarTree
from openmdao.util.testutil import assert_raises


class InputTree(VariableTree):
    pass


class OutputTree(VariableTree):
    pass


class TestComponent(Component):
    input = VarTree(InputTree(), iotype='in')
    output = VarTree(OutputTree(iotype='out'))


class TestCase(unittest.TestCase):

    def test_basic(self):
        comp = TestComponent()

        code = "VarTree(InputTree)"
        msg = "default_value must be an instance of VariableTree or subclass"
        assert_raises(self, code, globals(), locals(), TypeError, msg)

        code = "comp.input = OutputTree()"
        msg = ": 'input' must be an instance of %s.%s" \
              % (InputTree.__module__, InputTree.__name__)
        assert_raises(self, code, globals(), locals(), TypeError, msg,
                      use_exec=True)


if __name__ == '__main__':
    unittest.main()
