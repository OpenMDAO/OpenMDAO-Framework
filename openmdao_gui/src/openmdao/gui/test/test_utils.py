import unittest

from openmdao.gui.util import unique_shortnames


class UtilsTestCase(unittest.TestCase):

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test_unique_shortnames(self):

        lst = ['a.b.c.foo', 'x.y.z.foo', 'a.b.z', 'aa.bb.cc',
               'openmdao.lib.components.expected_improvement_multiobj.MultiObjExpectedImprovement']
        dct = unique_shortnames(lst)
        self.assertEqual(set(dct.values()), set(['cc', 'z', 'c.foo', 'z.foo', 'MultiObjExpectedImprovement']))


if __name__ == "__main__":
    unittest.main()
