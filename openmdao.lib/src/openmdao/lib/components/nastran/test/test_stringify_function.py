import unittest
import random
from openmdao.lib.components.nastran.nastran_util import stringify as s

inputs = [2,4,6,3,384, 39402294, 20495820595806,\
          3.556, 1.1, 13940402.349202, 0.0000002044444,
          0.9, 0.83359202393582, 0.1, 0.192829595202, 0.03839206799,
          0.002482849592245, .0284955291938333, 0.100000000000000001,
          1.0, 2.000000000000000023, 1.253643938402238, 20.12957202395,
          103.392034965720293, 1023.2784095950312, 29403.29340402292,
          392065.392222222222, 3920594.4294000559302, 12345678.2939502,
          123456789.987654321, 2395812345.13259202222] + \
          [10 ** i for i in range(17)] + \
          [10 ** i + 1 for i in range(17)]


class TestStringify(unittest.TestCase):
    def cmp_(self, i):
        a = float(i)
        t = s(i)[0] + s(i).replace("+", "e+")[1:].replace("-", "e-", 1)

        b = float(t)
        self.assertTrue((a-b)/a < 0.05)
        self.assertTrue(len(s(i)) <= 8)

    def test_interesting_inputs(self):
        for i in inputs:
            pos = s(i)
            neg = s(-i)
            self.cmp_(i)
            self.cmp_(-i)

    def test_stress(self):
        for i in range(10000):
            a = random.uniform(- 10 ** 10, 10 ** 10)
            self.cmp_(a)
            self.cmp_(-a)


if __name__ == "__main__":
    unittest.main()
