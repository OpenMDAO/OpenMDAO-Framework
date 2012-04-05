import unittest

# does not work with nose. nose apparently finds a test that
#   is not really a test and we get this error
#Traceback (most recent call last):
#  File "/hx/u/hschilli/openmdao/T809-add-functional-gui-testing/devenv/lib/python2.6/site-packages/nose-0.11.3-py2.6.egg/nose/case.py", line 186, in runTest
#    self.test(*self.arg)
#TypeError: do_qqq_expected() takes exactly 1 argument (0 given)


class TestPreReqs(unittest.TestCase):
    pass

def under_qqq(x):
    return 2*x

def create_qqq (pair):
    def do_qqq_expected(self):
        self.assertEqual(under_qqq(pair[0]), pair[1])
    return do_qqq_expected

for k, pair in enumerate ([(23, 55), (4, 32)]):
    test_method = create_qqq (pair)
    test_method.__name__ = 'test_expected_%d' % k
    setattr (TestPreReqs, test_method.__name__, test_method)
