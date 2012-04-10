import unittest

class Tests(unittest.TestCase):
    def check(self, i, j):
        self.assertNotEquals(0, i-j)



for i in xrange(1, 4):
    for j in xrange(2, 6):
        def ch(i, j):
            return lambda self: self.check(i, j)
        setattr(Tests, "test_%r_%r" % (i, j), ch(i, j))

if __name__ == "__main__":
    unittest.main()
