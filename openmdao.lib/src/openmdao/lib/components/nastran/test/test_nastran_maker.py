import unittest

from nastran_maker import NastranMaker

class TestNastranMaker(unittest.TestCase):

    def go(self, text):
        self.maker = NastranMaker(text)

    def go_simple(self, line, outcome, args):
        self.maker = NastranMaker([line])
        for arg in args:
            self.maker.set(*arg)
        self.maker._output(10001)
        if self.maker.text[0] != outcome:
            print "expected", outcome, len(outcome)
            print "resulted", self.maker.text[0], len(self.maker.text[0])
        self.assertTrue(self.maker.text[0] == outcome)

    def test_simple(self):
        s = "PROP    12      5     "
        t = "PROP*   12              7               "
        self.go_simple(s, t, (("PROP", "12", 2, 7),))

    def test_long(self):
        s = "PROP*   1               13.5            4"
        t = "PROP*   1               13.5            7               "
        self.go_simple(s, t, (("PROP", "1", 3, 7),))

    def test_simple_cont(self):
        s = ["PBAR     1       1      40.     333.333 53.3333 259.865                 +      A", "+      A5.      2.      -5.     2.      -5.     -2.     5.      -2.     +      B", "+      B.833333 .833333"]
        t = ['PBAR*    1              7               40.             333.333         *10001          ', '*10001  53.3333         259.865                                         *10002          ', '*10002  5.              2.              -5.             2.              *10003          ', '*10003  -5.             -2.             5.              -2.             *10004          ', '*10004  .833333         .833333         ']

        self.go(s)
        self.maker.set("PBAR", "1", 2, 7)
        self.maker._output(10001)
        #print "expected", t
        #print "resulted", self.maker.text
        self.assertTrue(t == self.maker.text)

    def test_long_cont(self):
        s = ["PBAR*    1               1              40.             333.333         *      A", "*      A53.3333         259.865                                         *      B", "*      B5.              2.              -5.             2.              *      C", "*      C-5.             -2.             5.              -2.             *      D", "*      D.833333         .833333                                         *      E", "*      E"]
        t = ['PBAR*    1              7               40.             333.333         *10001          ', '*10001  53.3333         259.865                                         *10002          ', '*10002  5.              2.              -5.             2.              *10003          ', '*10003  -5.             -2.             5.              -2.             *10004          ', '*10004  .833333         .833333                                         ']
        self.go(s)
        self.maker.set("PBAR", "1", 2, 7)
        self.maker._output(10001)
        self.assertTrue(t == self.maker.text)


if __name__ == "__main__":
    unittest.main()
