import unittest

from openmdao.lib.components.nastran.nastran_replacer import NastranReplacer

class TestNastranReplacer(unittest.TestCase):

    def go(self, text, variables):
        self.replacer = NastranReplacer(text)
        self.replacer.replace(variables)
        return self.replacer.text

    def go_single(self, text, variables):
        return self.go([text], variables)[0]

    def easy_test(self, text, outcome, variables):
        t = self.go_single(text, variables)
        self.assertTrue(t == outcome)

    def test_no_changes(self):
        s = "hello, i'm andrew"
        self.easy_test(s, s, {})

    def test_normal_replace(self):
        s = "my height: %h"
        t = "my heighHEIGHT  "
        self.easy_test(s, t, {"h" : "HEIGHT"})

    def test_normal_replace_beginning(self):
        s = "%h blahhREAL DATA"
        t = "andrew  REAL DATA"
        self.easy_test(s, t, {"h" : "andrew"})

    def test_overwrite_variable(self):
        s = "METHOD %*n"
        t = "METHOD 103"
        self.easy_test(s, t, {"*n" : 103})

    def test_overwrite_variable_2(self):
        s = "METHOD %*n DONT REMOVE"
        t = "METHOD 103 DONT REMOVE"
        self.easy_test(s, t, {"*n" : 103})

    def test_overwrite_long_var(self):
        s = "METHOD %*method"
        t = "METHOD 103     "
        self.easy_test(s, t, {"*method": 103})


if __name__ == "__main__":
    unittest.main()
