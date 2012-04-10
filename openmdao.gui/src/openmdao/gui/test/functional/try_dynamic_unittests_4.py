import unittest
import sys

# works with python and unittest but nose does not discover the tests

# some magic code
__examples__ = "__examples__"

def for_examples(*examples):
    def decorator(f, examples=examples):
      setattr(f, __examples__, getattr(f, __examples__,()) + examples)
      return f
    return decorator

class TestCaseWithExamplesMetaclass(type):
  def __new__(meta, name, bases, dict):
    def tuplify(x):
      if not isinstance(x, tuple):
        return (x,)
      return x
    for methodname, method in dict.items():
      if hasattr(method, __examples__):
        dict.pop(methodname)
        examples = getattr(method, __examples__)
        delattr(method, __examples__)
        for example in (tuplify(x) for x in examples):
          def method_for_example(self, method = method, example = example):
            method(self, *example)
          methodname_for_example = methodname + "(" + ", ".join(str(v) for v in example) + ")"
          dict[methodname_for_example] = method_for_example
    return type.__new__(meta, name, bases, dict)

class TestCaseWithExamples(unittest.TestCase):
  __metaclass__ = TestCaseWithExamplesMetaclass
  pass

unittest.TestCase = TestCaseWithExamples

class DummyTest(unittest.TestCase):
  @for_examples(1, 2)
  @for_examples(3, 4)
  def test_is_smaller_than_four(self, value):
    self.assertTrue(value < 4)

  @for_examples((1,2),(2,4),(3,7))
  def test_double_of_X_is_Y(self, x, y):
    self.assertEqual(2 * x, y)

if __name__ == "__main__":
  unittest.main()





