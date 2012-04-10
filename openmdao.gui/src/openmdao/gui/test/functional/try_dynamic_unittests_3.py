import unittest
import sys

# even when just running from python you get and error
#  File "try_dynamic_unittests_3.py", line 21, in decorator
#    name_for_parameter = method.__name__ + "(" + args_for_parameter + ")"
#AttributeError: 'NoneType' object has no attribute '__name__'


import unittest

# some magic code will be added here later

def for_examples(*parameters):

  def tuplify(x):
    if not isinstance(x, tuple):
      return (x,)
    return x

  def decorator(method, parameters=parameters):
    for parameter in (tuplify(x) for x in parameters):

      def method_for_parameter(self, method=method, parameter=parameter):
        method(self, *parameter)
      args_for_parameter = ",".join(repr(v) for v in parameter)
      name_for_parameter = method.__name__ + "(" + args_for_parameter + ")"
      frame = sys._getframe(1)  # pylint: disable-msg=W0212
      frame.f_locals[name_for_parameter] = method_for_parameter
    return None
  return decorator



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





