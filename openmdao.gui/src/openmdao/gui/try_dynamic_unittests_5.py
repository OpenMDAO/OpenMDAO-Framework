#!/usr/bin/env python
# file: test_pairs_nose.py
from nose.tools import eq_ as eq

def f(x):
    return 2* x

def test_pairs(): 
    for input, output in [ (2, 332), (234, 99213), (9, 3), ]:
        yield _test_f, input, output

def _test_f(input, output):
    try:
        eq(f(input), output)
    except AssertionError:
        if input == 9: # expected failure
            from nose.exc import SkipTest
            raise SkipTest("expected failure")
        else:
            raise

if __name__=="__main__":
   import nose; nose.main()
   
