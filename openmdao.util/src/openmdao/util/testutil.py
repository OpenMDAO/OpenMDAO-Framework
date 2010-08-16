"""Utilities for the OpenMDAO test process"""

import os.path
import sys
from math import isnan

def assertRaisesError(test_case_instance, code, err_type, err_msg):
    """ Determine that `code` raises `err_type` with `err_msg`. """
    try:
        eval(code)
    except err_type, err:
        test_case_instance.assertEqual(str(err), err_msg)
    else:
        test_case_instance.fail("Expecting %s" % err_type)


def assert_raises(test_case, code, globals, locals, exception, msg,
                  use_exec=False):
    """
    Determine that `code` raises `exception` with `msg`.
    `globals` and `locals` are arguments for :meth:`eval`.
    If `use_exec`, then evaluate `code` with `exec` rather than :meth:`eval`.
    This is necessary for testing statements that are not expressions.
    """
    try:
        if use_exec:
            exec code in globals, locals
        else:
            eval(code, globals, locals)
    except exception as exc:
        test_case.assertEqual(str(exc)[:len(msg)], msg)
    else:
        test_case.fail('Expecting %s' % exception)


def assert_rel_error(test_case, actual, desired, tolerance):
    """
    Determine that the relative error between `actual` and `desired`
    is within `tolerance`.
    """
    if isnan(actual) and not isnan(desired):
        test_case.fail('actual nan, desired %s, error nan, tolerance %s'
                       % (desired, tolerance))
    
    error = (actual - desired) / desired
    if abs(error) > tolerance:
        test_case.fail('actual %s, desired %s, error %s, tolerance %s'
                       % (actual, desired, error, tolerance))


def find_python():
    """ Return path to the OpenMDAO python command"""
    return sys.executable


def make_protected_dir():
    """
    Returns the the absolute path of an inaccessible directory.
    Files cannot be created in it, it can't be :meth:`os.chdir` to, etc.
    Not supported on Windows.
    """
    directory = '__protected__'
    if os.path.exists(directory):
        os.rmdir(directory)
    os.mkdir(directory)
    os.chmod(directory, 0)
    return os.path.join(os.getcwd(), directory)


# this decorator is based on a code snippet by vegaseat at daniweb.
# See http://www.daniweb.com/code/snippet216689.html
def print_timing(func):
    def wrapper(*args, **kwargs):
        t1 = time.time()
        res = func(*args, **kwargs)
        t2 = time.time()
        print '%s took %0.3f ms' % (func.func_name, (t2-t1)*1000.0)
        return res
    return wrapper

