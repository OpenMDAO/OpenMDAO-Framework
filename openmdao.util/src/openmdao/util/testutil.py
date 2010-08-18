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

    test_case : :class:`unittest.TestCase`
        TestCase instance used for assertions.

    code : string
        Statement to be executed.

    globals, locals : dict
        Arguments for :meth:`eval`.

    exception : Exception
        Exception that should be raised.

    msg : string
        Expected message from exception.

    use_exec : bool
        If True, then evaluate `code` with :func:`exec` rather than
        :func:`eval`.  This is necessary for testing statements that are not
        expressions.
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

    test_case : :class:`unittest.TestCase`
        TestCase instance used for assertions.

    actual : float
        The value from the test.

    desired : float
        The value expected.

    tolerance : float
        Maximum relative error ``(actual - desired) / desired``.
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

