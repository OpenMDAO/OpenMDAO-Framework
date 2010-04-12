import os.path
import sys


def assertRaisesError(test_case_instance, code, err_type, err_msg):
    """ Determine that `code` raises `err_type` with `err_msg`. """
    try:
        eval(code)
    except err_type, err:
        test_case_instance.assertEqual(str(err), err_msg)
    else:
        test_case_instance.fail("Expecting %s" % err_type)


def assert_rel_error(test_case_instance, actual, desired, tolerance):
    """
    Determine that the relative error between `actual` and `desired`
    is within `tolerance`.
    """
    error = (actual - desired) / desired
    if abs(error) > tolerance:
        test_case_instance.fail('actual %s, desired %s, error %s, tolerance %s'
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

