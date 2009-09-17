import os.path


def assertRaisesError(test_case_instance, code, err_type, err_msg):
    """ Determine that `code` raises `err_type` with `err_msg`. """
    test_failed = False
    try:
        eval(code)
        test_failed = True
    except err_type, err:
        test_case_instance.assertEqual(str(err), err_msg)
    except:
        test_case_instance.fail("Expecting %s" % err_type)
    if test_failed:
        test_case_instance.fail("Expecting %s" % err_type)


def find_python():
    """
    Return path to python in buildout/bin. Assumes it is somewhere
    in the current directory tree.  Returns just 'python' if not found.
    """
    # TODO: win32 support
    cwd = os.getcwd()
    if cwd.endswith('buildout'):
        return os.path.join(cwd, 'bin', 'python')

    index = cwd.rfind(os.sep)
    while index > 0:
        python = os.path.join(cwd[:index], 'buildout', 'bin', 'python')
        if os.path.exists(python):
            return python
        end = index - 1
        index = cwd.rfind(os.sep, 0, end)

    return 'python'

