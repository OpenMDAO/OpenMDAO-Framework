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


def find_python(hint):
    """
    Return path to python in buildout/bin.  `hint` should be a peer directory
    of 'buildout' found in the current directory path.
    """
    # TODO: win32 support
    cwd = os.getcwd()
    python = 'python'
    if cwd.endswith('buildout'):
        python = os.path.join(cwd, 'bin', python)
    else:
        index = cwd.find(hint)
        if index > 0:
            python = os.path.join(cwd[:index], 'buildout', 'bin', python)
    return python

