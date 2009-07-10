import os.path

def assertRaisesError(testCaseInstance,code,errType,errMsg):
    testFailed = False
    try:
        eval(code)
        testFailed = True
    except errType,err:
        testCaseInstance.assertEqual(str(err),errMsg)
    except:
        testCaseInstance.fail("Expecting %s"%errType)
    if testfailed: testCaseInstance.fail("Expecting %s"%errType)


def find_python(hint):
    """ Return path to python in buildout/bin. """
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

