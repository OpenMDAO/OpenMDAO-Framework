def assertRaisesError(testCaseInstance, code, errType, errMsg):
    try:
        eval(code)
    except errType,err:
        testCaseInstance.assertEqual(str(err), errMsg)
    except Exception:
        testCaseInstance.fail("Expecting %s" % errType)
    else:
        testCaseInstance.fail("Expecting %s" % errType)