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