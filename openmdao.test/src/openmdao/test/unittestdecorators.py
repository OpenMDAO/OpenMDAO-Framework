"""A couple of unittest decorators from Kelly Yancey's blog at
http://kbyanc.blogspot.com/2007/06/pythons-unittest-module-aint-that-bad.html

Copyright (c) 2007, Kelly Yancey

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
"""

def TODO(func):
    """unittest test method decorator that ignores
       exceptions raised by test
   
    Used to annotate test methods for code that may
    not be written yet.  Ignores failures in the
    annotated test method; fails if the text
    unexpectedly succeeds.
    """
    def wrapper(*args, **kw):
        try:
            func(*args, **kw)
            succeeded = True
        except:
            succeeded = False
        assert succeeded is False, \
               "%s marked TODO but passed" % func.__name__
    wrapper.__name__ = func.__name__
    wrapper.__doc__ = func.__doc__
    return wrapper


def PlatformSpecific(platformList):
    """unittest test method decorator that only
       runs test method if os.name is in the
       given list of platforms
    """
    def decorator(func):
        import os
        def wrapper(*args, **kw):
            if os.name in platformList:
                return func(*args, **kw)
        wrapper.__name__ = func.__name__
        wrapper.__doc__ = func.__doc__
        return wrapper
    return decorator

"""
class ExampleTestCase(unittest.TestCase):
    @TODO
    def testToDo(self):
        MyModule.DoesNotExistYet('boo')

    @PlatformSpecific(('mac', ))
    def testMacOnly(self):
        MyModule.SomeMacSpecificFunction()

    @TODO
    @PlatformSpecific(('nt', 'ce'))
    def testComposition(self):
        MyModule.PukePukePuke()
"""

