.. index:: testing
.. index:: nose
       
Testing
-------

By default, your top level ``buildout/bin`` directory will contain a script
called ``test`` that uses a Python package called `nose
<http://somethingaboutorange.com/mrl/projects/nose>`_ to run all of the unit
tests for any package that you specify. For example, to run all of the unit
tests in the openmdao namespace package, do the following:

::

   bin/test openmdao
   
which should generate output something like this:

::

   ......................................................................
   .................................................
   ----------------------------------------------------------------------
   Ran 119 tests in 0.888s

   OK

To run the full test suite, which includes the openmdao namespace package and
other packages from eggsrc, type:

::

    bin/test --all
    
    
To get a list of options available with ``bin/test``, type ``bin/test --help``
from the ``buildout`` directory.
   
.. index: test coverage
   
Test Coverage
=============

There is a Python package called  `coverage
<http://nedbatchelder.com/code/modules/rees-coverage.html>`_ that is accessible
through ``bin/test`` that makes it easy to determine if your tests cover every
line of code in your source files.  To get a coverage report for the openmdao
package, do the following from the ``buildout`` directory:

::

   bin/test openmdao --with-coverage --cover-package=openmdao
   
The report should look something like this:

::

   ................................................................................
   ..
   Name                                Stmts   Exec  Cover   Missing
   -----------------------------------------------------------------
   openmdao                                5      0     0%   2-6
   openmdao.lib                            0      0   100%   
   openmdao.lib.components                 0      0   100%   
   openmdao.lib.drivers                    0      0   100%   
   openmdao.lib.drivers.conmindriver     183    179    97%   149, 233-234, 271
   openmdao.lib.factories                  0      0   100%   
   openmdao.lib.variables                  0      0   100%   
   openmdao.main                           6      3    50%   5-7
   openmdao.main.arrayvar                 48     47    97%   32
   openmdao.main.assembly                103    101    98%   95, 129
   openmdao.main.component                47     41    87%   58, 92, 99, 106, 121, 142
   openmdao.main.constants                 4      4   100%   
   openmdao.main.constraint               44     43    97%   24
   openmdao.main.container               201    185    92%   22-24, 138, 156, 166, 253-254, 276-277, 337, 340, 356, 359, 367-368
   openmdao.main.containervar             50     29    58%   29, 38-43, 49-55, 66-72, 82
   openmdao.main.driver                   18     15    83%   35, 40-41
   openmdao.main.exceptions                5      5   100%   
   openmdao.main.expreval                122    115    94%   27, 32, 36, 40, 85, 177, 222
   openmdao.main.factory                   6      5    83%   25
   openmdao.main.factorymanager           21     16    76%   28, 33-37
   openmdao.main.float                    70     54    77%   38-41, 49-53, 58-61, 69-73, 105, 120
   openmdao.main.hierarchy                49     46    93%   34, 40, 59
   openmdao.main.importfactory            28     25    89%   47-49
   openmdao.main.int                      42     24    57%   31-34, 39-46, 51-54, 59-66
   openmdao.main.interfaces               54     54   100%   
   openmdao.main.logger                    9      9   100%   
   openmdao.main.pkg_res_factory          61     59    96%   88, 114
   openmdao.main.string                   42     28    66%   31-34, 42-46, 51-54, 62-66
   openmdao.main.stringlist               56     40    71%   31-34, 42-46, 51-54, 62-66, 92, 95
   openmdao.main.tarjan                   58     26    44%   52-71, 78-96, 100
   openmdao.main.variable                138    113    81%   22, 54, 65, 73, 101-104, 112, 117, 129, 141, 184, 202, 227, 263, 265-270, 276, 282-285, 289-290
   openmdao.main.vartypemap               19     17    89%   42-45
   openmdao.main.workflow                 56     35    62%   30, 43, 56, 61-75, 79, 86-88, 92
   -----------------------------------------------------------------
   TOTAL                                1545   1318    85%   
   ----------------------------------------------------------------------
   Ran 82 tests in 5.678s

   OK

The numbers in the *Missing* column indicate lines or ranges of lines that are
not covered by the current set of tests.

If you edit source code, the coverage data may become inaccurate, so you should
clear the coverage database by issuing the following command:

::

   bin/test openmdao --cover-erase

.. index: pair: tests; adding
.. index: pair: tests; unit
.. index: unittest

Adding New Tests
================

Generally, you should write your tests using Python's `unittest
<http://docs.python.org/library/unittest.html>`_ framework if possible,
although the nose_ package is able to discover and run tests that do not use
unittest_.

The following is a simple example of a unit test written using the unittest_
framework.


.. parsed-literal::

    import unittest

    class TestSomeFunctions(unittest.TestCase):

        def setUp(self):
            # put setup code here. It will run at the beginning of each
            # test function (function with name that starts with 'test')

        def tearDown(self):
            # put code here that you want to be run after each test function
            # is completed

        def testfunct1(self):
            # a test function

        def test_some_stuff(self):
            # another test function

    if __name__ == '__main__':
        unittest.main()


The ``unittest.TestCase`` class provides a number of functions to
test output during a test function.  For example:

``self.assertTrue(expr[,msg])``
    Test will fail if expr does not evaluate to True.
    
``self.assertEqual(val1,val2)``
    Test will fail if val1 != val2
        
``self.assertNotEqual(val1,val2)``
    Test will fail if val1 == val2
        
``self.assertAlmostEqual(val1,val2[,places=7])``
    Test will fail if val1 differs from val2 by more than a small
    amount of decimal places.
    
``self.fail([msg])``
    Test will fail and display the given message.
    
Often in a test you will want to make sure that a specific exception is raised
when a certain thing happens, and usually you want to verify that the error
message contains certain information.  The unittest_ framework provides an
``assertRaises`` function that does part of this job, but it does not allow
you to check the error message, so the preferred way to test exceptions is
shown in the code below. In this example, we will assume that the exception
we are interested in is a ``ValueError``, and note that we would place our
test function inside of our ``unittest.TestCase`` derived class.

.. parsed-literal::

    def test_myexception(self):
        try:
            # perform action here that should raise exception
        except ValueError, err:
            self.assertEqual(str(err), "this should be my expected error message")
        else:
            self.fail('expected a ValueError')

Note that the ``else`` block after the ``except`` is important because we
want the test to fail if no exception is raised at all.  Without the else
block, the test would pass if no exception were raised.


Test File Locations
+++++++++++++++++++

Unit tests are typically placed in a ``test`` subdirectory within the
directory where the source code being tested is located.  For example,
the test files for ``openmdao.main`` are located in
``openmdao.main/src/openmdao/main/test``.
