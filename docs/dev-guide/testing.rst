.. index:: testing
.. index:: nose
       
.. _Testing:

Testing
=======

By default, your top level ``devenv/bin`` directory will contain a script
called ``openmdao_test`` that uses a Python package called `nose
<http://somethingaboutorange.com/mrl/projects/nose>`_ to run all of the unit
tests for any package that you specify. For example, to run the full set
of openmdao unit tests, type:

::

   openmdao_test
   
which should generate output something like this:

::

    ........................................................................
    ........................................................................
    .....................................................................
    ----------------------------------------------------------------------
    Ran 293 tests in 142.846s
    
    OK


To run unit tests for a package only, for example ``openmdao.main``, type:

::

    openmdao_test openmdao.main
    
    
To get a list of options available with ``openmdao_test``, type  ``openmdao_test --help``
from the ``devenv`` directory.  Since the ``openmdao_test`` script uses the *nose* testing
framework internally, all options available when running *nosetests* should also be
available when using ``openmdao_test``.

.. index: test coverage


Test Coverage
-------------

A Python package called `coverage 
<http://nedbatchelder.com/code/modules/rees-coverage.html>`_, which is accessible through
``openmdao_test``, makes it easy to determine if your tests cover every
line of code in your source files.  To get a coverage report for the openmdao
package, go to the ``devenv`` directory and type the following:

::

   openmdao_test openmdao --with-coverage --cover-package=openmdao
   
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

   openmdao_test openmdao --cover-erase

.. index: pair: tests; adding
.. index: pair: tests; unit
.. index: unittest

Adding New Tests
----------------

Generally, you should write your tests using Python's `unittest
<http://docs.python.org/library/unittest.html>`_ framework if possible,
although the nose_ package is able to discover and run tests that do not use
unittest.

The following is a simple example of a unit test written using the unittest
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
    Test will fail if *expr* does not evaluate to True.
    
``self.assertEqual(val1,val2)``
    Test will fail if *val1* != *val2*
        
``self.assertNotEqual(val1,val2)``
    Test will fail if *val1* == *val2*
        
``self.assertAlmostEqual(val1,val2[,places=7])``
    Test will fail if *val1* differs from *val2* by more than a small
    number of decimal places.
    
``self.fail([msg])``
    Test will fail and display the given message.
    
Often in a test you will want to make sure that a specific exception is raised
when a certain thing happens, and usually you want to verify that the error
message contains certain information.  The unittest framework provides an
``assertRaises`` function that does part of this job, but it does not allow
you to check the error message. So the preferred way to test exceptions is
shown in the code below. In this example, we will assume that the exception
we are interested in is a *ValueError*. Note that we would place our
test function inside of our ``unittest.TestCase`` derived class.

.. parsed-literal::

    def test_myexception(self):
        try:
            # perform action here that should raise exception
        except ValueError, err:
            self.assertEqual(str(err), "this should be my expected error message")
        else:
            self.fail('expected a ValueError')

Note that the *else* block after the *except* is important because we want the
test to fail if no exception is raised. Without the *else*  block, the
test would pass if no exception were raised.


*Test File Locations*
+++++++++++++++++++++

Unit tests are typically placed in a ``test`` subdirectory within the
directory where the source code being tested is located.  For example,
the test files for ``openmdao.main`` are located in
``openmdao.main/src/openmdao/main/test``.


.. _Testing-Code-in-the-Documentation:

Testing Code in the Documentation
----------------------------------

The OpenMDAO documentation includes quite a few examples of Python code. These
examples are used to explain how to use features of the OpenMDAO API as well as
how to develop new components and plugins. Thus, it is imperative that any code
examples included in the documentation (particularly the *User Guide* and the
*Developer Guide*) be tested to ensure that the code is error-free. 

Fortunately, there are tools built into the :term:`Sphinx` reStructuredText processor that
make the process of testing code samples much easier. Sphinx includes a builder
called *doctest* as a plugin in its standard library. Instead of building a
readable document as is done by the HTML builder, the doctest builder scans the
documentation files for code blocks and tests them. 

There are two types of code examples found in the documentation. The first type 
is a block of code as would be found in a Python script:

.. testcode::

    from openmdao.examples.enginedesign.engine import Engine
    my_engine = Engine("new_engine")
    
The second type of code example is a copy of an interactive shell session:

.. doctest::

    >>> print "Hello!"
    Hello!
    
Both of these types of code samples must be tested, although the way to
accomplish this differs slightly in each case. Unlike the doctest module built into Python,
which can handle only shell session blocks, the doctest builder included with
Sphinx can handle both of these code sample types.


*Testing Code Blocks*
+++++++++++++++++++++

The doctest builder in Sphinx provides a flexible environment to easily and
effectively test blocks of code using a set of directives. The test code
directive is used to mark a block of text that should be tested and treated
as Python code. It is not always possible to execute a standalone block of
Python code without first executing some preliminary code containing any
prerequisites (e.g., imports.) The *testsetup* block makes it possible to run
the preliminary code. This block is hidden by default, so it does not show
up in the generated HTML documentation. Additionally, there is a *testoutput*
block, where any output that is generated by the *testcode* block should be
included so that it can be tested.

A simple example of how to implement these three blocks is shown here:

::

    .. testsetup:: Group1
    
    # Put any preliminary code that needs to run before the sample code. 
    # This block does not show up when Sphinx builds the HTML
    
    .. testcode:: Group1

    # This is the sample code that shows up in your docs
    
    .. testoutput:: Group1
    
    # If your code block outputs anything when executed, then that output
    # needs to go in this block.

*Group1* is a label that we've given this set of blocks. You can have
multiple labels in your documents. Also, the testsetup and
testoutput blocks are both optional. Some code examples don't need either.
You can have multiple testcode blocks for a single testsetup block. The
environment is preserved across all of the testcode blocks in a given group, so
that the code executed in the first testcode block in Group1 affects all later
blocks in Group1.

The label is optional, and defaults to *default* when not explicitly defined.

The *doctest* directive is used to specify blocks of interactive shell Python
code. If the directive is omitted, the doctest builder can often
find the Python blocks by itself, but it is still a good idea to include it so
that you can control the environment.

::

    .. doctest:: Group2
    
    >>> # This code is tested

The doctest blocks share their workspace in a similar manner as the testcode
blocks.  There are other options that can be enabled for the doctest blocks, but
so far the default ones have been fine.

More details on using the doctest builder can be found here: http://sphinx.pocoo.org/ext/doctest.html


.. _Including-Code-Straight-from-the-Source:

*Including Code Straight from the Source*
+++++++++++++++++++++++++++++++++++++++++

At times it is more efficient to directly include code from a source
file. The built-in Sphinx directive that enables this is called the *literalinclude*
block:

::

    .. literalinclude:: ../../openmdao.examples/openmdao/examples/enginedesign/engine_wrap_c.py
       :start-after: engine_weight = 0.0
       :end-before: # end engine.py
       :language: python
       
The first line contains the relative path location of the file that is to be 
included. Since you rarely want to include an entire file, the options
``start-after`` and ``end-before`` can be used to define the bookends that
bound the block of text to be included. 
       
Sometimes, it makes more sense to grab specific lines from a file. This can
also be done with the *lines* option.

::

    .. literalinclude:: ../../openmdao.examples/openmdao/examples/enginedesign/engine_wrap_c.py
       :lines: 3,7-12,45
       :language: python

More details on the literalinclude directive can be found at http://sphinx.pocoo.org/markup/code.html.       
       
*Helpful Tips*
++++++++++++++

* Tracebacks don't have to be accurately reproduced (and they can't be
  anyway). Handle these by replacing the traceback with ellipses:

    >>> my_engine.set("RPM",7500)
    Traceback (most recent call last):
        ...
    TraitError: Trait 'RPM' must be a float in the range [1000.0, 6000.0] but a value 
    of 7500 <type 'int'> was specified.

* Indentation is not preserved between code blocks in the same group. This means that all functions and
  class definitions effectively close when the block ends. If you need to show code from the middle of a
  function class, you may have to get creative in what you place in your testsetup block (e.g., defining
  *self* as something.)

* Care should be taken to assure that each block of code is being tested. One way to do this is to
  purposefully introduce an error into a block to verify that it is caught.

* Be wary of including code by line number. If the source file is changed and lines
  are added or removed, then the included code might not be what was intended.

* To include a numerical example in a doctest block, you can use ellipses to match the output to a
  specific tolerance. For example, this block of text passes: 

    >>> import numpy
    >>> numpy.pi 
    3.14...

* Sphinx automatically generates syntax highlighting for the code block, but it can get confused if you mix tabs and spaces.
    
*Running the Tests*
+++++++++++++++++++

The doctests are automatically run whenever you run ``openmdao_test``, but you can
also run them separately. In an active openmdao virtual environment, type:

::

    openmdao_testdocs

If the test was successful, you should see output similiar to the following:

::

    Doctest summary
    ===============
      156 tests
        0 failures in tests
        0 failures in setup code
    build succeeded.

If any tests fail, they will be noted in this summary, and specific tracebacks
will be given for each failure earlier in the output.

.. note:: If you make changes to the docs, you must rebuild the docs by running ``openmdao_build_docs``.


