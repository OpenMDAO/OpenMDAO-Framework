
.. _Troubleshooting-OpenMDAO:

.. _Troubleshooting:

Troubleshooting
===============
There have been some known issues with installing OpenMDAO in certain settings.  In this section, 
some of the most common installation pitfalls will be addressed.  Also discussed will be the actions that a
user could take to receive assistance when faced with an inscrutable installation problem or error message.

Forums
------

The OpenMDAO Open-Source Questions and Answers (OSQA) Forum (http://openmdao.org/forum) is the first place
that a user should go to try to find an answer.  A user should search this forum before asking a question, commenting on 
existing questions where appropriate (e.g. "I have this problem, too!") but only asking her question once 
it is determined that the concern is a unique one.  Users should also follow forum ettiquite, 
upvoting and downvoting answers and comments, and formally accepting an answer when satisfied.

Inspecting Your Python Environment
----------------------------------

**envdump**

Sometimes users have trouble with OpenMDAO installation or with some failing tests, and the 
reasons why are not readily apparent.  At these times, it can be helpful to examine the environment into 
which the installation is being attempted.  OpenMDAO provides a specific environment-dumping script for extracting this
information. 
 
If installation has succeeded, then from a prompt in an activated environment, a user can simply type:
::

      envdump

And a full output of the machine's environment will be printed to the screen.

If installation isn't working then you won't have the script yet, so you can download the envirodump.py script from the Miscellaneous Downloads 
section openmdao.org/downloads.  Once dowloaded, one can manually run the script by typing:
::

      python envirodump.py
      
This should produce the same results as running the envdump console script.  Sometimes, examining the output
of the environment will expose obvious path problems that can expidite the troubleshooting process.


**Capturing Envdump, Installation Output, or Test Output**
At times, some problems will be not be obvious fixes, and you might want to get help from the OpenMDAO developers.
The first step, as discussed above, should be to post in the OpenMDAO forums and ask your questions there. Posting in the forums
makes sure that the answers are archived for others who might have the same problem. If the problem can't be solved easily though, 
the OpenMDAO developers might request some more detailed information from you. In these cases you will need to capture the output of an 
installation, a test run, or an envdump (or more than one of these events) in order to send these full 
reports to the OpenMDAO development team for inspection.

When directing standard output (I/O stream 1) and standard error (I/O stream 2) into the same file, try:

::

      command 1> filename 2>&1

(Where "command" is something like ``python envirodump.py`` or ``openmdao test``, and where filename is something
like ``my_envdump.txt`` or ``my_testoutput.out``.)

Once the outputs have been captured into properly-named files, please email the files to the OpenMDAO development 
team using the email support@openmdao.org


Common Install Problems
-----------------------

 * In general, OpenMDAO supports the specified versions of Python found at python.org.  Alternate Python builds, such as Enthought,
   Active State python or python(x,y) are not officially supported at this time, though some may work with OpenMDAO.  
 
 * OpenMDAO does not support Python 3.x, nor does it support Python versions below 2.6.5.  Having the incorrect
   Python installation in the PATH and/or not having the OpenMDAO-approved Python version in the PATH will only lead
   to problems.
 
 * Verions of numpy and scipy that are incompatible with the Python version installed often cause problems such as failing tests.  
   Several users have had problems in which they had an existing Python installation and exisiting versions of numpy or scipy installed.  
   The users updated Python, but not the numpy or scipy to the corresponding versions, which can cause strange errors that are tough to diagnose.
 
 * Remember when installing new Python versions or compilers, and then adding them to the PATH, that
   restarting the system shell is necessary to see the change. Put more simply, you need to close the command window (a.k.a. terminal window)
   and open a new one so the changes to the PATH will take effect. 
 
 * Trying to run things without activating the virtual environment will cause things to seem to not be installed. You need to activate your environment 
   every time you open a new shell. 
 
 * If you have multiple versions of Python installed on your machine, then make sure that the version 
   that's being run when you type ``python`` is an OpenMDAO-compatible version.  Otherwise, you need 
   to specify the Python version in the command line, e.g. ``python2.6 go-openmdao.py``.
  
 * Not remembering to deactivate the virtual environment when finished will cause problems if changes are made to the 
   virtualenv.  It would need to be deactivated, changed, rebuilt, and then reactivated.

**Windows Specific**

  * Usernames with spaces have been known to cause various problems in our tests for distributed simulations.
   
  * When Python is installed on Windows, the PATH environment variable won't be updated to inlcude the installation
    directory.  This must be done manually, and if it isn't, can result in problems.


**Mac OS X Specific**

  * Installing the proper version of XCode and the proper combination of C and Fortran compilers is the most
    challenging thing to get OpenMDAO running with Leopard, Snow Leopard and Lion.  See the User Guide for more details on
    getting this correct.
  
  * Using g77 instead of gfortran.  If you have g77 installed, and you're seeing errors like ``ld: library not found for -lcc_dynamic``.
    To avoid g77 being found, you can just rename g77 to _g77 or something else.  Then gfortran will be
    used in its place, and things will work properly again.


**Linux Specific**

  * Not running installation or testing in bash will cause things to fail.




