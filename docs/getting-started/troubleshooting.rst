
.. _Troubleshooting-OpenMDAO:

.. _Troubleshooting:

Troubleshooting
===============

Some users have had issues installing OpenMDAO under certain circumstances. In this section, we'll address some of
the most common installation pitfalls.  We'll also discuss what you can do to receive
assistance when faced with an installation problem or error message that you don't understand.

Forum
------

The OpenMDAO Open Source Question and Answer (OSQA) Forum (http://openmdao.org/forum) is the first
place that you should go to find an answer to your question. Search this forum before asking a
question or commenting on existing questions (e.g., "I have this problem, too!"). If a question is
asked just once, it's probably unique. Remember to follow forum etiquette -- upvote and downvote
answers and comments, and formally accept an answer when you are satisfied.

Inspecting Your Python Environment
----------------------------------

**envdump**

You may have trouble installing OpenMDAO or experience a test failure, and the reasons why might not
be readily apparent. If this happens, it may help to examine the environment where you are
attempting the installation. OpenMDAO provides a specific environment-dumping script for extracting
this information. 

If the installation was successful, then from a prompt in an activated environment, you can simply type:
::

      envdump

A full output of the machine's environment will be printed to the screen.

If the installation was not successful, then you won't have the script yet, but you can download the
``envirodump.py`` script from the **Miscellaneous Downloads** section at
``openmdao.org/downloads``.  Once the script is downloaded, to manually run it, type:

::

      python envirodump.py
      
This should produce the same results as running the envdump console script.  Sometimes, examining the output
of the environment will expose obvious path problems that can expedite the troubleshooting process.


**Capturing Envdump, Installation Output, or Test Output**

At times, a problem will not have an obvious fix, and you might want to get help from the OpenMDAO
developers. The first step, as discussed above, should be to post your question in the OpenMDAO
forum. Posting in the forum makes sure that the answers are archived for others who have the same
problem. If the problem can't be solved easily though, the OpenMDAO developers might request more
detailed information from you. In these cases you will need to capture the output of an attempted
installation, a test run, or an envdump (or more than one of these events) and send the full
report(s) to the OpenMDAO development team for inspection.

When directing standard output (I/O stream 1) and standard error (I/O stream 2) into the same file, try:

::

      command 1> filename 2>&1

(Where ``command`` is something like ``python envirodump.py`` or ``openmdao test``, and where ``filename`` is something
like ``my_envdump.txt`` or ``my_testoutput.out``.)

Once the outputs have been captured into properly named files, please email the files to the OpenMDAO development 
team using the email ``support@openmdao.org``.


Common Install Problems
-----------------------

* In general, OpenMDAO supports the specified versions of Python found at ``python.org``. At this time we do not officially
  support alternative Python builds, such as Enthought, ActiveState Python or Python(x,y), although some may
  work with OpenMDAO.  

* OpenMDAO does not support Python 3.x or any Python versions below 2.7.  Having the incorrect
  Python installation in the PATH and/or not having the OpenMDAO-approved Python version in the PATH will only lead
  to problems.

* Versions of numpy and scipy that are incompatible with the Python version installed often cause
  problems, such as failing tests.  Several users have had problems when they already had an
  existing Python installation and existing versions of numpy or scipy installed.  The users updated
  Python -- but not the numpy or scipy -- to the corresponding versions. In such cases strange
  errors can occur that are tough to diagnose.

* Remember that when you install new Python versions or compilers and then add them to the PATH, it's necessary to restart the system
  shell to see the change. Put more simply, you need to close the command window (a.k.a. terminal window) and open a new one so the changes
  to the PATH will take effect. 

* If you try to run things without activating the virtual environment, it will seem as though programs have not been installed. You
  need to activate your environment every time you open a new shell. 

* If you have multiple versions of Python installed on your machine, then make sure that the version 
  that's being run when you type ``python`` is an OpenMDAO-compatible version.  Otherwise, you need 
  to specify the Python version in the command line, e.g., ``python2.7 go-openmdao.py``.

* If you forget to deactivate the virtual environment when finished, and the virtualenv gets changed, you'll
  encounter problems. You'll have to deactivate the virtualenv, change it, rebuild, and then reactivate it.

**Windows Specific**

* Usernames with spaces have been known to cause various problems in our tests for distributed simulations.

* When you install Python on Windows, the PATH environment variable won't be updated to include the installation
  directory. You'll need to update it manually, and if you don't, problems may result.


**Mac OS X Specific**

* Installing the proper version of XCode and the proper combination of C and Fortran compilers is the most
  challenging thing to get OpenMDAO running with Leopard, Snow Leopard, and Lion.  See the :ref:`System-Requirements` for
  more details on getting this correct.

* When the system is trying to use g77 instead of gfortran: If you have g77 installed and you're seeing errors like ``ld: library not found
  for -lcc_dynamic``, you can just rename g77 to _g77 or something else to avoid g77 being found. Then gfortran will be used, and things will
  work properly again.


**Linux Specific**

* If you do not install or test in bash, failures will occur.




