.. index:: branch; working on

.. _Working-on-Your-Branch:

Working on Your Branch
======================

Any changes you make in your repository should always be done on a branch other
than *master* or *dev*.  You should keep those branches clean and just use them
to track changes in the corresponding branches in the official OpenMDAO-Framework
repository.

To create a new branch to work in, do the following:

::

   git branch <branch_name>
   

To switch to an existing branch, do:

::

   git checkout <branch_name>
   

A shorthand for creating a branch and then switching to it is:

::

   git checkout -b <branch_name>


The name you give your branch should reflect the purpose of the branch to
avoid confusion with other branches in your repository. And don't fix multiple
bugs or add multiple features on the same branch. If you keep your branch
changes small by targeting a specific bug or feature, the maintainers of the
project will have a much easier time merging in your changes. And remember, in
*Git*, creating branches is cheap and quick, so there's no need to worry about
creating lots of branches.

.. note:: Make sure to check which branch you're on whenever you create a new
     branch.  Creating a branch off of the wrong branch can bring in unexpected
     changes and generally cause confusion.
   


.. _Creating-the-Virtual-Environment:


Creating the Virtual Environment
--------------------------------

OpenMDAO operates inside of a virtual python environment. To create that environment, 
run ``python go-openmdao-dev.py`` from the top directory of your
repository. The script will check the version of Python you are running. **You must
be running version 2.6.** (To find out which Python version you are
running, you can type ``python --version``.)

.. note:: If you're using Visual Studio on Windows, you need to run the installer from a 
   command window that has the Visual Studio environment variables set. The
   easiest way to do this is to select the *Visual Studio 2008 Command Prompt*
   from the *Visual Studio Tools* menu under *Microsoft Visual C++ 2008
   Express Edition* in the Start menu. If you're using something other than
   the Express edition, then the name of the Start menu option will be
   slightly different, i.e., replace "Express" with "Professional" or
   "Standard."


::

   python go-openmdao-dev.py
   
Running ``go-openmdao-dev.py`` creates a ``devenv`` directory at the top of
your repository and populates it with all of the packages that OpenMDAO
depends upon. It also installs the openmdao namespace packages in your virtual
Python environment as "develop" eggs so that you can make changes to the
source code and immediately see the results without having to rebuild any
distributions.

      
.. _Activating-the-Virtual-Environment:

Activating the Virtual Environment
----------------------------------

The next step is to activate your virtual Python environment. Change your directory to
``devenv``. 

On Linux or Mac OS X, you must be running the Bash shell. If you are in Bash, omit this step.

  ::

     bash
   
 
  Next, type the following, making sure to include the "." in the command:

  ::

     . bin/activate



Or, on Windows, type:

  ::

     Scripts\activate

At this point, your ``devenv`` directory should contain the following subdirectories, unless you are
on Windows. On Windows, the directory structure is slightly different, as noted below.

``bin`` 
    Contains Python and a number of other scripts that are associated with the Python
    packages that are installed in the virtual environment. On **Windows,** this
    directory is called ``Scripts``.

``etc``
    Contains miscellaneous files that don't fit in ``bin, lib,`` or ``include``.
    
``include``
    Contains Python C header files. If you are on **Windows,** you will not have this directory.


``lib``
    Contains Python standard library and installed modules.

After your virtual Python environment has been activated, you can add other 
distributions to the environment by using ``easy_install`` or :term:`pip` in
the same manner that you would add packages to the system level Python.

If you need to build the OpenMDAO docs, you can run ``openmdao_build_docs``.
Running ``openmdao_docs`` will display the documents in HTML in the default browser.

You can deactivate the environment by typing:


:: 

  deactivate
  
 
  
.. note:: Whenever you switch to a different branch within your repository,
   you should deactivate your virtual environment and re-run
   ``go-openmdao-dev.py``, unless you're certain that no OpenMDAO package
   dependencies have changed.



Now that you have built and activated your virtual development environment,
you are ready to use OpenMDAO.

The following sections provide information on how to carry out some basic
actions in the development environment. You would perform these actions on a
repository that you cloned from the OpenMDAO-Framework repository on GitHub.

.. note::  In some cases the examples are written from the Linux perspective. 
   Windows users need to replace the ``/`` with a ``\``.

.. index:: pair: source code; editing and debugging
.. index:: pair: source files; adding
.. index:: guidelines

Guidelines for Development
--------------------------

We try to follow the `PEP 8`__ style guidelines for Python code, and we use `pylint`__ 
to check for PEP 8 compliance.

.. __: http://www.python.org/dev/peps/pep-0008
.. __: http://www.logilab.org/857


Adding New Source Files
-----------------------

If you create new files or directories that you want to be part of OpenMDAO, you
must add them to your repository by running the command:

::

   git add <filename>
        
If ``<filename>`` is a directory, all files and subdirectories (and their
files) within the directory will also be added to the repository, unless they
match any of the patterns in the ``.gitignore`` file(s) located above it in
the repository directory tree. To add a new pattern for Git to ignore, edit the
appropriate ``.gitignore`` file.  Directories to be ignored should end with a 
forward slash (/), and glob patterns are allowed.


If you add a file or directory to the repository by mistake, type:

::

   git rm --cached <filename>
   
This will remove the file from the staging area but will **not** delete it from the
working tree.  Leaving off the ``--cached`` will cause the file to be deleted from
the working tree in addition to being removed from the staging area.


.. index:: Committing changes

.. _Committing-changes:

Committing Changes 
------------------

After you make changes on your branch, make sure you :term:`commit`, or
record, these changes to your local repository. To see if you have uncommitted
changes or untracked files, type:

::

   git status -s
  

Here's an example of the kind of output you might see:

::

    M README.txt
   M  go-openmdao-dev.py
   ?? anewfile.py
   ?? somejunk.txt


The first thing to look at are the files on lines beginning with ``??``, which indicates
that the file has not been added to the repository.  If any of these file are supposed
to be part of the repository, you should ``git add`` them.

The files on lines beginning with ``M`` have been modified. If you omit the ``-s`` argument,
you'll get a more verbose output that can be helpful if you're not sure what to do.
For a full discussion of all options and output formats for the *status* command,
use Git's built-in help:

::

   git help status
   

You can use the ``git help`` command to get detailed help information about
any Git command.  The help command has the form:

::

   git help <command>


When you commit changes, you must add a commit message. To commit and add the
message on the command line (inside quotation marks), type:

::

  git commit -am "<commit message>"

If you omit the ``m`` option and press *Enter,* a text editor will open a
new file where you must enter the required commit message.  

.. note:: It's very important to add the ``-a`` option to ``git commit``,
   because if you don't, only the *staged* files will be committed.  This can lead
   to very confusing behavior and should be avoided.

One nice GitHub feature is that if you're working on a particular GitHub
issue, you can include the text "closes GH-???" in your commit comment,
replacing the *???* with the number of the GitHub issue, and GitHub will
automatically close that issue for you when your commit makes its way back to
the original repository.


Running Tests
-------------

For detailed information on running tests on your branch, please see :ref:`Testing`.



.. _Working_with_Remote_Repositories:

Working with Remote Repositories
--------------------------------

You won't be the only one making updates to the OpenMDAO source code, so from
time to time you'll want to update your repository with the latest information
from the master OpenMDAO-Framework repository. To do that, you need
to first understand about *remotes*, which are just short aliases for remote
repositories that you need to interact with.

When you first clone the OpenMDAO-Framework repository on GitHub, Git will
automatically add a remote to your local repository called *origin* that
refers back to the OpenMDAO-Framework repository. Branches in remote
repositories are referred to using names of the form
``<remote_name>/<branch_name>``. In the case of OpenMDAO, there will be two
branches, named *origin/master* and *origin/dev*. A local branch named
*master* will also be created automatically when you first create your
repository.

There will be times when you want to reference other remote repositories
beyond just *origin*.  To add new remotes to your repository, you use
the ``git remote add`` command. It has the following form:

::

   git remote add <shortname> <url>
   

Assuming that you have created a personal fork of the OpenMDAO-Framework
repository as discussed in
:ref:`Making-a-Personal-Fork-of-OpenMDAO-Framework`, you can now add a remote
for it called *myfork*.


::

   git remote add myfork https://userid@github.com/userid/OpenMDAO-Framework.git
   
where *userid* is your Github userid.


Using the ``git remote`` command with no arguments will give a list of
the remotes that you currently reference in your repository.


To update references to remote branches in your local repository, you
need to *fetch* their data from the remote repository they live in. For
example, to get the latest updates from *origin*, you would enter:

::

   git fetch origin
   
   
This just updates your repository database with new data from origin's remote
branches, but to update your local branch, you'll have to merge the
remote branch with yours. For example, say someone updated the *dev* branch in
the official OpenMDAO-Framework repository and you want to update your current branch
with those changes. Assuming you've already fetched the data from origin using
``git fetch origin``, you can merge it to your current branch using:

::

   git merge origin/dev
   

Or you can also use the *pull* command, which combines a fetch and a merge:

::

   git pull origin dev
   

When you merge another branch into yours, if there are any changes to the
``go-openmdao-dev.py`` file or to any of the ``setup.py`` files in the source
tree, it's a good idea to remove the old virtual environment and build a new
one to ensure that the correct packages will be used based on the
correct package dependencies for the current code snapshot. To do this, first
make sure your virtual environment is deactivated by typing ``deactivate``,
then remove the ``devenv`` directory from the top level of the repository,
then run ``python go-openmdao-dev.py`` again. Then reactivate your virtual
environment.


After a merge, it's good practice to confirm that all tests still pass by typing:

::

  openmdao_test
  
 
You are now ready to continue development on your branch.


.. _if-you-have-a-conflict:

**- If you HAVE a conflict,** the ``git merge <branchname>`` command will fail.  Here's
an example of a failed merge:

::


   $ git merge somebranch
   Auto-merging somefile.txt
   CONFLICT (content): Merge conflict in somefile.txt
   Automatic merge failed; fix conflicts and then commit the result.


If you do a ``git status`` after a failed merge, all of the files with conflicts 
will be listed as *unmerged*.

The unmerged file will contain a section for each conflict that includes contributions
from the two merged branches.  Each contribution is clearly delimited, so it's possible
to just fire up your favorite editor and update the conflicts in each file.  It's 
easier though if you use the ``git mergetool`` command, which will bring up a graphical
three way diff tool that should make it a lot easier to see what's going on.  The
particular diff tool that is used depends on the platform where you run the command.
You can edit your personal Git configuration to make it use whatever your favorite
graphical diff tool may be.

Once you've updated the conflicted files, you tell Git that the conflicts are resolved
by running ``git add`` on each file. After resolving all conflicts, run ``openmdao_test``
to verify that everything is in working order, then commit the changes from the
merge:

::


   git commit -am "<commit comment>"


.. index:: branch; pushing to GitHub

Contributing Your Changes
-------------------------

At some point you'll finish adding your new feature or implementing your bug
fix and you'll want to get your changes into the official version of OpenMDAO.
Here's a little checklist to go through to make sure that your update is
actually finished:

1. If you've changed anything that needs to be documented, update the OpenMDAO docs.
2. If you updated the docs, rebuild them by running ``openmdao_build_docs`` to make sure
   that you didn't break them.
3. Add unit tests for whatever functionality you updated.  If it was a bug fix, put in a test
   that shows the bug is fixed. If you added a new feature, then add some tests to verify
   that it works as expected.
4. Run the OpenMDAO test suite using the ``openmdao_test`` command.  If any tests fail,
   fix them and rerun the tests until they all pass.

Once you've done all of these things, you're ready to push your changes up to
your personal OpenMDAO fork and ask the OpenMDAO maintainers to merge your changes
into the official *dev* branch.

To push the changes from your *<branchname>* branch up to your fork which you
earlier named *myfork*, use the ``git push`` command:

::

   git push myfork <branchname>


Once that's done, *myfork/<branchname>* will have your changes and you can make a pull
request to the OpenMDAO maintainers.  To issue a pull request, follow these steps:

1. Go to the page for your personal OpenMDAO fork on GitHub.

2. Select the branch you wish to have *pulled* from the **Switch Branches** dropdown
   near the top of the page.

3. Push the *Pull Request* button.

4. You will be prompted to fill in a description of your changes.  The message near 
   the top of the page should read something like 
   "You're asking OpenMDAO to pull 1 commit into OpenMDAO:dev from <userid>:<branchname>",
   where <userid> is your GitHub userid and <branchname> is the name of the branch to
   be pulled.  If the source and destination branches are correct, push the 
   "Send Pull Request" button.  Otherwise, click on *Change Commits* and modify the
   branch names.
