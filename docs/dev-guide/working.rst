.. index:: branch; working on

.. _Working-on-Your-Branch:

Working on Your Branch
======================

Now that you have built and activated your virtual development environment, you are ready to use
OpenMDAO. If you have not activated your environment, please do so before proceeding. See
:ref:`Activating-the-Virtual-Environment`.

The following sections provide information on how to carry out some basic
actions in the development environment. You would perform these actions on a
repository that you cloned from your fork of the OpenMDAO repository on Github.

.. note::  In some cases the examples are written from the Linux perspective. Windows users
   need to replace the ``/`` with a ``\``.

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
the repository directory tree. To add a new pattern for git to ignore, edit the
appropriate ``.gitignore`` file.  Directories to be ignored should end with a 
forward slash (/), and glob patterns are allowed.


If you add a file or directory to the repository by mistake, type:

::

   git rm --cached <filename>
   
This will remove the file from the staging area but will **not** delete it from the
working tree.  Leaving off the ``--cached`` will cause the file to be deleted from
the working tree in addition to being removed from the staging area.


.. index:: Committing changes

.. _Commiting-changes:

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
use git's built-in help:

::

   git help status
   

You can use the ``git help`` command to get detailed help information about
any git command.  The help command has the form:

::

   git help <command>


When you commit changes, you must add a commit message. To commit and add the
message on the command line (inside quotation marks), type:

::

  git commit -am "<commit message>"

If you omit the ``m`` option and press *Enter,* a text editor will open a
new file where you must enter the required commit message.  

.. note: It's very important not to forget to add the ``-a`` option to ``git commit``,
   because if you don't, only the *staged* files will be committed.  This can lead
   to very confusing behavior and should be avoided.

One nice Github feature is that if you're working on a particular Github
issue, you can include the text "closes GH-???", replacing the *???* with the
number of the Github issue, and Github will automatically close that issue for
you when your commit makes its way back to the original repository.


Running Tests
-------------

For detailed information on running tests on your branch, please see :ref:`Testing`.



.. _Working_with_Remote_Repositories:

Working with Remote Repositories
--------------------------------

You won't be the only one making updates to the OpenMDAO source code, so from
time to time you'll want to update your repository with the latest information
from the master OpenMDAO repository. In order to do that, you need to first
understand about *remotes*, which are just short aliases for remote
repositories that you need to interact with.

When you first clone the OpenMDAO repository on Github, git will automatically
add a remote to your local repository called *origin* that refers back to the
OpenMDAO repository. In addition, git creates a branch in your repository with
name of the form: ``origin/<branch_name>`` for each branch in your remote
repository. In general, branches in remote repositories are referred to using
names of the form ``<remote_name>/<branch_name>``. In the case of OpenMDAO,
there will be two branches, named *origin/master* and *origin/dev*.  A local
branch named *master* will also be created automatically when you first create
your repository.

There will be times when you want to reference other remote repositories
beyond just *origin*. In order to add new remotes to your repository, you use
the ``git remote add`` command. It has the following form:

::

   git remote add <shortname> <url>
   
   
As an example let's add a remote to our personal fork of OpenMDAO on Github.
If you haven't created one yet, simply log into Github and go to
http://github.com/OpenMDAO/OpenMDAO-Framework. There, near the top of the page you'll
see a *Fork* button. Press it and you're done.

Now that you have a remote repository that you want to reference from your local
repository, you can add a remote for it, calling it *myfork*.

::

   git remote add myfork git@github.com:userid/OpenMDAO-Framework.git
   
   
where *userid* is your Github userid. Note that in this case the URL we've
used is an SSH URL, because we want to be able to write to our OpenMDAO fork.
For remote repositories that we only need read access to, we would use a URL
of the form ``git://github.com/some_userid/some_repo_name.git``.

Using the ``git remote`` command with no arguments will give a list of
the remotes that we currently reference in our repository.


In order to update references to remote branches in your local repository, you
need to *fetch* their data from the remote repository they live in. For
example, to get the latest updates from *origin*, you would enter:

::

   git fetch origin
   
   
This just updates your repository database with new data from origin's remote branches, but
in order to update your local branch, you'll have to merge the remote branch with yours. For
example, say someone updated the *dev* branch in the official OpenMDAO repository and you 
want to update your current branch with those changes.  Assuming you've already fetched
the data from origin using ``git fetch origin``, you can merge it to your current branch
using:

::

   git merge origin/dev
   

When you merge another branch into yours, if there are any changes to the ``go-openmdao-dev.py``
file or to any of the ``setup.py`` files in the source tree, it's a good idea to remove the old
virtual environment and build a new one in order to ensure that the correct packages will be
used based on the correct package dependencies for the current code snapshot.  To do this,
first make sure your virtual environment is deactivated by typing ``deactivate``, then
remove the ``devenv`` directory from the top level of the repository, then run
``python go-openmdao-dev.py`` again.  Then reactivate your virtual environment.


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
You can edit your personal git configuration to make it use whatever your favorite
graphical diff tool may be.

Once you've updated the conflicted files, you tell git that the conflicts are resolved
by running ``git add`` on each file. After resolving all conflicts, run ``openmdao_test``
to verify that everything is in working order, then commit the changes from the
merge:

::


   git commit -am "<commit comment>"


.. index:: branch; pushing to Github

Contributing Your Changes
-------------------------

At some point you'll finish adding your new feature or implemeting your bug
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
your personal OpenMDAO fork and ask the OpenMDAO maintainers to merge in your
changes.

To push the changes from your *<branchname>* branch up to your fork which you
earlier named *myfork*, use the ``git push`` command:

::

   git push myfork <branchname>


Once that's done, *myfork/<branchname>* will have your changes and you can make a pull
request to the OpenMDAO maintainers.  You can do this by going to the page for your
OpenMDAO fork on Github and pushing the *Pull Request* button.  You will be prompted to
fill in a description of your changes, then just hit the "Send Pull Request" button.

