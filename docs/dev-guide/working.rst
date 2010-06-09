.. index:: branch; working on

.. _Working-on-Your-Branch:

Working on Your Branch
======================

Now that you have built and activated your virtual development environment, you are ready to use
OpenMDAO. If you have not activated your environment, please do so before proceeding. See
:ref:`Activating-the-Virtual-Environment`.

The following sections provide information on how to carry out some basic
actions in the development environment. You would perform these actions on a
branch that you pulled from Launchpad.

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

   bzr add <filename>
        
If ``<filename>`` is a directory, all files and subdirectories (and their files) within the
directory will also be added to the repository, unless they match any of the patterns in the
``.bzrignore`` file located in the top level directory of the branch.  To add a new pattern for
Bazaar to ignore, type:

::

   bzr ignore <pattern>
   
where ``<pattern>`` can be a filename or a wildcard expression, e.g., ``*.exe``.


If you add a file or directory to the repository by mistake, type:

::

   bzr remove <filename> --keep
   
This will remove the file from the repository but will **not** delete it.


.. index:: Committing changes

Committing Changes 
------------------

After you make changes on your branch, make sure you :term:`commit`, or record, these changes to your
local repository. To see if you have uncommitted changes, type:

::

  bzr status                     
  
If there are uncommitted changes, the modified files will be listed. Any files that were not added to
the repository will be listed as "unknown."  

When you commit changes, you must add a commit message. To commit and add the message on the command
line (inside quotation marks), type:

::

  bzr commit -m "<commit message>"

If you omit the ``-m`` and press *Enter,* your default text editor will open a new file where you must
enter the required commit message. 


Running Tests
-------------

For detailed information on running tests on your branch, please see :ref:`Testing`.


.. _Merging-to-Your-Branch:

Merging to Your Branch
-----------------------

As you work on your branch, you may want to update it (merge out) from the OpenMDAO :term:`trunk` on
Launchpad (for developers who have this privilege). You cannot merge from the trunk to your branch if
you have uncommitted changes. 

You should be in the top level directory of your branch. To check whether you have uncommitted
changes, type:

::

  bzr status                       
  
The next step is required only if you have uncommitted changes. You many add a message on the command
line (using ``-m``) or press *Enter* to type the required message using your default text editor.

::

  bzr commit -m "<commit_message>" 
  
Next, you will change directory to go to your virtual development environment:

::

  cd devenv                        
  
On **Linux** or **OS X,** you must be running Bash to activate your environment. If you are in Bash, omit
this next step.

::
 
  bash
  
To activate your virtual development environment, type:

::

  . bin/activate   
  

Or, on **Windows,** type:
 
::
  
   Scripts\activate
   
If you have doc changes, you can build the docs from scratch, by typing:
 
:: 
   
  openmdao_build_docs              
  

To display the docs:
 
:: 
 
  openmdao_docs                    
  
Before merging you should run all the unit tests. You may merge *only* if all tests pass.
To run tests, type:

:: 
 
  openmdao_test                    
  
And finally, to merge from the openmdao trunk to your branch, type:

::

  bzr merge lp:openmdao            
 

**- If you have no merge conflicts,** you can continue. 

It's a good idea to remove the old virtual environment, so type:

::

  rm -rf devenv 
  
To build your new virtual dev environment, type:

::                     
  
  python go-openmdao-dev.py   
  

To change to the dev environment:  

::
  
  cd devenv

On **Linux** or **OS X,** you must be running Bash to activate your environment. If you
are in Bash, omit this next step.

::
    
  bash
     
To activate your virtual developement environment, type:
  
::
  
  . bin/activate
    
    
Or, on **Windows,** type:
   
::
   
  Scripts\activate
     
To confirm that all tests still pass, type:     
  
::

  openmdao_test                    
  
If all tests pass, you may commit the changes from your merge. (This avoids mixing up merge
changes with any later changes you make.) Type:

::

  bzr commit -m "<commit_message>" 
  
 
You are now ready to continue development on your branch.


.. _if-you-have-a-conflict:

**- If you HAVE a conflict,** Bazaar will display not only the changes in
files or directories, but it will also indicate the number of conflicts and
where they occur. See the following example:


.. figure:: ../images/quick-ref/merge_conflict.png
   :align: center
   
   Example of Conflicts When Merging


In the above example, the "+N" indicates new files or directories. The "M" indicates
modified files or directories. If a file or directory is deleted, "-D" appears before its
name. In this example two conflicts must be resolved before proceeding.

If you have a conflict, please refer to `Resolving Conflicts
<http://doc.bazaar.canonical.com/bzr.2.1/en/user-guide/resolving_conflicts.html>`_
in the *Bazaar User Guide.*


.. index:: branch; pushing to Launchpad

.. _Pushing-a-Branch-Back-to-Launchpad:

Pushing a Branch Back to Launchpad
-----------------------------------

The following instructions are for Linux, OS X, and Windows. However, on Windows,
depending on how you created your :ref:`SSH keys <Creating-Your-Key-on-Windows>`, you may
need to have Pageant running before you can merge to your branch or push it to Launchpad.

First, make sure all of your changes are committed and that your your branch
builds successfully and passes all tests.

You will push your branch up to the openmdao repository, but the changes do not become a
part of the development trunk until one of the reviewers merges it. Note that your branch
may be reviewed by a senior developer as well as a tech writer (to make sure the
documentation is consistent and clear). 

On the branch to be pushed, type the following command, replacing ``userid`` with your
Launchpad userid and replacing ``branch_name`` with the name of the branch you are
pushing.

::

  bzr push lp:~userid/openmdao/branch_name 

Now that your branch is on Launchpad, you can request that it be merged by following 
the instructions below.

1. Go to `OpenMDAO <https://launchpad.net/openmdao>`_ on Launchpad and log in if you are
   not logged in already.

2. Click on the *Code* tab at the top of the page to go to *Bazaar branches of
   OpenMDAO.* On this page you will see the openmdao trunk, which should be the first
   branch listed. Below that you should see all branches that have been uploaded but are
   not on the trunk, including the branch that you just pushed up. (You may need to
   refresh your browser window.)

3. Click on the name of your branch to take you to the page for that branch. On this new
   page you will see the command for getting this branch (pulling it down to your work
   area). Note that you are the owner of the branch; as such, you are the only one who can
   "push" to it. (If you wanted to collaborate with someone and have your branch available
   to pull down, you could leave it there and not immediately propose a merge.)

4. Click on *Propose for merging.* You will see a new page, *Propose branch for merging.*

5. In the *Description of the Change* box provide the information requested below the box.
   If your branch is associated with a Trac ticket, and you have already provided detailed
   information about your changes in Trac (possibly even a test), you may just want to
   refer to that ticket (e.g., "See Trac ticket 30."). 

6. When you have completed the description, click the *Propose Merge* button.
   (Alternatively you many cancel the merge request at this point.) If you clicked on the
   *Propose Merge* button, a new page, *Proposal to merge branch,* will appear. It shows
   the proposed branch (your branch name) and what it will merge into (lp:openmdao). Your
   description of the changes is shown. 

   At the bottom of the page is a message that says an updated diff will be available in a few minutes. If you
   wish to view a graphical interface of the differences, refresh your browser window and a new display will pop up
   showing the differences between the trunk and your branch (in color). This file may be downloaded and saved if
   desired.

You have now completed the process for proposing that your branch be merged. In a short
time, you will receive a copy of an email that went to the gatekeeper of all merge
proposals. The email will show you as the sender, and the subject will be the merge of
your branch to openmdao:  ``[Merge]lp:~username/openmdao/branch_name into openmdao``. The
email will contain the proposal for merge and an attachment showing the differences. (This
diff file is just a text file and is not very readable.)

After the proposal for merge has been reviewed, you will get an email from the reviewer
indicating whether the proposal was approved or disapproved.

- If your proposal for merge was approved, you will get an email from
  ``noreply@launchpad.net`` after your branch has been merged. 

- If your proposal for merge was disapproved, you can continue working on your branch. (If
  you have a Trac ticket open, it will be transitioned back to the WORKING state.)



i
