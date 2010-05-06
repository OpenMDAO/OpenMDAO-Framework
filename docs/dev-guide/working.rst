.. index:: branch; working on

.. _Working-on-Your-Branch:

Working on Your Branch
======================

You need to build and activate your virtual development environment before you
can use OpenMDAO. If you have not done this, please see 
*Activating the Virtual Environment*.

.. todo:: turn *Activating the Virtual Environment* into a link.


.. note::  In some cases the examples are written from the Linux perspective. Windows users
   need to replace the ``/`` with a ``\``.

.. index:: pair: source code; editing and debugging
.. index:: pair: source files; adding

Adding New Source Files
-----------------------

If you create new files or directories that you want to be part of OpenMDAO, you
must add them to your repository by running the command:

::

   bzr add <filename>
        
If ``<filename>`` is a directory, all files within the directory will also be
added to the repository, unless they match any of the patterns in the
``.bzrignore`` file located in the top level directory of the branch.  To add a
new pattern for Bazaar to ignore, type:

::

   bzr ignore <pattern>
   
where ``<pattern>`` can be a filename or a wildcard expression, e.g., ``*.exe``.


If you add a file or directory to the repository by mistake, type:

::

   bzr remove <filename> --keep
   
This will remove the file from the repository but will **not** delete it.


.. _Merging-to-Your-Branch:

Merging to Your Branch
-----------------------

As you work on your branch, you may want to update it from openmdao ``trunk`` on
Launchpad to avoid conflicts when you push your branch back to the trunk (for
developers who have this privilege). 

You should be in the top level directory of your branch. Type:

::

  bzr status                       (Checks branch for uncommitted changes; you cannot merge if you have any.) 
  bzr commit -m "<commit_message>" (Required only if you have uncommitted changes. You may add a message on the
                    command line or omit the "-m" and press "Enter" to type the required message
                    using your default text editor.)
  cd devenv                        (Takes you to your virtual development environment.) 
  source bin/activate              (Activates your virtual development environment [requires bash]
                                    On Windows: "Scripts\activate.bat")
  openmdao_build_docs              (Builds the docs from scratch. Optional if no doc changes.)
  openmdao_docs                    (Checks that docs display correctly. Optional if no doc changes.)
  openmdao_test                    (Runs all openmdao unit tests. Merge only if they all pass.) 
  bzr merge lp:openmdao            (Merges from the trunk)

**- If you have no conflicts,** you can continue. Type:

::

  rm -rf devenv                (Removes the old virtual dev environment)
  python2.6 go-openmdao-dev.py (Builds your new virtual dev environment)
  cd devenv
  source bin/activate          (Activate the virtual dev environment [use Scripts\activate on Windows])
  openmdao_test                (Confirms that all tests still pass.)
  bzr commit -m "<commit_message>"  (Commit changes from merge to avoid mixing them up with any later
                                     changes you make)
  
You are now ready to continue development on your branch.


.. _if-you-have-a-conflict:

**- If you HAVE a conflict,** Bazaar will display not only the changes in
files or directories, but it will also indicate the number of conflicts and
where they occur. See the following example:


.. figure:: ../images/quick-ref/merge_conflict.png
   :align: center
   
   Example of Conflicts When Merging


In the above example, the "+N" indicates new files or directories. The "M"
indicates modified files or directories. If a file or directory is deleted,
"-D" appears before its name. In this example there are two conflicts that
must be resolved before proceeding.

If you have a conflict, please refer to `Resolving Conflicts
<http://doc.bazaar.canonical.com/bzr.2.1/en/user-guide/resolving_conflicts.html>`_
in the *Bazaar User Guide.*


.. index:: branch; pushing to Launchpad

.. _Pushing-a-Branch-Back-to-Launchpad:

Pushing a Branch Back to Launchpad
-----------------------------------

The following instructions are for both Linux, OS X, and Windows. However, on
Windows, depending on how you created your SSH keys, you may need to have
Pageant running before you can merge to your branch or push it to Launchpad.

First, make sure all of your changes are committed and that your your branch
builds and passes all tests.

**- If you have commit privileges** (you are a member of the *OpenMDAO Devs* group)

In order to avoid unexpected changes to recent revision numbers on the trunk,
you should always merge your branch to the trunk instead of merging the trunk
to your branch.  This is a little more work because you have to make a separate
branch from the trunk, then merge your development branch to that one, then 
merge your local version of the trunk up to the trunk on Launchpad.

If you have any conflicts when merging, you must resolve them before you can
continue. If you have a conflict, please refer to `Resolving Conflicts
<http://doc.bazaar.canonical.com/bzr.2.1/en/user-guide/resolving_conflicts.html>`_
in the *Bazaar User Guide.*

Type the following:

:: 
  
  bzr branch lp:openmdao                   (Gets a copy of the openmdao trunk)
  cd openmdao                              (Takes you to the trunk copy)
  bzr merge <path_to_your_merging_branch>  (Merges your branch to the trunk copy)
  python2.6 g-openmdao-dev.py              (Builds virtual environment for trunk copy)
  cd devenv                                (Takes you to the virtual environment on the trunk copy)
  source bin/activate                      (Activates trunk copy's virtual environment on Linux or OS X [requires bash]. 
                                            On Windows: "Scripts\activate")
  openmdao_test                            (Confirms that all tests pass)
  bzr commit -m <comment>                  (Commits your merge changes to trunk copy [assuming tests pass])
  
If you can build successfully and pass the tests after the merge, you may push
your branch to the OpenMDAO trunk on Launchpad. You must have a Launchpad
account and you must have your public SSH key registered with it in order to
push a branch. Type:

::
  
  bzr push lp:openmdao                (Pushes your merged trunk copy to openmdao trunk)

Your branch becomes the latest revision of openmdao on Launchpad.


**- If you do NOT have commit privileges** (you are a member of the *Contrib* group only)

You will push your branch up to the openmdao repository, but the changes do not become a part of the
development trunk until one of the reviewers merges it. 

You need to be somewhere on the branch to be pushed. Then type the following
command, replacing ``userid`` with your Launchpad userid and replacing
``branch_name`` with the name of the branch you are pushing.

::

  bzr push lp:~userid/openmdao/branch_name 

Now that your branch is in on Launchpad, you can request that it be merged by following 
the instructions below.

1. Go to `OpenMDAO <https://launchpad.net/openmdao>`_ on Launchpad and log in if you are not logged in
   already.

2. Click on the *Branches* tab at the top of the page to go to *Bazaar branches of OpenMDAO.* On this page
   you will see the openmdao trunk (which has a star in front of *Development*). Below that you should see
   all branches that have been uploaded but are not on the trunk, including the branch that you just pushed
   up. (You may need to refresh your screen.)

3. Click on the name of your branch to take you to the page for that branch. On this new page you will see the
   command for getting this branch (pulling it down to your work area). Note that you are the owner of the
   branch; as such, you are the only one who can "push" to it. (If you wanted to collaborate with someone and
   have your branch available to pull down, you could leave it there and not immediately propose a
   merge.)

4. Click on *Propose for merging.* You will see a new page, *Propose branch for merging.*

5. In the *Description of the Change* box provide the information requested below the box. If your branch is
   associated with a Trac ticket, and you have already provided detailed information about your changes in Trac
   (possibly even a test), you may just want to refer to that ticket (e.g., "See Trac ticket 30."). 

6. When you have completed the description, click the *Propose Merge* button. (Alternatively you many cancel the
   merge request at this point.) If you clicked on the *Propose Merge* button, a new page, *Proposal to merge
   branch,* will appear. It shows the proposed branch (your branch name) and what it will merge into
   (lp:openmdao). Your description of the changes is shown. 
   
   At the bottom of the page is a message that says an updated diff will be available in a few minutes. If you
   wish to view a graphical interface of the differences, refresh your screen and a new screen will pop up
   showing the differences between the trunk and your branch (in color). This file may be downloaded and saved if
   desired.

You have now completed the process for proposing that your branch be merged. In a short time, you will receive a
copy of an email that went to the gatekeeper of all merge proposals. The email will show you as the sender, and the
subject will be the the merge of your branch to openmdao:  ``[Merge]lp:~username/openmdao/branch_name into
openmdao``. The email will contain the proposal for merge and an attachment showing the differences.
(This diff file is just a text file and is not very readable.)

After the proposal for merge has been reviewed, you will get an email from the reviewer indicating whether the
proposal was approved or disapproved.

- If your proposal for merge was approved, you will get an email from ``noreply@launchpad.net`` after your branch
  has been merged. 

- If your proposal for merge was disapproved, you can continue working on your branch. (If you have a Trac
  ticket open, it will be transitioned back to the WORKING state.)



