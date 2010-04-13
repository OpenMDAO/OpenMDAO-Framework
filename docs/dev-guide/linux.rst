
.. accessing Launchpad (Linux)::

Accessing OpenMDAO Code on Launchpad (Linux)
============================================

If you are behind a proxy (this applies to GRC team members), you must create SSH keys for
each platform you want to use before you can merge from openmdao to your branch or push a branch back
to openmdao on Launchpad. (An SSH key  is not needed create a branch.) If you are not behind a proxy,
you may omit this step.

These instructions assume that you already have a Launchpad account. If you do not, please go
to https://launchpad.net and register for an account. Go ahead and log in to your Launchpad
account, since you will need to be logged in to register your key. 

Creating Your Key on Linux
---------------------------

1. You should be in your home directory on your Linux machine (for GRC users, it is torpedo). At
   the prompt, type: ``ssh-keygen -t rsa``. 
2. When prompted, press ``Enter`` to accept the default file name for your key. 
3. Press "Enter" when prompted for a password and then press it again to
   confirm that you are not entering a password. Your key pair is stored in ``~/.ssh/
   as id_rsa.pub`` (public key) and ``id_rsa`` (private key).

.. note::  In the unusual event that the ``ssh-keygen`` command fails, you may need to install
   OpenSSH. To do this requires that you have admin privileges. On Ubuntu, you can install
   OpenSSH by opening your terminal and typing: ``sudo apt-get install openssh-client``. 


Registering the Key with Launchpad
----------------------------------

You need to register and upload the *public* portion of your SSH key to Launchpad. 

1. Open your public key in a text editor and copy its contents to your clipboard. The public key
   file has the extension *.pub*; for example:  ``id_rsa.pub`` 
2. You must be logged into Launchpad for this step. Go to your `SSH keys
   <https://launchpad.net/people/+me/+editsshkeys>`_ page. 
3. Paste your public key into the text box and then click the *Import Public Key* button (below the
   text box) to continue. 


Notifying Launchpad of Your Userid
----------------------------------

You need to provide Launchpad with your userid before you can merge from openmdao to your branch or
push a branch back to openmdao. In your home directory on your Linux machine, type: 

::

  bzr launchpad-login userid

.. note:: If you do not know your userid, log in to Launchpad and click on your name in the upper
   right-hand corner. This takes you to an *Overview* page. In the first column, under *User Information*, 
   you should see **OpenID login**. The hyphenated name in the web address is your userid (e.g.,
   ``john-j-smith``).

If the above command failed, you may be missing an ``authentication.conf`` file or have incomplete
information in your ``bazaar.conf`` file. To check whether you have these files, type the following
commands:

1. ``cd  ~/.bazaar``.
2. ``ls``
3. If you do not see the ``authentication.conf`` file, use a text editor of your choice to
   create it.  
4. In the blank file that you just created, type:

::
     
     [Launchpad]
     host = .launchpad.net
     scheme = ssh
     user = launchpad-username    (e.g., john-j-smith)
  
5. Save and exit the ``authentication.conf`` file.
6. Use your text editor to open the ``bazaar.conf`` file. Make sure your Launchpad username is in the
   file. If it isn't, add it at the end of the file, for example:  ``launchpad_username = john-j-smith``.
7. Save any changes and exit the file.


.. index:: branch; pulling from Launchpad

Pulling a Branch from Launchpad
-------------------------------

1. Go to your working directory. (For GRC users, this is ``/Openmdao/dev/<your_working_directory>``).
2. To pull a branch down from the openmdao trunk on Launchpad, type: 

::
  
  bzr branch lp:openmdao branchname



.. index:: branch; pushing to Launchpad


.. _Pushing-a-Branch-Back-to-Launchpad:

Pushing a Branch Back to Launchpad
----------------------------------

First, make sure all your changes are committed and that your your branch builds and passes all tests. Go to
the branch you want to push. (At GRC, that would be ``Openmdao/dev/<your_working_directory/<branch_name>``.)
Type:

::

  cd /OpenMDAO/dev/<your_working_directory>/<branch_name>    (Takes you to the branch to be merged.) 
  bzr status                     (Checks for uncommitted changes. You cannot merge if there are any.)
  deactivate                     (Deactivates your old virtual environment if it's active)
  rm -rf devenv                  (Removes your old virtual environment)
  python2.6 go-openmdao-dev.py   (Builds your new virtual environment in the devenv directory)
  cd devenv                      (Goes to your new virtual environment.)
  source bin/activate            (Activates your new virtual environment)
  openmdao_test --all            (Runs the test suite. Tests should pass on your branch.)
  bzr commit -m "<commit_message>"     (Needed only if you have uncommitted changes.)

Your branch is now ready for merge.  If you have commit priveleges to the openmdao trunk on
launchpad, then you must follow the instructions in the following section.  If not, you
can simply push your branch up to launchpad as follows:


Next, you must branch from the openmdao trunk, then merge your current branch to your copy of the
trunk.  This is necessary because if you merge the other direction and then push to launchpad,
it will mess up the log for the trunk, making it harder to find information about recent
merges. If you have any conflicts, you must resolve them before you can continue. 
(See :ref:`if you have a conflict <if-you-have-a-conflict>`.) 

:: 
  
  cd /OpenMDAO/dev/<your_working_directory>   (Takes you to your top level development directory.) 
  bzr branch lp:openmdao                      (Gets a copy of the openmdao trunk.)
  cd openmdao                                 (Takes you to the trunk copy)
  bzr merge ../<your merging branch>          (Merges your branch to the trunk copy)
  python2.6 g-openmdao-dev.py                 (Build virtual environment for trunk copy)
  cd devenv                                   (Takes you to the virtual environment on the trunk copy.)
  source bin/activate                         (Activates trunk copy's virtual environment)
  test_openmdao --all                         (Confirms that all tests pass.)
  bzr commit -m <comment>                     (Commits your merge changes to trunk copy (assuming tests pass)
  
If you can build successfully and pass the tests after the merge, you may push your branch to openmdao. 
You must be logged into Launchpad to push a branch.

**- If you have commit privileges** (you are a member of the *OpenMDAO Devs* group), type:

::
  
  bzr push lp:openmdao                 (Pushes your merged trunk copy to openmdao trunk.)

Your branch becomes the latest revision of openmdao on Launchpad.


**- If you do NOT have commit privileges** (you are a member of the *Contrib* group only), you push your
branch up to the openmdao repository, but the changes do not become a part of the development trunk until
one of the reviewers merges it. 

To push your branch, type the following command, replacing ``userid`` with your hyphenated Launchpad userid
and replacing ``branch_name`` with the name of the branch you want to push.

::

  bzr push lp:~userid/openmdao/branch_name 

Now that your branch is in on Launchpad, you must request that it be merged. Please follow the instructions
below.

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


