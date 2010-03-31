Accessing OpenMDAO Code on Launchpad (Linux)
============================================

If you are behind a proxy (this applies to GRC team members), you must create SSH keys for
each platform you want to use before you can push a branch to Launchpad. If you are not
behind a proxy, you may omit this step.

These instructions assume that you already have a Launchpad account. If you do not, please go
to https://launchpad.net and register for an account. Go ahead and log in to your Launchpad
account, since you will need to be logged in to register your key. 

Creating Your Key on Linux
---------------------------

1. You should be in your home directory on your Linux machine (for GRC users, it is torpedo). At
   the prompt, type: ``ssh-keygen -t rsa``. 
2. When prompted, press ``Enter`` to accept the default file name for your key. 
3. Enter, and then confirm, a password to protect your SSH key. Your key pair is stored in ``~/.ssh/
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
	
You need to provide Launchpad with your userid before you can push a branch to openmdao. (Anyone can pull
a branch from openmdao.) In your home directory on your Linux machine, type: 

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

Pulling a Branch 
----------------

1. Go to your working directory. (For GRC users, this is ``/Openmdao/dev/<your_working_directory>``).
2. To pull a branch down from the openmdao trunk on Launchpad, type: 

::
  
  bzr branch lp:openmdao branchname


Pushing a Branch Back to Launchpad
----------------------------------

After you have finished making changes to your branch, you can then push those changes back up to
the trunk. You must be logged into Launchpad to push a branch to openmdao.

- If you have commit privileges (team members at GRC do) and want to push a branch to the trunk, you
  must first merge out from the trunk. Go to the branch you want to push. (At GRC, that would be
  ``Openmdao/dev/<your_working_directory/<branch_name``.) 

  The next steps assume that you have committed the changes on your branch and that everything built
  successfully and passed all tests. 
  
  Type the following commands:

::
  
  1. bzr merge lp:openmdao    
  2. cd buildout
  3. repo.py fix
  4. python2.6  isolated bootstrap
  5. bin/buildout
  6. bin/test --all
                          
If you can build successfully and pass the tests after the merge, you may push your branch to openmdao.
Type:

::
  
  bzr push lp:openmdao

This option makes your branch the latest revision on the trunk.

- If you do **not** have commit privileges, you need to upload your branch to your user area on Launchpad
  and then submit a request for merge. In the following command, replace ``userid`` with your Launchpad id,
  and replace ``branch_name`` with the name of the branch you want to push up to Launchpad.

::

  bzr push lp:~userid/openmdao/branch_name 

This option does not merge your branch to the trunk.  


TODO:
  * Add details about how developers without commit privileges request taht a branch be merged and
    where it goes when sent to openmdao. 
  * Add a section on how outside developers use Trac to submit, track, and transition their bugs or
    enhancements. This section probably goes on the website, but we need a link from here to that
    information (when it is available).
