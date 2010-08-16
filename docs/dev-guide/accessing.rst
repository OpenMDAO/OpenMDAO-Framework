
.. accessing Launchpad (Linux)::

Accessing OpenMDAO on Launchpad -- SSH Keys
============================================

The source repository for OpenMDAO is read-accessible to the public, so making
a :term:`branch` from it does not typically require that you have a :term:`Launchpad` account.
However, if you are behind a proxy, you *may* need to not only have a Launchpad
account but also register your public SSH key with your Launchpad account
before you can read or write to the repository. If you require *write* access
to the OpenMDAO project on Launchpad, either to push changes directly to the
trunk or to push up personal branches for merge approval, you will need to
have a Launchpad account and to register your public SSH key with that
account, regardless of whether you are behind a proxy or not. The following
section describes how to register your SSH key with Launchpad.

These instructions assume that you already have a Launchpad account. If you do
not, please go to https://launchpad.net and register for an account. Go ahead
and log in to your Launchpad account, since you will need to be logged in to
register your key.

Linux or Mac OS X Users
-----------------------

Please follow these instructions for the SSH key creation/registration process
if you use Linux or Mac OS X.

*Creating Your Key on Linux*
++++++++++++++++++++++++++++

1. First, check to see if you already have an SSH key.  Look for a file called ``~/.ssh/id_rsa.pub``.
   If the file is there, skip to the next section and learn how to register your key with Launchpad.
2. You should be in your home directory on your Linux machine. At the prompt, type: ``ssh-keygen -t rsa``. 
3. When prompted, press *Enter* to accept the default file name for your key. 
4. Press *Enter* when prompted for a password and then press it again to
   confirm that you are NOT entering a password. Your key pair is stored in ``~/.ssh/
   as id_rsa.pub`` (public key) and ``id_rsa`` (private key).

.. note::  In the unusual event that the ``ssh-keygen`` command fails, you may need to install
   OpenSSH. To do this requires that you have admin privileges. On Ubuntu, you can install
   OpenSSH by opening your terminal and typing: ``sudo apt-get install openssh-client``. 

*Registering the Key with Launchpad*
++++++++++++++++++++++++++++++++++++

You need to register and upload the *public* portion of your SSH key to Launchpad. 

1. Open your public key in a text editor and copy its contents to your clipboard. The public key
   file has the extension ``.pub``; for example:  ``id_rsa.pub`` 
2. You must be logged into Launchpad for this step. Go to your `SSH keys
   <https://launchpad.net/people/+me/+editsshkeys>`_ page. 
3. Paste your public key into the text box and then click the *Import Public Key* button (below the
   text box) to continue. 


*Notifying Launchpad of Your Userid*                                                    
++++++++++++++++++++++++++++++++++++

You need to provide Launchpad with your userid before you can merge from openmdao to your branch or
push a branch back to openmdao. In your home directory on your Linux machine, type: 

::

  bzr launchpad-login userid

.. note:: If you do not know your userid, log in to Launchpad and click on your name in the upper
   right-hand corner. This takes you to an *Overview* page. In the first column, under *User
   Information*, you should see **OpenID login**. The web address will contain your userid. 

If the above command failed, you may be missing an ``authentication.conf`` file or have incomplete
information in your ``bazaar.conf`` file. To check whether you have these files, type the following
commands:

1. ``cd  ~/.bazaar``
2. ``ls``
3. If you do not see the ``authentication.conf`` file, use a text editor of your choice to
   create it.  
4. In the blank file that you just created, type:

::
     
     [Launchpad]
     host = .launchpad.net
     scheme = ssh
     user = <launchpad-userid>    
  
5. Save and exit the ``authentication.conf`` file.
6. Use your text editor to open the ``bazaar.conf`` file. Make sure your Launchpad userid is in the
   file. If it isn't, add it at the end of the file, for example: ``launchpad_userid = john-j-smith``.
7. Save any changes and exit the file.


.. seealso:: For information on creating a branch, building, and pushing a branch to
             openmdao on Launchpad, see :ref:`Working-on-Your-Branch`.

Windows Users
-------------

If you use Windows, please follow these instructions for SSH key creation and registration. 

.. _Creating-Your-Key-on-Windows:

*Creating Your Key on Windows*
++++++++++++++++++++++++++++++

1. In your user directory (on Vista, for example, ``C:\Users\<win-username-here>)``, create a new directory
   called ``.ssh``. **Creating this directory with Explorer will not work because of the leading dot!** To
   create this directory, start a command prompt and navigate (using the ``cd`` command) to your user
   directory and type ``md .ssh``.

2. Assuming :term:`PuTTY` is installed in ``C:\Putty``, go there and run :term:`PuTTYgen`. If it isn't
   installed, go to this `page <http://www.putty.org>`_ and download it. 

3. Within PuTTYgen, click the *Generate* button and move your mouse around as instructed. **Do not 
   enter a key passphrase.**

4. Once the key is generated, go to the *Conversions* menu and choose *Export OpenSSH Key*. (DON'T USE
   the *Save Private Key* button.) In the save dialog, navigate to the ``.ssh`` folder and save the
   key that you created in Step 1 above. Save the private key as ``id_rsa`` (no file extension).
   PuTTYgen will warn you that you're making a key without a passphrase. Click *YES* to let PuTTYgen
   know that you are sure you want to save this key without a passphrase to protect it. 

   Check to make sure your key is not created with a ``.ppk`` extension. If it has an extension, use
   Explorer to navigate to this file and rename it so it has NO extension.


*Registering the Key with Launchpad*
+++++++++++++++++++++++++++++++++++++

1. In your ``.ssh`` directory, create the file: ``id_rsa.pub``  

2. Copy the string from the field titled *Public Key for pasting into OpenSSH authorized_keys file:* onto
   your clipboard; then paste it into the newly-created ``id_rsa.pub`` file. Save this file.

3. Start a browser and navigate to ``Launchpad.net``. Log in to your Launchpad account. Once logged in, you can
   either edit your user information or  go directly to the URL:  
   ``https://launchpad.net/~lp-userid-goes-here/+editsshkeys``.

4. Your clipboard should still contain ``id_rsa.pub``. Paste the contents of ``id_rsa.pub`` into the *Add an
   SSH key* field. Click *Import Public Key* and wait for the key to be added.

5. From the Windows command line (assuming you have Bazaar installed and assuming you have a LaunchPad
   account) you should now be able to do: ``bzr launchpad-login <your-lp-userid-here>``.

.. note:: If you do not know your userid, in Launchpad click on your name in the upper
   right-hand corner. This takes you to an *Overview* page. In the first column, under *User Information*, 
   you should see **OpenID login**. The web address will contain your userid. 

6. If everything worked correctly as you followed these steps, you should be able to create a branch
   using ``bzr branch lp:openmdao`` without getting any SSH key errors.
