
.. accessing Launchpad (Linux)::

Accessing OpenMDAO on Launchpad -- SSH Keys
============================================

If you are behind a proxy, you must create SSH keys for each platform you want to use
before you can merge from openmdao to your branch or push a branch back to openmdao on
Launchpad. (An SSH key is not needed create a branch.) If you are not behind a proxy, you
may omit this step.

These instructions assume that you already have a Launchpad account. If you do not, please go
to https://launchpad.net and register for an account. Go ahead and log in to your Launchpad
account, since you will need to be logged in to register your key. 

Linux Users
-----------

Please follow these instructions for the SSH key creation/registration process if you use Linux.

*Creating Your Key on Linux*
++++++++++++++++++++++++++++

1. You should be in your home directory on your Linux machine. At the prompt, type: ``ssh-keygen -t rsa``. 
2. When prompted, press ``Enter`` to accept the default file name for your key. 
3. Press "Enter" when prompted for a password and then press it again to
   confirm that you are not entering a password. Your key pair is stored in ``~/.ssh/
   as id_rsa.pub`` (public key) and ``id_rsa`` (private key).

.. note::  In the unusual event that the ``ssh-keygen`` command fails, you may need to install
   OpenSSH. To do this requires that you have admin privileges. On Ubuntu, you can install
   OpenSSH by opening your terminal and typing: ``sudo apt-get install openssh-client``. 

*Registering the Key with Launchpad*
++++++++++++++++++++++++++++++++++++

You need to register and upload the *public* portion of your SSH key to Launchpad. 

1. Open your public key in a text editor and copy its contents to your clipboard. The public key
   file has the extension *.pub*; for example:  ``id_rsa.pub`` 
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
   right-hand corner. This takes you to an *Overview* page. In the first column, under *User Information*, 
   you should see **OpenID login**. The web address should contain your userid. (In some cases
   this may be a hyphenated name, such as  ``john-j-smith``; in other cases, it may be a first initial
   and last name, such as ``jsmith``.)

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
     user = <launchpad-username>    (e.g., john-j-smith)
  
5. Save and exit the ``authentication.conf`` file.
6. Use your text editor to open the ``bazaar.conf`` file. Make sure your Launchpad username is in the
   file. If it isn't, add it at the end of the file, for example:  ``launchpad_username = john-j-smith``.
7. Save any changes and exit the file.


.. seealso:: For information on creating a branch, building, and pushing a branch to
             openmdao on Launchpad, see :ref:`Working-on-Your-Branch`.

Windows Users
-------------

Please follow these instructions for the SSH key creation/registration process if you use Windows.

*Creating Your Key on Windows*
++++++++++++++++++++++++++++++

**Windows (PuTTY)**

If you already have PuTTY installed on your machine, omit Step #1.


1. Download the PuTTY Key Generator from here:
   http://www.chiark.greenend.org.uk/~sgtatham/putty/download.html

2. Go to Putty and run ``puttygen.exe`` to bring up the PuTTY Key Generator. 
  
3. Click on the *Generate* button. You must move your mouse around in the blank area, so PuTTYGen knows there is
   a human behind the request.

4. Leave the *Key passphrase* and *Confirmation passphrase* boxes blank. Click *Save public key* and choose a
   location to save it. 
   
5. Click *Save private key*. When asked whether you are sure you want to save the key without a passphrase, click
   *yes* and save it to a secure location.

6. Keep your Puttygen window open and copy the public key from it. (You need to register the
   key with Launchpad.)

7. Run/install Pageant as either a standalone .exe or as part of the entire putty package. 

      
8. Make sure Pageant is running and right-click on the icon in the lower right corner of your screen. 

9. Select the *Add Key* option. (This step is crucial if you want to connect to Launchpad from a Windows PC.)
   
   You can now register your key.


.. note:: Follow the steps below if you wish to use Pageant under Cygwin.

**Cygwin/Windows (PuTTY)**

1. Follow the procedure in the **Windows-(PuTTY)** section.

2. Add ``BZR_SSH`` as an environment variable with the value *plink*.

3. Close all existing bash terminals and open a new terminal.

4. Run ``plink INSERT_YOUR_USERNAME_HERE@bazaar.launchpad.net`` and accept *yes* to store the server's
   host key.


*Registering the key with Launchpad*
+++++++++++++++++++++++++++++++++++++

You need to register and upload the *public* portion of your SSH key to Launchpad. 

1. Go to your `SSH keys <https://launchpad.net/people/+me/+editsshkeys>`_ page. 

2. Paste your public key into the text box and then click the *Import Public Key* button (below the
   text box) to continue. 
   

*Notifying Launchpad of Your Userid*
+++++++++++++++++++++++++++++++++++++
	
You need to provide Launchpad with your userid before you can merge from openmdao to your branch or
push a branch back to openmdao. In your home directory on your Windows machine, type: 

::

  bzr launchpad-login userid

.. note:: If you do not know your userid, log in to Launchpad and click on your name in the upper
   right-hand corner. This takes you to an *Overview* page. In the first column, under *User Information*, 
   you should see **OpenID login**. The web address should contain your userid. (In some cases
   this may be a hyphenated name, such as  ``john-j-smith``; in other cases, it may be a first initial
   and last name, such as ``jsmith``.)



 
