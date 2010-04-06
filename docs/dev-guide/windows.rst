Accessing OpenMDAO Code on Launchpad (Windows)
==============================================

If you are behind a proxy (this applies to GRC team members), you must create SSH keys for
each platform you want to use before you can push a branch to Launchpad. If you are not
behind a proxy, you may omit this step.

These instructions assume that you already have a Launchpad account. If you do not, please go
to https://launchpad.net and register for an account. Go ahead and log in to your Launchpad
account, since you will need to be logged in to register your key. 


Creating Your Key on Windows
----------------------------

**Windows (PuTTY)**

People at GRC should follow these directions; however, omit Step #1 since the PuTTY Key Generator is already
installed.


1. Download the PuTTY Key Generator from here:
   http://www.chiark.greenend.org.uk/~sgtatham/putty/download.html

2. Go to Putty and run ``puttygen.exe``. 

   For GRC users, go to your (C:) drive, double-click on the *Putty* folder, and double-click on *puttygen* to
   bring up the Putty Key Generator. 
  
3. Click on the *Generate* button. You must move your mouse around in the blank area, so PuTTYGen knows there is
   a human behind the request.

4. Leave the *Key passphrase* and *Confirmation passphrase* boxs blank. Click *Save public key* and choose a
   location to save it. 
   
5. Click *Save private key*. When asked whether you are sure you want to save the key without a passphrase, click
   *yes* and save it to a secure location.

6. Keep your Puttygen window open and copy the public key from it. (You need to register the
   key with Launchpad.)

7. Run/install Pageant as either a standalone .exe or as part of the entire putty package. 

   GRC users should have Pageant on their (C:) drive. Double-click on it to run it. 
   
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


Registering the key with Launchpad
----------------------------------

You need to register and upload the *public* portion of your SSH key to Launchpad. 

1. Go to your `SSH keys <https://launchpad.net/people/+me/+editsshkeys>`_ page. 

2. Paste your public key into the text box and then click the *Import Public Key* button (below the
   text box) to continue. 
   

Notifying Launchpad of Your Userid
----------------------------------
	
You need to provide Launchpad with your userid before you can merge from openmdao to your branch or
push a branch back to openmdao. In your home directory on your Windows machine, type: 

::

  bzr launchpad-login userid

.. note:: If you do not know your userid, log in to Launchpad and click on your name in the upper
   right-hand corner. This takes you to an *Overview* page. In the first column, under *User Information*, 
   you should see **OpenID login**. The hyphenated name in the web address is your userid (e.g.,
   ``john-j-smith``).

Pulling a Branch from Launchpad
-------------------------------

If you wish to pull a branch from openmdao, from your home directory, type:

::
  
  bzr branch lp:openmdao branchname


Push a Branch to Launchpad
--------------------------

Pageant must be running before you can merge to your branch or push it to Launchpad. Please
see the instructions for the Linux machine on :ref:`Pushing-a-Branch-Back-to-Launchpad`. If
you have a conflict, please see `Resolving Conflicts
<http://doc.bazaar.canonical.com/bzr.2.1/en/user-guide/resolving_conflicts.html>`_ in the
*Bazaar User Guide*.






 
