Accessing OpenMDAO Code on Launchpad (Windows)
==============================================

.. note:: These instructions are incomplete and have not been tested by multiple users.

If you are behind a proxy (this applies to GRC team members), you must create SSH keys for
each platform you want to use before you can push a branch to Launchpad. If you are not
behind a proxy, you may omit this step.

These instructions assume that you already have a Launchpad account. If you do not, please go
to https://launchpad.net and register for an account. Go ahead and log in to your Launchpad
account, since you will need to be logged in to register your key. 


Creating Your Key on Windows
----------------------------

**Windows (PuTTY)**

1. Download the PuTTY Key Generator from here:
   http://www.chiark.greenend.org.uk/~sgtatham/putty/download.html

2. Run ``puttygen.exe`` and click the generate button. PuTTYGen will ask you to move your mouse around
   in the blank area to generate entropy.

3. Set a key passphrase and confirm it.

4. Click *Save public key* and choose a location to save it. Click *Save private key* and choose a
   secure location to save it. Keep the locations secret.

5. Keep your Puttygen window open and copy the public key from it.
   (You need to register the key with Launchpad).

6. Run/Install Pageant as either a standalone .exe or as part of the entire putty package. Make sure Pageant is running and rt+click the icon in the notification area. Add the private key you saved earlier. This step is crucial if you want to connect to launchpad from a Windows PC.


**Cygwin/Windows (PuTTY)**

Follow these steps if you wish to use Pageant under Cygwin.

1. Follow the procedure in the **Windows (PuTTY)** section.

2. Add ``BZR_SSH`` as an environment variable with the value *plink*.

3. Close all existing bash terminals and open a new terminal.

4. Run ``plink INSERT_YOUR_USERNAME_HERE@bazaar.launchpad.net`` and accept "yes" to store the server's
   host key.
   

Registering the key with Launchpad
----------------------------------

You need to register and upload the *public* portion of your SSH key to Launchpad. 

1. Go to your `SSH keys <https://launchpad.net/people/+me/+editsshkeys>`_ page. 

2. Paste your public key into the text box and then click the *Import Public Key* button (below the
   text box) to continue. 
 
 
Notifying Launchpad of Your Userid
-----------------------------------

You need to provide Launchpad with your userid before you can push a branch to openmdao. 

Incomplete.
