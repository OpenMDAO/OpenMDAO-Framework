
.. accessing Github (Linux)::

Accessing OpenMDAO on Github -- SSH Keys
============================================

The source repository for OpenMDAO is read-accessible to the public, so making
a clone of it does not require that you have a :term:`Github`
account. If you plan to contribute to the OpenMDAO project on Github, you will
need to have a Github account and to register your public SSH key with that
account. The following section describes how to register your SSH key with
Github.

These instructions assume that you already have a Github account. If you do
not, please go to https://github.com and register for an account. Go ahead
and log in to your Github account, since you will need to be logged in to
register your key.

Linux or Mac OS X Users
-----------------------

Please follow these instructions for the SSH key creation/registration process
if you use Linux or Mac OS X.

*Creating Your Key on Linux*
++++++++++++++++++++++++++++

1. First, check to see if you already have an SSH key.  Look for a file called ``~/.ssh/id_rsa.pub``.
   If the file is there, skip to the next section and learn how to register your key with Github.
2. You should be in your home directory on your Linux machine. At the prompt, type: 
   ``ssh-keygen -t rsa -C "your_email@youremail.com"``. 
3. When prompted, press *Enter* to accept the default file name for your key. 
4. Press *Enter* when prompted for a password and then press it again to
   confirm that you are NOT entering a password. Your key pair is stored in ``~/.ssh/
   as id_rsa.pub`` (public key) and ``id_rsa`` (private key).

.. note::  In the unusual event that the ``ssh-keygen`` command fails, you may need to install
   OpenSSH. To do this requires that you have admin privileges. On Ubuntu, you can install
   OpenSSH by opening your terminal and typing: ``sudo apt-get install openssh-client``. 


Windows Users
-------------

If you use Windows, you must use *Git --> Git Bash* to start up a bash terminal, but after
that the instructions are the same as for linux.
   

*Registering the Key with Github*
+++++++++++++++++++++++++++++++++

You need to register and upload the *public* portion of your SSH key to Github. 

1. Open your public key in a text editor and copy its contents to your clipboard. The public key
   file has the extension ``.pub``; for example:  ``id_rsa.pub`` 
2. You must be logged into Github for this step. Go to *Account Settings --> SSH Public Keys*, 
   then click on *Add another public key*.
3. Enter a name into the *Title* text field to remind you which machine/account you're 
   entering ssh information for.
4. Paste your public key into the *Key* text box and then click the *Add Key* button (below the
   text box) to continue. 

