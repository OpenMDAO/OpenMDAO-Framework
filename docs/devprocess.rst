
-------------------------------------
Working With the OpenMDAO Source Code
-------------------------------------

.. contents:: Contents

.. sectnum::



Overview
--------

This is the beginning of the documentation for OpenMDAO developers that
attempts to explain the process of how OpenMDAO development works, and
how to interact with the various tools we use for configuration management,
testing, deployment, etc.  Right now, this is only a very basic outline
that needs to be greatly improved before it is ready for production use,
so if you're reading this now, consider yourself an alpha tester of the
process.  When you come across something that doesn't work or is confusing,
or you have thought of a way to improve some aspect of the process, please
write it down and email it to Bret.A.Naylor@nasa.gov or edit the file
yourself. The source file for this tutorial can be found in
``docs/devprocess.rst`` in the top level directory of your OpenMDAO source 
branch. If you don't know what a branch is, that will be explained below.


Getting Started
---------------

Before you can start working on source code or running tests, you need to get
your own copy to work with.  You do this by asking the configuration
management system, in our case we are using `bazaar <http://bazaar-vcs.org>`_,
to create a branch for you.  You can change anything in your branch without
affecting anyone else's code.  Once your changes are complete and have been
through the verification and validation  process, they can be merged with the
'official' version.


Where Is The Code?
==================

The OpenMDAO project source files are located under ``/OpenMDAO/dev``.  This
directory is what is called a shared repository, meaning that any branches
created under it share the same version tree.  Under ``OpenMDAO/dev`` is a
dirctory called ``trunk``.  This is the *official* version of the OpenMDAO
source. Developers cannot write directly to this version.  Writing to the
trunk can only be done by the configuration manager.  To make changes to
the code, a developer must first create a branch.


Creating Your Branch
====================

Before you create your branch, you should ``cd`` to
``/OpenMDAO/dev/developers/<username>`` where ``<username>`` is your user name
on *torpedo*.  bazaar_ will let you create your
branch anywhere you choose, but for purposes of organization for the OpenMDAO
project, you should create all of your branches under your user subdirectory
within the OpenMDAO shared repository.  This will allow the configuration
manager to easily find your branch when it needs to be merged.  Since my
user name is *bnaylor*, I would do the following:

::

  cd /OpenMDAO/dev/developers/bnaylor

Normally, you'll want to create your branch using the *trunk* as a basleline.
In some cases it will be necessary to branch off of another branch instead of
the trunk, but for now, let's just assume we're branching off of the trunk.
To create your branch, enter the following command:

::

  bzr branch /OpenMDAO/dev/trunk <my_branch_name>


where ``<my_branch_name>`` should follow the naming convention:

::

  T<ticket number>-<desc>


where *desc* is a short description of what the branch is for. For example:

::

  T1043-objserver_fix


would be a branch to fix something in the objserver and the ticket number is
1043.

            
Bootstrapping the Branch
========================

When you first create your branch, there are a couple of things that you'll
need to do to set things up before you can start working. The first step is to
create an isolated, empty python environment.  This is necessary in order to
ensure that changes made to the system level python environment don't affect
the branch you're working on, and changes you make to your python environment
don't affect the system environment or any other developer's environment. The
second step is to create what is called a *buildout*.


Creating an Isolated Python Environment
+++++++++++++++++++++++++++++++++++++++

We'll use something called `virtualenv
<http://pypi.python.org/pypi/virtualenv>`_ to create our virtual python
environment, and we'll use the ``--no-site-packages`` option to prevent our
environment from seeing any of the system level python packages.  But before
we issue the virtualenv command, we should cd into the top level directory of
our new branch.

::

  cd /OpenMDAO/dev/developers/bnaylor/T1043-objserver_fix
  
  
Once we're there, we can then create our virtual environment using virtualenv.

::
    
  virtualenv --no-site-packages <my-virtual-dir>
  
where ``<my-virtual-dir>`` can be named anything you like, but for the sake of
consistency across the OpenMDAO project, let's name it
``virtual-T<ticket number>``.  So in our example case, our virtualenv command
would be:

::

  virtualenv --no-site-packages virtual-T1043

One of the things that virtualenv_ does for us is modify the prompt of our
shell to let us know we're in a virtual environment. It does this by placing
the name of the top virtual environment directory in parens at the beginning
of the prompt.  For example:

::

  (virtual-T1043)[bnaylor@torpedo buildout]
  
Putting the ticket number in the name of the top level virtual directory will
help avoid confusion for those people that may need to interact with multiple
branches at the same time.

        - cd <my-virtual-dir>
        - Create buildout directory and copy bootstrap.py file there
            - (Windows)   
                - md buildout
                - copy <branch-dir>\branch_config\bootstrap.py buildout
                - copy <branch-dir>\branch_config\buildout.cfg buildout
            - (linux/mac) mkdir buildout
                - cp <branch-dir>/buildout_config/bootstrap.py buildout
                - cp <branch-dir>/buildout_config/buildout.cfg buildout
        - Activate the virtual environment
            - (Windows)  bin\activate
            - (linux/mac)
               - bash (if not already running bash)
               - source bin/activate
        - Bootstrap the buildout environment (only needed the first time)
            - cd buildout
            - python bootstrap.py  # bootstraps zc.buildout
        - Edit buildout.cfg (if necessary)
            - add any eggs/parts needed for your development
        - Run buildout (this should build the full environment needed)
            - bin/buildout
        - Test the buildout
            - bin/test openmdao -v

    - setting up Wing to work with your buildout
        - edit Project Properties
        - add eggs and develop-egg dirs to PYTHONPATH
        - save project
        

The Download Cache
==================

    - cd distrib-cache/dist
    - wget <url to distrib>  # pull distrib from the web

    
Adding New Source Files
=======================

    - bzr commands
    - paster create templates
        - basic package
        - nested namespace package
        
        
Testing
=======

    - unittest
    - nose


Creating New zc.buildout Recipes
================================

    - build an egg
    - zc.buildout entry points for each recipe
    - simple API
        - __init__(self, options, name, buildout)
        - install(self)  # returns list of files/dirs for later uninstall
        - update(self)
        - uninstall(self) # usually not necessary




