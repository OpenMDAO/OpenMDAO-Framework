.. index:: simple example

.. _`A-Simple-Tutorial`:

Overview
==========

In this section, you are going to learn how to execute a simple optimization
problem using the OpenMDAO script interface. To get the most out of this
tutorial, you should be familiar (though you don't have to be proficient) with
the Python language and the concepts presented in
:ref:`Introduction-to-the-OpenMDAO-Framework`. You should understand the terms
:term:`Component`, :term:`Assembly`, and :term:`Driver`. If you don't have
much experience with Python, we recommend trying `Dive into Python
<http://diveintopython.org/>`_. It is an excellent introduction to Python that
is licensed under the GNU Free Documentation License, so you can download and
use it as you wish.

The problem we present here is a paraboloid that is a function of two input variables. Our goal is
to find the minimum value of this function over a particular range of interest. First, we will solve
this problem with no constraints. Then we will add constraints and solve the problem again. We will
not be providing analytical gradients. The optimizer will calculate numerical gradients internally.

If we express the problem as a block diagram, we can see how to set it up in OpenMDAO:

.. _`OpenMDAO-overview`:

.. figure:: ../../examples/openmdao.examples.simple/openmdao/examples/simple/Simple1.png
   :align: center
   :alt: Diagram shows the Paraboloid component and optimizer with arrows going between them representing the variables and minimization objective.

   A Simple Optimization Problem
   
The optimizer is the :term:`Driver`. Its job is to manipulate the two design
variables (*x* and *y*) to minimize the output of the ``paraboloid`` function
(*f*). The Paraboloid equation fits into the OpenMDAO process as a
:term:`Component`. This Paraboloid component contains a method that operates
on the inputs (*x* and *y*) and returns the value of the function (*f*)
evaluated at those inputs. Both the driver and the component are contained in
an :term:`Assembly`, which maintains the connections between the driver and
the component and knows how to run the system.

The following instructions will help you locate the directory containing
the pieces needed for the model.

If you have downloaded the latest release version from the website, the files you need should be
here:

:: 

  openmdao-X.X.X/lib/python2.6/site-packages/openmdao.examples.simple-X.X.X-######.egg/openmdao/examples/simple
    
``X.X.X`` is the current OpenMDAO version, and ``######`` is a string that
contains the Python version and the operating system description. This path may
vary depending on your system and version, but there will be only one
*simple* egg.
    
If you are a developer and have a branch from the source repository, the files you need will
be here:

::

  examples/openmdao.examples.simple/openmdao/examples/simple
  
    
Getting Started
----------------

The first thing you must do before running OpenMDAO is to activate the environment. If
you have not done this, then please refer to the instructions in the section on 
:ref:`installation <Installation>`.

With the environment activated, you can run OpenMDAO from anywhere by typing ``python`` and
the name of a Python script. We recommend creating a clean directory somewhere and starting
your work there. You will need some kind of editor to create the Python files, so use
whichever editor you prefer.

