.. index:: simple tutorial

Overview
==========

In this section, you are going to learn how to execute a simple optimization problem using the
OpenMDAO script interface. To get the most out of this tutorial, you should be familiar (though you
don't have to be proficient) with the Python language. If you don't have much experience with Python, we recommend
trying `Dive into Python <http://www.diveintopython.net/>`_. It is an excellent introduction to Python
that is licensed under the GNU Free Documentation License, so you can download and use it as you
wish.

The problem we present here is a paraboloid that is a function of two input variables. We will work with Our goal is
to find the minimum value of this function over a particular range of interest. First, we will solve
this problem with no constraints. Then we will add constraints and solve the problem again. Next we'll 
take a look at how you can specify analytic derivatives for your components and what effect that has on the 
optimization. Lastly, We will run a Design of Experiments (DOE) on the problem and plot 
the results to visually identify the minumum. 

If we express the problem as a block diagram, we can see how to set it up in OpenMDAO:

.. _`OpenMDAO-overview`:

.. figure:: Simple1.png
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

