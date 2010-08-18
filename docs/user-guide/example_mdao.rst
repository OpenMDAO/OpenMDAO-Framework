
.. index:: MDAO tutorial problem

.. _Tutorial-MDAO-Architectures:

Tutorial: MDAO Architectures
============================

This tutorial shows how to create a model to solve a simple problem consisting of
two coupled disciplines using several MDAO strategies including:

#. Multidisciplinary Design Feasible (MDF)
#. Independent Design Feasible (IDF)
#. Collaborative Optimization (CO)

The tutorial will introduce you to some new topics that include using the itereation
hieararchy to set up models with nested optimization loops, using a solver to "close
the loop" in a coupled multidisciplinary simulation, and using a broadcaster to set
the values of design variables in multiple places at one time.

All of these tutorials use the Sellar problem which consists of 2 disciplines as follows:

Multidisciplinary Design Feasible (MDF)
---------------------------------------
