
.. index:: DOEgenerators

.. _DOEgenerators:

DOEgenerators
=============

.. index:: Full Factorial

.. FullFactorial:

The DOEdriver, which is a driver that executes a Design of Experiments, includes a
mandatory socket that takes a DOEgenerator. This DOEgenerator contains the algorithm
that is used to select the design points that will be exectued by the DOEdriver. OpenMDAO
includes several kinds of DOEdrivers which are listed below.

*FullFactorial*
~~~~~~~~~~~~~~~

The FullFactorial DOEgenerator implements a full factorial Design of Experiments. In
other words, it generates a set of design points that fully span the range of the
parameters at the requested resolution.

(See the source documentation for more information on 
:ref:`FullFactorial<openmdao.lib.doegenerators.full_factorial.py>`.)


.. index:: Optimal Latin Hypercube, OptLatinHypercube

.. OptLatinHypercube:

*OptLatinHypercube*
~~~~~~~~~~~~~~~~~~~

The OptLatinHypercube component implements an algorithm that produces an optimal
latin hypercube based on an evolutionary optimization of its Morris-Mitchell sampling
criterion.

(See the source documentation for more information on 
:ref:`OptLatinHypercube<openmdao.lib.doegenerators.optlh.py>`.)

