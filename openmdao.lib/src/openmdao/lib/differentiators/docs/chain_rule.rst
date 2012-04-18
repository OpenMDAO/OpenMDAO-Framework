
.. index:: Differentiators, ChainRule

A `differentiator` is a special object that can be used by a driver to calculate
the first or second derivatives of a workflow. The derivatives are calculated
from the parameter inputs to the objective and constraint outputs. Any driver
that has been derived from the DriverUsesDerivatives base class contains a Slot
called `Differentiator`. This slot can take a Differentiator object.

.. _ChainRule:

*ChainRule*
~~~~~~~~~~~~~~~~~~

The ``ChainRule`` differentiator calculates the gradient of a Drivers's
workflow using forward automatic differentiation (i.e., successive
application of the chainrule from the Parameters to the Objectives and
Constraints.)

This differentiator is under construction. At present, it works for any
workflow that contains Assemblies or Components for which derivatives have
been specified for all connected components. Work is underway to include
finite differencing of subsections of the model, and to support nested
drivers. If your model requires any of these features, then you should use
the ``FiniteDifference`` differentiator.


*Source Documentation for chain_rule.py*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~