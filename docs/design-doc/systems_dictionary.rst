
Systems Dictionary
---------------------

The following is a short summary of the types of systems that OpenMDAO will
create from your model when the System Hierarchy is formed.

SimpleSystem
+++++++++++++

In the System Hierarchy, the ``SimpleSystem`` is analogous to a ``Component``
in the iteration hierarhcy. Every component in your model will have a
corresponding ``SimpleSystem`` in the system tree, provide that it's in a
workflow. A SimpleSystem can execute the component and can provide the matrix
vector product of the Jacobian for the derivative solver.

SerialSystem
+++++++++++++

A ``SerialSystem`` is a container system for collections of SimpleSystems. As
the name implies, the component subsystems are executed sequentially in the
order directed by the connectivity graph. Every driver's workflow has a
corresponding SerialSystem in the system hierarchy. These SerialSystems can
execute the workflow and can calculate a gradient between any inputs and
outputs of the component systems contained therein.

.. _`SimpleSystem`:

.. figure:: arch_simplesystem-1.png
   :align: center
   :alt: For a model containing a single component, OpenMDAO creates a SimpleSystem.

The figure above shows the System Hierarchy for a model that has a single
``Component`` in the Driver's workflow. The topmost system is the
``SerialSystem`` for the base driver's workflow. This system contains a
``SimpleSystem`` as a subsystem. The SimpleSystem represents the component 'Comp1'.

ParallelSystem
+++++++++++++++

ParamSystem
++++++++++++

.. _`ParamSystem`:

.. figure:: arch_paramsystem-1.png
   :align: center
   :alt: Parameters are contained in parameter systems.

OpaqueSystem
+++++++++++++

SolverSystem
+++++++++++++

FiniteDiffDriverSystem
+++++++++++++++++++++++

InVarSystem
++++++++++++