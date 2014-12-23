
Systems Dictionary
---------------------

The following is a short summary of the types of systems that OpenMDAO will
create from your model when the System Hierarchy is formed.

SimpleSystem
+++++++++++++

In the System Hierarchy, the ``SimpleSystem`` is analogous to a ``Component``
in the iteration hierarhcy. Every component in your model will have a
corresponding ``SimpleSystem`` in the system tree, provide that it's in a workflow.

.. _`SimpleSystem`:

.. figure:: arch_simplesystem-1.png
   :align: center
   :alt: For a model containing a single component, OpenMDAO creates a SimpleSystem.

   For a model containing a single component, OpenMDAO creates a SimpleSystem.

SerialSystem
+++++++++++++

ParamSystem
++++++++++++

ParallelSystem
+++++++++++++++

OpaqueSystem
+++++++++++++

SolverSystem
+++++++++++++

FiniteDiffDriverSystem
+++++++++++++++++++++++

InVarSystem
++++++++++++