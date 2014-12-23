
Systems Dictionary
---------------------

The following is a short summary of the types of systems that OpenMDAO will
create from your model when the System Hierarchy is formed. All systems
inherit from the ``System`` base class.

SimpleSystem
+++++++++++++

In the System Hierarchy, the ``SimpleSystem`` is analogous to a ``Component``
in the iteration hierarhcy. Every component in your model will have a
corresponding ``SimpleSystem`` in the system tree, provided that it's in a
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

Both SerialSystem and ParallelSystem inherit from ``CompoundSystem``, which
is never used in the System Hierarchy.

ParamSystem
++++++++++++

If your model contains a driver that allows ``Parameters``, then the system
hierarchy will contain a ``ParamSystem`` for each parameter. This system does
little calculation and is mostly a bookkeeping entity.

.. _`ParamSystem`:

.. figure:: arch_paramsystem-1.png
   :align: center
   :alt: Parameters are contained in parameter systems.

This figure shows a model where a component named 'Comp1', with input
'Comp1.x' and output 'Compx1.y' is contained in the workflow of a driver that
has a parameter and an objective. The parameter gives us a ParamSystem called
'Comp1.x' which comes before 'Comp1' in the subsystem execution order. The
objective also gives us another SimpleSystem that executes last.

InVarSystem
++++++++++++

An ``InvarSystem`` is a system that is created for an input for which a
gradient has been requested when there is not a corresponding parameter, and
hence no ParamSystem. These will be generated whenever you have a change in
scope, such as the presence of a subasembly, for which a gradient needs to be
calculated in the inner scope and given to the outer scope. In those cases,
the InVarSystem marks the input variables in the subscope, much as the
ParamSystem marks them for a driver with parameters. InVarSystems are also
creeated when you call calc_gradient manually on a driver and give it inputs
that are not already parameters. This system also does little calculation.

OpaqueSystem
+++++++++++++

If a Driver requires derivatives from its workflow system, and if analytic
derivatives are not defined for all of its components or subdrivers, then the
System Hierarchy is generated differently. OpenMDAO will identify all of the
subsystems that can't provide derivatives and group them into an
``OpaqueSystem``. The algorithm attempts to group them into the smallest
number of OpaqueSystems it can based on their connectivity. An OpaqueSystem
contains its own vectors, so when it executes, it must copy its variables
between the outer and inner scope before and after it executes its
subsystems.

When an OpaqueSystem is linearized, it performs a finite difference between
its boundary inputs and boundary outputs, and then caches that Jacobian for
use in the linear equations solution.

.. _`OpaqueSystem`:

.. figure:: arch_opaquesystem-1.png
   :align: center
   :alt: Opaque systems allow finite difference on submodels.

For this figure, we have expanded our model to contain two components
connected in series. We also have one parameter and one objective. The System
Hierarchy that results contains two more levels. The SimpleSystems for Comp1
and Comp2 are grouped together in the SerialSystem "FD_('Comp1', 'Comp2')."
This SerialSystem is contained in an OpaqueSystem called ('Comp1', 'Comp2')
which can now provide derivatives by finite difference. There is also an
additional ``InVarSystem`` for 'Comp1.x' in the OpaqueSystem's SerialSystem
because of the scope change across the OpaqueSystem boundary.

FiniteDiffDriverSystem
+++++++++++++++++++++++

.. _`FiniteDiffDriverSystem`:

.. figure:: arch_finitedifferencesystem-1.png
   :align: center
   :alt: Some drivers must be finite differenced.

SolverSystem
+++++++++++++

EqConstraintSystem
+++++++++++++++++++

AssemblySystem
+++++++++++++++


