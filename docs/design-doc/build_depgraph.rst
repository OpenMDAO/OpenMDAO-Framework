The Dependency Graph
--------------------

The configuration of an OpenMDAO model is represented by a directed graph
we call a dependency graph.  Each Assembly has its own dependency graph.
Each component in the Assembly has a node in the graph, and all of the input
and output variables of that component also have their own nodes in the graph.

The dependency graph was originally designed to support interactive modification
of the model configuration, so changes in the model were immediately reflected
in the structure of the graph.  These include things like adding and removing
components or variables, connecting and disconnecting variables, replacing
components, etc.

When we started to redesign OpenMDAO to support distributed computing using MPI,
we decided that maintaining that interactivity was no longer viable.  We decided
to move the creation of the dependency graph so that it happens during setup
of the MPI data structures, which happens just before execution.

This hasn't been implemented yet, but it should greatly simplify the code
relating to the graph because we no longer have to keep the graph 'in sync' with
the configuration of the model.  Now we will simply build the graph to reflect
the model at that time.

