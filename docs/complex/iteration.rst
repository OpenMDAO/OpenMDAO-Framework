.. index:: iteration-hierarchy

Summary: Using the Iteration Hierarchy
=======================================

This tutorial introduced the iteration hierarchy, a powerful concept which allows
you to form arbitrarily complex process models. Some of the key points are
summarized here:

 ::

   * The top driver in an assembly is always called "driver"
   * An assembly can contain multiple drivers
   * Every driver has a workflow
   * Workflows can contain multiple components (assemblies, drivers)
   * A component instance can appear in multiple workflows
   * Execution order in a workflow is infered from the data connections
   * If there are no data connections, execution order in a workflow is the order order the components were added to the workflow

You have now completed the more complex tutorial problem.
