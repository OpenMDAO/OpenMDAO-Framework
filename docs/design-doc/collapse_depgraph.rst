
Collapse depgraph
------------------

At the beginning of setup, just prior to execution, we start with a 
dependency graph.  This graph includes nodes for components as well as
their variables, and any variables belonging to the graph's parent Assembly
as well.

- show depgraph diagram here

We then perform a number of transformations on the graph.  First, if we have
any variables that are VarTrees, we 'explode' them, replacing the single
variable node with a collection of variable nodes, one for each leaf node
of the VarTree.

Then, if we're given specific inputs and outputs of interest, we prune the 
graph down to only those nodes that are relevant based on those inputs
and outputs.

- show relevant depgraph here

The next step is to take every connected source variable and collapse it
and all of its destination variables into a single node. This new 'collapsed'
node is given a name that combines the source variable name and each
destination variable name into a tuple of the form:

    (srcname, (destname1, destname2, ...))

We collapse all of the variable connections because whenever two or more
variables are connected, they all represent a single value.  In later parts of
setup, we combine the values of these variables into vectors that can 
be very large, so collapsing the connections gets rid of unnecessary duplicated
data in those vectors.

- show collapsed graph here

