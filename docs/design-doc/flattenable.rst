Flattenable vs Non-flattenable Variables
-------------------------------------------

In order to support distributed computing through MPI, OpenMDAO needs to
store all of the model variables in a distributed vector so that it can be
passed between processes and so that gradients can be calculated efficiently.
Not every ``Variable`` can be handled this way, so variables are classified
into two categories: flattenable and non-flattenable.

A flattenable variable is a differentiable variable that can be transformed
to and from a 1-dimensional floating point array. All Float and all Arrays
with a float ``dtype`` fall into this category. Integers and integer arrays
are not flattenable. While they can be converted into a 1d float array by
casting them as floats, they cannot be converted back without rounding or
truncation. In addition, integers are not inherently differentiable as
component outputs and intputs.

A non-flattenable variable is everything else. This includes integers and
integer arrays, strings, dictionaries, lists, and any custom object.

Flattenable variables are all passed via the distributed vector while
unflattenable variables are passed using the old OpenMDAO mechanism for data
passing.

A custom data object can be converted into a flattenable variable by defining
three methods on the object: ``get_flattened_size``, ``get_flattened_value``,
and ``set_flattened_value``. Similarly, a flattenable variable can be
excluded from the distributed vector by giving it the metadata ``noflat=True``.
