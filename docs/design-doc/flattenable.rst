Flattenable vs Non-flattenable Variables
-------------------------------------------

In order to support distributed computing through MPI, OpenMDAO needs to
store all of the model variables in a distributed vector so that it can be
passed between processes and so that gradients can be calculated efficiently.
Not every ``Variable`` can be handled this way, so variables are classified
into two categories: flattenable and non-flattenable.

A flattenable variable is a variable that can be transformed into a
1-dimensional floating point array. All Float and all Arrays with a float
``dtype`` fall into this category.
