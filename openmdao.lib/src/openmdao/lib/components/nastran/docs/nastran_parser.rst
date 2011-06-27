.. index:: NastranParser

.. _NastranParser:

*NastranParser*
~~~~~~~~~~~~~~~

NastranParser tries to parse the grid out of each page of output. It identifies 1) a header for the
page, then 2) the grid's headers, and finally 3) its values. If it parses a page correctly, the
query for information is much like querying a database, but much simpler. See the following example.

::

    >>> a = Float(0.0, iotype="out",
              nastran_header="displacement vector",
              nastran_subcase=1, # this must be an integer
              nastran_constraints={"column name" : "value"},
              nastran_columns=["column name"])

Once these values are specified, NastranParser will try to find the header in the output, then apply
the constraints to the grid, and yield a smaller grid with the viable rows and the acceptable columns
(specified by ``nastran_columns``). Note that ``a`` is a two-dimensional Python array. Each row will be a row
in a grid and will contain only the columns listed in ``nastran_columns``.

NastranParser accepts the name of the header as a string of all lower case letters with sane spacing as
well as the header presented in the output file (stripped of spaces at the beginning and end). 

.. note:: As of this writing, if it cannot find the header, it will break. If it cannot find the column names
   you specify, it will break. Right now, even though you specify a smaller grid of values than you want
   returned, the value of the variable will be only ``result[0][0]``. This will change in future versions.

One of the main reasons to support retrieving multiple columns is that you can access the parser
outside of design variable declaration. NastranComponent has an attribute ``parser``, which is the
NastranParser after it's run Nastran. After you call ``super(...).execute()``, you could retrieve values by
calling the parser's ``get`` function, in an identical fashion to the design variable declaration:

::

    >>> displacement_vector = self.parser.get("displacement vector",
                                              1,
                                              {"POINT ID." : "443"},
                                              ["T2"])

Do note that ``displacement_vector`` is a two-dimensional array. In this example, it has one value
(``[[value]]``), but if more columns or more rows were allowed, you would get a bit bigger two-dimensional
array. 


``self.parser.get`` has an optional argument that is useful in parsing grids that have more than one value
per column. A good example can be found in ``test/practice-grid.row-width.txt``. As you can see, if you
wanted to select the data for element id 1, you'd actually want those 15 rows of data. So, you invoke ``get``
with the optional argument ``row_width``. By using ``row_width``, once you find a row that satisfies your
constraints, it'll include the remaining (``row_width-1``) rows in the output.

It is important to understand how NastranParser works. It is a heuristic-based parser. This means that the
developers have built something that correctly identifies most grids that they have thrown at it. Since
there is no official Nastran output specification, it might not work on your grid. This is a known problem
without a known solution.

Another, perhaps more pressing, problem is that NastranParser uses the data in the grid to help the
parsing task. This means that if the data changes significantly, you *could* get different parses.
While this is not very likely, it is a possibility. Currently, if this happens, the hope is that the
``get`` function will break because you'll try to access a column that NastranParser doesn't
recognize. While this is a real problem, it is not showstopping because most of the time
NastranParser will parse the grid correctly regardless and because, under most runs, the data
doesn't undergo drastic changes. One example of a drastic change would be omitting an entire column
of values during one execution and then having values in the next iteration. Another example would
be going from a floating point number to ``0.0``. The problem is that the floating point numbers are
long and usually block unnecessary columns from forming. But if there is a column of ``0.0``, the
parsing problem might think there's an extra column. If you are worried about inconsistencies in
parsing, you could isolate the particular grid you are parsing and change.

*Source Documentation for nastran_parser.py*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
