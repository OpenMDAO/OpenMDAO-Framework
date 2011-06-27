.. index:: NastranReplacer

.. _NastranReplacer:

*NastranReplacer (the Crude Way)* 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

NastranReplacer looks at the Nastran input file and replaces all instances of ``%varname`` with the current
value of the design variable. The length of ``varname`` is limited to seven characters since, along with the
percent sign, it must fit in an eight-character block. You can use the same placeholder in multiple places,
but it will give you a warning.

The main shortcoming, and the reason why it is the crude way, is that the input variable is placed in the
same block as the placeholder variable, which limits its precision. When using an optimizer with a very
small step size, it's possible that eight characters aren't enough to distinguish between iterations.

There is a secondary mode of operation. If you specify a variable that starts with an asterisk (e.g.,
``%*myvar``), NsatranReplacer will overwrite the variable and keep on overwriting for the length of the
value. This is useful when you want to insert a value that doesn't correspond to an eight-character wide
block. The best example is if you wanted to replace the number in the line ``METHOD 103``. If you tried
replacing it with a normal variable (if you insert ``XXXXXXXX``), you would get either ``METHOD 1XXXXXXXX``
or ``XXXXXXXX03``. Using overwrite variables you can insert ``104`` in the expression ``METHOD %*n``, and it
will yield ``METHOD 104``.

The asterisk variables are very useful when replacing variables that aren't in the bulk data section. When
you want to replace a bulk value (in a card), NastranMaker is much more appropriate since it understands
the bulk data format. Replacing bulk data with NastranReplacer is highly discouraged.

*Source Documentation for nastran_replacer.py*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
