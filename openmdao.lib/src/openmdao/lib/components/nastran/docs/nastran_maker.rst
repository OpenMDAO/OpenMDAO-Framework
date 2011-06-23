.. index:: NastranMaker

.. _NastranMaker:

*NastranMaker*
~~~~~~~~~~~~~~

NastranMaker does not rely on placeholder variables; instead, you must provide the `keyword`, the
`id`, and the `fieldnum` to change a card. NastranMaker will find the right card to modify and will
convert the entire card to long form. This way, you get 16 characters to express numbers. It also
allows you to keep the Nastran input unmodified, instead of littering it with placeholder variables.
Below is an example:

::

    >>> t1 = Float(10., desc="Thickness of pshell #1",
               iotype="in",
               nastran_card="PSHELL",
               nastran_id="1",
               nastran_fieldnum=3)

Note that the ``Nastran_card`` (the keyword) and the id must be strings, while the fieldnum must be an
integer. To make sense of which fields to change, an understanding of Nastran is required. Each field
specifies a different attribute that can be modified. To find out which fields modify which attributes,
consult the Nastran documentation. (See the `MSC.Nastran 2004 Quick Reference Guide
<http://www.google.com/search?source=ig&hl=en&rlz=1G1GGLQ_ENUS331&q=MSC.Nastran+2004+Quick+Reference+Guide&aq=f&aqi=&aql=&oq=&gs_rfai=CECsdPelqTJjaLozmNJ_-vcwGAAAAqgQFT9AJml8>`_.)


In general, a sample input line will look something like this:

::

    PSHELL         8       4       3

Here, ``PSHELL`` is the keyword, or the type of thing that you're modifying. The first number is usually the
id, so in this case, it is 8. In this example, there are two attributes, with values 4 and 3, that control
something about this ``PSHELL``. As an example, for a ``PSHELL``, the second argument (4) dictates which
material card you're referencing, and the third argument (3) specifies the thickness.

.. index:: nastran_make_hook

.. note:: To use NastranMaker without actually defining the traits in your subclass, you can
	  implement the function ``nastran_maker_hook`` in your subclass. This function will be
	  called with one argument, the ``NastranMaker`` object. It is called after it has processed
	  all the input variables that are visible on traits. The function's return is ignored.
	  Right after it finishes, ``NastranMaker`` writes out the Nastran file that will be run.   

*Source Documentation for nastran_maker.py*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
