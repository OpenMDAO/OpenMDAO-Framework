.. index:: Sphinx, reStructuredText

.. _Sphinx-and-reStructuredText:

Sphinx and reStructuredText
===========================

OpenMDAO uses :term:`reStructuredText` (reST), a plaintext markup language that is part of the Docutils
text-processing system, to create user documents and to document source code. Sphinx is a Python documentation generator
that converts reST to HTML. While reST provides the basic structure (Docutils is the parsing and translating
suite), Sphinx has specific directives that are used with reST to produce the desired results. For more
information, please see the references that follow.


References:

* A reStructuredText Primer: http://docutils.sourceforge.net/docs/user/rst/quickstart.html 
* Sphinx Python Documentation Generator: http://sphinx-doc.org/contents.html 


Some Basics 
-----------

* The reST markup language assumes that a paragraph is the space between two blank
  lines. One blank line between paragraphs is treated the same as four blank
  lines between paragraphs.

* You need at least one space between words and after periods. Extra spaces are
  ignored. 
  
* Use one asterisk for emphasis (*italics*): ``*italics*`` 

* Use two asterisks for strong emphasis (**boldface**) ``**boldface**`` 

* When numbering items (such as steps in a task or process), DO NOT indent the numbers -- put them
  flush left. When this section was written, indented numbers did not display correctly in Internet
  Explorer (IE). Additionally, IE had issues displaying numbered items that were long and wrapped to
  the next line.

* All text in headings (level one, level two, level three, etc.) must be underlined. In OpenMDAO
  documents, many of the document titles are overlined and underlined, while the other headings are
  underlined only. It is not necessary to use overline, and it's easier not to. Just be
  consistent within your document; all levels must be different. Please see the :ref:`Style-Guide`
  for more information.

* If you get a Sphinx build error that says "Unexpected indentation," it is probably because
  Sphinx is expecting a blank line, such as after a literal text block. Your line may have
  wrapped and confused Sphinx. In this case, try pulling the text up to the previous line even
  if it extends out past the margin of your window. Or, you could press **Enter** to go to the next
  line, but be sure to indent the text on the new line.  
  

.. index:: literal text

Literal Text
------------

**- Inline literal text:**

 Use this markup for code samples or or any time you want literal text. 

 Typing this:

 ::

   ``Inline literal text``  
   
 will result in this:

 | ``Inline literal text`` 

 .. seealso:: :ref:`Using-Inline-Literal-Text`


**- Literal code block:**

  To introduce a literal code block, use a double colon and skip a line before the
  text. Also, you must indent at least two spaces. For example, typing this:


  ::

    ::
  
      self.connect('velocity', 'chassis.velocity')
      self.connect('velocity', 'transmission.velocity')
      self.connect('tire_circumference', 'chassis.tire_circ')
      self.connect('tire_circumference', 'transmission.tire_circ')

  will produce this:

  ::

    self.connect('velocity', 'chassis.velocity')
    self.connect('velocity', 'transmission.velocity')
    self.connect('tire_circumference', 'chassis.tire_circ')
    self.connect('tire_circumference', 'transmission.tire_circ')

  Syntax highlighting is done automatically for Python code if Pygments (a Python
  syntax highlighter) is installed.

  .. note::
     You can also use this special marker (``::``) to introduce non-code literal
     text for use in examples. 

.. index:: hyperlinks; creating

Hyperlinks
----------

**- Internal link -- to a section title**

 You can place a label before a section title using ``:ref:`label-name```. The
 section you are cross-referencing can be in the same file, a different
 file, or even a different document within the MDAO user documentation. However, 
 label names must be unique.

 For example, if you are in the *Developer's Guide* and want to refer the user to
 the problem overview in the more complex tutorial, you would type
 something like the following in the source file:

 ::
  
   Please see the more complex tutorial problem, specifically the :ref:`Problem-Overview`.

 In the more complex tutorial you would place the label before the section title, as follows:

 ::

   .. _`Problem-Overview`:
  
   Problem Overview
   ----------------

      This tutorial covers some of the more advanced capabilities of OpenMDAO. You should . . . 
 

 Note the hyphenation between words in the label and the cross reference to the label.

 You can use same type of cross-reference label with figures. See :ref:`Figures`.


**- Internal links -- to an arbitrary location**

  Labels that aren't placed before a section title can still be referenced, but you must give the link
  an explicit title using this syntax: ``:ref:`Link title <label-name>```.  For example, the cross
  reference ``:ref:`Workflow Overview <workflow overview>``` appears in the HTML text file as:

    :ref:`Workflow Overview <workflow overview>`
  
  The label below was placed above the paragraph in the more complex tutorial that discusses the
  workflow and shows a figure of it, 

     ``.. _workflow overview`:``
   
  So clicking on the cross reference in the text file takes you to where the label was placed. In this case an  
  arbitrary label was created rather than cross referencing to the figure title.
 

**- Seealso directive**

  This directive is similar to the internal link to a section title that was previously
  described. However, when you use this directive, the text (cross reference)
  appears in a highlighted box that spans the width of the page. 


  For example, typing this:

  ::  

    .. seealso:: :ref:`Git-Commands`

  results in:

  .. seealso:: :ref:`Helpful-Git-Commands`


  You must also place a label before the section referred to, for example:

  ::  

    .. _Git-Commands:


**- External link -- to a specific web address**

  Use ```Link text <http://target>`_`` for inline web links. 

  For example, typing:
	 ```Python  2.7 <https://www.python.org/download/releases/2.7/>`_``

  will result in the following hyperlink: 
	`Python  2.7 <https://www.python.org/download/releases/2.7/>`_ 

  If the link text should be the Web address, you don't need special markup; just
  type the address in the reST file, and the parser will find the
  link/mailing address in the text.

  For example, typing:
	``http:s//www.python.org/download/releases/2.7/`` 
	
  will result in this hyperlink:
	https://www.python.org/download/releases/2.7/


**- External link -- to more than one web address (anonymous hyperlink)**

  On occasion you may want to use identical text as the hyperlink to different web
  addresses. In such a case, you must create anonymous hyperlinks. No text label precedes
  the web address; however, a double underscore is required after the text link
  (i.e., ```reStructuredText`__`` in the example that follows) and before the
  web address (e.g., ``.. __: http://docutils.sourceforge.net/rst.html``). 

  This is an example where *reStructuredText* acts as the hyperlink to two different web
  addresses. 

  You type this:
    
  :: 

    `reStructuredText`__

    .. __: http://docutils.sourceforge.net/docs/user/rst/quickstart.html


  to go to the reStructuredText Primer:
  
    `reStructuredText`__

    .. __: http://docutils.sourceforge.net/docs/user/rst/quickstart.html

  And you type the same link text but a slightly different web address:

  ::

    `reStructuredText`__

    .. __: http://docutils.sourceforge.net/docs/user/rst/cheatsheet.txt
    

  to take you to the reStructuredText Cheat Sheet:

    `reStructuredText`__

    .. __: http://docutils.sourceforge.net/docs/user/rst/cheatsheet.txt


.. _Figures:

Figures
-------

An `image` is a simple picture, and reST has an image directive. However, since a `figure`
is an image (picture, diagram, graphic) that optionally has a caption and/or legend, it's
just easier to use the figure directive and either include or omit a caption as desired.
All figures go in the directory with the ``.rst`` file that references it; therefore, when
you reference a figure, there is no path, just the figure's name. A figure may be a
``.gif, .png, .jpg``, or other file type; it's up to the author.

Most figures in the OpenMDAO docs are center aligned, but it is up to the author to
determine alignment. Some browsers automatically center a figure's caption or place it
flush left.

If you have a figure caption, you must leave a blank line before the caption. Also leave a
blank line after the caption since it ends a paragraph. 

To link to, or cross reference, a figure that appears later in the text, use the
figure's caption as the label. Some text would precede the figure and refer to it, such
as the following (in reST format): ``The figure `Class Diagram of Core Classes`_
describes the . . . .``  

::

  .. _`Class Diagram of Core Classes`:

  .. figure:: ModelClasses.png     
     :align: center

     Class Diagram of Core Classes

A cross reference is not necessary, but it may be helpful to the reader. 

Add Lines/Maintain Line Break
------------------------------

To add an extra line after a figure or table, use the vertical bar ("|") found above
the backslash on the keyboard. Put it on a line by itself.

Also use the vertical bar when you want to maintain line breaks, such as inside a
specific block of text. If your text needs to be indented, then first indent, type the
vertical bar, leave a space, and then type the desired text.


Lists/Bullets
-------------

To create a list: 

* Place an asterisk (or hyphen or plus sign) at the start of a paragraph (list item). 

* Indent any line after the first line in a list item so it aligns with the
  first line. The same goes for numbered lists. 
  
* Leave a blank line after the last list item.

You may insert a blank line between list items, but it is not necessary and does not change
how they appear.

**- Bullet list:**

 Typing this:

 ::
  
   * Determine acceleration required to reach next velocity point
   * Determine correct gear
   * Solve for throttle position that matches the required acceleration
  
 will result in this:

 * Determine acceleration required to reach next velocity point
 * Determine correct gear
 * Solve for throttle position that matches the required acceleration


**- Numbered list:**

 You can type the numbers, like this:

 ::

   1. Torque seen by the transmission
   2. Fuel burn under current load

 or use a # sign to auto number the items, like this:
  
 ::
  
   #. Torque seen by the transmission
   #. Fuel burn under current load  

 In either case, you get this:

 1. Torque seen by the transmission
 2. Fuel burn under current load


**- Nested list:**

 To create a nested list, you must place a blank line between the parent list and
 the nested list and indent the nested list.

 ::

   * Item 1 in the parent list
   * Subitems in the parent list

     * Beginning of a nested list
     * Subitems in nested list

   * Parent list continues 
  

 In this case, it results in this:

 * Item 1 in the parent list
 * Subitems in the parent list

    * Beginning of a nested list
    * Subitems in nested list

 * Parent list continues 

 You may notice that even though we didn't put a blank line between items in the parent list,
 a blank line appears between them because of our nested list. Whenever there is nested bullet list or
 a bullet is longer than one paragraph, a blank line appears between bullet items. Otherwise, there is no blank
 line between bullet items. Consequently, different sets of bullets within the same document will
 look different (inconsistent). This is the way reST or Sphinx currently works, and you cannot
 change it.  


Tables
------

**- Simple table:**

 The following table is an example of simple table. When you create a simple
 table, you must:

 * Leave at least 2 spaces between columns
 * Make sure the space between columns is free of text
 * Make sure the overline and underlines are all of identical length


 A table that looks like this:

 ::

   ==================  ===========================================  =======
   **Variable**	       **Description**			            **Units**
   ------------------  -------------------------------------------  -------
   power	       Power produced by engine		            kW
   ------------------  -------------------------------------------  -------
   torque	       Torque produced by engine		    N*m
   ------------------  -------------------------------------------  -------
   fuel_burn	       Fuel burn rate				    li/sec
   ------------------  -------------------------------------------  -------
   engine_weight       Engine weight estimate			    kg
   ==================  ===========================================  =======

 results in this:

 ==================  ===========================================  =======
 **Variable**	     **Description**			          **Units**
 ------------------  -------------------------------------------  -------
 power		     Power produced by engine			  kW
 ------------------  -------------------------------------------  -------
 torque	             Torque produced by engine			  N*m
 ------------------  -------------------------------------------  -------
 fuel_burn	     Fuel burn rate				  li/sec
 ------------------  -------------------------------------------  -------
 engine_weight	     Engine weight estimate			  kg
 ==================  ===========================================  =======

 The table that is generated does not have a box outline around it. To get the
 header rows to be boldface, you must use the two asterisks (**) markup before and
 after the text you want in bold. 

 However, with Sphinx 1.0 beta 2 (released May 30, 2010), the header row
 automatically appears in boldface and the row background appears in color if you
 use a double broken line under the header row (``====``) instead of a single broken
 line (``----``). Additionally, in all tables a space appears after the vertical
 lines separating cells. 

 So using the double broken line under the header, like this:

 ::

   ==================  ===========================================  =======
   Variable	       Description			            Units
   ==================  ===========================================  =======
   power	       Power produced by engine		            kW
   ------------------  -------------------------------------------  -------
   torque	       Torque produced by engine		    N*m
   ------------------  -------------------------------------------  -------
   fuel_burn	       Fuel burn rate				    li/sec
   ------------------  -------------------------------------------  -------
   engine_weight       Engine weight estimate			    kg
   ==================  ===========================================  =======


 results in this:

 ==================  ===========================================  =======
 Variable	     Description			          Units
 ==================  ===========================================  =======
 power		     Power produced by engine			  kW
 ------------------  -------------------------------------------  -------
 torque	             Torque produced by engine			  N*m
 ------------------  -------------------------------------------  -------
 fuel_burn	     Fuel burn rate				  li/sec
 ------------------  -------------------------------------------  -------
 engine_weight	     Engine weight estimate			  kg
 ==================  ===========================================  =======


**- Grid table:**

 Grid tables are more cumbersome to produce because they require lines between
 columns and rows, and at the intersections of columns and rows. Use a simple table
 unless you have cell content or row and column spans that cannot be displayed using a
 simple table. 

 The grid table uses these characters:

 * Equals sign ("=") to separate an optional header row from the table body
 * Vertical bar ("|") to create column separators 
 * Hyphen ("-") to create row separators
 * Plus sign ("+") for the intersections of rows and columns

 Typing this:

 ::


   +------------------------+------------+-----------+----------+
   | Header row, column 1   | Header 2   | Header 3  | Header 4 |
   | (header rows optional) |            |           |          |
   +========================+============+===========+==========+
   | body row 1, column 1   | column 2   | column 3  | column 4 |
   +------------------------+------------+-----------+----------+
   | body row 2             |Cells may span columns, if desired.|
   +------------------------+------------+----------------------+
   | body row 3             | Cells could| - Table cells        |
   +------------------------+ also span  | - contain            |
   | body row 4             | rows, as   | - body elements.     |
   |                        | shown in   |                      |
   |                        | this       |                      |
   |                        | example.   |                      |
   +------------------------+------------+----------------------+


 will produce this:

 +------------------------+------------+-----------+----------+
 | Header row, column 1   | Header 2   | Header 3  | Header 4 |
 | (header rows optional) |            |           |          |
 +========================+============+===========+==========+
 | body row 1, column 1   | column 2   | column 3  | column 4 |
 +------------------------+------------+-----------+----------+
 | body row 2             |Cells may span columns, if desired.|
 +------------------------+------------+----------------------+
 | body row 3             | Cells could| - Table cells        |
 +------------------------+ also span  | - contain            |
 | body row 4             | rows, as   | - body elements.     |
 |                        | shown in   |                      |
 |                        | this       |                      |
 |                        | example.   |                      |
 +------------------------+------------+----------------------+


Index Items
------------

Sphinx automatically creates index entries from all object descriptions (e.g., functions, classes,
or attributes). However, you may wish to add index items in a file as you are writing. 

Index entries should precede the section or paragraph containing the text to be indexed. *Note that
all index entries are placed flush left.* Some examples follow.

**- Single term** 
 
 Typing a single term, like this:
 
 ::
  
   .. index:: branch        

 will cause it to appear in the index as:
   
 ::
 
   branch

**- Pair**  
 
 If you type a pair of words, for example:
 
 ::
 
   .. index:: pair: Python; module

 they will appear in the index in two places. In the example, under the P's as:

 ::

   Python
      module

 and under the M's as:

 ::
   
   module
      Python


**- Modified single**
   
 You may also modify an entry, for example:
 
 ::
 
   .. index:: plugins; registering
    
        
 and it will appear as:
 
 ::

   plugins, 
      registering 


**- Shortcut for single entries**

 You can put several single-word entries on the same line, separated by commas, like this:
 
 ::

   .. index:: Component, Assembly, Driver, plugins

  
Testing Code
------------

For details on testing code in the documentation, please refer
to :ref:`Testing-Code-in-the-Documentation` in the *Developer's Guide.*


Code from the Source
---------------------

See :ref:`Including-Code-Straight-from-the-Source` in the *Developer's Guide.*


.. note::  Whenever you include a code snippet, a list, a block of text, or similar syntax, be
	   sure to leave a blank line after the text. You might even need to extend the last
	   line of text so it doesn't wrap. This should avoid a Sphinx "Unexpected Indentation"
	   error. 


