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
* Sphinx Python Documentation Generator: http://sphinx.pocoo.org/contents.html 


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
  flush left. When numbers are indented, they do not display correctly in Internet Explorer (although
  they look fine in Firefox and Safari). If the numbered item is long and you wrap it, it could also
  cause display issues in IE.

* All text in headings (level one, level two, level three, etc.)
  must be underlined. (In OpenMDAO documents, generally the title is overlined
  and underlined, while the other headings are underlined only. All levels must
  be different. Please see the :ref:`Style-Guide` for more information.)
  
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
 the tutorial problem overview in the *User Guide*, you would type
 something like the following in the source file:

 ::
  
   Please see the tutorial problem, specifically the :ref:`Problem-Overview`.

 In the *User Guide* you would place the label before the section title, as follows:

 ::

   .. _Problem-Overview:
  
   Problem Overview
   ----------------

   The overall objective of the tutorial problem is to design . . . . 
 

 Note the hyphenation between words in the label and the cross reference to the label.

 You can use same type of cross-reference label with figures. See :ref:`Figures`.


**- Internal links -- to an arbitrary location**

  Labels that aren't placed before a section title can still be referenced, but you must give the link
  an explicit title using this syntax: ``:ref:`Link title <label-name>```.  For example, the cross
  reference ``:ref:`process model <process-model>``` appears in the HTML text file as:

    :ref:`process model <process-model>`
  
  The label below was placed above the paragraph in the *User Guide* that discusses the process model and shows
  a figure of it, 
   
     ``.. _`process-model`:``
   
  So clicking on the cross reference in the text file takes you to where the label was placed. In this case an  
  arbitrary label was created rather than cross referencing to the figure title.
 

**- Seealso directive**

  This directive is similar to the internal link to a section title that was previously
  described. However, when you use this directive, the text (cross reference)
  appears in a highlighted box that spans the width of the page. 


  For example, typing this:

  ::  

    .. seealso:: :ref:`Bazaar-Commands`

  results in:

  .. seealso:: :ref:`Helpful-Bazaar-Commands`


  You must also place a label before the section referred to, for example:

  ::  

    .. _Bazaar-Commands:


**- External link -- to a specific web address**

  Use ```Link text <http://target>`_`` for inline web links. 

  For example, typing:
	 ```Python  2.2.1 <http://www.python.org/download/releases/2.1.1/license/>`_``

  will result in the following hyperlink: 
	`Python  2.2.1 <http://www.python.org/download/releases/2.1.1/license/>`_ 

  If the link text should be the Web address, you don't need special markup; just
  type the address in the reST file, and the parser will find the
  link/mailing address in the text.

  For example, typing:
	``http://www.python.org/download/releases/2.1.1/license/`` 
	
  will result in this hyperlink:
	http://www.python.org/download/releases/2.1.1/license/


**- External link -- to more than one web address (anonymous hyperlink)**

  On occasion you may want to use identical text as the hyperlink to different web
  addresses. In such a case, you must create anonymous hyperlinks. No text label precedes
  the web address; however, a double underscore is required after the text link
  (i.e., ```reStructuredTest`__`` in the example that follows) and before the
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

**- Generated figures**

  In the OpenMDAO documentation, we have been using the open source Dia application to create
  diagrams (figures) and saving them as .png files. (A script automatically resizes the Dia
  files for our documentation.) Since these files may need to be updated, they go in the
  ``docs/generated_images`` directory on your branch.

  Here is an example of how to link to a figure:

  ::

    .. _`Class Diagram of Core Classes`:

    .. figure:: ../generated_images/ModelClasses.png     
       :align: center

       Class Diagram of Core Classes


  In the above example, ``.. _`Class Diagram of Core Classes`:`` is an optional label that is used for
  cross referencing to this figure. In this case there was some preceding text: ``The figure `Class Diagram of
  Core Classes`_ shows...``. A cross reference is not necessary, but if you are discussing a figure
  that appears later in the text, it is helpful to the reader. 

  The path to the image is: ``.. figure:: ../generated_images/ModelClasses.png``.
  Generally we align our figures *center*, as shown in the example, but that is up to the
  author.

  Last is the figure caption: ``Class Diagram of Core Classes``. You must leave a blank
  line before the caption. You would also leave a blank line after it, since it is the end of a
  paragraph. (In Firefox, figure captions are automatically centered, but in Internet Explorer
  they appear flush left.) 


**- Static figures** 

  Static figures are stored in ``docs/images/<document_directory>`` on your branch. Here is an
  example from the *User Guide* where the author pulled in a static figure titled *EPA City
  Driving Profile.* 


  :: 

    .. figure:: ../images/user-guide/EPA-city.gif
       :align: center

       EPA City Driving Profile


Add Lines/Maintain Line Break
------------------------------

If you want to add an extra line after a graphic or table, use the vertical bar ("|")
found above the backslash on the keyboard. Put it on a line by itself.

 
Also use the vertical bar when you want to maintain line breaks. You might want
to do this inside a specific block of text. If your text needs to be
indented, then first indent, type the vertical bar, leave a space, and then type
the desired text.


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
 look different (inconsistent). This is the way reST or Sphinx currently works, and the author cannot
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

 The table that is generated does not have a box outline around it. And to get the
 header rows to be boldface, you must use the two asteriks (**) markup before and
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

If you wish to add index items to a file as you are writing, please do. Additionally the tech
writer will review new documentation and add index (and glossary) entries as needed. Index
entries should precede the section or paragraph containing the text to be indexed.
*Note that all index entries are placed flush left.* Some examples follow.

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


