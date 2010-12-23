.. index:: Style Guide

.. _Style-Guide:


Style Guide 
===========

This document provides some general guidelines for writing documentation, both code and user
documents. It is by no means inclusive.

For Python coding conventions, please refer to the `Python Style Guide
<http://www.python.org/dev/peps/pep-0008/>`_.

.. index:: docstring standard

Standard for Coding Docstrings
------------------------------

OpenMDAO uses documentation strings, or `docstrings,` written in `reStructuredText
<http://docutils.sourceforge.net/rst.html>`_ (reST) to document code. Please follow the docstring
standard described below. 

We would like to acknowledge the NumPy/SciPy standard, which we referred to
frequently when developing our own standard.


*Docstring*
+++++++++++

A docstring describes a module, function, class, or method definition. A docstring is a special
attribute of an object (``object.__doc__``) and, for consistency, is surrounded by triple
double-quotes, for example::

   """A simple iteration driver. Basically runs a workflow, passing the output 
   to the input for the next iteration. Relative change and number of 
   iterations are used as termination criteria. 
   """

A docstring may be one line or spread over several lines. Line length should not exceed 80 characters.


*Sections of a Docstring*
++++++++++++++++++++++++++

The following sections can be included in a docstring, but in many cases, are optional. 


1. **Summary (required)**

   You need at least a one-line summary that does not use variable names or the function name, for
   example::

     class ExternalCode(Component):
          """ Run an external code as a component. """

   You may want a longer summary (a few sentences) to clarify functionality. Do not discuss
   implementation details or background theory in the summary.


2. **Parameters (required)**

   These are descriptions of the function arguments, keywords, and their respective types. Parameters
   should follow the docstring summary. For the parameter types, be as precise as possible. A few
   examples of parameters, their types, and descriptions follow::

     *Parameters*
       
     value: ndarray
         Array of values to insert.
        
     row_start: integer
         Starting row for inserting the array. This is relative
         to the anchor, and can be negative.
        
     row_end: integer
         Final row for the array, relative to the anchor.
        
     field_start: integer
         Starting field in the given row_start as denoted by 
         delimiter(s). 
        
     field_end: integer
         The final field the array uses in row_end. 
         We need this to figure out if the template is too small or large.
        
     sep: str (optional) (Currently unsupported)
         Separator to append between values if we go beyond the template.
     
   
   - Do not put a space before the colon. 

   - Indent the description four spaces. 
   
   - For consistency, please capitalize the first word of the description and put a period at the end,
     even though it is not a complete sentence. In many cases, the first part of the description,
     although a sentence fragment, will be followed by a sentence.  

  In the above example, we italicized *Parameters* (indicated by the single asterisk on either side of
  the word) and used it as a heading for the parameters section. We did this merely to show how it can
  be done. Parameters and returns should be clear to users, so section headings aren't required. On
  the other hand, you might want them for the optional sections.    

  If a keyword argument is not necessary, put ``optional`` (lower case) after it in parentheses::

     x: int (optional)

  Optional keyword parameters have default values that are displayed in brackets as part of the
  function signature::

     include_errors: bool (optional) [False] 
  
  Default values can also be explained in the description::

    scaler: float (optional) 
        Multiplicative scale factor applied to both sides of the constraint's boolean expression. It
	should be a positive nonzero value. Default is unity (1.0).

  When a parameter can assume only one of a fixed set of values, those values can be listed in
  braces::

     order: {'C', 'F', 'A'}
         Description of order.         

  When two or more input parameters have exactly the same type, shape, and description, they can
  be combined::

    x1, x2: array_like
        Input arrays, description of x1, x2.  
	 
   
3. **Returns (required, if any)**

   Any returns should follow the parameters. Use the same format as for parameters.
   

4. **Raises (optional)**

   This section lists errors that get raised and under what conditions::
      
     *Raises*
     
     LinAlgException
          If the matrix is not numerically invertible.
   
   You may want to include this section for errors that aren't obvious or that have a good chance of
   getting raised.


5. **Notes (optional)**

   This section is for additional information about the code. Include any information that will be
   helpful to users.

6. **References (optional)**

   References should augment the docstring and not be required to understand it. If you have a *Notes*
   section and happened to cite references in it using the reST text ``[1]_, [2]_``, you can
   include the actual references in this section. For example, to cite the article below, include it
   as follows::

       .. [1] Keane, A. J., "Statistical Improvement Criteria for Use in Multiobjective Design
	  Optimization," AIAA JOURNAL, Vol. 44, 2006, pp. 879-891.

   It will be rendered as:

   .. [1] Keane, A. J., "Statistical Improvement Criteria for Use in Multiobjective Design
	  Optimization," AIAA JOURNAL, Vol. 44, 2006, pp. 879-891.

   If possible, avoid referencing sources of a temporary nature, such as web pages. Follow the
   format above and number all references, starting from one, in the order you cite them.


7. **Examples (optional)**

   This section should illustrate usage. Use the Python doctest format.

   When providing multiple examples, separate them by blank lines. Leave blank lines
   above and below the comments explaining the examples::

     >>> np.add(1, 2)
     3

     Comment explaining the second example

     >>> np.add([1, 2], [3, 4])
     array([4, 6])

   You do not need to use the doctest markup ``<BLANKLINE>`` to indicate empty lines in the
   output.
   
   
*Other Information*
+++++++++++++++++++
   
- When referring to functions in the same sub-module, no prefix is needed; the tree is searched
  upwards for a match.

- Add prefixes to functions from other sub-modules appropriately. For example, when documenting
  the ``scipy.random`` module, refer to a function in ``scipy.fft`` by::

    fft.fft2: 2-D fast discrete Fourier transform

- When referring to an entirely different module::

    scipy.random.norm: Random variates, PDFs, etc.

- If there are points in the docstring that deserve special emphasis, you can use the reST directives
  for a note or warning. Syntax is:

  ::

    .. warning:: Warning text.

    .. note:: Note text.

  It is seldom necessary to use either directive, but one situation in which a warning might be useful
  is for marking a known bug that has not yet been fixed.

  .. note:: A note directive is different from the *Notes* section of a docstring. A note will
     appear in a gray box.
     
- Line spacing and indentation are important. New paragraphs are marked with a blank line. Indentation in
  paragraphs indicates that the output is indented. Sphinx will complain if a paragraph appears to be
  indented for no reason.


General Documentation Issues
----------------------------

.. index:: underlines in reST

*Underlines (and Overlines)*
++++++++++++++++++++++++++++

This section pertains only to the user documents, not source code. You should never use underlines
in source code as Sphinx will complain. 

**- Document title:**


In general, use underline and overline for the title of document (in the
index file for the document). In OpenMDAO, we typically use a double line for
titles as shown in the following example:

::

  ============
  User Guide
  ============

The overline and underline must be the same length, or you will get an error and the
build will fail. If you use just underline and it is shorter than the text, you will get
a warning, but the documents will build. If you use just underline, and it is longer than
the text, Sphinx doesn't seem to mind.


**- File title:**

Use just underline for the title of a file. (This would be the Heading 1
equivalent). For example:

:: 

  Overview
  ========

The double underline is preferred, but you may see documentation written early in
the project that varies from this guideline.


**- Heading Levels**

OpenMDAO documents do not have outline numbers so that they look different from
print documents. While there are no outline numbers, there are heading levels.

In general it is preferred that the heading levels go no more than four levels deep. If
there are more than four levels, readers may forget where they are in the hierarchy. Of
course, there are always exceptions, and the subject matter should be your guide.

When creating your document, be sure each heading level has a different underline
style, and be consistent. The following example shows how heading levels may
be set up:

::  

  Heading 1     (Title of the file)
  =========   
  
  
  Heading 2
  ---------
  
  
  *Heading 3*
  +++++++++++
  
  
  Heading 4
  ~~~~~~~~~
   
 
Notice that the italics designation (a set of asterisks) is used for Heading 3. Heading levels 2 and 3 are similar in size, so asterisks are used to
better designate the hierarchy.

About Heading Levels:

* If you are adding text to an existing file, you *must* use the same
  underlining scheme as the file's creator.
* If you are adding a new file to an existing document, it is not imperative (but it is
  preferred) that you use the same underlining scheme as used in the rest of the document.
  However you do it, you must be consistent.
* If you are creating a new document, you do not have to use the same underlining
  scheme as shown in the example. As long as each heading level is different and
  you are consistent, it is fine. It is better to use what you will remember. 


The tech writer will review new documentation to make sure you are consistent.
Although, if you are not consistent, Sphinx will complain when you try to build. 

         
*Italics*
++++++++++

Use italics for the following:

* Titles of books and our OpenMDAO documents (e.g., *User Guide*)

* The first time you refer to a term  (". . . is called a *Component*.")

* In place of quotation marks (in many cases). Before the widespread use of
  computers and desktop publishing, italics could not be designated, so quotation marks
  were used. Now italics can easily be specified and, in general, should be used in place
  of quotes, for example, when discussing a parameter in a code sample.  (If it's a
  long parameter, it might be better to use literal text.
 

.. index:: Python; capitalization

*Capitalization* 
++++++++++++++++

Class Names
~~~~~~~~~~~~~

Always capitalize class names (e.g., Component, Assembly, Driver, Engine, etc.).
Sometimes, a  class represents a concept having the same name. In that case, the name
of the concept would generally *not* be capitalized. 

Fortran
~~~~~~~~~

Capitalize only the first letter of *Fortran* unless you are
referring to a version earlier than Fortran 90, when it was known as FORTRAN (e.g.,
FORTRAN 77).


HTML
~~~~~~

This initialism stands for *HyperText Markup Language* and should be typed in
all caps.


Python 
~~~~~~

Capitalize *Python* when referring to the programming language, for example, a
*Python* module. However, *python* should be lower case when it refers to an
OpenMDAO path name, script name, command, part of a URL, etc., that is lower case.

This rule also applies to other programming languages or software programs; for example,
``Enthought, Inc.`` is capitalized, but ``enthoughts.traits.api`` is not. 


reStructuredText
~~~~~~~~~~~~~~~~~

Please capitalize the appropriate letters and type it as one word:
reStructuredText.


website
~~~~~~~

In our OpenMDAO documents, we will not capitalize *website* but use all lower case letters
(one word). Please **do not** use any of the following variants: *Web site, web site,* or
*Website.* Not only is *website* easier to type, but in 2003, in a survey of over 150 WordBiz Report
subscribers, 65% of those surveyed preferred *website* -- even those who thought *Web site*
was correct. And as noted in *The American Heritage Dictionary,* 4th ed., the use of
*website* reflects the trend of other technological expressions which have moved to
unhyphenated forms as they become more familiar (e.g., email, online). The main thing is
consistency, so please be consistent and use *website.* 


*Numbers*
+++++++++

*  Write out numbers between zero and nine (0--9) when they are modifiers (two
   assemblies). 
*  Numbers above nine may be written in digit form (12 components).
*  Numbers that are values should always be written in digit form (default value is 8).


.. _Using-Inline-Literal-Text:

*Inline Literal Text*
+++++++++++++++++++++

Inline literal test is designated by back quotes (the same computer key as the
tilde) enclosing the specified text. ``Inline literal text`` can be used in many
situations; in OpenMDAO documentation please use it to designate the following:

::

  path names: 		``/OpenMDAO/dev/shared/working_main``
  directory names:   	``docs/dev-guide``
  api names: 	 	``openmdao.main.api``
  Python packages:	``openmdao.lib``
  file names		``example.rst``
   
which will result in text that looks like this:

* ``/OpenMDAO/dev/shared/working_main``
* ``docs/dev-guide``
* ``openmdao.main.api``
* ``openmdao.lib``
 
Additionally, if a word or phrase contains an underscore (_) or a dot (.), use literal text, so it
is easier to read, e.g., ``_init_`` function, ``self.driver``, and ``optimization_constrained.py``.


*Abbreviations and Acronyms*
+++++++++++++++++++++++++++++

An acronym is a pronounceable word formed from the  initial letter or letters of major
parts of a compound term. An abbreviation is usually formed in the same way but is not
pronounced as a word. Abbreviations are often lowercase or a mix of lowercase and
uppercase. Acronyms are almost always all caps, regardless of the capitalization style
of the spelled-out form. 

	| ``Abbreviation: 	mph,for miles per hour; MB/s, for megabytes per second`` 
	| ``Acronym: 		ROM, for read-only memory``

Acronyms and abbreviations should go in the glossary. We have one glossary and one
index for all OpenMDAO user documents. 

* *When to spell out:* In general, spell out the term when you introduce it. You may
  also spell out an abbreviation or acronym if you think your audience may not be
  familiar with it. 
* *How to spell out:* Generally put the spelled-out version first, with the
  abbreviation or acronym in parentheses, for example:

	| ``Internet service provider (ISP)``
		
  If the abbreviation or acronym is much more familiar than the spelled-out version,
  you can put the abbreviation or acronym first, followed by the spelled-out version in
  parentheses, or you can explain that the abbreviation is short for the spelled-out
  version and place the spelled-out version in italics.
  
   	| You can share your personal URL (Uniform Resource Locator).
	| An Internet address is sometimes called a *URL,* short for *Uniform
	  Resource Locator.*
  
* *File types:* Use all caps for abbreviations of file types.
		 

	| ``JPEG file, PDF file, MP3 file``
		
  Filename extensions, which indicate the file type, should be in lowercase.
			
	| ``.jpg, .pdf, .mp3``
			
* *Punctuation:* Don't use periods except in abbreviations for customary (non-metric)
  units of measure and in the abbreviations U.S., a.m., and p.m.

* *Plural:* Don't add an apostrophe before the "s" when you form the plural of an
  abbreviation.
  
  	| ``CDs, URLs``


**Abbreviations:**

2D - Abbreviation for two-dimensional. No hyphen in the abbreviation. 

3D - Abbreviation for three-dimensional. No hyphen in the abbreviation.


*Hyphens and Dashes*
+++++++++++++++++++++

**- Hyphen:**

Use a hyphen (-) for the following:

* Simple compound modifiers (twentieth-century writers) -- Do not use a hyphen with
  adverb-adjective modifiers (wholly owned subsidiary)
* Certain prefixes and suffixes	-- American English tends toward the omission of
  hyphens, particularly for certain prefixes (co-, pre-, mid-, non-, anti-, de-,
  etc.) However, a hyphen is required when a prefix is applied to a proper noun
  (un-American, non-Sphinx). 
* Adjectival phrases formed by connecting numbers and words 

  * Numerals or words for numbers 	(320-foot wingspan, twenty-eight-year-old man)
  * Spelled out fractions	(two-thirds majority)
  * Symbols or SI units that are spelled out	(25-kilogram sphere, as opposed to 25 kg
    sphere)
    
* Two-word numbers less than a hundred  (twenty-nine)

.. note:: Do not put a hyphen in the word *plugin.* While *plug-in* is a correct
   variation, we must be consistent, and we chose to omit the hyphen. (This
   follows the trend of omitting hyphens in technological terminology.)

**- En dash:**

Use an en dash (--) for the following:

* To indicate a range (pp. 25--36, June--July 2006, 1:00--2:00 p.m., etc. Note
  that there are no spaces on either side of the dash.  

* For parenthetical expressions	-- Use an en dash (--) and leave a single space on
  either side. 

* To contrast values or show a  relationship between two things (New York--London flight,
  Supreme Court's 5--4 decision)  

In reST an en dash is formed by typing two hyphens (minus signs).

*Comma (in a Compound Sentence)*
++++++++++++++++++++++++++++++++++

* Use a comma before "and" when you have a compound sentence, for example:

    ``Many analysis components will require some representation of geometry, and
    that representation could vary in detail from simple parameters, e.g., length,
    up to a full 3D mesh.``

 | A comma is required before the "and" because the sentence has two independent
   clauses, i.e., each clause has a subject and a verb, making the sentence compound. 

*  Do *not* use a comma before "and" when the construction is merely a compound verb,
   as in:


     ``Some of these effects were derived from empirical data and are essentially
     valid over an engine speed ranging from 1000 RPM to 6000 RPM.``
    
 | In this case the sentence has one subject *(some)* but two verbs *(were derived*
   and *are*). It is not a compound sentence.

