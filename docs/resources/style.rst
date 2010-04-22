.. index:: Style Guide

.. _Style-Guide:


Style Guide 
===========

This document provides some general guidelines for writing documentation. It is by no means
inclusive.

For Python coding conventions, please refer to the `Python Style Guide
<http://www.python.org/dev/peps/pep-0008/>`_.

.. index:: docstrings standard

Standard for Coding Docstrings
------------------------------

A documentation string (docstring) is a string that describes a module, function, class, or
method definition. NumPy, SciPy, and SciKits already follow a common convention for docstrings
that provides consistency. Since an acceptable standard already exists, the OpenMDAO project will
follow it.

Please refer to NumPy's `Docstring Standard
<http://projects.scipy.org/numpy/wiki/CodingStyleGuidelines#docstring-standard>`_ when coding
docstrings.


.. index:: underlines in reST

Underlines (and Overlines)
--------------------------

**- Document title:**


In general, use underline and overline for the title of document (in the
index file for the document). In OpenMDAO, we typically use a double line for
titles as shown in the following example:

::

  ============
  User's Guide
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

         
Italics
-------

Use italics for the following:

* Titles of books and our OpenMDAO documents (e.g., *Plugin Developer's Guide*)

* The first time you refer to a term  (". . . is called a *Component*.")

* In place of quotation marks (in many cases). Before the widespread use of
  computers and desktop publishing, italics could not be designated, so quotation marks
  were used. Now italics can easily be specified and, in general, should be used in place
  of quotes, for example, when discussing a parameter in a code sample.  
 
* Function names (e.g., the *execute()* function)


.. index:: Python; capitalization

Capitalization 
---------------

*Class Names*
+++++++++++++

Always capitalize class names (e.g., Component, Assembly, Driver, Engine, etc.).
Sometimes, a  class represents a concept having the same name. In that case, the name
of the concept would generally *not* be capitalized. 

*Fortran*
+++++++++

Capitalize only the first letter of *Fortran* unless you are
referring to a version earlier than Fortran 90, when it was known as FORTRAN (e.g.,
FORTRAN 77).


*HTML*
++++++

This initialism stands for *HyperText Markup Language* and should be typed in
all caps.


*Python* 
++++++++

Capitalize *Python* when referring to the programming language, for example, a
*Python* module. However, *python* should be lower case when it refers to an
OpenMDAO path name, script name, command, part of a URL, etc., that is lower case.

This rule also applies to other programming languages or software programs; for example,
*Enthought, Inc.* is capitalized, but *enthoughts.traits.api* is not. 


*reStructuredText*
++++++++++++++++++

Please capitlaize the appropriate letters and type it as one word:
reStructuredText.


*website*
+++++++++

In our OpenMDAO documents, we will not capitalize *website* but use all lower case letters
(one word). Please **do not** use any of the following variants: *Web site, web site,* or
*Website.* Not only is *website* easier to type, but in 2003, in a survey of over 150 WordBiz Report
subscribers, 65% of those surveyed preferred *website* -- even those who thought *Web site*
was correct. And as noted in *The American Heritage Dictionary,* 4th ed., the use of
*website* reflects the trend of other technological expressions which have moved to
unhyphenated forms as they become more familiar (e.g., email, online). The main thing is
consistency, so please be consistent and use *website.* 


Numbers 
-------

*  Write out numbers between zero and nine (0--9) when they are modifiers (two
   assemblies). 
*  Numbers above nine may be written in digit form (12 components).
*  Numbers that are values should always be written in digit form (default value is 8).


.. _Using-Inline-Literal-Text:

Inline Literal Text
--------------------

Inline literal test is designated by back quotes (the same computer key as the
tilde) enclosing the specified text. ``Inline literal text`` can be used in many
situations; in OpenMDAO documentation please use it to designate the following:

::

  path names: 		``/OpenMDAO/dev/shared/working_main``
  directory names:   	``docs/dev-guide``
  api names: 	 	``openmdao.main.api``
  Python packages:	``openmdao.recipes``
   
which will result in text that looks like this:

* ``/OpenMDAO/dev/shared/working_main``
* ``docs/dev-guide``
* ``openmdao.main.api``
* ``openmdao.recipes``
 

Abbreviations and Acronyms
--------------------------

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


Hyphens and Dashes
------------------

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

Comma (in a Compound Sentence)
-------------------------------

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


Login vs Log in
---------------

The verb is *Log in* and *Log into* as "Log *in* using the password provided" or
"Log *into* the MDAO eRoom." The noun or adjective is *Login,* e.g., "You will need
valid *Login* credentials to use the system." (not logon, log, log-in, etc.)


