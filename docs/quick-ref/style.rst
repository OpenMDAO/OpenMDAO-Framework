.. index:: Style Guide

.. _Style-Guide:

Style Guide
===========

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

The overline and underline must be the same length, or you will get an error and
the build will fail. 

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

About heading levels:

* If you are adding text to an existing file, you *must* use the same
  underlining scheme as the file's creator.
* If you are adding a new file to an existing document, it is not imperative (but it is
  preferred) that you use the same underlining scheme as used in the rest of the document.
  Consistency is a must.
* If you are creating a new document, you do not have to use the same underlining
  scheme as shown in the example. As long as each heading level is different and
  you are consistent, it is fine. It is better to use what you will remember. 


The tech writer will review new documentation to make sure you are consistent.
Although, if you are not consistent, Sphinx will complain when you try to build. 


Fonts
------

Please follow the guidelines in the next section.

.. index:; buildout recipes

*Buildout recipe fonts*
+++++++++++++++++++++++

Buildout recipes have configuration parameters. Please adhere to the guidelines
for specifying parameters in OpenMDAO buildout recipes:

* **Required** parameters should be in boldface type, designated in reStructuredText
  (reST) by double asterisks, for example, ``**parameter name**``, which results in
  **parameter name**.  
* *Optional* parameters should be in italics, designated by single asterisks, for
  example, ``*parameter name*``, which results in *parameter name*.
  
          
*Italics font (In general)*
+++++++++++++++++++++++++++

Use italics for the following:

* Titles of books and our OpenMDAO documents (e.g., *Plugin Developer's Guide*)
* The first time you refer to a term  (". . . is called a *Component*.")
* When referring to text inside a code sample that you are discussing. Please write it
  exactly as shown in the code sample. 

For example, if the code says: 

	| ``c = Float (0.0, iostatus= 'out')``

and you're referring to part of it, you would italicize it, as in:  *c, Float,* and
*'out'*.  

This is for cases where the references are short. For longer references, you probably want
to use literal inline markup.

.. index:: Python; capitalizing

Capitalization (Python & other software packages) 
--------------------------------------------------

Capitalize *Python* when referring to the programming language, for example, a
*Python* module.  However, *python* should be lower case when it refers to an
OpenMDAO path name, script name, command, part of a URL, etc., that is lower case.

This rule also applies to other software programs or packages;  for example,
*Enthought, Inc.* is capitalized, but *enthoughts.traits.api* is not. 


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
  Python packages	``openmdao.recipes``
   
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


Commas (before "and" in a compound sentence)
--------------------------------------------

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


