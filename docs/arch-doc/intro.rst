Introduction
------------

.. index:: OpenMDAO 
.. index:: MDAO 
.. index:: Fundamental Aeronautics Program

This document describes the initial architecture of OpenMDAO, which is the next
generation open source (OS) Multidisciplinary Analysis and Optimization (MDAO)
Framework being developed under NASA's Fundamental Aeronautics Program (FAP)
Subsonic Fixed Wing Project. 


Identification
===============

This initial architecture is based on the MDAO requirements submitted in January
2008 and prioritized in subsequent months. The requirements prioritization was
completed in mid-September 2008. The requirements leading to the OS MDAO
framework design can be found in the MDAO *Software Requirements Specification*
and the MDAO Functional Requirements database. Note however that the
requirements mentioned there cover not only the framework but also a number of
engineering tools meant to be integrated into the framework. Since the OS MDAO
framework is under development, only a document ID (MDAO-Architecture) exists at
this time. No document number, version number, release number, or other
applicable data are currently available. Additional identification information
will be included in future versions of this document as it becomes known.


Purpose  
========

This document concentrates only on the architecture of the open source framework
itself and the interfaces between the framework and the engineering tools. This
document is intended to be used by the MDAO Working Group and will be updated as
needed.


.. index:: system; overview
.. index:: Fundamental Aeronautics Program


System Overview
===============

NASA's Fundamental Aeronautics Program, under the Aeronautics Research Mission
Directorate (ARMD), has a top-level goal to develop system-level,
multidisciplinary capabilities for both civilian and military applications. To
work toward achieving this goal, the Subsonic Fixed Wing Project has an
overarching objective to develop fast and effective physics-based
multidisciplinary analysis and design tools with quantified levels of uncertainty
that enable virtual access to the flight envelope and virtual expeditions
through the design space. NASA is investing in this effort to improve in-house
analysis and design capabilities important for guiding technology investment in
fundamental aeronautics areas. 

For more details on the purpose of NASA's MDAO OS framework, see the MDAO
*Vision and Scope Document.*


Referenced Documents
====================

The following documents were used as references in writing this
architecture document.

**NASA Documents:**

==================  ================================================
*Document Number*	 	  *Title*	
------------------  ------------------------------------------------
NPR 7150.2           NASA Software Engineering Requirements
------------------  ------------------------------------------------
GLPR 7150.1          GRC Software Engineering Requirements
------------------  ------------------------------------------------
GRC-SW-TPLT-SDD      Software Design Description Template
------------------  ------------------------------------------------
N/A	     	     FAP Multidisciplinary Analysis 
		     and Optimization Vision and Scope
------------------  ------------------------------------------------
N/A	     	     FAP Multidisciplinary Analysis and Optimization 
		     Software Requirements Specification    
==================  ================================================


For those with access to the Glenn Research Center's eRoom, the MDAO Glossary
contains definitions of acronyms, terms, and software tools relevant to the MDAO
capability. This glossary is located at:
https://collaboration.grc.nasa.gov/eRoom/NASAc1f1/MDAOActivityWorkspace/0_fa294


**Other References:**

- Clements, Paul, et al, *Documenting Software Architectures: Views and Beyond,* Addison-Wesley, Boston, 2003.
- Gamma, Erich, et al, *Design Patterns: Elements of Reusable Object-Oriented Software,* Addison-Wesley, Reading, MA, 1994.
- Langtangen, H. P., *Python Scripting for Computational Science,* Third Edition, Springer-Verlag, Berlin, 2008.
- von Weitershausen, Phillip, *Web Component Development with Zope 3,* Springer-Verlag, Berlin, 2005.


