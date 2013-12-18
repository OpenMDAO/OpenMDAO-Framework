.. _units:


Appendix: Summary of Units
=============================

.. index::  units

Generally SI units (http://www.bipm.org/utils/common/pdf/si_brochure_8_en.pdf)
are preferred, but these often don't cover the quantities of interest in a
convenient manner. Outside of SI there are numerous non-standard unit names,
sometimes multiple names for the same physical quantity (e.g., ``a`` and ``yr``),
sometimes very similar names for different quantities (e.g., ``ton``).
OpenMDAO includes a default set of units definitions as well as methods to
enable you to update/extend the definitions or completely replace them. 
See the source documentation for the :ref:`units package<openmdao.units.units.py>`.

The following units are defined in the OpenMDAO units package:

.. literalinclude:: ../openmdao.units/openmdao/units/unitLibdefault.ini
