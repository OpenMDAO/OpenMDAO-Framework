.. index:: script interface

.. _`OpenMDAO-script-interface`:


OpenMDAO Fundamentals
=====================

OpenMDAO provides a programmatic interface that allows you to write a Python
script that describes the structure of the model and provides the ability to
interact with objects in the framework.

The goal of this section is to explain and demonstrate several aspects of the OpenMDAO
script interface. This section is intended primarily as a reference. If you are an
inexperienced user, we recommend that you read and understand the examples in
:ref:`A-Simple-Tutorial-Problem` and :ref:`A-More-Complex-Tutorial-Problem` before
reading this section.

The following sections discuss the the basics of OpenMDAO.

.. index:: package

OpenMDAO Package Hierarchy
---------------------------

*Package* is a Python concept that provides a structure for organizing
variables and functions in a logical hierarchical fashion. Packages allow you
to import needed functions and class definitions into the Python environment
using dotted module names, where the branch levels are separated by a period
(".").

The OpenMDAO package hierarchy includes several subpackages, all of which are prefixed by 
``openmdao.``:

- ``openmdao.main`` -- core infrastructure for the framework
- ``openmdao.lib`` -- OpenMDAO's standard library, containing some important plugins (drivers, traits, etc.) that are available to users of the framework
- ``openmdao.units`` -- unit definition and conversion
- ``openmdao.examples`` -- tutorials and example problems for learning OpenMDAO
- ``openmdao.util`` -- utilities used by OpenMDAO but can also be used standalone
- ``openmdao.test`` -- functions and classes used strictly for unit testing

OpenMDAO users and component developers will likely need only the first three of these (``main,
lib,`` and ``units``). Importing classes and functions from OpenMDAO's libraries is performed with
the same syntax as loading any other Python module:

.. testcode:: package

    from openmdao.main.api import Component, Assembly
    from openmdao.lib.drivers.api import CONMINdriver

Here, the fundamental OpenMDAO component classes *Component* and *Assembly* are
loaded from ``openmdao.main.api``, along with the CONMIN driver from ``openmdao.lib.drivers.api``.

To simplify the imports, a selection of the most commonly used imports was
placed in the pseudo-package ``openmdao.main.api``. You can obtain a complete
listing of what is available in this module by using the ``dir()`` command in
Python. Likewise, a pseudo-package was also created to house some of the most
commonly used imports from the standard library. In general, it contains
variables and drivers. Most of these items are also explained elsewhere
in the documentation.

Importing more objects into the namespace of your module increases the
likelihood of name collision, so you should import only the objects that you need.
You should avoid using ``from <modname> import *`` because it puts every object
from the given module into the current namespace. 

.. testcode:: package

    # BAD
    from openmdao.main.api import *
    
    # INCONVENIENT
    import openmdao.main.api
    
    # GOOD
    from openmdao.main.api import Component, Assembly, Driver


Naming Conventions
--------------------

Components and variables that are instantiated into the OpenMDAO model 
hierarchy must follow the same naming syntax as attributes in the Python
language. To summarize, they can include only alphanumeric
characters and the underscore, and the lead character cannot be a number.
Any attempt to create a component or a variable that does not conform
to Python's syntax should result in an exception. This restriction was required
because these entities essentially exist as Python variables. One unfortunate
side effect is that names with spaces are not allowed. OpenMDAO checks for
compliance when a variable or Component instance is created:

    >>> from openmdao.main.api import Assembly
    >>> from openmdao.examples.enginedesign.chassis import Chassis
    >>> top = Assembly('top')
    >>> top.add('chassis1',Chassis())
    <openmdao.examples.enginedesign.chassis.Chassis object at ...
    >>> top.add('the chassis',Chassis())
    Traceback (most recent call last):
    ...
    NameError: name 'the chassis' contains illegal characters

In the OpenMDAO source and examples, we've tried to follow the `PEP 8
<http://www.python.org/dev/peps/pep-0008/>`_ standard, which specifies a naming
convention for component instance names and variable names. For all
variable names, PEP 8 prescribes the use of lower case names with words
separated by underscores. Naturally, PEP 8 compliance is not a requirement
that will be forced on users, but it is a good style guideline.

