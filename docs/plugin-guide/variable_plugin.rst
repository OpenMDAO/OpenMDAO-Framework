
Building a Variable Plugin
==========================

Sometimes it's necessary to create a new type of variable that can be passed 
between OpenMDAO components.  This section describes how to do this using a 
pure Python OpenMDAO plugin.

Let's assume we want to have a variable that represents a set of Cartesian 
coordinates, with the value of the variable being a 3-tuple of floating point
values representing the *x, y,* and *z* position.

As before when we created a component plugin, we'll use ``plugin_quickstart`` to
generate the directory structure for our distribution, but this time we use
the **-g** option to specify the plugin group as ``openmdao.variable``.  
Also, this time around we'll specify a class name of *Coordinates* using
the **-c** option.

::


    plugin_quickstart coord -c Coordinates -g openmdao.variable


Since we said our distribution name is going to be *coord*, that means that
``plugin_quickstart`` created a skeleton of our plugin class definition in 
the ``src/coord/coord.py`` file.  After editing that file, it looks like this:

::

    from openmdao.main.variable import Variable
    
    class Coordinates(Variable):
    
        def __init__(self, default_value = (0.,0.,0.), **metadata):
            super(Coordinates, self).__init__(default_value=default_value,
                                             **metadata)
    
        def validate(self, object, name, value):
            if isinstance(value, tuple) and len(value) == 3 and \
               all([isinstance(val,(float,int)) for val in value]):
                return value
            else:
                self.error(object, name, value)


OpenMDAO provides a base class for framework visible inputs and outputs called
``Variable``, so that's the base class for our coordinates variable. If a
Component object contains a ``Variable`` instance that has a metadata
attribute named *iotype* then that instance object is exposed to the framework
as a variable whose value can be passed between components. Valid values for
*iotype* are ``'in'`` and ``'out'``.

One thing that can be a little confusing to people first using Variables is that
the Variable object itself is just a validator and possibly a converter. The
object that actually gets passed around between components is the *value* that
the variable corresponds to and not the variable itself. For example, if we had a
component named *wheel* that contained one of our Coordinates variables named
``center_location``, then the value of ``wheel.center_location`` would be a
3-tuple, not a Coordinates object.

We override the base class constructor so we can supply a default value of
(``0.,0.,0.``) if the caller doesn't supply one. After that, the only function we
need to supply is the ``validate`` function, which will be called with the
following arguments:

**object**
    The object that contains the value of our coordinates variable

**name**
    The name of our coordinates variable

**value**
    The value that our current value is being replaced with


Our ``validate`` function should test that the value we've been called with is
valid. In this particular case, we just need to verify that the value is a
3-tuple and it has float or int entries. If the value is acceptable, then we
just return it. We don't need to do it in this case, but in other custom
traits, we could convert the value before returning it. If the value
is not acceptable, then we call the ``error`` function, which will raise an
exception, typically a ValueError or a TypeError depending on the nature of
the error.

That's all of the source code required to make our Coordinates variable 
functional.  As in the earlier section, where we made a component plugin,
we need to specify the metadata for our distribution by editing the 
``setup.cfg`` file and add any extra documentation that we want to the
``docs/usage.rst`` file and the ``README.txt`` file.  When that's done,
as before, we run ``plugin_makedist``; the end result should be a
source distribution named ``coord-0.1.tar.gz``.  The version id of our 
plugin defaulted to **0.1** because we didn't specify it when we ran
``plugin_quickstart``.


Building a Driver Plugin
========================

Drivers are Components that have some extra API functions to let them
deal with iterating over workflows, handling design variables, etc.
Driver plugins are built and distributed in the same way as component
plugins, so we can use the ``plugin_quickstart`` script to create a
starting directory structure for our plugin:

::


    plugin_quickstart mydriver -c MyDriver -g openmdao.driver


By settng the **-g** option to ``openmdao.driver``, we tell ``plugin_quickstart``
to generate a source file containing a Driver class.  The rest of the plugin
development process is the same as described in the :ref:`build-pure-python-plugin-label`
section.




