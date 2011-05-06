.. index:: Component

Components and Variables
==========================

In the previous section, three component models were outlined that together form a vehicle model that can simulate
performance. These models have all been implemented as OpenMDAO components written in Python. This
section will examine these components.

The following instructions will help you locate the directory containing the pieces
needed for the model.

If you have downloaded the latest release version from the website, the files you need should be
here:

::

  openmdao-X.X.X/lib/python2.6/site-packages/openmdao.examples.enginedesign-X.X.X-######.egg/openmdao/examples/enginedesign

``X.X.X`` is the current OpenMDAO version, and ``######`` is a string that
contains the Python version and the operating system description. This path will 
vary depending on your system and version, but there will be only one
*enginedesign* egg.

If you are a developer and have a branch from the source repository, the files you need will be
here:

::

  examples/openmdao.examples.enginedesign/openmdao/examples/enginedesign

The three engine models have been implemented in ``transmission.py, engine.py,`` and ``chassis.py``. It will
be useful to browse these files as you learn some of the basic concepts in this tutorial.

**Building a Python Component**

The first thing we will do is create an OpenMDAO Component called *Transmission.* You may recall
that a Component contains inputs and outputs and provides a method to calculate its outputs
from its inputs. We must create the variables that define these inputs and outputs.

.. testcode:: Code2

    from openmdao.main.api import Component, convert_units
    from openmdao.lib.datatypes.api import Float, Int, Enum

    
    class Transmission(Component):
        """ A simple transmission model."""
    
        ratio1 = Float(3.54, iotype='in', 
                       desc='Gear ratio in First Gear')
        ratio2 = Float(2.13, iotype='in', 
                       desc='Gear ratio in Second Gear')
        ratio3 = Float(1.36, iotype='in', 
                       desc='Gear ratio in Third Gear')
        ratio4 = Float(1.03, iotype='in', 
                       desc='Gear ratio in Fourth Gear')
        ratio5 = Float(0.72, iotype='in', 
                       desc='Gear ratio in Fifth Gear')
        final_drive_ratio = Float(2.8, iotype='in', 
                                  desc='Final Drive Ratio')
        tire_circ = Float(75.0, iotype='in', units='inch', 
                          desc='Circumference of tire (inches)')

        current_gear = Enum(0, (0,1,2,3,4,5), iotype='in', desc='Current Gear',
                            aliases=('N','1st','2nd','3rd','4th','5th'))
        velocity = Float(0., iotype='in', units='mi/h',
                         desc='Current Velocity of Vehicle')

        RPM = Float(1000., iotype='out', units='rpm',
                    desc='Engine RPM')        
        torque_ratio = Float(0., iotype='out',
                             desc='Ratio of output torque to engine torque')    

The *Int* and *Float* variable types were introduced in other tutorials, but some new parameters are
used here, including the *Enum* type.

.. index:: PEP 8

In this example, we use more complicated names for our variables, so we should cover what makes a
valid variable name. Variables are also Python variables, so they must follow Python's standard
naming convention. They must begin with a letter or underscore and should consist of only
alphanumeric characters and the underscore. Keep in mind that a leading underscore is generally used
for private data or functions. **Spaces cannot be used in a variable name.** Generally, we've tried
to follow the `PEP 8 <http://www.python.org/dev/peps/pep-0008/>`_  standard for component instance 
names as well as Python variable names. PEP 8 prescribes the use of lower case names with words
separated by underscores.

The required parameter *iotype*, the optional parameter *desc*, and the
default value are covered in previous tutorials. The parameter *units* is
introduced here and is used in a few of the variables in Transmission. OpenMDAO contains a Units
module that allows for unit checking and conversion between outputs and
inputs of components. Our units definitions are based on the those given in
Scientific Python and can be found in :ref:`units`. If an output and an
input of the same unit class (e.g., length) are connected in the framework, the value is automatically
converted as it is passed from the output to the input. If an output and an input
of a different unit class are connected (e.g., length and time), an exception is
raised. A variable with no units defined can be connected to a variable with units.

**Only the Float and Array types perform automatic unit conversion and checking.**

The Transmission component uses an enumerated list to define the valid gear positions:

.. testcode:: Code2

        current_gear = Enum(0, (0,1,2,3,4,5), iotype='in', desc='Current Gear',
                        aliases=('N','1st','2nd','3rd','4th','5th'))

An *enumeration* is a discrete variable that has a finite number of valid states. This
transmission is a 5-speed manual, so the valid states are gears 1 through 5 and neutral. The
constructor begins with a default value and a :term:`tuple` containing all of the valid states. Sometimes
it is beneficial to add as the *alias* parameter a second tuple containing a more descriptive
tag. In this case, the alias *'N'* tells you that a value of 0 sets the gear to Neutral. The
Enum is not type-restrictive. You could use the alias strings as the values, though typically
the values are needed because you have some code that operates on an integer number. In our
case, our simulation will upshift by adding 1 to the current gear, which it couldn't do to
the strings. For more information, see :ref:`Enums`.

Finally, ``transmission.py`` needs to do something when it is executed. This code
illustrates how to use the input and output variables to perform a calculation. 

.. testcode:: Code2

    def execute(self):
        """ The 5-speed manual transmission is simulated by determining the
            torque output and engine RPM via the gear ratios.
            """
    
        ratios = [0.0, self.ratio1, self.ratio2, self.ratio3, self.ratio4,
                  self.ratio5]
        
        gear = self.current_gear
        differential = self.final_drive_ratio
        tire_circ = self.tire_circ
        velocity = convert_units(self.velocity, 'mi/h', 'inch/min')
        
        self.RPM = (ratios[gear]*differential*velocity)/(tire_circ)
        self.torque_ratio = ratios[gear]*differential
        
        # At low speeds, hold engine speed at 1000 RPM and partially engage the clutch
        if self.RPM < 1000.0 and self.current_gear == 1 :
            self.RPM = 1000.0
    
You may recall that inputs and outputs are attributes of our component, so they are accessed using
``self.variablename``. It is generally a good idea to create a local copy of a variable for doing calculations in the component for improved efficiency and ease of reading.

We have also imported and used the ``convert_units`` function to convert the value of velocity
from units of mi/h to units of inch/min. This makes the units consistent for the calculation
of RPM. The ``convert_units`` function provides unit conversion capability for your internal
variables. We could also change the definition of the velocity Float, specifying the units
as ``'inch/min'=``, and then the ``convert_units`` call would not be needed.

The transmission model is now complete; the next section will show how to interact with
it in the Python shell. The engine and chassis are created in a similar manner. However, the 
engine's speed is valid only within a range 1000 to 6000 RPM, primarily because the engine model
is only valid in this range. We addressed this by adding two outputs ``overspeed`` and 
``underspeed`` to warn when the engine has gone over or under the maximum or minimum RPM.

OpenMDAO's variables also include a way to specify a minimum and a maximum value. For example,
the throttle is valid from 0.01 (closed with a small amount of idle flow) to 1.0 (wide
open.)

.. testcode:: Code2

    throttle = Float(1.0, low=0.01, high=1.0, iotype='in', 
                     desc='Throttle position (from low idle to wide open)')

The *low* and *high* attributes are used to specify a minimum and maximum value
for the throttle. If the throttle is set to a value outside of these limits, an
exception will be raised. OpenMDAO execution is terminated unless this
exception is caught elsewhere and some kind of recovery behavior is defined.
