
Executing a Component in the Python Shell
==========================================

The Python implementations of the three component models (``engine.py,
transmission.py, chassis.py``) should all make sense now. This section will
demonstrate how to instantiate and use these components in the Python shell.
Make sure that you have activated your Python environment before running the
Python interpreter so that you can access OpenMDAO and the example problems.

Please enter the Python shell. Create an instance of the Engine class by typing the following:

.. doctest:: vars

    >>> from openmdao.examples.enginedesign.engine import Engine
    >>> my_engine = Engine()

The object ``my_engine`` is an engine model with default values for all of its inputs. We can interact
with the inputs and outputs by direct inspection:

.. doctest:: vars

    >>> my_engine.bore
    82.0

Let's change the engine speed from its default value (1000 RPM) to 2500 RPM.

.. doctest:: vars

    >>> my_engine.RPM = 2500
    >>> my_engine.RPM
    2500.0

The value of the engine RPM is now 2500. Let's see what happens if we try to
set the RPM to something invalid.

.. doctest:: vars

    >>> my_engine.RPM = "Hello"
    Traceback (most recent call last):
        ...
    ValueError: Variable 'RPM' must be a float having units compatible with 'rpm' but a value of Hello <type 'str'> was specified.
    
The value ``"Hello"`` is invalid because RPM expects a value of type Float. Now,
let's try setting the throttle to a value that exceeds the maximum.

.. doctest:: vars

    >>> my_engine.throttle = 2.0
    Traceback (most recent call last):
        ...
    ValueError: Variable 'throttle' must be a float in the range [0.01, 1.0] but a value of 2.0 <type 'float'> was specified.

OpenMDAO raises an exception indicating that the maximum value for throttle has
been violated. This exception can be handled to provide some logical response
to this situation. Now, run the engine and examine the power and torque at
2500 RPM.

.. doctest:: vars

    >>> my_engine.run()
    >>> my_engine.torque
    203.963228...
    >>> my_engine.power
    53.3974483...
    
You run a component by calling the ``run`` method. If you recall, you define an ``execute`` method when you
create a component. The ``run`` method calls ``execute`` but also does some other things. Always run your
component with ``run.``
