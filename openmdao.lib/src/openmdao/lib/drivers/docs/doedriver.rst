.. index:: DOEdriver, design of experiments

.. _DOEdriver:

*DOEdriver*
~~~~~~~~~~~

The DOEdriver provides the capability to execute a DOE on a workflow. This
Driver supports the IHasParameters and the IHasResponses interfaces. At
execution time, the driver will use the list of parameters added to it by the
user to create a specific DOE and then iteratively execute the DOE cases on
the workflow.

Users can pick from any of the DOEgenerators provided in the standard
library or provide their own custom instance of a DOEgenerator. A DOEgenerator
must be plugged into the DOEgenerator socket on the DOEdriver in order to
operate.

    .. testcode:: DOEdriver

        from openmdao.main.api import Assembly
        from openmdao.lib.drivers.api import DOEdriver
        from openmdao.lib.doegenerators.api import FullFactorial

        from openmdao.lib.optproblems.branin import BraninComponent

        class Analysis(Assembly):
            def configure(self):

                self.add('branin', BraninComponent())
                self.add('driver', DOEdriver())
                self.driver.workflow.add('branin')

                self.driver.add_parameter('branin.x', low=-5., high=10.)
                self.driver.add_parameter('branin.y', low=0., high=15.)
                self.driver.add_response('branin.f_xy')

                #use a full factorial DOE with 2 variables, and 3 levels
                #   for each variable
                self.driver.DOEgenerator = FullFactorial(num_levels=3)

The *min* and *max* metadata of the parameters are used to denote the range for
each variable over which the DOE will span.

By default, the normalized parameter values are written to a CSV file in the
driver's execution directory, with a name of ``<driver-name>.csv``.  This can be
used later to rerun all of the cases by using a :class:`CSVFile` DOE generator.
You can also select which cases should be run by filling the `case_filter` slot.

*Source Documentation for doedriver.py*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
