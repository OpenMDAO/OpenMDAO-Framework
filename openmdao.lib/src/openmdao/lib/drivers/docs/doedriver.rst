.. index:: DOEdriver, design of experiments

.. _DOEdriver:

*DOEdriver*
~~~~~~~~~~~

The DOEdriver provides the capability to execute a DOE on a workflow.
This Driver supports the IHasParameters interface. At execution time, 
the driver will use the list of parameters added to it by the user to 
create a specific DOE and then iteratively execute the DOE cases on the
workflow. 

Users can pick from any of the DOEgenerators provided in the standard
library or provide their own custom instance of a DOEgenerator. A DOEgenerator
must be plugged into the DOEgenerator socket on the DOEdriver in order to
operate. 
    
    .. testcode:: DOEdriver
    
        from openmdao.main.api import Assembly
        from openmdao.lib.drivers.api import DOEdriver
        from openmdao.lib.doegenerators.api import FullFactorial

        from openmdao.examples.expected_improvement.branin_component import BraninComponent
        
        class Analysis(Assembly): 
            def __init__(self,doc=None): 
                super(Analysis,self).__init__()
                
                self.add('branin', BraninComponent())
                self.add('driver', DOEdriver())
                self.driver.workflow.add('branin')

                self.driver.add_parameter('branin.x')
                self.driver.add_parameter('branin.y')
                
                #use a full factorial DOE with 2 variables, and 3 levels
                #   for each variable
                self.driver.DOEgenerator = FullFactorial(num_levels=3)
   
The *min* and *max* metadata of the parameters are used to denote the range for
each variable over which the DOE will span.

*Source Documentation for doedriver.py*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
