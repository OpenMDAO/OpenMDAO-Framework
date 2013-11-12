

from openmdao.main.api import Component
from openmdao.main.datatypes.api import Float


# Make sure that your class has some kind of docstring. Otherwise
# the descriptions for your variables won't show up in the
# source documentation.
class MyComponent(Component):
    """An example Component plugin class. """
    
    x = Float(0.0, iotype='in', desc='some input')
    y = Float(0.0, iotype='out', desc='x + 1')

    def execute(self):
        """y = x + 1"""
        self.y = self.x + 1.0
        
