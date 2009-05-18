#public symbols
__all__ = ["Wrapper"]

__version__ = "0.1"

from openmdao.main import Variable
from openmdao.main.variable import INPUT,OUTPUT

class Wrapper(Variable):
    """A Variable which wraps any arbitrary Python object, allowing access to its methods and attributes. 
    Can only be connected with other Wrapper instances which contain the exact same type as itself."""
    
    def validate_var(self, var):
        """Raise a TypeError if the connecting Variable is incompatible. This
        is called on the INPUT side of a connection."""
        #super(Wrapper, self).validate_var(var)

        if not isinstance(var.get_value(),type(self.get_value())):
            raise TypeError("%s contains an object of type: %s which is incompatible with the type contained in %s: %s"\
                            %(self.get_pathname(),type(self.get_value()),var.get_pathname(),type(var.get_value())))

