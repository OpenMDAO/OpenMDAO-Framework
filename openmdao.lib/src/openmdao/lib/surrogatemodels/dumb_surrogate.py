
import zope.interface

from openmdao.main.interfaces import implements, ISurrogate
from openmdao.main.uncertain_distributions import NormalDistribution

class DumbSurrogate(object): 
    implements(ISurrogate)
    
    def __init__(self,X=None,Y=None):
        pass
            
    def get_uncertain_value(self, value): 
        """Returns a NormalDistribution centered around the value, with a 
        standard deviation of 0."""
        return NormalDistribution(value, 0.)

    def predict(self, new_x):
        """Calculates a predicted value of the response based on the current
        trained model for the supplied list of inputs.
        """
        #return NormalDistribution(f, RMSE)
        pass
        

    def train(self,X,Y):
        """Train the surrogate model with the given set of inputs and outputs."""
        pass