from math import cos, pi

from openmdao.main.api import Component
from openmdao.lib.api import Float
from openmdao.main.uncertain_distributions import NormalDistribution
from openmdao.examples.singleEI.branin_component import BraninComponent
from enthought.traits.api import HasTraits,implements
from openmdao.main.interfaces import ISurrogate


class NoisyBraninComponent(HasTraits): 
    implements(ISurrogate)  

    def __init__(self,X=None,Y=None):
        super(NoisyBraninComponent, self).__init__()
    
    def train(self):
        pass
        
    def predict(self,new_x):
        x = new_x[0]
        y = new_x[1]
        f_xy = (y-(5.1/(4.*pi**2.))*x**2.+5.*x/pi-6.)**2.+10.*(1.-1./(8.*pi))*cos(x)+10.
        return NormalDistribution(f_xy,.1)
     
