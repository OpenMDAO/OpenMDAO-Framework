import numpy as np

from openmdao.main.api import Component
from openmdao.main.datatypes.api import Float, Array


class LinearDistribution(Component): 
    """Takes two Float inputs and provides n Float outputs with a linear 
    variation between them. Units can be optionally provided. If use_array is 
    True (default), then the output is an array. Otherwise, the output will 
    be a set of separate variables""" 

    def __init__(self, n=3, units=None, use_array=True): 
        super(LinearDistribution, self).__init__()
        
        self._n = n

        self.add('offset', Float(0.0, iotype="in", 
                    desc="offset applied to the linear distribution outputs", units=units))
        self.add('start', Float(iotype='in', 
            desc="input closest to the hub", units=units))
        self.add('end', Float(iotype='in', 
            desc="input closest to the tip", units=units))

        self.add('delta', Float(iotype='out', 
            desc='step size for each of the %d levels'%n, units=units))

        if use_array: 
            self.add('output', Array(iotype='out', 
                desc='linearly spaced values from start to end inclusive of the bounds', 
                default_value=np.ones(n), shape=(n,), 
                dtype=Float, units=units))
        else: 
            for i in range(0, n): 
                self.add('output_%d'%i, Float(1, iotype="out", desc="linearaly spaced output %d"%i, units=units))    
        
    def execute(self): 
        
        out = np.linspace(self.start, self.end, self._n) + self.offset
        self.output = out
        self.delta = out[1]-out[0]
