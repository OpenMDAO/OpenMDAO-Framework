
from enthought.traits.api import HasTraits

import ordereddict

from openmdao.lib.traits.float import Float
from openmdao.lib.traits.int import Int
from openmdao.lib.traits.enum import Enum
from openmdao.lib.traits.array import Array
from enthought.traits.api import implements

from openmdao.main.expreval import ExprEvaluator
from openmdao.main.interfaces import IHasParameters


class HasParameters(HasTraits): 
    """This class provides an implementation of the IHasParameters interface"""

    implements(IHasParameters)

    def __init__(self):
        super(HasParameters, self).__init__()
        self._parameters = ordereddict.OrderedDict()
    
    def add_parameter(self, param_name, low=None, high=None):
        if param_name in self._parameters:
            self.raise_exception("Trying to add '%s' to driver as a parameter, but it is already in the driver."%param_name,KeyError)
        
        expreval = ExprEvaluator(param_name, self.parent, single_name=True) 
        
        if low is not None and high is not None: #use specified, overrides any trait defaults that would have been found
            param_low = low, param_high = high
        else: 
            
            #split up the param_name string to be able to get the trait. 
            path = ".".join(param_name.split(".")[0:-1]) #get the path to the object
            target = param_name.split(".")[-1] #get the last part of the string after the last "."
            obj = getattr(self.parent,path)
            
            t = obj.trait(target) #get the trait
            if t and t.is_trait_type(Enum):
                param_low,param_high = None,None
            elif t and (t.is_trait_type(Float) or t.is_trait_type(Int)): #can't be an Enum, so maybe it's a Float, Int
                
                if hasattr(t,"low"): 
                    param_low = t.low
                elif low: 
                    param_low = low
                else: 
                    self.raise_exception("No value was specified for the 'low' argument, "
                                         "and no default was found in the public variable metadata",ValueError)
                
                if hasattr(t,"high"):
                    param_high = t.high
                elif high: 
                    param_high = high                
                else: 
                    self.raise_exception("No value was specified for the 'high' argument, "
                                         "and no default was found in the public variable metadata",ValueError)
                
             
            elif array_test.search(target): #can't figure out what the ranges should be
                if not(isinstance(val,float) or isinstance(val,int) or isinstance(val,int32) or \
                       isinstance(val,int64) or isinstance(val,float32) or isinstance(val,float64)
                      ):
                    self.raise_exception("Only array values of type 'int' or 'float' are allowed as "
                                         "parameters")
                    
                self.raise_exception("values for 'high' and 'low' arguments are required when specifying "
                                     "an Array element as a parameter. They were not given for '%s'"%param_name,TypeError)
            else: 
                self.raise_exception("Improper parameter type. Must be Float,Int, or an element of "
                                     "an Array.",ValueError)
        
        
        self._parameters[param_name] = {'expreval':expreval,'low':param_low,'high':param_high}
    
    def remove_parameter(self,param_name):
        try:
            del self._parameters[param_name]
        except KeyError:
            self.raise_exception("Trying to remove parameter '%s', but it is not in the driver" %
                                 param_name, KeyError)
    
    def list_parameters(self):
        return self._parameters.keys()
    
    def clear_parameters(self):
        self._parameters = ordereddict.OrderedDict()
        
    def set_parameters(self, X): 
        """Pushes the values in the X put to their corresponding public variables in the model
        
        X: iterator
            iterator of input values with an order defined to match the order of parameters returned 
            by the list_parameter method. X must support the len() function.
        """
        if len(X) != len(self._parameters):
            raise ValueError("number of input values (%s) != number of parameters (%s)" % 
                             (len(X),len(self._parameters)))
        for x, param_name, param in zip(X, self._parameters.iteritems()): 
            param['expreval'].set(x)
            
    def iter_parameters(self):
        return self._parameters.iteritems()
            