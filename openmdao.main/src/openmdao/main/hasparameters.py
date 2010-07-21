
from enthought.traits.api import HasTraits

import ordereddict

from openmdao.lib.traits.float import Float
from openmdao.lib.traits.int import Int
from openmdao.lib.traits.enum import Enum
from openmdao.lib.traits.array import Array
from enthought.traits.api import implements

from openmdao.main.expreval import ExprEvaluator
from openmdao.main.interfaces import IHasParameters

class _Parameter(object): 
    
    def __init__(self, low=None, high=None, expr=None):
        self.low = low
        self.high = high
        self.expreval = expr

class HasParameters(object): 
    """This class provides an implementation of the IHasParameters interface"""

    def __init__(self, parent):
        self._parameters = ordereddict.OrderedDict()
        self._parent = parent

    def add_parameter(self, varname, low=None, high=None):
        """Adds a parameter to the driver. 
        
        varname : string
            name of the public variable the driver should vary during execution.
            
        low : float, optional
            minimum allowed value of the parameter
            
        high : float, optional
            maximum allowed value of the parameter
        
        If neither 'low' nor 'high' are specified, the min and max will
        default to the values in the metadata of the public variable being
        referenced. If they are not specified in the metadata and not provided
        as arguments a ValueError is raised.
        """
        if varname in self._parameters: 
            self._parent.raise_exception("Trying to add parameter '%s' to driver, "
                                         "but it's already there" % varname,
                                         AttributeError)
        
        parameter = _Parameter()
        parameter.expreval = ExprEvaluator(varname, self._parent.parent, single_name=True) 
        
        try:
            metadata = self._parent.parent.get_metadata(varname)
        except AttributeError:
            self._parent.raise_exception("Can't add parameter '%s' because it doesn't exist." % varname,
                                         AttributeError)
        
        meta_low = metadata.get('low') # this will be None if 'low' isn't there
        if low is None:
            parameter.low = meta_low
        else:  # low is not None
            if meta_low is not None and low < meta_low:
                self._parent.raise_exception("Trying to add parameter '%s', " 
                                             "but the lower limit supplied (%s) exceeds the " 
                                             "built-in lower limit (%s)." % 
                                             (varname, low, meta_low), ValueError)
            parameter.low = low

        meta_high = metadata.get('high') # this will be None if 'high' isn't there
        if high is None:
            parameter.high = meta_high
        else:  # high is not None
            if meta_high is not None and high > meta_high:
                self._parent.raise_exception("Trying to add parameter '%s', " 
                                             "but the upper limit supplied (%s) exceeds the " 
                                             "built-in upper limit (%s)." % 
                                             (varname, high, meta_high), ValueError)
            parameter.high = high
            
        if parameter.low is None: 
            self._parent.raise_exception("Trying to add parameter '%s', "
                                         "but no lower limit was found and no " 
                                         "'low' argument was given. One or the "
                                         "other must be specified." % varname,ValueError)
        if parameter.high is None: 
            self._parent.raise_exception("Trying to add parameter '%s', "
                                         "but no upper limit was found and no " 
                                         "'high' argument was given. One or the "
                                         "other must be specified." % varname,ValueError)
            
        # the parameter has been created, with expr and low/high. 
        # Just add it to the storage list
        self._parameters[varname] = parameter
            
    def remove_parameter(self, varname): 
        try:
            del self._parameters[varname]
        except KeyError:
            self._parent.raise_exception("Trying to remove parameter '%s' "
                                         "that is not in the driver." % varname,
                                         AttributeError)
    def list_parameters(self):
        return sorted(self._parameters.keys())
    
    def clear_parameters(self): 
        self._parameters = ordereddict.OrderedDict()
        
    def get_parameters(self):
        return self._parameters
                
    #def add_parameter(self, param_name, low=None, high=None):
        #if param_name in self._parameters:
            #self.raise_exception("Trying to add '%s' to driver as a parameter, but it is already in the driver."%param_name,KeyError)
        
        #expreval = ExprEvaluator(param_name, self._parent, single_name=True) 
        
        #if low is not None and high is not None: #use specified, overrides any trait defaults that would have been found
            #param_low = low, param_high = high
        #else: 
            
            ##split up the param_name string to be able to get the trait. 
            #path = ".".join(param_name.split(".")[0:-1]) #get the path to the object
            #target = param_name.split(".")[-1] #get the last part of the string after the last "."
            #obj = getattr(self.parent,path)
            
            #t = obj.trait(target) #get the trait
            #if t and t.is_trait_type(Enum):
                #param_low,param_high = None,None
            #elif t and (t.is_trait_type(Float) or t.is_trait_type(Int)): #can't be an Enum, so maybe it's a Float, Int
                
                #if hasattr(t,"low"): 
                    #param_low = t.low
                #elif low: 
                    #param_low = low
                #else: 
                    #self.raise_exception("No value was specified for the 'low' argument, "
                                         #"and no default was found in the public variable metadata",ValueError)
                
                #if hasattr(t,"high"):
                    #param_high = t.high
                #elif high: 
                    #param_high = high                
                #else: 
                    #self.raise_exception("No value was specified for the 'high' argument, "
                                         #"and no default was found in the public variable metadata",ValueError)
                
             
            #elif '[' in target: #can't figure out what the ranges should be
                #if not(isinstance(val,float) or isinstance(val,int) or isinstance(val,int32) or \
                       #isinstance(val,int64) or isinstance(val,float32) or isinstance(val,float64)
                      #):
                    #self.raise_exception("Only array values of type 'int' or 'float' are allowed as "
                                         #"parameters")
                    
                #self.raise_exception("values for 'high' and 'low' arguments are required when specifying "
                                     #"an Array element as a parameter. They were not given for '%s'"%param_name,TypeError)
            #else: 
                #self.raise_exception("Improper parameter type. Must be Float,Int, or an element of "
                                     #"an Array.",ValueError)
        
        
        #self._parameters[param_name] = {'expreval':expreval,'low':param_low,'high':param_high}
    
    #def remove_parameter(self,param_name):
        #try:
            #del self._parameters[param_name]
        #except KeyError:
            #self.raise_exception("Trying to remove parameter '%s', but it is not in the driver" %
                                 #param_name, KeyError)
    
    #def list_parameters(self):
        #return self._parameters.keys()
    
    #def clear_parameters(self):
        #self._parameters = ordereddict.OrderedDict()
        
    def set_parameters(self, X): 
        """Pushes the values in the X put to their corresponding public variables in the model
        
        X: iterator
            iterator of input values with an order defined to match the order of parameters returned 
            by the list_parameter method. X must support the len() function.
        """
        if len(X) != len(self._parameters):
            raise ValueError("number of input values (%s) != number of parameters (%s)" % 
                             (len(X),len(self._parameters)))

        for x, (param_name, param) in zip(X, self._parameters.iteritems()): 
            param.expreval.set(x)
            
    def iter_parameters(self):
        return self._parameters.iteritems()
            