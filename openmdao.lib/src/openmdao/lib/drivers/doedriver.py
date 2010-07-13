from __future__ import division

import random

from numpy import array,zeros,size,argsort,unique,sum,floor,equal,bincount,sqrt,diff
from numpy.linalg import norm

from enthought.traits.api import HasTraits, Event, implements, ListStr, Instance

from openmdao.main.api import Case
from openmdao.main.interfaces import IDOEgenerator
from openmdao.lib.api import Float,Int, Enum
from openmdao.lib.drivers.caseiterdriver import CaseIterDriverBase
from openmdao.lib.doegenerators.optlh import OptLatinHypercube

import time


class _Parameter(object): 
    
    def __init__(self, name):
        self.name = name
        self.low = None
        self.high = None


class DOEdriver(CaseIterDriverBase): 
    
    def __init__(self, *args, **kwargs):
        super(DOEdriver, self).__init__(*args, **kwargs)
        self._parameters = []  # need parameter ordering to map to DOE values, so no dict here
        self._event_vars = []
    
    DOEgenerator = Instance(IDOEgenerator, desc='Iterator supplying normalized DOE values', 
                            required=True)
    
    case_outputs = ListStr([], iotype='in', 
                           desc='A list of outputs to be saved after each case is run')
    
    def get_case_iterator(self):
        """Returns a new iterator over the Case set"""
        return self._get_cases()

    def add_event_var(self, varname):
        """Adds an event variable to the driver, which the driver will the set
        before each iteration. 
                
        varname : string
            name of the public event variable that should be set before execution
        """
        
        if varname in self._event_vars: 
            self.raise_exception("Trying to add event_var '%s' to driver, " % varname,
                                 "but it is already in there", RuntimeError)
        
        try:
            if not self.parent.get_metadata(varname, 'type') == 'event':
                self.raise_exception("'%s', is not an Event variable." % varname, RuntimeError)
        except AttributeError:
            self.raise_exception("Can't add event '%s' because it doesn't exist" % varname,
                                 AttributeError)
        self._event_vars.append(varname)
        

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
        if varname in [x.name for x in self._parameters]: 
            self.raise_exception("Trying to add parameter '%s' to driver, "
                                 "but it's already there" % varname,
                                 RuntimeError)
        
        parameter = _Parameter(varname)
        
        try:
            metadata = self.parent.get_metadata(varname)
        except AttributeError:
            self.raise_exception("Can't add parameter '%s' because it doesn't exist." % varname,
                                 AttributeError)
        
        meta_low = metadata.get('low') # this will be None if 'low' isn't there
        if low is None:
            parameter.low = meta_low
        else:  # low is not None
            if meta_low is not None and low < meta_low:
                self.raise_exception("Trying to add parameter '%s', " 
                                     "but the lower limit supplied (%s) exceeds the " 
                                     "built-in lower limit (%s)." % 
                                     (varname, low, meta_low), RuntimeError)
            parameter.low = low

        meta_high = metadata.get('high') # this will be None if 'high' isn't there
        if high is None:
            parameter.high = meta_high
        else:  # high is not None
            if meta_high is not None and high > meta_high:
                self.raise_exception("Trying to add parameter '%s', " 
                                     "but the upper limit supplied (%s) exceeds the " 
                                     "built-in upper limit (%s)." % 
                                     (varname, high, meta_high), RuntimeError)
            parameter.high = high
            
        if parameter.low is None: 
            self.raise_exception("Trying to add parameter '%s', "
                                 "but no lower limit was found and no " 
                                 "'low' argument was given. One or the "
                                 "other must be specified." % varname,ValueError)
        if parameter.high is None: 
            self.raise_exception("Trying to add parameter '%s', "
                                 "but no upper limit was found and no " 
                                 "'high' argument was given. One or the "
                                 "other must be specified." % varname,ValueError)
            
        # the parameter has been created, with expr and low/high. 
        # Just add it to the storage list
        self._parameters.append(parameter)
            
    def remove_event_var(self, varname):
        try:
            self._event_vars.remove(varname)
        except ValueError:
            self.raise_exception("Trying to remove event variable '%s' "
                                 "that is not in the driver." % varname, 
                                 ValueError)
        
    def remove_parameter(self, varname): 
        for param in self._parameters:
            if param.name == varname:
                self._parameters.remove(param)
                break
        else:
            self.raise_exception("Trying to remove parameter '%s' "
                                 "that is not in the driver." % varname,
                                 RuntimeError)
    
    def list_event_vars(self): 
        return sorted(self._event_vars)
            
    def list_parameters(self):
        return sorted([x.name for x in self._parameters])
    
    def clear_event_vars(self): 
        self._event_vars = []
        
    def clear_parameters(self): 
        self._parameters = []
    
    def _get_cases(self):
        if self.DOEgenerator.num_design_vars != len(self._parameters):
            self.raise_exception("number of DOE values (%s) != number of parameters (%s)"%
                                 (self.DOEgenerator.num_design_vars,len(self._parameters)),
                                 ValueError)
        for row in self.DOEgenerator:
            inputs = []
            for val, parameter in zip(row, self._parameters):
                #convert DOE values to variable values
                value = parameter.low+(parameter.high-parameter.low)*val
                if '[' in parameter.name:
                    raise RuntimeError("array entry design vars not supported yet")
                else:
                    inputs.append((parameter.name, None, value))
            
            # now add any event variables
            for varname in self._event_vars:
                inputs.append((varname, None, True))

            outputs = [(x,None,None) for x in self.case_outputs]
            yield Case(inputs=inputs, outputs=outputs)
            
