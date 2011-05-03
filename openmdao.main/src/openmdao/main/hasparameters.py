import ordereddict
import itertools
from numpy import float32, float64, int32, int64

from openmdao.main.expreval import ExprEvaluator

class Parameter(object): 
    
    def __init__(self, low=None, high=None, expr=None, fd_step=None):
        self.low = low
        self.high = high
        self.expreval = expr
        self.fd_step = fd_step
        
    def __repr__(self): 
        return '<Parameter(low=%s,high=%s,expr=%s,fd_step=%s)>'%(self.low,self.high,self.expreval.text,self.fd_step)
    

class HasParameters(object): 
    """This class provides an implementation of the IHasParameters interface."""

    _do_not_promote = ['get_expr_depends']
    
    def __init__(self, parent):
        self._parameters = ordereddict.OrderedDict()
        self._parent = parent

    def add_parameters(self, param_iter):
        """Takes an iterator of tuples of the form (param_name, low, high,fd_step)
        and adds the parameters to the driver.
        """
        if len(param_iter[0]) ==3: 
            for name, low, high in param_iter:
                self._parent.add_parameter(name, low=low, high=high)
                
        if len(param_iter[0]) ==4: 
            for name, low, high, fd_step in param_iter:
                self._parent.add_parameter(name, low=low, high=high, fd_step=fd_step)        
            
    def add_parameter(self, name, low=None, high=None, fd_step=None):
        """Adds a parameter to the driver. 
        
        name: string or tuple of strings
            Name of the variable the driver should vary during execution.
            If a tuple of names is given, then the driver will set all names given
            to the same value whenever it varies this parameter during execution
            
        low: float (optional)
            Minimum allowed value of the parameter.
            
        high: float (optional)
            Maximum allowed value of the parameter.
            
        fd_step: float (optional)
            Step-size to use for finite difference calculation. If no value is
            given, the differentitator will use its own default
        
        If neither "low" nor "high" is specified, the min and max will
        default to the values in the metadata of the variable being
        referenced. If they are not specified in the metadata and not provided
        as arguments, a ValueError is raised.
        """
        orig_names = name
        if isinstance(name,str): 
            names = frozenset([name])
        else: 
            names = frozenset(name)
            

        if orig_names in self._parameters: 
                self._parent.raise_exception("Trying to add parameter '%s' to driver, "
                                             "but it's already there" % (orig_names,),
                                             AttributeError)
        flat_names = list(itertools.chain(self.list_parameters()))
        for name in names: 
            if name in flat_names: 
                self._parent.raise_exception("Trying to add group of parameters '%s' to driver, "
                                             "but one of them is already there" % (orig_names,),
                                             AttributeError)
        parameters = []        
        for name in names: 
            parameter = Parameter()
            parameter.expreval = ExprEvaluator(name, self._parent)
        
            if not parameter.expreval.is_valid_assignee():
                self._parent.raise_exception("Can't add parameter '%s' because it refers to multiple objects." % name,
                                             ValueError)
        
            
            try:
                var_name = list(parameter.expreval.get_referenced_varpaths())[0]
                metadata = self._parent.parent.get_metadata(var_name)
            except AttributeError:
                self._parent.raise_exception("Can't add parameter '%s' because it doesn't exist." % name,
                                             AttributeError)
            try:
                val = parameter.expreval.evaluate()
            except Exception as err:
                self._parent.raise_exception("Can't add parameter because I can't evaluate '%s'." % name, 
                                             ValueError)
            if not isinstance(val,(float,float32,float64,int,int32,int64)):
                self._parent.raise_exception("The value of parameter '%s' must be of type float or int, but its type is '%s'." %
                                             (name,type(val).__name__), ValueError)
            
            meta_low = metadata.get('low') # this will be None if 'low' isn't there
            if low is None:
                parameter.low = meta_low
            else:  # low is not None
                if meta_low is not None and low < meta_low:
                    self._parent.raise_exception("Trying to add parameter '%s', " 
                                                 "but the lower limit supplied (%s) exceeds the " 
                                                 "built-in lower limit (%s)." % 
                                                 (name, low, meta_low), ValueError)
                parameter.low = low
    
            meta_high = metadata.get('high') # this will be None if 'high' isn't there
            if high is None:
                parameter.high = meta_high
            else:  # high is not None
                if meta_high is not None and high > meta_high:
                    self._parent.raise_exception("Trying to add parameter '%s', " 
                                                 "but the upper limit supplied (%s) exceeds the " 
                                                 "built-in upper limit (%s)." % 
                                                 (name, high, meta_high), ValueError)
                parameter.high = high
                
            values = metadata.get('values')
            if values is not None and len(values)>0:
                pass    # assume it's an Enum, so no need to set high or low
            else:
                if parameter.low is None:
                    self._parent.raise_exception("Trying to add parameter '%s', "
                                                 "but no lower limit was found and no " 
                                                 "'low' argument was given. One or the "
                                                 "other must be specified." % name,ValueError)
                if parameter.high is None: 
                    self._parent.raise_exception("Trying to add parameter '%s', "
                                                 "but no upper limit was found and no " 
                                                 "'high' argument was given. One or the "
                                                 "other must be specified." % name,ValueError)
                    
            if parameter.low > parameter.high:
                self._parent.raise_exception("Parameter '%s' has a lower bound (%s) that exceeds its upper bound (%s)" %
                                             (name, parameter.low, parameter.high), ValueError)
    
            parameter.fd_step = fd_step
            parameters.append(parameter)
        types = set([type(param.expreval.evaluate()) for param in parameters])
        if len(types) > 1: 
            self._parent.raise_exception("Can not add parameter %s because "
                             "%s are not the same type"%(tuple(orig_names)," and ".join(orig_names)))
            
        if len(parameters) == 1: #just one in there       
            self._parameters[orig_names] = parameters[0]
        else: 
            self._parameters[orig_names] = parameters
        
            
    def remove_parameter(self, name):
        """Removes the parameter with the given name."""
            
        try:
            del self._parameters[name]
        except KeyError:
            self._parent.raise_exception("Trying to remove parameter '%s' "
                                         "that is not in this driver." % (name,),
                                         AttributeError)
    def list_parameters(self):
        """Returns an alphabetized list of parameter names."""
        return sorted(self._parameters.keys())
    
    def clear_parameters(self):
        """Removes all parameters."""
        self._parameters = ordereddict.OrderedDict()
        
    def get_parameters(self):
        """Returns an ordered dict of parameter objects."""
        return self._parameters

    def set_parameters(self, values): 
        """Pushes the values in the iterator 'values' into the corresponding 
        variables in the model.
        
        values: iterator
            Iterator of input values with an order defined to match the order of parameters returned 
            by the list_parameter method. 'values' must support the len() function.
        """
        if len(values) != len(self._parameters):
            raise ValueError("number of input values (%s) != number of parameters (%s)" % 
                             (len(values),len(self._parameters)))

        for val, param in zip(values, self._parameters.values()):
            #if (param.low is not None and val < param.low) or (param.high is not None and val > param.high):
                #raise ValueError("parameter value (%s) is outside of allowed range [%s to %s]" %
                                 #(val, param.low, param.high))

            try:
                param.expreval.set(val)
            except AttributeError:    #broadcast parameters 
                for p in param: 
                    p.expreval.set(val)        

    def get_expr_depends(self):
        """Returns a list of tuples of the form (src_comp_name, dest_comp_name)
        for each dependency introduced by a parameter.
        """
        conn_list = []
        pname = self._parent.name
        for name,param in self._parameters.items():
            for cname in param.expreval.get_referenced_compnames():
                conn_list.append((pname, cname))
        return conn_list
    
