import ordereddict
import itertools
from numpy import float32, float64, int32, int64

from openmdao.main.expreval import ExprEvaluator
from openmdao.util.decorators import add_delegate

class Parameter(object): 
    
    def __init__(self, expreval, high=None, low=None, fd_step=None):
        self.low = low
        self.high = high
        self.fd_step = fd_step
        if not expreval.is_valid_assignee():
            raise ValueError("'%s' is not a valid parameter expression" % expreval.text)
        self._expreval = expreval

    def __str__(self):
        return self._expreval.text

    def __repr__(self): 
        return '<Parameter(target=%s,low=%s,high=%s,fd_step=%s)>'%(self._expreval.text,
                                                                   self.low,
                                                                   self.high,
                                                                   self.fd_step)
    @property
    def target(self):
        """Returns the target of this parameter."""
        return self._expreval.text
    
    @property
    def targets(self):
        """Returns a one element list containing the target of this parameter."""
        return [self._expreval.text]
    
    def evaluate(self, scope=None):
        """Returns the value of this parameter."""
        return self._expreval.evaluate(scope)
    
    def set(self, val, scope=None):
        """Assigns the given value to the variable referenced by this parameter."""
        self._expreval.set(val, scope)

    def get_metadata(self, metaname=None):
        """Returns a list of tuples of the form (varname, metadata), with one
        entry for each variable referenced by the parameter expression. The
        metadata value found in the tuple with be either the specified piece
        of metadata if metaname is provided, or the whole metadata dictionary
        for that variable if it is not.
        """
        return self._expreval.get_metadata(metaname)

    def get_referenced_compnames(self):
        """Return a set of Component names based on the 
        pathnames of Variables referenced in our target string. 
        """
        return self._expreval.get_referenced_compnames()
    
    def denormalize(self, value):
        """Return a scaled version of the given value that lies between
        our low and high values.  The value is assumed to be between
        0. and 1.
        """
        return self.low+(self.high-self.low)*value

class ParameterGroup(object):
    """A group of Parameters that are treated as one, i.e., they are all
    set to the same value.
    """
    
    def __init__(self, params):
        for param in params:
            # prevent multiply nested ParameterGroups
            if not isinstance(param, Parameter):
                raise ValueError("tried to add a non-Parameter object to a ParameterGroup")
        self._params = params[:]
        self.low = self._params[0].low
        self.high = self._params[0].high
        self.fd_step = self._params[0].fd_step
            
    @property
    def target(self): 
        return self._params[0].target
    
    @property
    def targets(self): 
        return [p.target for p in self._params]
    
    def set(self, value, scope=None):
        """Set all targets to the given value."""
        for p in self._params: 
            p.set(value, scope)
            
    def evaluate(self, scope=None):
        """Return the value of the first parameter in our target list. Values
        of all of our targets are assumed to be the same.
        """
        return self._params[0].evaluate(scope)

    def get_metadata(self, metaname=None):
        """Returns a list of tuples of the form (varname, metadata), with one
        entry for each variable referenced by a target expression. The
        metadata value found in the tuple with be either the specified piece
        of metadata if metaname is provided, or the whole metadata dictionary
        for that variable if it is not.
        """
        dct = {}
        for param in self._params:
            tup = param.get_metadata(metaname)
            dct.setdefault(tup[0], tup[1])
        return dct.items()

    def get_referenced_compnames(self):
        """Return a set of Component names based on the 
        pathnames of Variables referenced in our target strings. 
        """
        result = set()
        for param in self._params:
            result.union(param.get_referenced_compnames())
        return result
    
class HasParameters(object): 
    """This class provides an implementation of the IHasParameters interface."""

    _do_not_promote = ['get_expr_depends']
    
    def __init__(self, parent):
        self._parameters = ordereddict.OrderedDict()
        self._parent = parent

    def add_parameter(self, name, low=None, high=None, fd_step=None):
        """Adds a parameter or group of parameters to the driver.
        
        name: string or iter of strings
            Name of the variable(s) the driver should vary during execution.
            If an iterator of names is given, then the driver will set all names given
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
        if isinstance(name, str): 
            names = [name]
            key = name
        else: 
            names = name
            key = tuple(name)

        all_names = set(self.list_param_targets())
        for name in names: 
            if name in all_names: 
                self._parent.raise_exception("'%s' is already the target of a Parameter" % name,
                                             ValueError)
        parameters = []
        vals = []
        for name in names:
            try:
                parameter = Parameter(ExprEvaluator(name, self._parent),
                                      low=low, high=high, fd_step=fd_step)
            except Exception as err:
                self._parent.raise_exception("Can't add parameter: %s" % str(err),
                                             err.__class__)
            try:
                # metadata is in the form [(varname, metadata)], so use [0][1] to get
                # the actual metadata dict
                metadata = parameter.get_metadata()[0][1]
            except AttributeError:
                self._parent.raise_exception("Can't add parameter '%s' because it doesn't exist." % name,
                                             AttributeError)
            try:
                val = parameter.evaluate()
                vals.append(val)
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
        
        types = set([type(val) for val in vals])
        if len(types) > 1: 
            self._parent.raise_exception("Can not add parameter %s because "
                             "%s are not all of the same type"%(key," and ".join(names)))
            
        if len(parameters) == 1: #just one in there
            self._parameters[key] = parameters[0]
        else: 
            self._parameters[key] = ParameterGroup(parameters)
        
            
    def remove_parameter(self, name):
        """Removes the parameter with the given name."""
            
        try:
            del self._parameters[name]
        except KeyError:
            self._parent.raise_exception("Trying to remove parameter '%s' "
                                         "that is not in this driver." % (name,),
                                         AttributeError)
    def list_param_targets(self):
        """Returns an alphabetized list of parameter targets. Note that this
        list may contain more entries than the list of Parameter and
        ParameterGroup objects since ParameterGroups have multiple targets.
        """
        targets = []
        for param in self._parameters.values():
            targets.extend(param.targets)
        return sorted(targets)
    
    def clear_parameters(self):
        """Removes all parameters."""
        self._parameters = ordereddict.OrderedDict()
        
    def get_parameters(self):
        """Returns an ordered dict of parameter objects."""
        return self._parameters

    def set_parameters(self, values, case=None, normalized=False): 
        """Pushes the values in the iterator 'values' into the corresponding 
        variables in the model.  If the 'case' arg is supplied, the values
        will be set into the case and not into the model.
        
        values: iterator
            Iterator of input values with an order defined to match the 
            order of parameters returned by the list_parameter method. 
            'values' must support the len() function.
            
        case: Case (optional)
            If supplied, the values will be associated with their corresponding
            targets and added as inputs to the Case instead of being set directly
            into the model.
            
        normalized: bool (optional)
            If True, the given values will be assumed to be between 0. and 1. and
            will be scaled to lie between the low and high values of the corresponding
            parameter.  Defaults to False.
        """
        if len(values) != len(self._parameters):
            raise ValueError("number of input values (%s) != number of parameters (%s)" % 
                             (len(values),len(self._parameters)))

        if normalized:
            values = [p.denormalize(v) for v,p in zip(values, self._parameters.values())]

        if case is None:
            for val, param in zip(values, self._parameters.values()):
                param.set(val)
        else:
            for val, parameter in zip(values, self._parameters.values()):
                for target in parameter.targets:
                    case.add_input(target, val)
            return case


    def get_expr_depends(self):
        """Returns a list of tuples of the form (src_comp_name, dest_comp_name)
        for each dependency introduced by a parameter.
        """
        conn_list = []
        pname = self._parent.name
        for name,param in self._parameters.items():
            for cname in param.get_referenced_compnames():
                conn_list.append((pname, cname))
        return conn_list
    
