import ordereddict
import weakref

from numpy import float32, float64, int32, int64

from openmdao.main.expreval import ExprEvaluator
from openmdao.util.decorators import add_delegate

class Parameter(object): 
    
    def __init__(self, target, parent, high=None, low=None, 
                 scaler=None, adder=None, fd_step=None, scope=None):
        self._metadata = None
        
        if scaler is None and adder is None:
            self._transform = self._do_nothing
            self._untransform = self._do_nothing
        else:
            if scaler is None:
                scaler = 1.0
            else:
                try:
                    scaler = float(scaler)
                except TypeError:
                    parent.raise_exception("bad scaler", TypeError)
            if adder is None:
                adder = 0.0
            else:
                try:
                    adder = float(adder)
                except TypeError:
                    parent.raise_exception("bad adder", TypeError)
        
        self.low = low
        self.high = high
        self.scaler = scaler
        self.adder = adder
        self.fd_step = fd_step
        
        try:
            expreval = ExprEvaluator(target, scope)
        except Exception as err:
            parent.raise_exception("Can't add parameter: %s" % str(err),
                                   type(err))
        if not expreval.is_valid_assignee():
            parent.raise_exception("Can't add parameter: '%s' is not a valid parameter expression" % 
                                   expreval.text, ValueError)

        self._expreval = expreval
        
        try:
            # metadata is in the form [(varname, metadata)], so use [0][1] to get
            # the actual metadata dict (since we're a Parameter we'll only be
            # referencing one variable.
            metadata = self.get_metadata()[0][1]
        except AttributeError:
            parent.raise_exception("Can't add parameter '%s' because it doesn't exist." % target,
                                   AttributeError)
        try:
            self.vartypename = metadata['vartypename']
        except KeyError:
            self.vartypename = None
        try:
            val = expreval.evaluate()
        except Exception as err:
            parent.raise_exception("Can't add parameter because I can't evaluate '%s'." % target, 
                                   ValueError)
        if self.vartypename != 'Enum' and not isinstance(val,(float,float32,float64,int,int32,int64)):
            parent.raise_exception("The value of parameter '%s' must be of type float or int, but its type is '%s'." %
                                   (target,type(val).__name__), ValueError)
        
        self.valtypename = type(val).__name__

        meta_low = metadata.get('low') # this will be None if 'low' isn't there
        if low is None:
            self.low = meta_low
        else:
            if meta_low is not None and low < self._transform(meta_low):
                parent.raise_exception("Trying to add parameter '%s', " 
                                       "but the lower limit supplied (%s) exceeds the " 
                                       "built-in lower limit (%s)." % 
                                       (target, low, meta_low), ValueError)
            self.low = low

        meta_high = metadata.get('high') # this will be None if 'high' isn't there
        if high is None:
            self.high = meta_high
        else:  # high is not None
            if meta_high is not None and high > self._transform(meta_high):
                parent.raise_exception("Trying to add parameter '%s', " 
                                       "but the upper limit supplied (%s) exceeds the " 
                                       "built-in upper limit (%s)." % 
                                       (target, high, meta_high), ValueError)
            self.high = high
            
        if self.vartypename == 'Enum':
            return    # it's an Enum, so no need to set high or low
        else:
            if self.low is None:
                parent.raise_exception("Trying to add parameter '%s', "
                                       "but no lower limit was found and no " 
                                       "'low' argument was given. One or the "
                                       "other must be specified." % target,ValueError)
            if self.high is None: 
                parent.raise_exception("Trying to add parameter '%s', "
                                       "but no upper limit was found and no " 
                                       "'high' argument was given. One or the "
                                       "other must be specified." % target,ValueError)
        if low is None:
            self.low = self._transform(self.low)
        if high is None:
            self.high = self._transform(self.high)

        if self.low > self.high:
            parent.raise_exception("Parameter '%s' has a lower bound (%s) that exceeds its upper bound (%s)" %
                                   (target, self.low, self.high), ValueError)

    def __str__(self):
        return self._expreval.text

    def __repr__(self): 
        return '<Parameter(target=%s,low=%s,high=%s,fd_step=%s,scaler=%s,adder=%s)>' % \
               (self.target, self.low, self.high, self.fd_step, self.scaler, self.adder)
    
    def _transform(self, val):
        return (val+self.adder)*self.scaler
    
    def _untransform(self, val):
        return val/self.scaler - self.adder
    
    def _do_nothing(self, val):
        return val
    
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
        return self._untransform(self._expreval.evaluate(scope))
    
    def set(self, val, scope=None):
        """Assigns the given value to the variable referenced by this parameter."""
        self._expreval.set(self._transform(val), scope)

    def get_metadata(self, metaname=None):
        """Returns a list of tuples of the form (varname, metadata), with one
        entry for each variable referenced by the parameter expression. The
        metadata value found in the tuple with be either the specified piece
        of metadata if metaname is provided, or the whole metadata dictionary
        for that variable if it is not.
        """
        if self._metadata is None:
            self._metadata = self._expreval.get_metadata()
        if metaname is None:
            return self._metadata
        else:
            return [(name,self._metadata.get(metaname)) for name,val in self._metadata]

    def get_referenced_compnames(self):
        """Return a set of Component names based on the 
        pathnames of Variables referenced in our target string. 
        """
        return self._expreval.get_referenced_compnames()

    def get_referenced_varpaths(self):
        """Return a set of Variable names referenced in our target string."""
        return self._expreval.get_referenced_varpaths()

    
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
        self.scaler = self._params[0].scaler
        self.adder = self._params[0].adder
        self.fd_step = self._params[0].fd_step
            
    def __str__(self):
        return "%s" % self.targets

    def __repr__(self): 
        return '<ParameterGroup(targets=%s,low=%s,high=%s,fd_step=%s,scaler=%s,adder=%s)>' % \
               (self.targets, self.low, self.high, self.fd_step, self.scaler, self.adder)

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
            result.update(param.get_referenced_compnames())
        return result

    def get_referenced_varpaths(self):
        """Return a set of Variable names referenced in our target strings."""
        result = set()
        for param in self._params:
            result.update(param.get_referenced_varpaths())
        return result

class HasParameters(object): 
    """This class provides an implementation of the IHasParameters interface."""

    _do_not_promote = ['get_expr_depends', 'get_referenced_compnames', 
                       'get_referenced_varpaths', 'get_metadata']
    
    def __init__(self, parent):
        self._parameters = ordereddict.OrderedDict()
        self._parent = parent
        self._allowed_types = ['continuous']

    def add_parameter(self, target, low=None, high=None, 
                      scaler=None, adder=None, fd_step=None, name=None, scope=None):
        """Adds a parameter or group of parameters to the driver.
        
        target: string or iter of strings
            What the driver should vary during execution. A *target* is an expression
            that can reside on the left-hand side of an assignment statement, so 
            typically it will be the name of a variable or possibly a subscript 
            expression indicating an entry within an array variable, e.g., x[3].
            If an iterator of targets is given, then the driver will set all targets given
            to the same value whenever it varies this parameter during execution.
            
        low: float (optional)
            Minimum allowed value of the parameter. If scaler and/or adder
            are supplied, use the transformed value here.
            
        high: float (optional)
            Maximum allowed value of the parameter. If scaler and/or adder
            are supplied, use the transformed value here.
            
        scaler: float (optional)
            Value to multiply the possibly offset parameter value by 
            
        adder: float (optional)
            Value to add to parameter prior to possible scaling

        fd_step: float (optional)
            Step-size to use for finite difference calculation. If no value is
            given, the differentitator will use its own default
            
        name: str (optional)
            Name used to refer to the parameter in place of the name of the
            variable referred to in the parameter string.
            This is sometimes useful if, for example, multiple entries in the
            same array variable are declared as parameters.
            
        scope: object (optional)
            The object to be used as the scope when evaluating the expression.
        
        If neither "low" nor "high" is specified, the min and max will
        default to the values in the metadata of the variable being
        referenced. If they are not specified in the metadata and not provided
        as arguments, a ValueError is raised.
        """
        if isinstance(target, basestring): 
            names = [target]
            key = target
        else: 
            names = target
            key = tuple(target)

        if name is not None:
            key = name
            
        dups = set(self.list_param_targets()).intersection(names)
        if len(dups) == 1:
            self._parent.raise_exception("'%s' is already a Parameter target" % 
                                         dups.pop(), ValueError)
        elif len(dups) > 1:
            self._parent.raise_exception("%s are already Parameter targets" % 
                                         sorted(list(dups)), ValueError)
            
        parameters = [Parameter(name, self._parent, low=low, high=high, 
                                scaler=scaler, adder=adder, fd_step=fd_step,
                                scope=self._get_scope(scope)) 
                      for name in names]

        if key in self._parameters:
            self._parent.raise_exception("%s is already a Parameter" % key,
                                         ValueError)

        if len(parameters) == 1:
            self._parameters[key] = parameters[0]
        else: # defining a ParameterGroup
            types = set([p.valtypename for p in parameters])
            if len(types) > 1: 
                self._parent.raise_exception("Can't add parameter %s because "
                    "%s are not all of the same type" %
                    (key," and ".join(names)), ValueError)
            pg = ParameterGroup(parameters)
            pg.typename = parameters[0].valtypename
            self._parameters[key] = pg
        
    def remove_parameter(self, name):
        """Removes the parameter with the given name."""
        try:
            del self._parameters[name]
        except KeyError:
            self._parent.raise_exception("Trying to remove parameter '%s' "
                                         "that is not in this driver." % (name,),
                                         AttributeError)
    def list_param_targets(self):
        """Returns a list of parameter targets. Note that this
        list may contain more entries than the list of Parameter and
        ParameterGroup objects since ParameterGroups have multiple targets.
        """
        targets = []
        for param in self._parameters.values():
            targets.extend(param.targets)
        return targets
    
    def clear_parameters(self):
        """Removes all parameters."""
        self._parameters = ordereddict.OrderedDict()
        
    def get_parameters(self):
        """Returns an ordered dict of parameter objects."""
        return self._parameters

    def set_parameters(self, values, case=None, scope=None): 
        """Pushes the values in the iterator 'values' into the corresponding 
        variables in the model.  If the 'case' arg is supplied, the values
        will be set into the case and not into the model.
        
        values: iterator
            Iterator of input values with an order defined to match the 
            order of parameters returned by the get_parameters method. 
            'values' must support the len() function.
            
        case: Case (optional)
            If supplied, the values will be associated with their corresponding
            targets and added as inputs to the Case instead of being set directly
            into the model.
        """
        if len(values) != len(self._parameters):
            raise ValueError("number of input values (%s) != number of parameters (%s)" % 
                             (len(values),len(self._parameters)))

        if case is None:
            for val, param in zip(values, self._parameters.values()):
                param.set(val, self._get_scope(scope))
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
    
    def get_referenced_compnames(self):
        """Return a set of Component names based on the 
        pathnames of Variables referenced in our target strings. 
        """
        result = set()
        for param in self._parameters.values():
            result.update(param.get_referenced_compnames())
        return result

    def get_referenced_varpaths(self):
        """Return a set of Variable names referenced in our target strings.
        """
        result = set()
        for param in self._parameters.values():
            result.update(param.get_referenced_varpaths())
        return result
    
    def _get_scope(self, scope=None):
        if scope is None:
            try:
                return self._parent.get_expr_scope()
            except AttributeError:
                pass
        return scope

