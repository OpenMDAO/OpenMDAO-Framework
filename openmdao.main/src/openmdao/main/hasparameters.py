import ordereddict

from openmdao.main.expreval import ExprEvaluator
from openmdao.main.pseudocomp import ParamPseudoComponent
from openmdao.util.typegroups import real_types, int_types

__missing = object()

class Parameter(object):

    def __init__(self, target, high=None, low=None,
                 scaler=None, adder=None, start=None,
                 fd_step=None, scope=None, name=None):

        if scaler is None and adder is None:
            self._transform = self._do_nothing
            self._untransform = self._do_nothing
            scaler = 1.0
            adder = 0.0
        else:
            if scaler is None:
                scaler = 1.0
            else:
                try:
                    scaler = float(scaler)
                except (TypeError, ValueError):
                    raise ValueError("Bad value given for parameter's 'scaler' attribute.")
            if adder is None:
                adder = 0.0
            else:
                try:
                    adder = float(adder)
                except (TypeError, ValueError):
                    raise ValueError("Bad value given for parameter's 'adder' attribute.")

        self._metadata = None
        self.pcomp_name = None

        self.low = low
        self.high = high
        self.scaler = scaler
        self.adder = adder
        self.start = start
        self.fd_step = fd_step

        if name is not None:
            self.name = name
        else:
            self.name = target

        try:
            expreval = ExprEvaluator(target, scope)
        except Exception as err:
            raise err.__class__("Can't add parameter: %s" % str(err))
        if not expreval.is_valid_assignee():
            raise ValueError("Can't add parameter: '%s' is not a valid parameter expression" %
                             expreval.text)

        self._expreval = expreval

        try:
            # metadata is in the form (varname, metadata), so use [1] to get
            # the actual metadata dict
            metadata = self.get_metadata()[1]
        except AttributeError:
            raise AttributeError("Can't add parameter '%s' because it doesn't exist." % target)

        if 'iotype' in metadata and metadata['iotype'] == 'out':
            raise RuntimeError("Can't add parameter '%s' because '%s' is an output." % (target, target))
        try:
            # So, our traits might not have a vartypename?
            self.vartypename = metadata['vartypename']
        except KeyError:
            self.vartypename = None

        try:
            val = expreval.evaluate()
        except Exception as err:
            raise ValueError("Can't add parameter because I can't evaluate '%s'." % target)

        self.valtypename = type(val).__name__

        if self.vartypename == 'Enum':
            return    # it's an Enum, so no need to set high or low

        if not isinstance(val, real_types) and not isinstance(val, int_types):
            raise ValueError("The value of parameter '%s' must be a real or integral type, but its type is '%s'." %
                                   (target, type(val).__name__))

        meta_low = metadata.get('low')  # this will be None if 'low' isn't there
        if meta_low is not None:
            if low is None:
                self.low = self._untransform(meta_low)
            elif low < self._untransform(meta_low):
                raise ValueError("Trying to add parameter '%s', "
                                       "but the lower limit supplied (%s) exceeds the "
                                       "built-in lower limit (%s)." %
                                       (target, low, meta_low))
        else:
            if low is None:
                raise ValueError("Trying to add parameter '%s', "
                                       "but no lower limit was found and no "
                                       "'low' argument was given. One or the "
                                       "other must be specified." % target)

        meta_high = metadata.get('high')  # this will be None if 'low' isn't there
        if meta_high is not None:
            if high is None:
                self.high = self._untransform(meta_high)
            elif high > self._untransform(meta_high):
                raise ValueError("Trying to add parameter '%s', "
                                       "but the upper limit supplied (%s) exceeds the "
                                       "built-in upper limit (%s)." %
                                       (target, high, meta_high))
        else:
            if high is None:
                raise ValueError("Trying to add parameter '%s', "
                                   "but no upper limit was found and no "
                                   "'high' argument was given. One or the "
                                   "other must be specified." % target)

        if self.low > self.high:
            raise ValueError("Parameter '%s' has a lower bound (%s) that exceeds its upper bound (%s)" %
                                   (target, self.low, self.high))

    def initialize(self, scope):
        if self.start is None:
            self.set(self._untransform(self._expreval.evaluate(scope)))
        else:
            self.set(self._untransform(self.start), scope)

    def activate(self, scope, workflow=None):
        """Make this parameter active by creating the appropriate
        connections in the dependency graph.  This should NOT be called
        on parameters that are part of a ParameterGroup.
        """
        if self.pcomp_name is None:
            pseudo = ParamPseudoComponent(self)
            self.pcomp_name = pseudo.name
            scope.add(pseudo.name, pseudo)
        else:
            pseudo = getattr(scope, self.pcomp_name)

        pseudo.make_connections(workflow)

        self.initialize(scope)


    def deactivate(self, scope, workflow=None):
        """Make this parameter inactive by disconnecting it in the
        dependency graph and removing its callback from the target
        component.
        """
        if self.pcomp_name is None:
            return
        else:
            pseudo = getattr(scope, self.pcomp_name)
            pseudo.remove_connections(workflow)
            scope.remove(self.pcomp_name)
            self.pcomp_name = None


    def __eq__(self, other):
        if not isinstance(other, Parameter):
            return False
        return (self._expreval,self.scaler,self.adder,self.low,self.high,self.fd_step,self.start,self.name)== \
               (other._expreval,other.scaler,other.adder,other.low,other.high,other.fd_step,other.start,self.name)

    def __str__(self):
        return self._expreval.text

    def __repr__(self):
        return '<Parameter(target=%s,low=%s,high=%s,fd_step=%s,scaler=%s,adder=%s,start=%s,name=%s)>' % \
               self.get_config()

    def _transform(self, val):
        """ Unscales the variable (parameter space -> var space). """
        return (val + self.adder) * self.scaler

    def _untransform(self, val):
        """ Scales the variable (var space -> parameter space). """
        return val / self.scaler - self.adder

    def _do_nothing(self, val):
        return val

    def _get_scope(self):
        return self._expreval.scope

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
        #return self._untransform(self._expreval.evaluate(scope))
        if scope is None:
            scope = self._get_scope()
        pcomp = getattr(scope, self.pcomp_name)
        return pcomp.in0

    def set(self, val, scope=None):
        """Assigns the given value to the variable referenced by this parameter."""
        #self._expreval.set(self._transform(val), scope)
        if scope is None:
            scope = self._get_scope()
        pcomp = getattr(scope, self.pcomp_name)
        pcomp.set('in0', val)
        
        pcomp.run()
        self._expreval.set(pcomp.out0, src='.'.join([self.pcomp_name, 'out0']))

    def get_metadata(self, metaname=None):
        """Returns a list of tuples of the form (varname, metadata), with one
        entry for each variable referenced by the parameter expression. The
        metadata value found in the tuple will be either the specified piece
        of metadata, if metaname is provided, or the whole metadata dictionary
        for that variable if it is not.
        """
        if self._metadata is None:
            self._metadata = self._expreval.get_metadata()
        if metaname is None:
            return self._metadata[0]
        else:
            return [(name, self._metadata.get(metaname)) for name, val in self._metadata]

    def get_referenced_compnames(self):
        """Return a set of Component names based on the
        pathnames of Variables referenced in our target string.
        """
        return self._expreval.get_referenced_compnames()

    def get_referenced_varpaths(self):
        """Return a set of Variable names referenced in our target string."""
        return self._expreval.get_referenced_varpaths(copy=False)

    def copy(self):
        """Return a copy of this Parameter."""
        return Parameter(self._expreval.text, 
                         high=self.high, low=self.low,
                         scaler=self.scaler, adder=self.adder, 
                         start=self.start,
                         fd_step=self.fd_step, 
                         scope=self._get_scope(), name=self.name)

    def get_config(self):
        return (self.target, self.low, self.high, self.fd_step,
                self.scaler, self.adder, self.start, self.name)

    def override(self, low=None, high=None, 
                 scaler=None, adder=None, start=None,
                 fd_step=None, name=None):
        if low is not None: 
            self.low = low
        if high is not None: 
            self.high = high
        if scaler is not None: 
            self.scaler = scaler
        if start is not None: 
            self.start = start
        if fd_step is not None: 
            self.fd_step = fd_step
        if name is not None: 
            self.name = name


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
        param0 = self._params[0]

        self.low = max([x.low for x in self._params])
        self.high = min([x.high for x in self._params])
        self.start = param0.start
        self.scaler = param0.scaler
        self.adder = param0.adder
        self.fd_step = param0.fd_step
        self.name = param0.name
        self.pcomp_name = None
        self.typename = param0.valtypename

    def __eq__(self, other):
        if not isinstance(other,ParameterGroup):
            return False
        return (self._params,self.low,self.high,self.start,self.scaler,self.adder,self.fd_step,self.name)==\
               (other._params,other.low,other.high,other.start,other.scaler,other.adder,other.fd_step,self.name)

    def __str__(self):
        return "%s" % self.targets

    def __repr__(self): 
        return '<ParameterGroup(targets=%s,low=%s,high=%s,fd_step=%s,scaler=%s,adder=%s,start=%s,name=%s)>' % \
               (self.targets, self.low, self.high,self.fd_step, self.scaler, self.adder,self.start,self.name)

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
        metadata value found in the tuple will be either the specified piece
        of metadata, if metaname is provided, or the whole metadata dictionary
        for that variable if it is not.
        """
        dct = {'low':self.low,
               'high':self.high,
               'start':self.start,
               'scaler':self.scaler,
               'adder':self.adder,
               'fd_step':self.fd_step,
               'name':self.name}

        if metaname is not None:
            val = dct.get(metaname, __missing)
            if val is __missing:
                val = None
            return [(p.target, val) for p in self._params]
        else:
            return [(p.target, dct) for p in self._params]

    def get_referenced_compnames(self):
        """Return a set of Component names based on the 
        pathnames of Variables referenced in our target strings. 
        """
        result = set()
        for param in self._params:
            result.update(param.get_referenced_compnames())
        return result

    def get_referenced_vars_by_compname(self): 
        result = dict()
        for param in self._params: 
            comp = param.get_referenced_compnames().pop()
            try: 
                result[comp].update([param,])
            except KeyError: 
                result[comp] = set([param,])
        return result        

    def get_referenced_varpaths(self):
        """Return a set of Variable names referenced in our target strings."""
        result = set()
        for param in self._params:
            result.update(param.get_referenced_varpaths())
        return result

    def copy(self):
        return ParameterGroup([p.copy() for p in self._params])

    def get_config(self):
        return [p.get_config() for p in self._params]

    def _get_scope(self):
        return self._params[0]._get_scope()

    def override(self, low=None, high=None, 
                 scaler=None, adder=None, start=None,
                 fd_step=None, name=None):
        if low is not None:
            self.low = low
        if high is not None:
            self.high = high
        if scaler is not None:
            self.scaler = scaler
        if start is not None:
            self.start = start
        if fd_step is not None: 
            self.fd_step = fd_step
        if name is not None:
            self.name = name

        if self.pcomp_name:
            for param in self._params: 
                param.override(low, high, scaler, adder, start,
                               fd_step, name)

    def activate(self, scope, workflow=None):
        """Make this parameter active by creating the appropriate pseudocomp
        connections in the dependency graph.  The pseudocomponent is created
        if it doesn't already exist.
        """
        if self.pcomp_name is None:
            param0 = self._params[0]
            pseudo = ParamPseudoComponent(param0)
            self.pcomp_name = pseudo.name
            scope.add(pseudo.name, pseudo)
            param0.pcomp_name = pseudo.name
            for param in self._params[1:]:
                pseudo.add_target(param._expreval.text)
                param.pcomp_name = pseudo.name

        getattr(scope, self.pcomp_name).make_connections(workflow)

        self.initialize(scope)

    def deactivate(self, scope, workflow=None):
        """Make this parameter inactive by disconnecting it in the
        dependency graph and removing its callback from the target
        component.
        """
        if self.pcomp_name is None:
            return
        else:
            pseudo = getattr(scope, self.pcomp_name)
            pseudo.remove_connections(workflow)
            scope.remove(self.pcomp_name)
            self.pcomp_name = None
            for param in self._params:
                param.pcomp_name = None

    def initialize(self, scope):
        self._params[0].initialize(scope)



class HasParameters(object): 
    """This class provides an implementation of the IHasParameters interface."""

    _do_not_promote = ['get_expr_depends', 'get_referenced_compnames', 
                       'get_referenced_varpaths', 'get_metadata']

    def __init__(self, parent):
        self._parameters = ordereddict.OrderedDict()
        self._parent = parent
        self._allowed_types = ['continuous']

    def _item_count(self):
        """This is used by the replace function to determine if a delegate from the
        target object is 'empty' or not.  If it's empty, it's not an error if the
        replacing object doesn't have this delegate.
        """
        return len(self._parameters)

    def add_parameter(self, target, low=None, high=None, 
                      scaler=None, adder=None, start=None,
                      fd_step=None, name=None, scope=None):
        """Adds a parameter or group of parameters to the driver.

        target: string or iter of strings or Parameter
            What the driver should vary during execution. A *target* is an expression
            that can reside on the left-hand side of an assignment statement, so 
            typically it will be the name of a variable or possibly a subscript 
            expression indicating an entry within an array variable, e.g., x[3].
            If an iterator of targets is given, then the driver will set all targets given
            to the same value whenever it varies this parameter during execution.
            If a Parameter instance is given, then that instance is copied into the driver
            with any other arguments specified, overiding the values in the given parameter. 

        low: float (optional)
            Minimum allowed value of the parameter. If scaler and/or adder
            is supplied, use the transformed value here.

        high: float (optional)
            Maximum allowed value of the parameter. If scaler and/or adder
            is supplied, use the transformed value here.

        scaler: float (optional)
            Value to multiply the possibly offset parameter value by. 

        adder: float (optional)
            Value to add to parameter prior to possible scaling.

        start: any (optional)
            Value to set into the target or targets of a parameter before starting 
            any executions. If not given, analysis will start with whatever values
            are in the target or targets at that time. 

        fd_step: float (optional)
            Step-size to use for finite difference calculation. If no value is
            given, the differentitator will use its own default.

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

        if self._parent.parent:
            parent_cnns = self._parent.parent.list_connections()
            for lhs, rhs in parent_cnns:
                if rhs == target:
                    self._parent.raise_exception("'%s' is already a Parameter target" % target,
                                                 RuntimeError)

        if isinstance(target, (Parameter, ParameterGroup)): 
            self._parameters[target.name] = target
            target.override(low, high, scaler, adder, start, fd_step, name)
            target.deactivate(self._get_scope(scope))
        else:     
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

            if key in self._parameters:
                self._parent.raise_exception("%s is already a Parameter" % key,
                                             ValueError)
            try:
                if len(names) == 1:
                    self._parameters[key] = target = \
                        Parameter(names[0], low=low, high=high,
                                  scaler=scaler, adder=adder, start=start,
                                  fd_step=fd_step, name=key,
                                  scope=self._get_scope(scope))
                else:  # defining a ParameterGroup
                    parameters = [Parameter(n, low=low, high=high,
                                            scaler=scaler, adder=adder, 
                                            start=start,
                                            fd_step=fd_step, name=key,
                                            scope=self._get_scope(scope))
                                  for n in names]
                    types = set([p.valtypename for p in parameters])
                    if len(types) > 1: 
                        raise ValueError("Can't add parameter %s because "
                                         "%s are not all of the same type" %
                                         (key," and ".join(names)))
                    target = ParameterGroup(parameters)
                    self._parameters[key] = target
            except Exception as err:
                self._parent.raise_exception(str(err), type(err))

        if hasattr(self._parent, 'workflow'):
            workflow = self._parent.workflow
        else:
            workflow = None
        target.activate(self._get_scope(scope), workflow)

        self._parent._invalidate()

    def remove_parameter(self, name):
        """Removes the parameter with the given name."""
        param = self._parameters.get(name)
        if param:
            param.deactivate(self._get_scope())
            del self._parameters[name]
        else:
            self._parent.raise_exception("Trying to remove parameter '%s' "
                                         "that is not in this driver." % (name,),
                                         AttributeError)
        self._parent._invalidate()

    def get_references(self, name):
        """Return references to component `name` in preparation for subsequent
        :meth:`restore_references` call.

        name: string
            Name of component being removed.
        """
        # Just returning everything for now.
        return self._parameters.copy()

    def remove_references(self, name):
        """Remove references to component `name`.

        name: string
            Name of component being removed.
        """
        for pname, param in self._parameters.items():
            if name in param.get_referenced_compnames():
                self.remove_parameter(pname)

    def restore_references(self, refs, name):
        """Restore references to component `name` from `refs`.

        name: string
            Name of component being removed.

        refs: object
            Value returned by :meth:`get_references`.
        """
        # Not exactly safe here...
        if isinstance(refs, ordereddict.OrderedDict):
            self._parameters = refs
        else:
            raise TypeError('refs should be ordereddict.OrderedDict, got %r'
                            % refs)

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
        for name in self._parameters.keys():
            self.remove_parameter(name)
        self._parameters = ordereddict.OrderedDict()
        self._parent._invalidate()

    def get_parameters(self):
        """Returns an ordered dict of parameter objects."""
        return self._parameters

    def init_parameters(self): 
        """Sets all parameters to their start value if a 
        start value is given
        """ 
        for key,param in self._parameters.iteritems():
            if param.start is not None: 
                param.set(param.start, self._get_scope())
        self._parent._invalidate()

    def set_parameter_by_name(self, name, value, case=None, scope=None): 
        """Sets a single parameter by its name attribute.

        name: str
            Name of the parameter. This is either the name alias given when
            the parameter was added or the variable path of the parameter's
            target if no name was given.

        value: object (typically a float)
            Value of the parameter to be set.

        case: Case (optional)
            If supplied, the values will be associated with their corresponding
            targets and added as inputs to the Case instead of being set directly
            into the model.
        """

        param = self._parameters[name]
        if case is None:
            param.set(value, self._get_scope(scope))
        else:
            for target in param.targets:
                case.add_input(target, value)
            return case

    def set_parameters(self, values, case=None, scope=None): 
        """Pushes the values in the iterator 'values' into the corresponding 
        variables in the model.  If the 'case' arg is supplied, the values
        will be set into the case and not into the model.
        
        values: iterator
            Iterator of input values with an order defined to match the 
            order of parameters returned by the get_parameters method. All  
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

    def list_pseudocomps(self):
        """Returns a list of pseudocompont names associcated with our
        parameters.
        """
        return [p.pcomp_name for p in self._parameters.values()]

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

    def mimic(self, target):
        old = self._parameters
        self.clear_parameters()
        try:
            for name, p in target.get_parameters().items():
                self._parameters[name] = p.copy()
        except Exception:
            self._parameters = old
            raise
