from collections import OrderedDict
import weakref
import sys

from openmdao.main.datatypes.api import List, VarTree
from openmdao.main.expreval import ExprEvaluator
from openmdao.main.interfaces import obj_has_interface, ISolver, IDriver
from openmdao.main.variable import make_legal_path
from openmdao.main.vartree import VariableTree
from openmdao.main.depgraph import base_var

from openmdao.util.typegroups import real_types, int_types
from openmdao.util.graph import fix_single_tuple

from numpy import array, ndarray, ndindex, ones
from openmdao.main.mpiwrap import MPI

__missing = object()


class ParameterBase(object):
    """Abstract base class for parameters."""

    def __init__(self, target, high=None, low=None,
                 scaler=None, adder=None, start=None,
                 fd_step=None, scope=None, name=None,
                 _expreval=None):
        """If scaler and/or adder are not None, then high, low, and start, if
        not None, are assumed to be expressed in unscaled form. If high and low
        are not supplied, then their values will be pulled from the target
        variable (along with a start value), and are assumed to be in scaled
        form, so their values will be unscaled prior to being stored in the
        Parameter.
        """

        if scaler is None:
            scaler = 1.0
        if adder is None:
            adder = 0.0
        self._scaling_required = scaler != 1. or adder != 0.

        self.low = low
        self.high = high
        self.scaler = scaler
        self.adder = adder
        self.start = start
        self.fd_step = fd_step

        self.name = name or target

        if _expreval is None:
            try:
                _expreval = ExprEvaluator(target, scope)
            except Exception as err:
                raise err.__class__("Can't add parameter: %s" % str(err))
            if not _expreval.is_valid_assignee():
                raise ValueError("Can't add parameter: '%s' is not a valid"
                                 " parameter expression" % _expreval.text)
        self._expreval = _expreval

        try:
            self._metadata = self._expreval.get_metadata()
        except AttributeError:
            raise AttributeError("Can't add parameter '%s' because it doesn't"
                                 " exist." % target)

        # 'raw' metadata is in the form [(varname, metadata)],
        # so use [0][1] to get the actual metadata dict
        metadata = self._metadata[0][1]

        if 'iotype' in metadata and metadata['iotype'] == 'out':
            raise RuntimeError("Can't add parameter '%s' because '%s' is an"
                               " output." % (target, target))
        try:
            # So, our traits might not have a vartypename?
            self.vartypename = metadata['vartypename']
        except KeyError:
            self.vartypename = None

    def __str__(self):
        return self._expreval.text

    def _transform(self, val):
        """ Unscales the variable (parameter space -> var space). """
        if self._scaling_required:
            return (val + self.adder) * self.scaler
        else:
            return val

    def _untransform(self, val):
        """ Scales the variable (var space -> parameter space). """
        if self._scaling_required:
            return val / self.scaler - self.adder
        else:
            return val

    def _get_scope(self):
        """Return scope of target expression."""
        return self._expreval.scope

    @property
    def target(self):
        """The target of this parameter."""
        return self._expreval.text

    @property
    def targets(self):
        """A one element list containing the target of this parameter."""
        return [self._expreval.text]

    def initialize(self, scope, param_owner):
        """Set parameter to initial value."""
        if self.start is not None:
            start = self.start
            self.set(start, param_owner)

    def set(self, val, param_owner):
        """Assigns the given value to the target referenced by this parameter,
        must be overridden."""
        raise NotImplementedError('set')

    def get_metadata(self, metaname=None):
        """Returns a list of tuples of the form (varname, metadata), with one
        entry for each variable referenced by the parameter expression. The
        metadata value found in the tuple will be either the specified piece
        of metadata, if metaname is provided, or the whole metadata dictionary
        for that variable if it is not.
        """
        if metaname is None:
            return self._metadata[0]
        else:
            return [(name, self._metadata.get(metaname))
                    for name, val in self._metadata]

    def get_referenced_compnames(self):
        """Return a set of Component names based on the
        pathnames of Variables referenced in our target string.
        """
        return self._expreval.get_referenced_compnames()

    def get_referenced_varpaths(self, refs=False):
        """Return a set of Variable names referenced in our target string."""
        return self._expreval.get_referenced_varpaths(copy=False, refs=refs)

    def get_config(self):
        """Return configuration arguments."""
        return (self.target, self.low, self.high, self.fd_step,
                self.scaler, self.adder, self.start, self.name)


class Parameter(ParameterBase):
    """ A scalar parameter. """

    def __init__(self, target, high=None, low=None,
                 scaler=None, adder=None, start=None,
                 fd_step=None, scope=None, name=None,
                 _expreval=None, _val=None, _allowed_types=None):
        """If scaler and/or adder are not None, then high, low, and start, if
        not None, are assumed to be expressed in unscaled form. If high and low
        are not supplied, then their values will be pulled from the target
        variable (along with a start value), and are assumed to be in scaled
        form, so their values will be unscaled prior to being stored in the
        Parameter.
        """
        super(Parameter, self).__init__(target, high, low,
                                        scaler, adder, start,
                                        fd_step, scope, name,
                                        _expreval)
        if scaler is not None:
            try:
                scaler = float(scaler)
            except (TypeError, ValueError):
                raise ValueError("Bad value given for parameter's 'scaler'"
                                 " attribute.")
        if adder is not None:
            try:
                adder = float(adder)
            except (TypeError, ValueError):
                raise ValueError("Bad value given for parameter's 'adder'"
                                 " attribute.")
        if _val is None:
            try:
                _val = self._expreval.evaluate()
            except Exception:
                raise ValueError("Can't add parameter because I can't evaluate"
                                 " '%s'." % target)

        self.valtypename = type(_val).__name__

        if self.vartypename == 'Enum':
            return    # it's an Enum, so no need to set high or low

        if _allowed_types is None or 'any' not in _allowed_types:
            if not isinstance(_val, real_types) and \
               not isinstance(_val, int_types):
                raise ValueError("The value of parameter '%s' must be a real or"
                                 " integral type, but its type is '%s'." %
                                 (target, type(_val).__name__))

        # metadata is in the form (varname, metadata), so use [1] to get
        # the actual metadata dict
        metadata = self.get_metadata()[1]

        meta_low = metadata.get('low')  # this will be None if 'low' isn't there
        if meta_low is not None:
            if low is None:
                self.low = self._untransform(meta_low)
            elif low < self._untransform(meta_low):
                raise ValueError("Trying to add parameter '%s', but the lower"
                                 " limit supplied (%s) exceeds the built-in"
                                 " lower limit (%s)." % (target, low, meta_low))

        elif _allowed_types is None or \
             'unbounded' not in _allowed_types and 'any' not in _allowed_types:
            if low is None:
                raise ValueError("Trying to add parameter '%s', "
                                 "but no lower limit was found and no "
                                 "'low' argument was given. One or the "
                                 "other must be specified." % target)

        meta_high = metadata.get('high')  # will be None if 'high' isn't there
        if meta_high is not None:
            if high is None:
                self.high = self._untransform(meta_high)
            elif high > self._untransform(meta_high):
                raise ValueError("Trying to add parameter '%s', but the upper"
                                 " limit supplied (%s) exceeds the built-in"
                                 " upper limit (%s)."
                                 % (target, high, meta_high))

        elif _allowed_types is None or \
             'unbounded' not in _allowed_types and 'any' not in _allowed_types:
            if high is None:
                raise ValueError("Trying to add parameter '%s', "
                                 "but no upper limit was found and no "
                                 "'high' argument was given. One or the "
                                 "other must be specified." % target)

        if self.low > self.high:
            raise ValueError("Parameter '%s' has a lower bound (%s) that"
                             " exceeds its upper bound (%s)" %
                             (target, self.low, self.high))

    def __eq__(self, other):
        if not isinstance(other, Parameter):
            return False
        return (self._expreval,self.scaler,self.adder,self.low,self.high,self.fd_step,self.start,self.name) == \
               (other._expreval,other.scaler,other.adder,other.low,other.high,other.fd_step,other.start,self.name)

    def __repr__(self):
        return '<Parameter(target=%s,low=%s,high=%s,fd_step=%s,scaler=%s,adder=%s,start=%s,name=%s)>' % \
               self.get_config()

    @property
    def names(self):
        """A one element list containing the name of this parameter."""
        return [self.name]

    @property
    def size(self):
        """Total scalar items in this parameter."""
        return 1

    def configure(self):
        """Reconfigure from potentially changed target."""
        pass

    def get_high(self):
        """Returns upper limits as a sequence."""
        return [self.high]

    def get_low(self):
        """Returns lower limits as a sequence."""
        return [self.low]

    def get_fd_step(self):
        """Returns finite difference step size as a sequence."""
        return [self.fd_step]

    def evaluate(self, scope=None):
        """Returns the value of this parameter as a sequence."""
        return [self._untransform(self._expreval.evaluate(scope))]

    def set(self, val, param_owner):
        """Assigns the given value to the target of this parameter."""
        transval = self._transform(val)
        try:
            param_owner._system.vec['u'][self._expreval.text] = transval
        except (KeyError, AttributeError):
            self._expreval.set(transval)

    def copy(self):
        """Return a copy of this Parameter."""
        return Parameter(self._expreval.text,
                         high=self.high, low=self.low,
                         scaler=self.scaler, adder=self.adder,
                         start=self.start,
                         fd_step=self.fd_step,
                         scope=self._get_scope(), name=self.name)

    def override(self, low=None, high=None,
                 scaler=None, adder=None, start=None,
                 fd_step=None, name=None):
        """Called by add_parameter() when the target is this Parameter."""
        if low is not None:
            self.low = low
        if high is not None:
            self.high = high
        if scaler is not None:
            self.scaler = scaler
        if adder is not None:
            self.adder = adder
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
            if not isinstance(param, (Parameter, ArrayParameter)):
                raise ValueError("tried to add a non-Parameter object to a"
                                 " ParameterGroup")
        self._params = params[:]
        param0 = self._params[0]

        self.low = max([x.low for x in self._params])
        self.high = min([x.high for x in self._params])
        self.start = param0.start
        self.scaler = param0.scaler
        self.adder = param0.adder
        self.fd_step = param0.fd_step
        self.name = param0.name
        self.typename = param0.valtypename

    def __eq__(self, other):
        if not isinstance(other, ParameterGroup):
            return False
        return (self._params,self.low,self.high,self.start,self.scaler,self.adder,self.fd_step,self.name) == \
               (other._params,other.low,other.high,other.start,other.scaler,other.adder,other.fd_step,self.name)

    def __str__(self):
        return "%s" % self.targets

    def __repr__(self):
        return '<ParameterGroup(targets=%s,low=%s,high=%s,fd_step=%s,scaler=%s,adder=%s,start=%s,name=%s)>' % \
               (self.targets, self.low, self.high, self.fd_step, self.scaler,
                self.adder, self.start, self.name)

    @property
    def names(self):
        """A one element list containing the name of this parameter."""
        return self._params[0].names

    @property
    def size(self):
        """Total scalar items in this parameter."""
        return self._params[0].size

    @property
    def target(self):
        """The target of the first parameter in the group."""
        return self._params[0].target

    @property
    def targets(self):
        """A list containing the targets of this parameter."""
        return [p.target for p in self._params]

    def configure(self):
        """Reconfigure from potentially changed target."""
        for param in self._params:
            param.configure()

    def get_high(self):
        """Returns upper limits as a sequence."""
        return self._params[0].get_high()

    def get_low(self):
        """Returns lower limits as a sequence."""
        return self._params[0].get_low()

    def get_fd_step(self):
        """Returns finite difference step size as a sequence."""
        return self._params[0].get_fd_step()

    def set(self, value, param_owner):
        """Set all targets to the given value."""
        for param in self._params:
            param.set(value, param_owner)

    def evaluate(self, scope=None):
        """Return the value of the first parameter in our target list as a
        sequence. Values of all of our targets are assumed to be the same.
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
        """Return a mapping from component name to referencing parameters."""
        result = dict()
        for param in self._params:
            comp = param.get_referenced_compnames().pop()
            try:
                result[comp].update([param,])
            except KeyError:
                result[comp] = set([param,])
        return result

    def get_referenced_varpaths(self, refs=False):
        """Return a set of Variable names referenced in our target strings."""
        result = set()
        for param in self._params:
            result.update(param.get_referenced_varpaths(refs=refs))
        return result

    def copy(self):
        """Return a copy of this ParameterGroup."""
        return ParameterGroup([p.copy() for p in self._params])

    def get_config(self):
        """Return list of configuration argument tuples."""
        return [p.get_config() for p in self._params]

    def _get_scope(self):
        """Return scope of first parameter in group."""
        return self._params[0]._get_scope()

    def override(self, low=None, high=None,
                 scaler=None, adder=None, start=None,
                 fd_step=None, name=None):
        """Called by add_parameter() when the target is this ParameterGroup."""
        if low is not None:
            self.low = low
        if high is not None:
            self.high = high
        if scaler is not None:
            self.scaler = scaler
        if adder is not None:
            self.adder = adder
        if start is not None:
            self.start = start
        if fd_step is not None:
            self.fd_step = fd_step
        if name is not None:
            self.name = name

    def initialize(self, scope, param_owner):
        """Set parameter to initial value."""
        for param in self._params:
            param.initialize(scope, param_owner)


class ArrayParameter(ParameterBase):
    """A parameter whose target is an array. If scaler and/or adder are not
    None, then high, low, and start, if not None, are assumed to be expressed
    in unscaled form. If high and low are not supplied, then their values
    will be pulled from the target variable (along with a start value), and are
    assumed to be in scaled form, so their values will be unscaled prior to
    being stored in the ArrayParameter.
    """

    def __init__(self, target, high=None, low=None,
                 scaler=None, adder=None, start=None,
                 fd_step=None, scope=None, name=None,
                 _expreval=None, _val=None, _allowed_types=None):
        super(ArrayParameter, self).__init__(target, high, low,
                                             scaler, adder, start,
                                             fd_step, scope, name,
                                             _expreval)
        if _val is None:
            try:
                _val = self._expreval.evaluate()
            except Exception:
                raise ValueError("Can't add parameter because I can't evaluate"
                                 " '%s'." % target)

        self.valtypename = _val.dtype.name

        if _val.dtype.kind not in 'fi':
            raise TypeError('Only float or int arrays are supported')

        dtype = self.dtype = _val.dtype
        self.shape = _val.shape
        self._size = _val.size

        # Use scalar arithmetic for transform/untransform if possible.
        if scaler is None:
            self._scaler = 1.
        else:
            _scaler = self._convert_sequence(scaler, dtype)
            if isinstance(_scaler, ndarray):
                self._scaler = _scaler
            else:
                self._scaler = float(scaler)

        if adder is None:
            self._adder = 0.
        else:
            _adder = self._convert_sequence(adder, dtype)
            if isinstance(_adder, ndarray):
                self._adder = _adder
            else:
                self._adder = float(adder)

        high = self._convert_sequence(high, dtype)
        low  = self._convert_sequence(low, dtype)

        # metadata is in the form (varname, metadata), so use [1] to get
        # the actual metadata dict
        metadata = self.get_metadata()[1]
        meta_low  = self._convert_sequence(metadata.get('low'), dtype)
        meta_high = self._convert_sequence(metadata.get('high'), dtype)

        highs = []
        lows = []

        for i in range(_val.size):
            _high = self._fetch('high', high, i)
            _low  = self._fetch('low', low, i)

            if meta_low is not None:
                _meta_low  = self._fetch('meta_low', meta_low, i)
                if _low is None:
                    _low = self._untransform(_meta_low)
                elif _low < self._untransform(_meta_low):
                    raise ValueError("Trying to add parameter '%s', but the"
                                     " lower limit supplied (%s) exceeds the"
                                     " built-in lower limit (%s)."
                                     % (target, _low, _meta_low))

            elif _allowed_types is None or \
                 'unbounded' not in _allowed_types and 'any' not in _allowed_types:
                if _low is None:
                    raise ValueError("Trying to add parameter '%s', "
                                     "but no lower limit was found and no "
                                     "'low' argument was given. One or the "
                                     "other must be specified." % target)

            if meta_high is not None:
                _meta_high = self._fetch('meta_high', meta_high, i)
                if _high is None:
                    _high = self._untransform(_meta_high)
                elif _high > self._untransform(_meta_high):
                    raise ValueError("Trying to add parameter '%s', but the"
                                     " upper limit supplied (%s) exceeds the"
                                     " built-in upper limit (%s)."
                                     % (target, _high, _meta_high))

            elif _allowed_types is None or \
                 'unbounded' not in _allowed_types and 'any' not in _allowed_types:
                if high is None:
                    raise ValueError("Trying to add parameter '%s', "
                                     "but no upper limit was found and no "
                                     "'high' argument was given. One or the "
                                     "other must be specified." % target)

            if _low > _high:
                raise ValueError("Parameter '%s' has a lower bound (%s) that"
                                 " exceeds its upper bound (%s)"
                                 % (target, _low, _high))

            highs.append(_high)
            lows.append(_low)

        self._high = array(highs, dtype)
        self._low  = array(lows, dtype)

        _fd_step = self._convert_sequence(fd_step, dtype)
        if isinstance(_fd_step, ndarray):
            self._fd_step = _fd_step.ravel()
        else:
            self._fd_step = [fd_step] * _val.size

    @staticmethod
    def _convert_sequence(val, dtype):
        """Convert sequence to array of dtype."""
        if isinstance(val, (list, tuple)):
            val = array(val, dtype).ravel()
        elif isinstance(val, ndarray):
            val = val.ravel()
        return val

    def _fetch(self, attr, val, i):
        """Fetch value for ith element of val (if it's an array)."""
        if isinstance(val, ndarray):
            if val.size == self._size:
                return val.flat[i]
            else:
                raise ValueError('%r size is %s but parameter size is %s'
                                 % (attr, val.size, self._size))
        else:
            return val

    def __eq__(self, other):
        if not isinstance(other, ArrayParameter):
            return False
        return self.get_config() == other.get_config()

    def __repr__(self):
        return '<ArrayParameter(target=%s,low=%s,high=%s,fd_step=%s,scaler=%s,adder=%s,start=%s,name=%s)>' \
               % self.get_config()

    @property
    def names(self):
        """A list containing the names of this parameter's scalar items."""
        names = []
        for index in ndindex(*self.shape):
            index = ''.join(['[%s]' % i for i in index])
            names.append('%s%s' % (self.name, index))
        return names

    @property
    def size(self):
        """Total scalar items in this parameter."""
        return self._size

    def configure(self):
        """Reconfigure from potentially changed target."""
        val = self._expreval.evaluate()
        if val.shape == self.shape:
            return

        for attr in ('low', 'high', 'scaler', 'adder', 'start', 'fd_step'):
            if isinstance(getattr(self, attr), (list, tuple, ndarray)):
                raise RuntimeError("Parameter %s can't be reconfigured,"
                                   " '%s' was not specified as a scalar"
                                   % (self, attr))
        self.shape = val.shape
        self._size = val.size
        # .start, ._scaler, and ._adder are scalars.

        if self._low.size:
            self._low = array([self._low[0]] * val.size, self.dtype)
            self._high = array([self._high[0]] * val.size, self.dtype)
            self._fd_step = [self._fd_step[0]] * val.size
        else:  # Started with empty array.
            if self.low is None:
                metadata = self.get_metadata()[1]
                self._low = array([metadata.get('low')] * val.size, self.dtype)
            else:
                self._low = array([self.low] * val.size, self.dtype)
            if self.high is None:
                metadata = self.get_metadata()[1]
                self._high = array([metadata.get('high')] * val.size, self.dtype)
            else:
                self._high = array([self.high] * val.size, self.dtype)
            self._fd_step = [self.fd_step] * val.size

    def get_high(self):
        """Returns upper limits as a sequence."""
        return self._high

    def get_low(self):
        """Returns lower limits as a sequence."""
        return self._low

    def get_fd_step(self):
        """Returns finite difference step size as a sequence."""
        return self._fd_step

    def evaluate(self, scope=None):
        """Returns the value of this parameter as a sequence."""
        # Use .flatten() rather than .flat to force a copy.
        # Forcing a copy to isolate data ownership.
        return self._untransform(self._expreval.evaluate(scope)).flatten()

    def set(self, value, param_owner):
        """Assigns the given value to the array referenced by this parameter."""
        copied = False
        if isinstance(value, (list, tuple)):
            value = self._convert_sequence(value, self.dtype)
            copied = True
        if isinstance(value, ndarray):
            if value.size == self._size:
                value = value.reshape(self.shape)
                if not copied:
                    # Forcing a copy to isolate data ownership.
                    value = value.copy()
            else:
                raise ValueError('value size is %s but parameter size is %s'
                                 % (value.size, self._size))
        else:
            value = value * ones(self.shape, self.dtype)

        transval = self._transform(value)

        try:
            param_owner._system.vec['u'][self._expreval.text] = transval.flatten()
        except (KeyError, AttributeError):
            self._expreval.set(transval)

    def copy(self):
        """Return a copy of this parameter."""
        return ArrayParameter(self.target,
                              high=self.high, low=self.low,
                              scaler=self.scaler, adder=self.adder,
                              start=self.start, fd_step=self.fd_step,
                              scope=self._get_scope(), name=self.name)

    def override(self, low=None, high=None,
                 scaler=None, adder=None, start=None,
                 fd_step=None, name=None):
        """Called by add_parameter() when the target is this ArrayParameter."""
        self._override('low',     low)
        self._override('high',    high)
        self._override('scaler',  scaler)
        self._override('adder',   adder)
        self._override('start',   start)
        self._override('fd_step', fd_step)

        if name is not None:
            self.name = name

    def _override(self, attr, val):
        """Helper for override()."""
        if val is not None:
            val = self._convert_sequence(val, self.dtype)
            if isinstance(val, ndarray):
                if val.size != self._size:
                    raise ValueError('%s size is %s but parameter size is %s'
                                     % (attr, val.size, self._size))
                val = val.ravel()
            setattr(self, attr, val)  # Set 'external' attribute.
            if attr in ('low', 'high', 'fd_step'):  # Force array.
                if not isinstance(val, ndarray):
                    val = val * ones(self._size, self.dtype)
            setattr(self, '_'+attr, val)  # Set 'internal' attribute.


class HasParameters(object):
    """This class provides an implementation of the IHasParameters interface."""

    _do_not_promote = ['get_expr_depends', 'get_referenced_compnames',
                       'get_referenced_varpaths', 'get_metadata']

    def __init__(self, parent):
        self._parameters = OrderedDict()
        self._allowed_types = ['continuous']
        if obj_has_interface(parent, ISolver):
            self._allowed_types.append('unbounded')
        self._parent = None if parent is None else weakref.ref(parent)

    def __getstate__(self):
        state = self.__dict__.copy()
        state['_parent'] = self.parent
        return state

    def __setstate__(self, state):
        self.__dict__.update(state)
        parent = state['_parent']
        self._parent = None if parent is None else weakref.ref(parent)

    @property
    def parent(self):
        """ The object we are a delegate of. """
        return None if self._parent is None else self._parent()

    def _item_count(self):
        """This is used by the replace function to determine if a delegate from
        the target object is 'empty' or not.  If it's empty, it's not an error
        if the replacing object doesn't have this delegate.
        """
        return len(self._parameters)

    def add_parameter(self, target, low=None, high=None,
                      scaler=None, adder=None, start=None,
                      fd_step=None, name=None, scope=None):
        """Adds a parameter or group of parameters to the driver.

        target: string or iter of strings or Parameter
            What the driver should vary during execution. A *target* is an
            expression that can reside on the left-hand side of an assignment
            statement, so typically it will be the name of a variable or
            possibly a subscript expression indicating an entry within an array
            variable, e.g., x[3]. If an iterator of targets is given, then the
            driver will set all targets given to the same value whenever it
            varies this parameter during execution. If a Parameter instance is
            given, then that instance is copied into the driver with any other
            arguments specified, overiding the values in the given parameter.

        low: float (optional)
            Minimum allowed value of the parameter. If scaler and/or adder
            is supplied, use the transformed value here. If target is an array,
            this may also be an array, but must have the same size.

        high: float (optional)
            Maximum allowed value of the parameter. If scaler and/or adder
            is supplied, use the transformed value here. If target is an array,
            this may also be an array, but must have the same size.

        scaler: float (optional)
            Value to multiply the possibly offset parameter value by. If target
            is an array, this may also be an array, but must have the same size.

        adder: float (optional)
            Value to add to parameter prior to possible scaling. If target is
            an array, this may also be an array, but must have the same size.

        start: any (optional)
            Value to set into the target or targets of a parameter before
            starting any executions. If not given, analysis will start with
            whatever values are in the target or targets at that time. If target
            is an array, this may also be an array, but must have the same size.

        fd_step: float (optional)
            Step-size to use for finite difference calculation. If no value is
            given, the differentiator will use its own default. If target is an
            array, this may also be an array, but must have the same size.

        name: str (optional)
            Name used to refer to the parameter in place of the name of the
            variable referred to in the parameter string.
            This is sometimes useful if, for example, multiple entries in the
            same array variable are declared as parameters.

        scope: object (optional)
            The object to be used as the scope when evaluating the expression.

        If neither "low" nor "high" is specified, the min and max will
        default to the values in the metadata of the variable being
        referenced.
        """

        if isinstance(target, (ParameterBase, ParameterGroup)):
            self._parameters[target.name] = target
            target.override(low, high, scaler, adder, start, fd_step, name)
        else:
            if isinstance(target, basestring):
                names = [target]
                key = target
            elif len(target) == 1:
                names = target
                key = target[0]
            else:
                names = target
                key = tuple(target)

            if name is not None:
                key = name

            dups = set(self.list_param_targets()).intersection(names)
            if dups:
                self.parent.raise_exception("%s are already Parameter targets"
                                             % sorted(list(dups)), ValueError)

            if key in self._parameters:
                self.parent.raise_exception("%s is already a Parameter" % key,
                                            ValueError)
            try:
                _scope = self._get_scope(scope)
                if len(names) == 1:
                    target = self._create(names[0], low, high, scaler, adder,
                                          start, fd_step, key, _scope)
                else:  # defining a ParameterGroup
                    parameters = [self._create(n, low, high, scaler, adder,
                                               start, fd_step, key, _scope)
                                  for n in names]
                    types = set([p.valtypename for p in parameters])
                    if len(types) > 1:
                        raise ValueError("Can't add parameter %s because "
                                         "%s are not all of the same type" %
                                         (key, " and ".join(names)))
                    target = ParameterGroup(parameters)
                self._parameters[key] = target
            except Exception:
                self.parent.reraise_exception(info=sys.exc_info())

        if IDriver.providedBy(self.parent):
            self.parent.config_changed()

    def _create(self, target, low, high, scaler, adder, start, fd_step,
                key, scope):
        """ Create one Parameter or ArrayParameter. """
        try:
            expreval = ExprEvaluator(target, scope)
        except Exception as err:
            raise err.__class__("Can't add parameter: %s" % err)
        if not expreval.is_valid_assignee():
            raise ValueError("Can't add parameter: '%s' is not a"
                             " valid parameter expression"
                             % expreval.text)
        try:
            val = expreval.evaluate()
        except Exception as err:
            val = None  # Let Parameter code sort out why.

        name = key[0] if isinstance(key, tuple) else key

        if isinstance(val, ndarray):
            return ArrayParameter(target, low=low, high=high,
                                  scaler=scaler, adder=adder,
                                  start=start, fd_step=fd_step,
                                  name=name, scope=scope,
                                  _expreval=expreval, _val=val,
                                  _allowed_types=self._allowed_types)
        else:
            return Parameter(target, low=low, high=high,
                             scaler=scaler, adder=adder,
                             start=start, fd_step=fd_step,
                             name=name, scope=scope,
                             _expreval=expreval, _val=val,
                             _allowed_types=self._allowed_types)

    def remove_parameter(self, name):
        """Removes the parameter with the given name."""
        param = self._parameters.get(name)
        if param:
            del self._parameters[name]
        else:
            self.parent.raise_exception("Trying to remove parameter '%s' "
                                         "that is not in this driver."
                                         % (name,), AttributeError)

        if IDriver.providedBy(self.parent):
            self.parent.config_changed()

    def config_parameters(self):
        """Reconfigure parameters from potentially changed targets."""
        for param in self._parameters.values():
            param.configure()

    def get_references(self, name):
        """Return references to component `name` in preparation for subsequent
        :meth:`restore_references` call.

        name: string
            Name of component being removed.
        """
        refs = OrderedDict()
        for pname, param in self._parameters.items():
            if name in param.get_referenced_compnames():
                refs[pname] = param
        return refs

    def remove_references(self, name):
        """Remove references to component `name`.

        name: string
            Name of component being removed.
        """
        to_remove = []
        for pname, param in self._parameters.items():
            if name in param.get_referenced_compnames():
                to_remove.append(pname)

        for pname in to_remove:
            self.remove_parameter(pname)

    def restore_references(self, refs):
        """Restore references to component `name` from `refs`.

        refs: object
            Value returned by :meth:`get_references`.
        """
        for pname, param in refs.items():
            try:
                if param._expreval.check_resolve():
                    self.add_parameter(param)
                else:
                    raise AttributeError("'%s' doesn't exist." % pname)
            except Exception as err:
                self.parent._logger.warning("Couldn't restore parameter '%s': %s"
                                            % (pname, str(err)))

    def list_param_targets(self):
        """Returns a list of parameter targets. Note that this
        list may contain more entries than the list of Parameter,
        ParameterGroup, and ArrayParameter objects since ParameterGroup
        instances have multiple targets.
        """
        targets = []
        for param in self._parameters.values():
            targets.extend(param.targets)
        return targets

    def list_param_group_targets(self):
        """Returns a list of tuples that contain the targets for each
        parameter group.
        """
        targets = []
        for param in self.get_parameters().values():
            targets.append(tuple(param.targets))
        return targets

    def clear_parameters(self):
        """Removes all parameters."""
        for name in self._parameters.keys():
            self.remove_parameter(name)
        self._parameters = OrderedDict()

    def get_parameters(self):
        """Returns an ordered dict of parameter objects."""
        return self._parameters

    def total_parameters(self):
        """Returns the total number of values to be set."""
        return sum([param.size for param in self._parameters.values()])

    def init_parameters(self):
        """Sets all parameters to their start value if a
        start value is given
        """
        for param in self._parameters.itervalues():
            if param.start is not None:
                param.set(param.start, self.parent)

    def set_parameter_by_name(self, name, value, case=None):
        """Sets a single parameter by its name attribute.

        name: str
            Name of the parameter. This is either the name alias given when
            the parameter was added or the variable path of the parameter's
            target if no name was given.

        value: object (typically a float)
            Value of the parameter to be set.

        case: Case (optional)
            If supplied, the values will be associated with their corresponding
            targets and added as inputs to the Case instead of being set
            directly into the model.
        """
        param = self._parameters[name]
        if case is None:
            param.set(value, self.parent)
        else:
            for target in param.targets:
                case.add_input(target, value)
            return case

    def set_parameters(self, values, case=None, scope=None):
        """Pushes the values in the iterator 'values' into the corresponding
        variables in the model.  If the 'case' arg is supplied, the values
        will be set into the case and not into the model.

        values: iterator (must support slicing)
            Iterator of input values with an order defined to match the
            order of parameters returned by the get_parameters method. All
            'values' must support the len() function.

        case: Case (optional)
            If supplied, the values will be associated with their corresponding
            targets and added as inputs to the Case instead of being set
            directly into the model.
        """
        if len(values) != self.total_parameters():
            raise ValueError("number of input values (%s) != expected number of"
                             " values (%s)" %
                             (len(values), self.total_parameters()))
        if case is None:
            start = 0
            for param in self._parameters.values():
                size = param.size
                if size == 1:
                    param.set(values[start], self.parent)
                    start += 1
                else:
                    end = start + size
                    param.set(values[start:end], self.parent)
                    start = end
        else:
            start = 0
            for param in self._parameters.values():
                size = param.size
                if size == 1:
                    for target in param.targets:
                        case.add_input(target, values[start])
                    start += 1
                else:
                    end = start + size
                    for target in param.targets:
                        case.add_input(target, values[start:end])
                    start = end
            return case

    def eval_parameters(self, scope=None, dtype='d'):
        """Return evaluated parameter values.

        dtype: string or None
            If not None, return an array of this dtype. Otherwise just return
            a list (useful if parameters may be of different types).
        """
        result = []
        for param in self._parameters.values():
            result.extend(param.evaluate(scope))
        if dtype:
            result = array(result, dtype)
        return result

    def get_lower_bounds(self, dtype='d'):
        """Return lower bound values.

        dtype: string or None
            If not None, return an array of this dtype. Otherwise just return
            a list (useful if parameters may be of different types).
        """
        result = []
        for param in self._parameters.values():
            result.extend(param.get_low())
        if dtype:
            result = array(result, dtype)
        return result

    def get_upper_bounds(self, dtype='d'):
        """Return upper bound values.

        dtype: string or None
            If not None, return an array of this dtype. Otherwise just return
            a list (useful if parameters may be of different types).
        """
        result = []
        for param in self._parameters.values():
            result.extend(param.get_high())
        if dtype:
            result = array(result, dtype)
        return result

    def get_fd_steps(self, dtype='d'):
        """Return fd_step values, they may include None.

        dtype: string or None
            If not None, return an array of this dtype. Otherwise just return
            a list (useful if it's valid to have None for a step size).
        """
        result = []
        for param in self._parameters.values():
            result.extend(param.get_fd_step())
        if dtype:
            result = array(result, dtype)
        return result

    def get_expr_depends(self):
        """Returns a list of tuples of the form (src_comp_name, dest_comp_name)
        for each dependency introduced by a parameter.
        """
        conns = set()
        pname = self.parent.name
        for param in self._parameters.values():
            for cname in param.get_referenced_compnames():
                conns.add((pname, cname))
        return list(conns)

    def get_referenced_compnames(self):
        """Return a set of Component names based on the
        pathnames of Variables referenced in our target strings.
        """
        result = set()
        for param in self._parameters.values():
            result.update(param.get_referenced_compnames())
        return result

    def get_referenced_varpaths(self, refs=False):
        """Return a set of Variable names referenced in our target strings.
        """
        result = set()
        for param in self._parameters.values():
            result.update(param.get_referenced_varpaths(refs=refs))
        return result

    def _get_scope(self, scope=None):
        if scope is None:
            try:
                return self.parent.get_expr_scope()
            except AttributeError:
                pass
        return scope

    def mimic(self, target):
        old = self._parameters
        self.clear_parameters()
        try:
            for name, param in target.get_parameters().items():
                self._parameters[name] = param.copy()
        except Exception:
            self._parameters = old
            raise


class HasVarTreeParameters(HasParameters):
    """ Parameters associated with a case driver which has VarTree inputs. """

    def __init__(self, parent):
        super(HasVarTreeParameters, self).__init__(parent)
        self._allowed_types = ['any']

    def add_parameter(self, target, low=None, high=None,
                      scaler=None, adder=None, start=None,
                      fd_step=None, name=None, scope=None, case_inputs_iotype='in'):
        """Adds a parameter or group of parameters to the driver."""

        super(HasVarTreeParameters, self).add_parameter(
                  target, low, high, scaler, adder, start, fd_step, name, scope)

        if name is not None:
            path = name
        elif isinstance(target, basestring):
            path = target
        elif isinstance(target, Parameter):
            path = target.name or target.target
        else:
            path = target[0]

        path = make_legal_path(path)
        obj = self.parent
        names = ['case_inputs'] + path.split('.')
        for name in names[:-1]:
            if obj.get_trait(name):
                val = obj.get(name)
            else:
                val = VariableTree()
                obj.add_trait(name, VarTree(val, iotype=case_inputs_iotype, deriv_ignore=True))
            obj = val

        name = names[-1]
        obj.add_trait(name, List(iotype=case_inputs_iotype, deriv_ignore=True, noflat=True))

    def remove_parameter(self, name):
        """Removes the parameter with the given name."""
        super(HasVarTreeParameters, self).remove_parameter(name)

        if isinstance(name, basestring):
            path = name
        else:
            path = name[0]

        path = make_legal_path(path)
        obj = self.parent

        names = ['case_inputs'] + path.split('.')
        for name in names[:-1]:
            obj = obj.get(name)

        name = names[-1]
        obj.remove_trait(name)
