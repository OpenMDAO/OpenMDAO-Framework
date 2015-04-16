
import ast
import weakref

from numpy import ndarray, zeros

from openmdao.main.array_helpers import flattened_size, \
                                        flattened_value, get_val_and_index, get_index
from openmdao.main.derivatives import applyJ, applyJT
from openmdao.main.expreval import ExprEvaluator, ConnectedExprEvaluator, _expr_dict
from openmdao.main.interfaces import implements, IComponent, IContainer, IVariableTree, \
                                     IAssembly, IPseudoComp
from openmdao.main.printexpr import transform_expression, print_node
from openmdao.main.mp_support import has_interface
from openmdao.main.mpiwrap import MPI_info
from openmdao.main.index import deep_getattr

from openmdao.units.units import PhysicalQuantity, UnitsOnlyPQ
from openmdao.util.typegroups import int_types, complex_or_real_types



def _remove_spaces(s):
    return s.translate(None, ' \n\t\r')

def _get_new_name(parent):
    while not has_interface(parent, IAssembly):
        parent = parent.parent

    return parent.new_pseudo_name()

def _get_varname(name):
    return name.split('[', 1)[0]


def do_nothing_xform(node):
    return node


def scaler_adder_xform(node, scaler, adder):
    """Returns an ast for the form (node+adder)*scaler"""
    if adder != 0.0:
        newnode = ast.BinOp(node, ast.Add(), ast.Num(adder))
    else:
        newnode = node
    if scaler != 1.0:  # do the add and the mult
        newnode = ast.BinOp(newnode, ast.Mult(), ast.Num(scaler))
    return ast.copy_location(newnode, node)


def unit_xform(node, in_units, out_units):
    """Transforms an ast into expr*scaler+adder where scaler
    and adder are from units conversion.
    """
    inpq = PhysicalQuantity(1.0, in_units)
    outpq = PhysicalQuantity(1.0, out_units)
    try:
        scaler, adder = inpq.unit.conversion_tuple_to(outpq.unit)
    except TypeError:
        raise TypeError("units '%s' are incompatible with assigning units of"
                        " '%s'" % (inpq.get_unit_name(), outpq.get_unit_name()))
    return scaler_adder_xform(node, scaler, adder)


class DummyExpr(object):
    def __init__(self):
        self.text = ''

    def refs(self):
        return ['']

    def get_metadata(self):
        return [('', {})]


def _invert_dict(dct):
    out = {}
    for k, v in dct.items():
        out[v] = k
    return out


class PseudoComponent(object):
    """A 'fake' component that is constructed from an ExprEvaluator.
    This fake component can be added to a dependency graph and executed
    along with 'real' components.
    """
    implements(IComponent, IPseudoComp)

    def __init__(self, parent, srcexpr, destexpr=None,
                 translate=True, pseudo_type=None, subtype=None,
                 exprobject=None):
        if destexpr is None:
            destexpr = DummyExpr()
        self._parent = None
        self.parent = parent
        self.name = _get_new_name(parent)
        self._inmap = {}  # mapping of component vars to our inputs
        self._meta = {}
        self._inputs = []

        # Flags and caching used by the derivatives calculation
        self.force_fd = False
        self._provideJ_bounds = None

        self._pseudo_type = pseudo_type  # a string indicating the type of pseudocomp
                                         # this is, e.g., 'units', 'constraint', 'objective',
                                         # or 'multi_var_expr'
        self._subtype = subtype  # for constraints, 'equality' or 'inequality'

        self._exprobj = exprobject  # object responsible for creation of this pcomp, e.g. a Constraint

        self._orig_src = srcexpr.text
        self._orig_dest = destexpr.text
        self.Jsize = None
        self.mpi = MPI_info()

        varmap = {}
        rvarmap = {}
        for i, ref in enumerate(srcexpr.ordered_refs()):
            in_name = 'in%d' % i
            self._inputs.append(in_name)
            self._inmap[ref] = in_name
            varmap[ref] = in_name
            rvarmap.setdefault(_get_varname(ref), set()).add(ref)
            setattr(self, in_name, 0.)

        refs = list(destexpr.refs())
        if refs:
            if len(refs) == 1:
                setattr(self, 'out0', None)
            else:
                raise RuntimeError("output of PseudoComponent must reference"
                                   " only one variable")
        varmap[refs[0]] = 'out0'
        rvarmap.setdefault(_get_varname(refs[0]), set()).add(refs[0])

        noflat = False
        for name, meta in srcexpr.get_metadata():

            # If any input is noflat, then the output must be too.
            if 'noflat' in meta:
                noflat = True
            for rname in rvarmap[name]:
                self._meta[varmap[rname]] = meta

        for name, meta in destexpr.get_metadata():
            if noflat and 'noflat' not in meta:
                meta['noflat'] = True
            for rname in rvarmap[name]:
                self._meta[varmap[rname]] = meta

        if translate:
            xformed_src = transform_expression(srcexpr.text, self._inmap)
        else:
            xformed_src = srcexpr.text

        out_units = self._meta['out0'].get('units')
        pq = None
        if out_units is not None:
            # evaluate the src expression using UnitsOnlyPQ objects

            tmpdict = {}

            # First, replace values with UnitsOnlyPQ objects
            for inp in self._inputs:
                units = self._meta[inp].get('units')
                if units:
                    tmpdict[inp] = UnitsOnlyPQ(0., units)
                else:
                    tmpdict[inp] = 0.

            pq = eval(xformed_src, _expr_dict, tmpdict)
            self._srcunits = pq.unit

            unitnode = ast.parse(xformed_src)
            try:
                unitxform = unit_xform(unitnode, self._srcunits, out_units)
            except Exception as err:
                raise TypeError("Incompatible units for '%s' and '%s': %s"
                                % (srcexpr.text, destexpr.text, err))
            unit_src = print_node(unitxform)
            xformed_src = unit_src
        else:
            self._srcunits = None

        self._srcexpr = ConnectedExprEvaluator(xformed_src,
                                               scope=self)

        # this is just the equation string (for debugging)
        if self._orig_dest:
            self._outdests = [self._orig_dest]
            if pq is None:
                sunit = dunit = ''
            else:
                sunit = "'%s'" % pq.get_unit_name()
                dunit = "'%s'" % out_units
            self._orig_expr = "%s %s -> %s %s" % (self._orig_src, sunit,
                                                  self._orig_dest, dunit)
        else:
            self._outdests = []
            self._orig_expr = self._orig_src

        self.missing_deriv_policy = 'error'
        self._negate = False

    def __getstate__(self):
        state = self.__dict__.copy()
        state['_parent'] = self.parent
        return state

    def __setstate__(self, state):
        self.__dict__.update(state)
        self.parent = state['_parent']

    @property
    def parent(self):
        """ Our parent assembly. """
        return None if self._parent is None else self._parent()

    @parent.setter
    def parent(self, parent):
        self._parent = None if parent is None else weakref.ref(parent)

    def check_config(self, strict=False):
        pass

    def cpath_updated(self):
        pass

    def is_differentiable(self):
        """Return True if analytical derivatives can be
        computed for this Component.
        """
        return True

    def get_pathname(self, rel_to_scope=None):
        """ Return full pathname to this object, relative to scope
        *rel_to_scope*. If *rel_to_scope* is *None*, return the full pathname.
        """
        return '.'.join((self.parent.get_pathname(rel_to_scope), self.name))

    def list_connections(self, is_hidden=False, show_expressions=False):
        """list all of the inputs and output connections of this PseudoComponent.
        If is_hidden is True, list the connections that a user would see
        if this PseudoComponent is hidden. If show_expressions is True (and
        only if is_hidden is also True) then list the connection expression
        that resulted in the creation of this PseudoComponent.
        """
        if is_hidden:
            if self._outdests:
                if show_expressions:
                    return [(self._orig_src, self._orig_dest)]
                else:
                    return [(src, self._outdests[0])
                            for src in self._inmap.keys() if src]
            else:
                return []
        else:
            conns = [(src, '.'.join((self.name, dest)))
                     for src, dest in self._inmap.items()]
            if self._outdests:
                conns.extend([('.'.join((self.name, 'out0')), dest)
                              for dest in self._outdests])
        return conns

    def list_inputs(self, connected=True):
        return self._inputs[:]

    def list_outputs(self, connected=True):
        return ['out0']

    def config_changed(self, update_parent=True):
        pass

    def list_comp_connections(self):
        """Return a list of connections between our pseudocomp and
        parent components of our sources/destinations.
        """
        conns = [(src.split('.', 1)[0], self.name)
                 for src, dest in self._inmap.items()]
        if self._outdests:
            conns.extend([(self.name, dest.split('.', 1)[0])
                          for dest in self._outdests])
        return conns

    def contains(self, name):
        return name == 'out0' or name in self._inputs

    def activate(self, scope, driver=None):
        scope.add(self.name, self)
        scope._depgraph.add_component(self.name, self)
        getattr(scope, self.name).make_connections(scope, driver)

    def make_connections(self, scope, driver=None):
        """Connect all of the inputs and outputs of this comp to
        the appropriate nodes in the dependency graph.
        """
        for src, dest in self.list_connections():
            #scope.connect(src, dest)
            scope._depgraph.connect(scope, src, dest)

        if driver is not None:
            scope._depgraph.add_driver_input(driver.name,
                                             self.name+'.out0')

    def run(self, case_uuid=''):
        if self._negate:
            setattr(self, 'out0', -self._srcexpr.evaluate())
        else:
            setattr(self, 'out0', self._srcexpr.evaluate())

    def evaluate(self):
        if self._negate:
            setattr(self, 'out0', -self._srcexpr.evaluate())
        else:
            setattr(self, 'out0', self._srcexpr.evaluate())

    def get(self, name):
        return getattr(self, name)

    def set(self, path, value):
        setattr(self, path, value)

    def get_metadata(self, traitpath, metaname=None):
        if metaname is None:
            return self._meta[traitpath]
        childname, _, restofpath = traitpath.partition('.')
        if restofpath:
            return getattr(self, childname).get_metadata(restofpath, metaname)
        else:
            return self._meta[traitpath].get(metaname)

    def set_itername(self, itername):
        self._itername = itername

    def linearize(self, first=False, second=False):
        """Component wrapper for the ProvideJ hook."""
        if first:
            return self.provideJ()
        if second:
            msg = "2nd derivatives not supported in pseudocomponent %s"
            raise RuntimeError(msg % self.name)

    def provideJ(self):
        """Calculate analytical first derivatives."""

        if self.Jsize is None:
            n_in = 0
            n_out = 0
            for varname in self.list_inputs():
                val = self.get(varname)
                width = flattened_size(varname, val, self)
                n_in += width
            for varname in self.list_outputs():
                val = self.get(varname)
                width = flattened_size(varname, val, self)
                n_out += width
            self.Jsize = (n_out, n_in)

        J = zeros(self.Jsize)
        grad = self._srcexpr.evaluate_gradient()

        i = 0
        for varname in self._inputs:
            val = self.get(varname)
            width = flattened_size(varname, val, self)
            J[:, i:i+width] = grad[varname]
            i += width

        if self._negate:
            return -J
        else:
            return J

    def ensure_init(self):
        """Make sure our inputs and outputs have been
        initialized.
        """
        # set the current value of the connected variable
        # into our input
        for ref, in_name in self._inmap.items():
            setattr(self, in_name,
                    ExprEvaluator(ref).evaluate(self.parent))
            if has_interface(getattr(self, in_name), IContainer):
                getattr(self, in_name).name = in_name

        # set the initial value of the output
        outval = self._srcexpr.evaluate()
        setattr(self, 'out0', outval)

    def list_deriv_vars(self):
        return tuple(self._inputs), ('out0',)

    def get_req_cpus(self):
        return (1, 1)

    def setup_init(self):
        self.Jsize = None
        self._provideJ_bounds = None

    def init_var_sizes(self):
        self.ensure_init()

    def setup_depgraph(self, dgraph):
        pass

    def setup_systems(self):
        return ()

    def setup_communicators(self, comm, scope=None):
        self.mpi.comm = comm

    def setup_variables(self):
        pass

    def setup_sizes(self):
        pass

    def setup_vectors(self, arrays=None):
        pass

    def post_setup(self):
        pass

    def get_flattened_value(self, path):
        """Return the named value, which may include
        an array index, as a flattened array of floats.  If
        the value is not flattenable into an array of floats,
        raise a TypeError.
        """
        val, idx = get_val_and_index(self, path)
        return flattened_value(path, val)

    def set_flattened_value(self, path, value):
        val,rop = deep_getattr(self, path.split('[',1)[0])
        idx = get_index(path)
        if isinstance(val, int_types):
            pass  # fall through to exception
        if isinstance(val, complex_or_real_types):
            if idx is None:
                setattr(self, path, value[0])
                return
            # else, fall through to error
        elif isinstance(val, ndarray):
            if idx is None:
                setattr(self, path, value)
            else:
                val[idx] = value
            return
        elif IVariableTree.providedBy(val):
            raise NotImplementedError("no support for setting flattened values into vartrees")

        raise TypeError("%s: Failed to set flattened value to variable %s" % (self.name, path))

    def get_req_default(self, self_reqired=None):
        return []

    def _input_updated(self, name, fullpath=None):
        pass

    def get_full_nodeset(self):
        """Return the full set of nodes in the depgraph
        belonging to this component.
        """
        return set((self.name,))

    def applyJ(self, system, variables):
        """ Wrapper for component derivative specification methods.
        Forward Mode.
        """
        applyJ(system, variables)

    def applyJT(self, system, variables):
        """ Wrapper for component derivative specification methods.
        Adjoint Mode.
        """
        applyJT(system, variables)

    def raise_exception(self, msg, exception_class=Exception):
        """Raise an exception."""
        coords = ''
        obj = self
        while obj is not None:
            try:
                coords = obj.get_itername()
            except AttributeError:
                try:
                    obj = obj.parent
                except AttributeError:
                    break
            else:
                break
        if coords:
            full_msg = '%s (%s): %s' % (self.get_pathname(), coords, msg)
        else:
            full_msg = '%s: %s' % (self.get_pathname(), msg)
        raise exception_class(full_msg)


class SimpleEQConPComp(PseudoComponent):
    """ This is a simple pseudocomponent used to encapsulate expressions of
    the form comp1.x = comp2.y. A separate PComp was needed to efficiently
    calculate the derivatives, especially for vector inputs.
    """

    def provideJ(self):
        """No need to pre-calculate."""
        pass

    def apply_deriv(self, arg, result):
        """ Matrix vector product with the Jacobian.
        """

        if 'in0' in arg:
            if self._negate:
                result['out0'] -= arg['in0']
            else:
                result['out0'] += arg['in0']

        if 'in1' in arg:
            if self._negate:
                result['out0'] += arg['in1']
            else:
                result['out0'] -= arg['in1']

    def apply_derivT(self, arg, result):
        """ Matrix vector product with the transpose Jacobian.
        NOTE: This function is probably never called, since the Newton solve
        is always forward.
        """

        if 'in0' in result:
            if self._negate:
                result['in0'] -= arg['out0']
            else:
                result['in0'] += arg['out0']

        if 'in1' in result:
            if self._negate:
                result['in1'] += arg['out0']
            else:
                result['in1'] -= arg['out0']


class SimpleEQ0PComp(PseudoComponent):
    """ This is a simple pseudocomponent used to encapsulate expressions of
    the form comp1.x = 0. A separate PComp was needed to efficiently
    calculate the derivatives, especially for vector inputs.
    """

    def provideJ(self):
        """No need to pre-calculate."""
        pass

    def apply_deriv(self, arg, result):
        """ Matrix vector product with the Jacobian.
        """

        result['out0'][:] += arg['in0'][:]

    def apply_derivT(self, arg, result):
        """ Matrix vector product with the transpose Jacobian.
        NOTE: This function is probably never called, since the Newton solve
        is always forward.
        """

        result['in0'][:] += arg['out0'][:]


class UnitConversionPComp(PseudoComponent):
    """ This is a simple pseudocomponent used to encapsulate unit
    conversions. A separate PComp was needed to efficiently calculate the
    derivatives, especially for vector inputs.
    """

    def ensure_init(self):
        """Make sure our inputs and outputs have been
        initialized.
        """
        super(UnitConversionPComp, self).ensure_init()

        src    = PhysicalQuantity(1.0, self._srcunits)
        target = self._meta['out0'].get('units')
        src.convert_to_unit(target)
        self.grad = src.get_value()

    def provideJ(self):
        """No need to pre-calculate."""
        pass

    def apply_deriv(self, arg, result):
        """ Matrix vector product with the Jacobian.
        """

        result['out0'][:] += self.grad*arg['in0'][:]

    def apply_derivT(self, arg, result):
        """ Matrix vector product with the transpose Jacobian.
        """

        result['in0'][:] += self.grad*arg['out0'][:]
