
import ast
from threading import RLock

from openmdao.main.numpy_fallback import array

from openmdao.main.expreval import ConnectedExprEvaluator, _expr_dict
from openmdao.main.printexpr import transform_expression, print_node
from openmdao.main.attrwrapper import UnitsAttrWrapper
from openmdao.main.interfaces import implements, IComponent

from openmdao.units.units import PhysicalQuantity, UnitsOnlyPQ

_namelock = RLock()
_count = 0

def _remove_spaces(s):
    return s.translate(None, ' \n\t\r')

def _get_new_name():
    global _count
    with _namelock:
        name = "_pseudo_%d" % _count
        _count += 1
    return name

def _get_varname(name):
    idx = name.find('[')
    if idx == -1:
        return name
    return name[:idx]

def do_nothing_xform(node):
    return node

def scaler_adder_xform(node, scaler, adder):
    """Returns an ast for the form (node+adder)*scaler"""
    if adder != 0.0:
        newnode = ast.BinOp(node, ast.Add(), ast.Num(adder))
    else:
        newnode = node
    if scaler != 1.0: # do the add and the mult
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
        raise TypeError("units '%s' are incompatible with assigning units of '%s'" % (inpq.get_unit_name(), outpq.get_unit_name()))
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
    for k,v in dct.items():
        out[v] = k
    return out


class PseudoComponent(object):
    """A 'fake' component that is constructed from an ExprEvaluator.
    This fake component can be added to a dependency graph and executed
    along with 'real' components.
    """
    implements(IComponent)
    
    def __init__(self, parent, srcexpr, destexpr=None, translate=True, pseudo_type=None):
        if destexpr is None:
            destexpr = DummyExpr()
        self.name = _get_new_name()
        self._inmap = {} # mapping of component vars to our inputs
        self._meta = {}
        self._valid = False
        self._parent = parent
        self._inputs = []
        self._pseudo_type = pseudo_type # a string indicating the type of pseudocomp
                                        # this is, e.g., 'units', 'constraint', 'objective',
                                        # or 'multi_var_expr'
        if destexpr.text:
            self._outdests = [destexpr.text]
        else:
            self._outdests = []

        varmap = {}
        for i,ref in enumerate(srcexpr.refs()):
            in_name = 'in%d' % i
            self._inputs.append(in_name)
            self._inmap[ref] = in_name
            varmap[_get_varname(ref)] = in_name
            setattr(self, in_name, None)

        refs = list(destexpr.refs())
        if refs:
            if len(refs) == 1:
                setattr(self, 'out0', None)
            else:
                raise RuntimeError("output of PseudoComponent must reference only one variable")
        varmap[_get_varname(refs[0])] = 'out0'
        
        for name, meta in srcexpr.get_metadata():
            self._meta[varmap[name]] = meta

        for name, meta in destexpr.get_metadata():
            self._meta[varmap[name]] = meta
            
        if translate:
            xformed_src = transform_expression(srcexpr.text, self._inmap)
        else:
            xformed_src = srcexpr.text
 
        out_units = self._meta['out0'].get('units')
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
                raise TypeError("Incompatible units for '%s' and '%s': %s" % (srcexpr.text, 
                                                                    destexpr.text, err))
            unit_src = print_node(unitxform)
            xformed_src = unit_src
        else:
            self._srcunits = None

        self._srcexpr = ConnectedExprEvaluator(xformed_src, scope=self)

        # this is just the equation string (for debugging)
        if destexpr and destexpr.text:
            out = destexpr.text
        else:
            out = 'out0'
        if translate:
            src = transform_expression(self._srcexpr.text, 
                                       _invert_dict(self._inmap))
        else:
            src = self._srcexpr.text

        self._expr_conn = (src, out)  # the actual expression connection

    def check_configuration(self):
        pass

    def get_pathname(self, rel_to_scope=None):
        """ Return full pathname to this object, relative to scope
        *rel_to_scope*. If *rel_to_scope* is *None*, return the full pathname.
        """
        return '.'.join([self._parent.get_pathname(rel_to_scope), self.name])

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
                    return [self._expr_conn]
                else:
                    return [(src, self._outdests[0]) 
                               for src in self._inmap.keys() if src]
            else:
                return []
        else:
            conns = [(src, '.'.join([self.name, dest])) 
                         for src, dest in self._inmap.items()]
            if self._outdests:
                conns.extend([('.'.join([self.name, 'out0']), dest) 
                                           for dest in self._outdests])
        return conns
    
    def list_inputs(self):
        return self._inputs[:]
    
    def list_outputs(self):
        return ['out0']

    def list_comp_connections(self):
        """Return a list of connections between our pseudocomp and
        parent components of our sources/destinations.
        """
        conns = [(src.split('.',1)[0], self.name) 
                     for src, dest in self._inmap.items()]
        if self._outdests:
            conns.extend([(self.name, dest.split('.',1)[0]) 
                                    for dest in self._outdests])
        return conns

    def make_connections(self, scope):
        """Connect all of the inputs and outputs of this comp to
        the appropriate nodes in the dependency graph.
        """
        for src, dest in self.list_connections():
            scope.connect(src, dest)

    def remove_connections(self, scope):
        """Disconnect all of the inputs and outputs of this comp
        from other nodes in the dependency graph.
        """
        for src, dest in self.list_connections():
            scope.disconnect(src, dest)

    def invalidate_deps(self, varnames=None, force=False):
        self._valid = False
        return None

    def get_invalidation_type(self):
        return 'full'

    def connect(self, src, dest):
        self._valid = False

    def run(self, ffd_order=0, case_id=''):
        self.update_inputs()

        src = self._srcexpr.evaluate()
        setattr(self, 'out0', src)
        self._valid = True    
        self._parent.child_run_finished(self.name)
            
    def update_inputs(self, inputs=None):
        self._parent.update_inputs(self.name)
        
    def update_outputs(self, names):
        self.run()

    def get(self, name, index=None):
        if index is not None:
            raise RuntimeError("index not supported in PseudoComponent.get")
        return getattr(self, name)

    def set(self, path, value, index=None, src=None, force=False):
        if index is not None:
            raise ValueError("index not supported in PseudoComponent.set")
        self.invalidate_deps()
        setattr(self, path, value)

    def get_metadata(self, traitpath, metaname=None):
        if metaname is None:
            return {}
        return None

    def is_valid(self):
        return self._valid

    def set_itername(self, itername):
        self._itername = itername

    def calc_derivatives(self, first=False, second=False, savebase=False,
                         required_inputs=None, required_outputs=None):
        if first:
            self.linearize()
        if second:
            msg = "2nd derivatives not supported in pseudocomponent %s"
            raise RuntimeError(msg % self.name)

    def linearize(self):
        """Calculate analytical first derivatives."""
        grad = self._srcexpr.evaluate_gradient()
        self.J = array([[grad[n] for n in self._inputs]])

    def provideJ(self):
        return tuple(self._inputs), ('out0',), self.J
