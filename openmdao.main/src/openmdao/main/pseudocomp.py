
import ast

from openmdao.main.numpy_fallback import array

from openmdao.main.expreval import ConnectedExprEvaluator
from openmdao.main.printexpr import transform_expression, print_node
from openmdao.main.attrwrapper import create_attr_wrapper, UnitsAttrWrapper
from openmdao.main.sym import SymGrad
from openmdao.util.log import logger

from openmdao.units.units import PhysicalQuantity

def _get_varname(name):
    idx = name.find('[')
    if idx == -1:
        return name
    return name[:idx]


class UnitTransformer(ast.NodeTransformer):
    """Transforms varname in an expression into (varname*scaler+adder) if 
    a unit conversion is necessary. 
    """
    def __init__(self, meta, out_units):
        # Units are stored in a metadata dict and found in meta[varname]['units'].
        self.meta = meta
        self.out_pq = PhysicalQuantity(1.0, out_units)
        super(UnitTransformer, self).__init__()
        
    def visit_Name(self, node):
        """Given a name, return the proper node depending on whether
        the name refers to a variable that requires a unit conversion.
        """
        meta = self.meta.get(node.id)
        if meta is not None:
            units = meta.get('units')
            if units is not None:
                pq = PhysicalQuantity(1.0, units)
                scaler, adder = pq.unit.conversion_tuple_to(self.out_pq.unit)

                # construct new node of form '(name*scaler+adder)'
                newnode = ast.BinOp(node, ast.Mult(), ast.Num(scaler))
                if adder != 0.0: # do the multiply and the add
                    newnode = ast.BinOp(newnode, ast.Add(), 
                                        ast.Num(scaler*adder))
                return ast.copy_location(newnode, node)
        return node


    
class PseudoComponent(object):
    """A 'fake' component that is constructed from an ExprEvaluator.
    This fake component can be added to a dependency graph and executed
    along with 'real' components.
    """

    def __init__(self, name, parent, srcexpr, destexpr):
        self.name = name

        self._mapping = {}
        self._meta = {}
        self._valid = False
        self._parent = parent
        self._inputs = []

        varmap = {}
        for name, meta in srcexpr.get_metadata():
            self._meta[name] = meta

        for i,ref in enumerate(srcexpr.refs()):
            in_name = 'in%d' % i
            self._inputs.append(in_name)
            self._mapping[ref] = in_name
            varmap[_get_varname(ref)] = in_name
            setattr(self, in_name, None)

        self._outdest = destexpr.text

        refs = list(destexpr.refs())
        if refs:
            if len(refs) == 1:
                setattr(self, 'out0', None)
            else:
                raise RuntimeError("output of PseudoComponent must reference only one variable")

        # attach metadata to local var names
        newmeta = {}
        for key, val in self._meta.items():
            newmeta[varmap[key]] = val
            
        for name, meta in destexpr.get_metadata():
            self._meta[name] = meta
            
        newmeta['out0'] = self._meta[_get_varname(refs[0])]
        self._meta = newmeta

        xformed_src = transform_expression(srcexpr.text, self._mapping)
        xformed_dest = transform_expression(destexpr.text, { destexpr.text: 'out0'})

        out_units = self._meta['out0'].get('units')
        if out_units is not None:
            unitnode = ast.parse(xformed_src)
            unit_src = print_node(UnitTransformer(self._meta, out_units).visit(unitnode))
            xformed_src = unit_src

        self._srcexpr = ConnectedExprEvaluator(xformed_src, scope=self)
        self._destexpr = ConnectedExprEvaluator(xformed_dest, scope=self)

        # this is just the equation string (for debugging)
        self._eqn = "%s = %s" % (self._destexpr.text, self._srcexpr.text)

    def list_connections(self):
        """list all of the inputs and outputs of this comp.
        """
        conns = [(src, '.'.join([self.name, dest])) 
                     for src, dest in self._mapping.items()]
        conns.append(('.'.join([self.name, 'out0']), self._outdest))
        return conns

    def make_connections(self, parent):
        """Connect all of the inputs and outputs of this comp to
        the appropriate nodes in the dependency graph.
        """
        for src, dest in self.list_connections():
            parent._connect(src, dest)

    def invalidate_deps(self, varnames=None, force=False):
        self._valid = False
        return None

    def connect(self, src, dest):
        self._valid = False

    def run(self):
        if not self._valid:
            self._parent.update_inputs(self.name, None)

        src = self._srcexpr.evaluate()
        if isinstance(src, PhysicalQuantity):
            units = self._meta['out0'].get('units')
            if units is not None:
                src = src.in_units_of(units).value
            else:
                src = src.value
        self._destexpr.set(src)
        self._valid = True

    def update_outputs(self, names):
        self.run()

    def get(self, name, index=None):
        if index is not None:
            raise RuntimeError("index not supported in PseudoComponent.get")
        units = self._meta[name].get('units')
        if units is not None:
            val = getattr(self, name)
            if isinstance(val, PhysicalQuantity):
                return val
            return PhysicalQuantity(val, units)
        return getattr(self, name)

    def set(self, path, value, index=None, src=None, force=False):
        if index is not None:
            raise ValueError("index not supported in PseudoComponent.set")
        if isinstance(value, UnitsAttrWrapper):
            setattr(self, path, value.pq)
        elif isinstance(value, PhysicalQuantity):
            setattr(self, path, value.value)
        else:
            setattr(self, path, value)

    def get_wrapped_attr(self, name, index=None):
        if index is not None:
            raise RuntimeError("pseudocomponent attr accessed using an index")
        return create_attr_wrapper(getattr(self, name), self._meta[name])

    def get_metadata(self, traitpath, metaname=None):
        """Retrieve the metadata associated with the trait found using
        traitpath.  If metaname is None, return the entire metadata dictionary
        for the specified trait. Otherwise, just return the specified piece
        of metadata.  If the specified piece of metadata is not part of
        the trait, None is returned.
        """
        if metaname is None:
            return {}
        return None

    def get_valid(self, names):
        return [self._valid]*len(names)

    def calc_derivatives(self, first=False, second=False, savebase=False):
        if first:
            self.linearize()

    def linearize(self):
        """Calculate analytical first derivatives."""
        grad = self._srcexpr.evaluate_gradient()
        self.J = array([[grad[n] for n in self._inputs]])

    def provideJ(self):
        return tuple(self._inputs), ('out0',), self.J
