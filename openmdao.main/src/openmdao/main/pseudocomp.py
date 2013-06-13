
import threading

from openmdao.main.expreval import ConnectedExprEvaluator
from openmdao.main.printexpr import transform_expression
from openmdao.main.attrwrapper import create_attr_wrapper
from openmdao.units.units import PhysicalQuantity

class PseudoComponent(object):
    """A 'fake' component that is constructed from an ExprEvaluator.
    This fake component can be added to a dependency graph and executed
    along with 'real' components.
    """

    _lock = threading.RLock()
    _count = 0

    def __init__(self, srcexpr, destexpr):
        with self._lock:
            self.name = '_%d' % self._count
            self._count += 1

        self._mapping = {}
        self._meta = {}

        if isinstance(srcexpr, basestring):
            srcexpr = ConnectedExprEvaluator(srcexpr, getter='get_wrapped_attr',
                                             scope=self)
        else:
            srcexpr.scope = self

        if isinstance(destexpr, basestring):
            destexpr = ConnectedExprEvaluator(destexpr, getter='get_wrapped_attr',
                                              is_dest=True, scope=self)
        else:
            destexpr.scope = self

        for name, meta in srcexpr.get_metadata():
            self._meta[name] = meta

        for name, meta in destexpr.get_metadata():
            self._meta[name] = meta

        for i,ref in enumerate(srcexpr.refs()):
            in_name = 'in%d' % i
            self._mapping[ref] = in_name
            setattr(self, in_name, None)

        refs = destexpr.refs()
        if refs:
            if len(refs) == 1:
                setattr(self, 'out0', None)
            else:
                raise RuntimeError("output of PseudoComponent must reference only one variable")

        # attach metadata to local var names
        newmeta = {}
        for key, val in self._meta.items():
            newmeta[self._mapping[key]] = val
        newmeta['out0'] = self._meta[refs[0]]
        self._meta = newmeta

        xformed_src = transform_expression(srcexpr.text, self._mapping)
        xformed_dest = transform_expression(destexpr.text, { destexpr.text: 'out0'})

        self._srcexpr = ConnectedExprEvaluator(xformed_src, scope=self)
        self._destexpr = ConnectedExprEvaluator(xformed_dest, scope=self)

    def make_connections(self, depgraph):
        """Connect all of the inputs and outputs of this comp to
        the appropriate nodes in the dependency graph.
        """
        pass

    def invalidate_deps(varnames, force=False):
        return None

    def run(self):
        # do we assume inputs are valid here?
        src = self._srcexpr.evaluate()
        if isinstance(src, PhysicalQuantity):
            units = self._meta['out0'].get('units')
            if units is not None:
                src = src.convert_to_unit(units)
            else:
                src = src.value
        self._destexpr.set(src)

    def get(self, name, index=None):
        if index is not None:
            raise RuntimeError("pseudocomponent attr accessed using an index")
        units = self._meta[name].get('units')
        if units is not None:
            return PhysicalQuantity(getattr(self, name), units)
        return getattr(self, name)

    def get_wrapped_attr(self, name, index=None):
        if index is not None:
            raise RuntimeError("pseudocomponent attr accessed using an index")
        return create_attr_wrapper(getattr(self, name), self._meta[name])

