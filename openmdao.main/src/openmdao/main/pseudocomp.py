
import threading

from openmdao.main.expreval import ConnectedExprEvaluator
from openmdao.main.printexpr import transform_expression


class PseudoComponent(object):
    """A 'fake' component that is constructed from an ExprEvaluator.
    This fake component can be added to a dependency graph and executed
    along with 'real' components.
    """

    _lock = threading.RLock()
    _count = 0

    def __init__(self, srcexpr, destexpr):
        with self._lock:
            self.name = '#%d' % self._count
            self._count += 1

        self._mapping = {}

        if isinstance(srcexpr, basestring):
            srcexpr = ConnectedExprEvaluator(srcexpr, scope=self)
        else:
            srcexpr.scope = self
        self._srcexpr = srcexpr

        if isinstance(destexpr, basestring):
            destexpr = ConnectedExprEvaluator(destexpr, scope=self)
        else:
            destexpr.scope = self
        self._destexpr = destexpr

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

        xformed = transform_expression(expr.text, self._mapping)

        self.expr = ExprEvaluator(xformed)

    def make_connections(self, depgraph):
        """Connect all of the inputs and outputs of this comp to
        the appropriate nodes in the dependency graph.
        """
        pass

    def invalidate_deps(varnames, force=False):
        return None

    def run(self):
        self.expr.evaluate(self)



class UnitsComponent(PseudoComponent):
    def __init__(self, unit_in, unit_out):
