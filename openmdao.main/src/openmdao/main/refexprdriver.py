#public symbols
__all__ = ["RefExprDriver"]

__version__ = "0.1"

import networkx as nx

from openmdao.main.interfaces import IDriver, IComponent, IAssembly
from openmdao.main.component import Component, STATE_WAITING, STATE_IDLE
from openmdao.main import Driver


class RefExprDriver(Driver):
    """A Driver that uses ExprEvaluators to set/get values in other components.
    This class handles registration of ExprEvaluators and automatically determines
    Component dependencies based on them."""
    
    def __init__(self, name, parent=None, doc=None):
        super(RefExprDriver, self).__init__(name, parent, doc=doc)
        self._ref_graph = nx.MultiDiGraph(name=name)
        self._ref_exprs = {}
        
    def _register_refexpr(self, refexpr, name):
        """Register an ExprEvaluator."""
        self._ref_exprs[name] = refexpr
    
    def get_ref_graph(self):
        """Return a graph showing dependencies based on reference expressions."""
        pass
    