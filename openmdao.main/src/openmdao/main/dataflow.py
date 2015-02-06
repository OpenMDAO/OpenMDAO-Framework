""" A workflow where the execution order is automatically inferred from the
data connections."""

from openmdao.main.sequentialflow import SequentialWorkflow
from openmdao.main.interfaces import IDriver
from openmdao.main.mp_support import has_interface
from openmdao.main.depgraph import gsort

__all__ = ['Dataflow']


class Dataflow(SequentialWorkflow):
    """
    A Dataflow consists of a collection of Components which are executed in
    data flow order.
    """
    pass