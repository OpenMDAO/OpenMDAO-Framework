"""
Exception classes for OpenMDAO.
"""
import sys
from traceback import print_exception
from StringIO import StringIO

class ConstraintError(ValueError):
    """Raised when a constraint is violated."""
    pass
        
class CircularDependencyError(RuntimeError):
    """Raised when a circular dependency occurs."""
    pass
        
class RunInterrupted(RuntimeError):
    """Raised when *run()* was interrupted, implying an inconsistent state."""
    pass

class RunStopped(RuntimeError):
    """Raised when *run()* was stopped, implying a consistent state but
    not necessarily reflecting input values."""
    pass

class NoFlatError(TypeError):
    """Raised when a value is not flattenable to a 1D float array."""
    pass

    
def traceback_str(exc):
    """Call this to get the traceback string associated with the given exception
    or tuple of the form (exc_class, exc, traceback).
    Returns the exception string if there is no traceback.
    """
    if isinstance(exc, tuple) and len(exc) == 3:
        s = StringIO()
        print_exception(*exc, file=s)
        return s.getvalue().strip()

    try:
        return exc.traceback
    except AttributeError:
        return str(exc)

def exception_str(exc):
    """Call this to get the exception string associated with the given exception
    or tuple of the form (exc, traceback) or (exc_class, exc, traceback).
    """
    if isinstance(exc, tuple) and len(exc) == 3:
        return str(exc[1])

    return str(exc)
