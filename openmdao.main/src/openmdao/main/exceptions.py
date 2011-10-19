"""
Exception classes for OpenMDAO.
"""

import traceback

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

class TracedError(Exception):
    """An exception that encapsulates another exception and its traceback"""
    def __init__(self, orig_exc, tback):
        self.traceback = tback.strip()
        self.orig_exc = orig_exc
    
    def __str__(self):
        return str(self.orig_exc)
    
    def __repr__(self):
        return "%s%s" % (self.__class__.__name__, self.args)
    
    def reraise(self, with_traceback=True):
        if with_traceback:
            raise self.orig_exc.__class__(self.traceback)
        else:
            raise self.orig_exc
    

def traceback_str(exc):
    """Call this to get the traceback string associated with the given exception.
    Returns the exception string if there is no traceback.
    """
    try:
        return exc.traceback
    except AttributeError:
        return str(exc)

