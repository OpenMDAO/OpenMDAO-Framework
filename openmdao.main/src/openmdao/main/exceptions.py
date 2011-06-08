"""
Exception classes for OpenMDAO.
"""

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

