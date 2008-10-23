

"""
Exception classes for OpenMDAO
"""

#public symbols
__all__ = []

__version__ = "0.1"


class ConstraintError(Exception):
    """Raised when a constraint is violated."""
    def __init__(self, msg):
        Exception.__init__(self, msg)
        
