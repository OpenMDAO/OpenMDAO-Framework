"""
Classes used for constraint checking in Variables.
"""

#public symbols
__all__ = []
__version__ = "0.1"

from openmdao.main.exceptions import ConstraintError

class Constraint(object):
    """Base class for constraint objects that can be added to
    Variables. Constraint objects have one function, called
    'test' that raises a ConstraintError if the constraint
    is violated.
    """
    def __init__(self):
        pass
    
    def test(self, value):
        """This should be overridden to perform a specific 
        constraint test on the value.
        """
        raise ConstraintError('no constraint defined')
    
    
class MinConstraint(Constraint):
    """Constrains a value to be >= another value."""
    def __init__(self, min_allowed):
        super(MinConstraint, self).__init__()
        self.min = min_allowed
        
    def test(self, value):
        """Raise a ConstraintError if value is less than min allowed."""
        if value < self.min:
            raise ConstraintError("constraint '"+str(value)+" >= "+
                                  str(self.min)+"' has been violated")
        
class MaxConstraint(Constraint):
    """Constrains a value to be <= another value."""
    def __init__(self, max_allowed):
        super(MaxConstraint, self).__init__()
        self.max = max_allowed        
        
    def test(self, value):
        """Raise a ConstraintError if value is greater than max allowed."""
        if value > self.max:
            raise ConstraintError("constraint '"+str(value)+" <= "+
                                  str(self.max)+"' has been violated")
        
        
class MinLengthConstraint(Constraint):
    """Constrains the length of a value to be >= a value."""
    def __init__(self, minlen):
        super(MinLengthConstraint, self).__init__()
        self.minlen = minlen
        
    def test(self, value):
        """Raise a ConstraintError if length of value is less 
        than min allowed.
        """
        try:
            length = len(value)
        except TypeError:
            raise ConstraintError('length constraint violated. value ('+
                                  str(value)+') has no length')
        if length < self.minlen:
            raise ConstraintError("min length constraint '"+str(length)+" >= "+
                                  str(self.minlen)+"' has been violated")
        
class MaxLengthConstraint(Constraint):
    """Constrains the length of a value to be <= a value."""
    def __init__(self, maxlen):
        super(MaxLengthConstraint, self).__init__()
        self.maxlen = maxlen
        
    def test(self, value):
        """Raise a ConstraintError if length of value is greater 
        than max allowed.
        """
        try:
            length = len(value)
        except TypeError:
            raise ConstraintError('length constraint violated. value ('+
                                  str(value)+') has no length')
        if length > self.maxlen:
            raise ConstraintError("max length constraint '"+str(length)+" <= "+
                                  str(self.maxlen)+"' has been violated")
        
        