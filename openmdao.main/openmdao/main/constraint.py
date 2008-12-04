
#public symbols
__all__ = []
__version__ = "0.1"

from openmdao.main.exceptions import ConstraintError

class Constraint(object):
    def __init__(self):
        pass
    
    def test(self, value):
        """This should be overridden to perform a specific constraint test on the value."""
        raise ConstraintError('no constraint defined')
    
    
class MinConstraint(Constraint):
    def __init__(self, min):
        self.min = min
        
    def test(self, value):
        if value < self.min:
            raise ConstraintError("constraint '"+str(value)+" >= "+
                                  str(self.min)+"' has been violated")
        
class MaxConstraint(Constraint):
    def __init__(self, max):
        self.max = max        
        
    def test(self, value):
        if value > self.max:
            raise ConstraintError("constraint '"+str(value)+" <= "+
                                  str(self.max)+"' has been violated")
        
        
class MinLengthConstraint(Constraint):
    def __init__(self, minlen):
        self.minlen = minlen
        
    def test(self, value):
        length = len(value)
        if length < self.minlen:
            raise ConstraintError("length constraint '"+str(length)+" >= "+
                                  str(self.minlen)+"' has been violated")
        
class MaxLengthConstraint(Constraint):
    def __init__(self, minlen):
        self.maxlen = maxlen
        
    def test(self, value):
        length = len(value)
        if length > self.maxlen:
            raise ConstraintError("length constraint '"+str(length)+" <= "+
                                  str(self.maxlen)+"' has been violated")
        
        