"""
Trait for a generic variable that has no validation.
"""

#public symbols
__all__ = ["Any"]

# pylint: disable-msg=E0611,F0401

from openmdao.main.variable import Variable

class Any(Variable):
    """A variable wrapper for a generic variable that has no validation..
       """
    
    def __init__(self, default_value=None, iotype=None, desc=None, 
                 **metadata):

        # Put iotype in the metadata dictionary
        if iotype is not None:
            metadata['iotype'] = iotype
            
        # Put desc in the metadata dictionary
        if desc is not None:
            metadata['desc'] = desc

        super(Any, self).__init__(default_value=default_value, **metadata)

