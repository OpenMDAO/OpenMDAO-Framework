
__all__ = ['AttrWrapper']

class AttrWrapper(object):
    """A class that encapsulates a value and any metadata necessary
    for validation of that value.  For example, an AttrWrapper for
    a Float object would include 'units' metadata to allow for unit
    compatability checking and conversion.
    """
    def __init__(self, value=None, **metadata):
        self.value = value
        self.metadata = metadata


