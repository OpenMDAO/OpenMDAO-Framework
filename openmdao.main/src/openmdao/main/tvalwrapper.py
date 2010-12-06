
__all__ = ['TraitValWrapper']

class TraitValWrapper(object):
    """A class that encapsulates a trait value and any metadata necessary
    for validation of that trait.  For example, a TraitValWrapper for
    a Float object would include 'units' metadata to allow for unit
    compatability checking and conversion.
    """
    def __init__(self, value=None, **metadata):
        self.value = value
        self.metadata = metadata


