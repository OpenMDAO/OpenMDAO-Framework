
__all__ = ['TraitValMetaWrapper']

class TraitValMetaWrapper(object):
    """A class that encapsulates a trait value and any metadata necessary
    for validation of that trait.  For example, a TraitValMetaWrapper for
    a UnitsFloat object would include 'units' metadata to allow for unit
    compatability checking and conversion.
    """
    def __init__(self, value=None, **metadata):
        self.value = value
        self.metadata = metadata


