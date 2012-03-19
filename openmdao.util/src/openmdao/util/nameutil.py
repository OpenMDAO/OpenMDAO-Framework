

def partition_names_by_comp(names, compmap=None):
    """Take an iterator of names and return a dict with component names
    keyed to lists of variable names.  Simple names (having no '.' in them)
    will have a key of None.
    
    For example, the list ['abc.def', 'abc.pdq', 'foo', 'bar'] would return
    the dict { 'abc': ['def','pdq'], None: ['foo', 'bar'] }
    
    If a compmap dict is passed in, it will be populated with data from the 
    iterator of names.
    """
    if compmap is None:
        compmap = {}
    for name in names:
        parts = name.split('.', 1)
        if len(parts) == 1:
            compmap.setdefault(None, []).append(name)
        else:
            compmap.setdefault(parts[0], []).append(parts[1])
    return compmap


