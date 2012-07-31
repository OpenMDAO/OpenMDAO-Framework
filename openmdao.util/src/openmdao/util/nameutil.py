import keyword
import re


def isidentifier(candidate):
    """Is the candidate string an identifier in Python 2.x
       ref: http://stackoverflow.com/questions/2544972/
    """
    is_not_keyword = candidate not in keyword.kwlist
    pattern = re.compile(r'^[a-z_][a-z0-9_]*$', re.I)
    matches_pattern = bool(pattern.match(candidate))
    return is_not_keyword and matches_pattern


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
