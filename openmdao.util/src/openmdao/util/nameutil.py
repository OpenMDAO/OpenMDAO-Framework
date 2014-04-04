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


def partition_names_by_comp(names, compmap=None, boundary_vars=()):
    """Take an iterator of names and return a dict with component names
    keyed to lists of variable names.  Simple names (having no '.' in them)
    will have a key of None.

    For example, the list ['abc.def', 'abc.pdq', 'foo', 'bar'] would return
    the dict { 'abc': ['def','pdq'], None: ['foo', 'bar'] }

    If a compmap dict is passed in, it will be populated with data from the
    iterator of names.

    boundary_vars is used to check for names that are boundary 
    VariableTree subvars so that they will be placed correctly in the
    None entry rather than causing the base name of the VariableTree
    to appear in the list of components.
    """
    if compmap is None:
        compmap = {}
    for name in names:
        parts = name.split('.', 1)
        if len(parts) == 1 or name in boundary_vars:
            compmap.setdefault(None, []).append(name)
        else:
            compmap.setdefault(parts[0], []).append(parts[1])
    return compmap


def partition_edges_by_comp(edges, compmap=None, boundary_vars=()):
    """Take an iterator of edges (u,v) and return a dict with component names
    keyed to lists of edges.  Simple names (having no '.' in them)
    will have a key of None.

    If a compmap dict is passed in, it will be populated with data from the
    iterator of edges.

    boundary_vars is used to check for names that are boundary 
    VariableTree subvars so that they will be keyed correctly to
    None rather than causing the base name of the VariableTree
    to appear in the list of components.
    """
    if compmap is None:
        compmap = {}
    for u, v in edges:
        uparts = u.split('.', 1)
        vparts = v.split('.', 1)
        if len(uparts) == 1 or u in boundary_vars:
            srccomp = None
        else:
            srccomp = uparts[0]

        if len(vparts) == 1 or v in boundary_vars:
            destcomp = None
        else:
            destcomp = vparts[0]

        compmap.setdefault((srccomp,destcomp), []).append((u,v))

    return compmap
