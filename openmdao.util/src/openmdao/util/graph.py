import pprint
from ordereddict import OrderedDict

_missing = object()

import networkx as nx
try:
    import matplotlib.pyplot as plt
except ImportError:
    plt = None
from io import BytesIO


def graph_to_svg(g):
    """ return the SVG of a matplotlib figure generated from a graph
        ref: http://pig-in-the-python.blogspot.com/2012/09/
    """
    if not plt:
        return None
    fig = plt.figure(figsize=(8, 8))
    ax = fig.add_subplot(111)
    nx.draw_shell(g, ax=ax)
    output = BytesIO()
    fig.savefig(output, format='svg')
    plt.close(fig)
    return output.getvalue()


def edges_to_dict(edges, dct=None):
    """Take an iterator of edges and return an ordered dict
    of sources mapped to lists of destinations.
    """
    if dct is None:
        dct = OrderedDict()
    for u, v in edges:
        dct.setdefault(u, []).append(v)
    return dct


def nodes_matching_all(graph, **kwargs):
    """Return an iterator over nodes matching all kwargs names and values.
    For example, nodes_matching_all(G, valid=True, boundary=True) would
    return a list of all nodes that are marked as valid that
    are also boundary nodes.
    """
    for n, data in graph.node.iteritems():
        for arg, val in kwargs.items():
            if data.get(arg, _missing) != val:
                break
        else:
            yield n


def nodes_matching_some(graph, **kwargs):
    """Return an iterator over nodes matching at least one of
    the kwargs names and values. For
    example, nodes_matching_some(G, valid=True, boundary=True) would
    return a list of all nodes that either are marked as valid or nodes
    that are boundary nodes, or nodes that are both.
    """
    for n, data in graph.node.iteritems():
        for arg, val in kwargs.items():
            if data.get(arg, _missing) == val:
                yield n
                break


def edges_matching_all(graph, **kwargs):
    """Return an iterator over edges matching all kwargs names and
    values. For example, edges_matching_all(G, foo=True, bar=True) would
    return a list of all edges that are marked with True
    values of both foo and bar.
    """
    for u, v, d in graph.edges(data=True):
        for arg, val in kwargs.items():
            if d.get(arg, _missing) != val:
                break
        else:
            yield (u, v)


def edges_matching_some(graph, **kwargs):
    """Return an iterator over edges matching some kwargs names
    and values. For example, edges_matching_some(G, foo=True, bar=True)
    would return a list of all edges that are marked with True
    values of either foo or bar or both.
    """
    for u, v, d in graph.edges(data=True):
        for arg, val in kwargs.items():
            if d.get(arg, _missing) == val:
                yield (u, v)
                break

def flatten_list_of_iters(lst):
    """Returns a list of simple values, flattening
    any sub-lists or sub-tuples, or if the input is a
    string it just returns that string.  NOTE: this only
    goes down one level.
    """
    if isinstance(lst, basestring):
        return [lst]
    else:
        ret = []
        for entry in lst:
            if isinstance(entry, basestring):
                ret.append(entry)
            else:
                ret.extend(entry)
        return ret

# FIXME: move this somewhere else...
def list_deriv_vars(comp):
    """A wrapper around the call to list_deriv_vars on the given
    Component that checks the return value to make sure it's a
    tuple.
    """
    tup = orig_tup = comp.list_deriv_vars()
    if isinstance(tup, list):
        tup = tuple(tup)

    if not isinstance(tup, tuple) or len(tup) != 2:
        raise ValueError(comp.get_pathname()+
                         ": The return value of list_deriv_vars() was not a tuple "
                         "of the form (invars, outvars). Value returned was %s" % orig_tup)

    tup0 = tup[0]
    tup1 = tup[1]

    # catch the one item tuple missing comma problem
    if isinstance(tup0, basestring):
        tup0 = (tup0,)
    if isinstance(tup1, basestring):
        tup1 = (tup1,)

    return (tup0, tup1)
