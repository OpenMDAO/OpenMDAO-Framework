import os
import time
import shutil
import json
import tempfile
import webbrowser

from networkx.readwrite.json_graph import node_link_data
from networkx.algorithms.components import strongly_connected_components
from networkx.algorithms.dag import is_directed_acyclic_graph
import networkx as nx 

from openmdao.main.ndepgraph import is_boundary_node, is_input_node, \
                                    base_var

_excluded_nodes = set([
    'force_execute',
    'directory',
    'external_files',
    'log_level',
    'exec_count',
    'derivative_exec_count',
    'itername',
    'create_instance_dir',
    'printvars',
])

_excluded_node_data = set([
    'pa_object',
])

_excluded_link_data = set([
    'sexpr',
    'dexpr',
])

def _clean_graph(graph):
    """Return a cleaned version of the graph. Note that this
    should not be used for really large graphs because it 
    copies the entire graph.
    """
    # copy the graph since we're changing node/edge metadata
    graph = graph.subgraph(graph.nodes_iter())

    conns = graph.list_connections()

    conn_nodes = set([u.split('[',1)[0] for u,v in conns])
    conn_nodes.update([v.split('[',1)[0] for u,v in conns])

    nodes_to_remove = []
    for node, data in graph.nodes_iter(data=True):
        parts = node.split('.',1)
        if len(parts) == 1:
            name = node
        else:
            name = parts[1]
            if '[' not in node and node not in conn_nodes:
                nodes_to_remove.append(node)
                continue

        if name in _excluded_nodes:
            nodes_to_remove.append(node)
        else: # update node metadata
            newdata = data
            for meta in _excluded_node_data:
                if meta in newdata:
                    if newdata is data:
                        newdata = dict(data) # make a copy of metadata since we're changing it
                        graph.node[node] = newdata
                    del newdata[meta]

    graph.remove_nodes_from(nodes_to_remove)

    for u,v,data in graph.edges_iter(data=True):
        newdata = data
        for meta in _excluded_link_data:
            if meta in newdata:
                if newdata is data:
                    newdata = dict(data)
                    graph.edge[u][v] = newdata
                del newdata[meta]

    for node, data in graph.nodes_iter(data=True):
        parts = node.split('.', 1)
        if len(parts) == 1:
            data['short'] = node
        else:
            data['short'] = parts[1]

    return graph

def approx_topo_sort(graph):
    """Return a topological-ish sort of the graph. if
    cycles are found, break them and then do the topo sort.
    """
    severed = []
    try:
        while not is_directed_acyclic_graph(graph):
            strong = strongly_connected_components(graph)
            for comps in strong:
                if len(comps) > 1:
                    severed.append((comps[0], 
                                    comps[1], 
                                    graph.edge[comps[0]][comps[1]]))
                    graph.remove_edge(comps[0], comps[1])
        return nx.topological_sort(graph)
    finally:
        for u,v,data in severed:
            graph.add_edge(u, v, **data)

def set_comp_position(graph, cname):
    """Sets x and y for comp and all of its
    vars and subvars.
    """
    pass

def set_layout(graph, xmax=960., ymax=500.):
    """Set the layout of the graph."""
    # boundary inputs first
    # pick longest path between boundary in and boundary out
    # grow outward from that path
    # boundary outputs last

    # find boundary vars
    bin_vars = []
    bout_vars = []
    for node in graph.nodes_iter():
        if node.startswith('@in'):
            bin_vars.append(node)
        elif node.startswith('@out'):
            bout_vars.append(node)
        elif is_boundary_node(graph, node):
            if is_input_node(graph, node):
                bin_vars.append(node)
            else:
                bout_vars.append(node)

    inbound_dy = float(ymax) / (len(bin_vars)+1)
    outbound_dy = float(ymax) / (len(bout_vars)+1)

    y = 0
    xin = float(xmax) * 0.09

    # set positions for the incoming boundary vars
    for bin in bin_vars:
        y += inbound_dy
        graph.node[bin]['x'] = xin
        graph.node[bin]['y'] = y

    # set positions for the outgoing boundary vars
    y = 0
    xout = xmax - xmax * 0.91
    for bout in bout_vars:
        y += outbound_dy
        graph.node[bout]['x'] = xout
        graph.node[bout]['y'] = y

    conns = graph.list_connections()
    nodes = set([u for u,v in conns])
    nodes.update([v for u,v in conns])
    bvars = set([base_var(graph, n) for n in nodes])
    nodes.update([n.split('.',1)[0] for n in nodes])
    nodes.update(bvars)

    cgraph = graph.component_graph()
    nodes.update(cgraph.nodes_iter())

    # this graph now has only nodes involved in connections
    graph = graph.subgraph(nodes)

    csort = approx_topo_sort(cgraph)

    ymid = float(ymax) / 2
    comp_dx = float(xmax) / (len(cgraph)+1)

    x = 0
    for comp in csort:
        x += comp_dx
        graph.node[comp]['x'] = x
        graph.node[comp]['y'] = ymid

    for comp in csort:
        succ = cgraph.successors(comp)
        if len(succ) > 1:
            dy = float(ymax) / (len(succ)+1)
            for i,s in enumerate(succ):
                graph.node[s]['y'] = i * dy

    cradius = 25
    cvar_dx = cradius/2. + 5 # x distance between comps and their vars
    cvar_dy = cradius/2. + 5 # y distance between comp vars on same side of a comp

    for node in bin_vars+bout_vars:
        x = graph.node[node]['x']
        y = graph.node[node]['y']
        children = graph._all_child_vars(node)
        child_y = y - (len(children) * cvar_dy) / 2.
        for v in children:
            if is_input_node(graph, node):
                graph.node[v]['x'] = x + cvar_dx
                graph.node[v]['y'] = child_y
                child_y += cvar_dy

    for node in csort:
        x = graph.node[node]['x']
        y = graph.node[node]['y']

        children = graph._all_child_vars(node)
        invars = [v for v in children if is_input_node(graph,v)]
        outvars = [v for v in children if v not in invars]
        ichild_y = y - (len(invars) * cvar_dy) / 2.
        ochild_y = y - (len(outvars) * cvar_dy) / 2.
        for v in invars:
            graph.node[v]['x'] = x - cvar_dx
            graph.node[v]['y'] = ichild_y
            ichild_y += cvar_dy

        for v in outvars:
            graph.node[v]['x'] = x + cvar_dx
            graph.node[v]['y'] = ochild_y
            ochild_y += cvar_dy

    # # check that all x,y values are set
    # for node, data in graph.nodes_iter(data=True):
    #     if 'x' not in data:
    #         print "x missing for node %s" % node
    #     if 'y' not in data:
    #         print "y missing for node %s" % node

    return graph

def plot_graph(graph, d3page='static_graph.html'):
    """Open up a display of the graph in a browser window."""

    tmpdir = tempfile.mkdtemp()
    fdir = os.path.dirname(os.path.abspath(__file__))
    shutil.copy(os.path.join(fdir, 'd3.js'), tmpdir)
    shutil.copy(os.path.join(fdir, d3page), tmpdir)

    graph = _clean_graph(graph)
    graph = set_layout(graph)
    data = node_link_data(graph)

    startdir = os.getcwd()
    os.chdir(tmpdir)
    try:
        # write out the json as a javascript var
        # so we we're not forced to start our own webserver
        # to avoid cross-site issues
        with open('__graph.js', 'w') as f:
            f.write("__mygraph__json = ")
            json.dump(data, f)
            f.write(";\n")

        # open URL in web browser
        wb = webbrowser.get()
        wb.open('file://'+os.path.join(tmpdir, d3page))
    finally:
        os.chdir(startdir)
        print "\nwaiting to remove temp directory '%s'... " % tmpdir
        time.sleep(10) # sleep to give browser time
                       # to read files before we remove them
        #shutil.rmtree(tmpdir)
        print "temp directory removed"





