import os
import time
import shutil
import json
import tempfile
import webbrowser

from networkx.readwrite.json_graph import node_link_data

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

    nodes_to_remove = []
    for node, data in graph.nodes_iter(data=True):
        parts = node.split('.',1)
        if len(parts) == 1:
            name = node
        else:
            name = parts[1]

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

    return graph

def set_link_weights(graph):
    """Set the weight attribute of links between comps and their
    variable nodes, and var nodes to subvar nodes, to be higher
    than connection nodes.
    """
    pass

def plot_graph(graph):
    """Open up a display of the graph in a browser window."""

    tmpdir = tempfile.mkdtemp()
    fdir = os.path.dirname(os.path.abspath(__file__))
    shutil.copy(os.path.join(fdir, 'd3.js'), tmpdir)
    shutil.copy(os.path.join(fdir, 'forcegraph.html'), tmpdir)

    graph = _clean_graph(graph)
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
        wb.open('file://'+os.path.join(tmpdir,'forcegraph.html'))
    finally:
        os.chdir(startdir)
        print "\nwaiting to remove temp directory '%s'... " % tmpdir
        time.sleep(10) # sleep to give browser time
                       # to read files before we remove them
        shutil.rmtree(tmpdir)
        print "temp directory removed"





